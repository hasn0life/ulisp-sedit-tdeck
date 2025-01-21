/*
  Based on:
  LispBox LispLibrary - Version 1.0 - June 2024
  Hartmut Grawe - github.com/ersatzmoco - June 2024

  Ported to the Lilygo T-Deck by hasn0life - Jan 2025

  This uLisp version licensed under the MIT license: https://opensource.org/licenses/MIT
*/

const char LispLibrary[] PROGMEM = R"lisplibrary(
;
; Extended ULOS functions
;
;
; Define a class
(defun class (&optional parent slots constructor)
  (let ((obj (when parent (list (cons 'parent parent)))))
  	(when (and constructor parent)
  		(when (symbolp parent) (setq parent (eval parent)))
  		(loop
	     (when (null parent) (return parent))
	     (unless (or (equal (search "_" (string (first parent))) 1) (search "parent" (string (first parent))) )
	      (push (cons (car (first parent)) (cdr (first parent))) obj))
	     (setq parent (cdr parent)))
  	)
    (loop
     (when (null slots) (return obj))
     (push (cons (first slots) (second slots)) obj)
     (setq slots (cddr slots)))
  )
)

; Get the value of a property slot in an instance/class or its parents
(defun gtv (obj slot)
  (when (symbolp obj) (setq obj (eval obj)))
  (let ((pair (assoc slot obj)))
    (if pair (cdr pair)
           (let ((p (cdr (assoc 'parent obj))))
             (and p (gtv p slot))))
  )
)

; Update a property in an instance/class
(defun stv (obj slot value)
  (when (symbolp obj) (setq obj (eval obj)))
  (let ((pair (assoc slot obj)))
    (when pair (setf (cdr pair) value))
  )
)

; Add property and method slots
(defun adp (obj slots)
	(let (newlist) 
    (loop
     (when (null slots) (return))
     (push (cons (first slots) (second slots)) newlist)
     (setq slots (cddr slots)))
    (set obj (append (eval obj) newlist)) 
  )
)

; Call a method in an object instance
;
(defun cmt (obj method &rest arguments) 
	(apply (eval (gtv obj method)) (append (list obj) arguments))
)

(defun write-text (str)
  (with-gfx (scr)
    (princ str scr)
  )
)

;
; LispBox screen editor
;
;
(defun se:init (sk)
	(case sk 
		(t 
		 (defvar se:code_col (class 'color '(red 220 green 220 blue 220)))
		 (defvar se:line_col (class 'color '(red 90 green 90 blue 90)))
		 (defvar se:border_col (class 'color '(red 63 green 40 blue 0)))
		 (defvar se:bg_col (class 'color '(red 0 green 0 blue 0)))
		 (defvar se:cursor_col (class 'color '(red 160 green 60 blue 0)))
		 (defvar se:emph_col (class 'color '(red 0 green 128 blue 0)))
		 (defvar se:alert_col (class 'color '(red 235 green 0 blue 0)))
		 (defvar se:input_col (class 'color '(red 220 green 220 blue 220)))
		)
	)

	(defvar se:origin (cons 34 18))
	(defvar se:txtpos (cons 0 0))
	(defvar se:txtmax (cons 47 21))
	(defvar se:offset (cons 0 0))
	(defvar se:scrpos (cons 0 0))
	(defvar se:lastc nil)
	(defvar se:funcname nil)
	(defvar se:filename nil)
	(defvar se:suffix nil)
	(defvar se:buffer ())
	(defvar se:curline "")
	(defvar se:tscale 1)
	(defvar se:leading (* 10 se:tscale))
	(defvar se:cwidth (* 6 se:tscale))
	(defvar se:openings ())
	(defvar se:closings ())
	(defvar se:lastmatch ())
	(defvar se:match nil)
	(defvar se:exit nil)

	
	(fill-screen)
	(draw-line 32 17 32 240 (cmt se:border_col '_to-16bit))
	(draw-line 0 239 320 239 (cmt se:border_col '_to-16bit))
	(set-cursor 0 0)
	(set-text-color (cmt se:bg_col '_to-16bit) (cmt se:cursor_col '_to-16bit))
	(write-text "   touchscreen+h Help")
)

(defun se:cleanup ()
	(makunbound 'se:buffer)
	(makunbound 'se:openings)
	(makunbound 'se:closings)
	(makunbound 'se:curline)
	(gc)
)

(defun se:hide-cursor ()
	(when se:lastmatch
		(let ((spos nil) (bpos (car se:lastmatch)) (br (cdr se:lastmatch)))
			(setf spos (se:calc-scrpos bpos))
			(set-cursor (car spos) (cdr spos)) 
			(set-text-color (cmt se:code_col '_to-16bit) (cmt se:bg_col '_to-16bit))
			(write-text (string br))
			(setf se:lastmatch nil)
		)
	)
	(when se:lastc 
		(set-cursor (car se:scrpos) (cdr se:scrpos)) 
		(set-text-color (cmt se:code_col '_to-16bit) (cmt se:bg_col '_to-16bit))
		(write-text (string se:lastc))
		(set-cursor 0 (cdr se:scrpos))
		(set-text-color (cmt se:line_col '_to-16bit))
		(write-text (string (1+ (cdr se:txtpos))))
	)
)
		
(defun se:show-cursor (&optional forceb)
	(let ((x (car se:txtpos))
		  (y (cdr se:txtpos))
		  (ox (car se:offset))
		  (oy (cdr se:offset))
		  (padx (car se:origin))
		  (pady (cdr se:origin))
		  (myc (code-char 32)))
		(setf se:curline (nth y se:buffer))
		(setf se:scrpos (se:calc-scrpos se:txtpos))
		(set-cursor (car se:scrpos) (cdr se:scrpos))
		(set-text-color (cmt se:code_col '_to-16bit) (cmt se:cursor_col '_to-16bit))
		#| check if cursor is within line string or behind last char |#
		(when (< x (length se:curline)) (setf myc (char se:curline x)))
		(setf se:lastc myc)
		(if forceb
			(progn
				(setf se:match t)
				(se:write-char (char-code myc))
				(setf se:match nil)
			)
			(se:write-char (char-code myc))
		)
		(set-cursor 0 (cdr se:scrpos))
		(set-text-color (cmt se:cursor_col '_to-16bit))
		(write-text (string (1+ y)))
	)
)

(defun se:toggle-match ()
	(keyboard-flush)
	(if se:match
		(progn
			(setf se:match nil) (setf se:openings ()) (setf se:closings ())
			(set-cursor 0 0)
			(set-text-color (cmt se:bg_col '_to-16bit) (cmt se:cursor_col '_to-16bit))
			(write-text "F1")
			(keyboard-flush)
		)
		(progn
			(setf se:match t)
			(set-cursor 0 0)
			(set-text-color (cmt se:bg_col '_to-16bit) (cmt se:emph_col '_to-16bit))
			(write-text "F1")
			(se:hide-cursor)
			(se:map-brackets)
			(keyboard-flush)
			(se:show-cursor)
		)
	)
)

(defun se:checkbr ()
	(keyboard-flush)
	(se:hide-cursor)
	(se:map-brackets t)
	(se:show-cursor t)
	(setf se:openings ())
	(setf se:closings ())
	(setf se:match nil)
	(set-cursor 0 0)
	(set-text-color (cmt se:bg_col '_to-16bit) (cmt se:cursor_col '_to-16bit))
	(write-text "F1")
	(keyboard-flush)
)

(defun se:map-brackets (&optional forcemp)
	(when (or se:match forcemp)
		(let ((myline "") (keys ()) (octr 0) (bl (length se:buffer)))
			(dotimes (y bl)
				(setf myline (nth y se:buffer))
				(dotimes (x (length myline))
					(when (equal (char-code (char myline x)) 40) (push (cons (cons x y) octr) se:openings) (push octr keys) (incf octr))
					(when (equal (char-code (char myline x)) 41) (push (cons (cons x y) (if keys (pop keys) nil)) se:closings))
				)
			)
		)
	)
)

(defun se:find-closing-bracket ()
	(let ((opentry (assoc* se:txtpos se:openings #'equal)) (clentry nil))
		(if opentry
			(progn
				(setf clentry (reverse-assoc* (cdr opentry) se:closings #'equal))
				(if clentry
					clentry
					nil
				)
			)
			nil
		)
	)
)

(defun se:find-opening-bracket ()
	(let ((clentry (assoc* se:txtpos se:closings #'equal)) (opentry nil))
		(if clentry
			(progn
				(setf opentry (reverse-assoc* (cdr clentry) se:openings #'equal))
				(if opentry
					opentry
					nil
				)
			)
			nil
		)
	)
)

(defun se:in-window (pos)
	(if (and (>= (car pos) (car se:offset)) (<= (car pos) (+ (car se:offset) (car se:txtmax))))
		(if (and (>= (cdr pos) (cdr se:offset)) (<= (cdr pos) (+ (cdr se:offset) (cdr se:txtmax))))
			t
			nil
		)
		nil
	)
)

(defun se:move-window (&optional forceshow)
	(let ((x (car se:txtpos))
		  (y (cdr se:txtpos))
		  (ox (car se:offset))
		  (oy (cdr se:offset))
		  (xm (car se:txtmax))
		  (ym (cdr se:txtmax)))
		(when (> x (+ ox xm))
			(setf (car se:offset) (+ (car se:offset) (- x (+ ox xm))))
			(setf forceshow t)
		)
		(when (> y (+ oy ym))
		    (setf (cdr se:offset) (+ (cdr se:offset) (- y (+ oy ym))))
			(setf forceshow t)
		)
		(when (< x ox)
			(setf (car se:offset) (- (car se:offset) (- ox x)))
			(setf forceshow t)
		)
		(when (< y oy)
			(setf (cdr se:offset) (- (cdr se:offset) (- oy y)))
			(setf forceshow t)
		)
		(when forceshow
			(se:show-text)
		)
	)
)

(defun se:calc-scrpos (tpos)
	(let ((sx 0) (sy 0) (ox (car se:offset)) (oy (cdr se:offset)) (padx (car se:origin)) (pady (cdr se:origin)))
		(setf sx (+ (* (- (car tpos) ox) se:cwidth) padx))
		(setf sy (+ (* (- (cdr tpos) oy) se:leading) pady))
		(cons sx sy)
	)
)

(defun se:calc-msgpos (tpos)
	(let ((sx 0) (sy 0) (padx (car se:origin)) (pady (cdr se:origin)))
		(setf sx (+ (* (car tpos) se:cwidth) padx 3))
		(setf sy (+ (* (cdr tpos) se:leading) pady))
		(cons sx sy)
	)
)

(defun se:write-char (cc)
	(let ((bpos nil) (spos nil))
		(cond
			((and (= cc 40) se:match)
				(setf bpos (se:find-closing-bracket))
				(when bpos
					(when (se:in-window bpos)
						(setf spos (se:calc-scrpos bpos))
						(set-cursor (car spos) (cdr spos))
						(set-text-color (cmt se:code_col '_to-16bit) (cmt se:emph_col '_to-16bit))
						(write-text ")")
						(setf se:lastmatch (cons bpos ")"))
					)
					(set-text-color (cmt se:code_col '_to-16bit) (cmt se:emph_col '_to-16bit))
				)
				(set-cursor (car se:scrpos) (cdr se:scrpos))
				(write-text (string (code-char cc)))
				(set-text-color (cmt se:code_col '_to-16bit) (cmt se:bg_col '_to-16bit))
			)
			((and (= cc 41) se:match)
				(setf bpos (se:find-opening-bracket))
				(when bpos
					(when (se:in-window bpos)
						(setf spos (se:calc-scrpos bpos))
						(set-cursor (car spos) (cdr spos))
						(set-text-color (cmt se:code_col '_to-16bit) (cmt se:emph_col '_to-16bit))
						(write-text "(")
						(setf se:lastmatch (cons bpos "("))
					)
					(set-text-color (cmt se:code_col '_to-16bit) (cmt se:emph_col '_to-16bit))
				)
				(set-cursor (car se:scrpos) (cdr se:scrpos))
				(write-text (string (code-char cc)))
				(set-text-color (cmt se:code_col '_to-16bit) (cmt se:bg_col '_to-16bit))
			)
			(t (write-text (string (code-char cc))))
		)
	)
)

(defun se:disp-line (y)
	(let ((ypos (+ (cdr se:origin) (* (- y (cdr se:offset)) se:leading))) (myl " "))
		(when (nth y se:buffer) (setf myl (concatenate 'string (nth y se:buffer) myl)))
		(set-text-color (cmt se:line_col '_to-16bit))
		(set-cursor 0 ypos)
		(write-text (string (1+ y)))

		(set-cursor (car se:origin) ypos)
		(set-text-color (cmt se:code_col '_to-16bit) (cmt se:bg_col '_to-16bit))
		(when (> (length myl) (car se:offset))
			(write-text (subseq myl (car se:offset) (min (length myl) (+ (car se:txtmax) (car se:offset) 1))))
		)
	)
)

(defun se:show-text ()
	(fill-rect 34 18 320 240 (cmt se:bg_col '_to-16bit))
	(fill-rect 0 18 33 240 (cmt se:bg_col '_to-16bit))
  (draw-line 33 17 33 240 (cmt se:border_col '_to-16bit))
	(let ((i 0) (ymax (min (cdr se:txtmax) (- (length se:buffer) (cdr se:offset) 1))))
		(loop
			(se:disp-line (+ i (cdr se:offset)))
			(when (= i ymax) (return))
			(incf i)
		)
	)
)

(defun se:show-dir ()
	(keyboard-flush)
	(se:hide-cursor)
	(fill-rect 34 18 320 240 (cmt se:bg_col '_to-16bit))
	(fill-rect 0 18 33 240 (cmt se:bg_col '_to-16bit))
	(set-text-color (cmt se:line_col '_to-16bit))
	(let ((spos (se:calc-scrpos (cons 0 0))))
		(set-cursor (car spos) (cdr spos))
		(write-text "/")
	)
	(let ((dirbuf (dir2)))
		(se:compile-dir dirbuf (cons 0 1))
	)
	(loop
		(when (keyboard-get-key t) (return))
	)
	(keyboard-flush)
	(se:show-text)
	(se:show-cursor)
)

(defun se:compile-dir (mydir dpos)
	(let ((spos nil))
		(loop
			(when (null mydir) (return (cons (- (car dpos) 3) (cdr dpos))))
			(let ((entry (pop mydir)))
				(unless  (> (cdr dpos) (cdr se:txtmax))
					(setf spos (se:calc-scrpos dpos))
					(set-cursor (car spos) (cdr spos))
					(incf (cdr dpos))
					(cond 
						((listp entry) (set-text-color (cmt se:code_col '_to-16bit))
							(write-text (car entry)))
						(t (set-text-color (cmt se:line_col '_to-16bit))
							(write-text entry)
							(setf dpos (se:compile-dir  (dir2 entry) (cons (+ 3 (car dpos)) (cdr dpos)))))
					)
				)
			)
		)
	)
)

(defun se:flush-buffer ()
	(keyboard-flush)
	(when (se:alert "Flush buffer")
		(se:hide-cursor)
		(setq se:buffer (list ""))
		(setf se:txtpos (cons 0 0))
		(setf se:offset (cons 0 0))
		(se:show-text)
		(se:show-cursor)
		(keyboard-flush))
)

(defun se:flush-line ()
	(keyboard-flush)
	(se:hide-cursor)
	(let* ((x (car se:txtpos))
	   (y (cdr se:txtpos))
	   (myl se:curline)
	   (firsthalf ""))
		(progn
			(setf firsthalf (subseq myl 0 x))
			(setf (nth y se:buffer) firsthalf)
			(setf se:curline (concatenate 'string (nth y se:buffer) " "))
			(setf se:lastc nil)
			(se:disp-line y)
		)
	)
	(se:map-brackets)
	(se:show-text)
	(se:show-cursor)
	(keyboard-flush)
)

(defun se:insert (newc)
	(se:hide-cursor)
	(let* ((x (car se:txtpos))
		   (y (cdr se:txtpos))
		   (myl se:curline)
		   (firsthalf "")
		   (scdhalf ""))
		(setf firsthalf (subseq myl 0 x))
		(setf scdhalf (subseq myl x (length myl)))
		(setf (nth y se:buffer) (concatenate 'string firsthalf (string newc) scdhalf))
		(incf (car se:txtpos))
		(setf se:curline (nth y se:buffer))
		(setf se:lastc nil)
		(if (> (car se:txtpos) (car se:txtmax)) (se:move-window) (se:disp-line y))
	)
	(se:map-brackets)
	(se:show-cursor)
)

(defun se:tab ()
	(keyboard-flush)
		(se:insert #\032)
		(se:insert #\032)
		(se:insert #\032)
		(se:insert #\032)
	(keyboard-flush)
)

(defun se:enter ()
	(se:hide-cursor)
	(let* ((x (car se:txtpos))
		   (y (cdr se:txtpos))
		   (myl se:curline)
		   (firsthalf "")
		   (scdhalf "")
		   (newl () ))
		(setf firsthalf (subseq myl 0 x))
		(setf scdhalf (subseq myl x (length myl)))
		(dotimes (i y)
			(push (nth i se:buffer) newl)
		)
		(push firsthalf newl)
		(push scdhalf newl)
		(dotimes (i (- (length se:buffer) (1+ y)))
			(push (nth (+ i (1+ y)) se:buffer) newl)
		)
		(setf se:buffer (reverse newl))
		(setf (car se:txtpos) 0)
		(incf (cdr se:txtpos))
		(setf se:curline (nth (1+ y) se:buffer))
		(setf se:lastc nil)
		(setf (car se:offset) 0)
		(se:move-window t)
	)
	(se:map-brackets)
	(se:show-cursor)
)

(defun se:delete ()
	(se:hide-cursor)
	(let* ((x (car se:txtpos))
		   (y (cdr se:txtpos))
		   (myl se:curline)
		   (firsthalf "")
		   (scdhalf ""))
		(if (> x 0)
			(progn
				(setf firsthalf (subseq myl 0 (1- x)))
				(setf scdhalf (subseq myl x (length myl)))
				(setf (nth y se:buffer) (concatenate 'string firsthalf scdhalf))
				(setf se:curline (concatenate 'string (nth y se:buffer) " "))
				(decf (car se:txtpos))
				(setf se:lastc nil)
				(se:disp-line y)
			)
			(when (> y 0)
				(setf scdhalf se:curline)
				(setf se:buffer (remove y se:buffer))
				(decf (cdr se:txtpos))
				(decf y)
				(setf se:curline (nth y se:buffer))
				(setf firsthalf se:curline)
				(if firsthalf
					(progn
						(setf (nth y se:buffer) (concatenate 'string firsthalf scdhalf))
						(setf (car se:txtpos) (length firsthalf))
					)
					(progn
						(setf (nth y se:buffer) scdhalf)
						(setf (car se:txtpos) 0)
					)
				)
				(se:move-window t)
			)
		)
	)
	(se:map-brackets)
	(se:show-cursor)
)

(defun se:left ()
	(se:hide-cursor)
	(cond
		#| xpos > 0 |#
		((> (car se:txtpos) 0) 
			(decf (car se:txtpos))
			(se:move-window)
		)
		#| xpos == 0, but ypos > 0 |#
		((> (cdr se:txtpos) 0)
			(decf (cdr se:txtpos))
			(setf (car se:txtpos) (length (nth (cdr se:txtpos) se:buffer)))
			(se:move-window)
		)
	)
	(se:show-cursor)
)

(defun se:right ()
	(se:hide-cursor)
	(cond
		#| xpos < eol |#
		((< (car se:txtpos) (length se:curline)) 
			(incf (car se:txtpos))
			(se:move-window)
		)
		#| xpos == eol, but ypos < end of se:buffer |#
		((< (cdr se:txtpos) (1- (length se:buffer)))
			(incf (cdr se:txtpos))
			(setf (car se:txtpos) 0)
			(se:move-window)
		)
	)
	(se:show-cursor) 
)

(defun se:up ()
	(se:hide-cursor)
	(cond
		#| ypos > 0 |#
		((> (cdr se:txtpos) 0) 
			(decf (cdr se:txtpos))
			(setf se:curline (nth (cdr se:txtpos) se:buffer))
			(when (> (car se:txtpos) (length se:curline)) 
				(setf (car se:txtpos) (length se:curline))
			)
			(se:move-window)
		)
	)
	(se:show-cursor)
)

(defun se:down ()
	(se:hide-cursor)
	(cond
		#| ypos < length of se:buffer |#
		((< (cdr se:txtpos) (1- (length se:buffer))) 
			(incf (cdr se:txtpos))
			(setf se:curline (nth (cdr se:txtpos) se:buffer))
			(when (> (car se:txtpos) (1+ (length se:curline))) (setf (car se:txtpos) (length se:curline)))
			(se:move-window)
		)
	)
	(se:show-cursor)
)

(defun se:linestart ()
	(se:hide-cursor)
	(setf (car se:txtpos) 0)
	(se:move-window)
	(se:show-cursor)
)

(defun se:lineend ()
	(se:hide-cursor)
	(setf (car se:txtpos) (length se:curline))
	(se:move-window)
	(se:show-cursor)
)

(defun se:nextpage ()
	(se:hide-cursor)
	(setf (cdr se:txtpos) (min (1- (length se:buffer)) (+ (cdr se:txtpos) (cdr se:txtmax) 1)))
	(se:move-window)
	(se:show-cursor)
)

(defun se:prevpage ()
	(se:hide-cursor)
	(setf (cdr se:txtpos) (max 0 (- (cdr se:txtpos) (cdr se:txtmax) 1)))
	(se:move-window)
	(se:show-cursor)
)

(defun se:docstart ()
	(se:hide-cursor)
	(setf se:txtpos (cons 0 0))
	(se:move-window)
	(se:show-cursor)
)

(defun se:run ()
	(let ((body "") (fname (se:input "Symbol name: " se:funcname 60)))
		(mapc (lambda (x) (setf body (concatenate 'string body x))) se:buffer)
		(if fname
			(when (se:alert (concatenate 'string "Bind code to symbol " fname " "))
				(eval (read-from-string (concatenate 'string (format nil "(defvar ~a" fname) (format nil " '~a)" body))))
				(se:msg "Done! Returning to REPL")
				(delay 2000)
				(se:clr-msg)
				(se:cleanup)
				(setf se:exit t)
			)
			(eval (read-from-string body))
		)
	)
)

(defun se:remove ()
	(let ((fname (se:input "DELETE file name: " nil 8 t)) (suffix (se:input "Suffix: ." "CL" 3 t)))
		(if (sd-file-exists (concatenate 'string "/" fname "." suffix))
			(when (se:alert (concatenate 'string "Delete file " fname "." suffix))
				(sd-file-remove (concatenate 'string "/" fname "." suffix))
				(se:msg "Done! Returning to editor.")
				(delay 1000)
				(se:clr-msg)
			)
		)
	)

)

(defun se:save ()
	(unless se:suffix (setf se:suffix "CL"))
	(let ((fname (se:input "SAVE file name: " se:filename 8 t)) (suffix (se:input "Suffix: ." se:suffix 3 t)) (overwrite t))
		(if (sd-file-exists (concatenate 'string "/" fname "." suffix))
			(unless (se:alert "File exists. Overwrite")
				(setf overwrite nil)
			)
		)
		(unless (or (< (length fname) 1) (< (length suffix) 1) (not overwrite))
			(with-sd-card (strm (concatenate 'string fname "." suffix) 2)
				(dolist (line se:buffer)
					(princ line strm)
					(princ (code-char 10) strm)
				)
			)
			(set-cursor (* 32 se:cwidth) 0)
			(set-text-color (cmt se:code_col '_to-16bit) (cmt se:cursor_col '_to-16bit))
			(setf se:filename fname)
			(setf se:suffix suffix)
			(write-text (concatenate 'string "FILE: " fname "." suffix "       "))
			(se:msg "Done! Returning to editor.")
			(delay 1000)
			(se:clr-msg)
		)
	)
)

(defun se:load ()
	(when (se:alert "Discard buffer and load from SD")
		(let ((fname (se:input "LOAD file name: " nil 8 t)) (suffix (se:input "Suffix: ." "CL" 3 t)) (line ""))
			(unless (or (< (length fname) 1) (< (length suffix) 1) (not (sd-file-exists (concatenate 'string "/" fname "." suffix))))
				(setq se:buffer ())
				(with-sd-card (strm (concatenate 'string "/" fname "." suffix) 0)
					(loop
						(setf line (read-line strm))
						(if line 
							(push line se:buffer)
							(return)
						)
					)
				)
				(setf se:buffer (reverse se:buffer))
				(se:hide-cursor)
				(se:map-brackets t)
				(set-cursor (* 36 se:cwidth) 0)
				(set-text-color (cmt se:code_col '_to-16bit) (cmt se:cursor_col '_to-16bit))
				(setf se:filename fname)
				(setf se:suffix suffix)
				(write-text (concatenate 'string "FILE: " fname "." suffix "       "))
				(setf se:txtpos (cons 0 0))
				(setf se:offset (cons 0 0))
				(se:show-text)
				(se:show-cursor)
			)
		)
	)
)

(defun se:alert (mymsg)
	(keyboard-flush)
	(se:msg (concatenate 'string mymsg " y/n ?") t)
	(let ((lk nil))
		(loop
			(when lk (return))
			(setf lk (keyboard-get-key t))
		)
		(se:clr-msg)
		(keyboard-flush)
		(if (or (= lk 121) (= lk 89))
			t
			nil
		)
	)
)

(defun se:msg (mymsg &optional alert cursor)
	(if alert
		(set-text-color (cmt se:alert_col '_to-16bit))
		(set-text-color (cmt se:emph_col '_to-16bit))
	)
  (fill-rect 33 50 280 110 (cmt se:bg_col '_to-16bit))
  (draw-rect 33 50 280 110 (cmt se:border_col '_to-16bit))
	(let ((spos (se:calc-msgpos (cons 0 (floor (/ (cdr se:txtmax) 2))))))
		;;(set-cursor (+ (car spos) 4) (+ (cdr spos) 4))
    (set-cursor (- (car spos) 2) (- (cdr spos) 2))
		(write-text mymsg)
	)
	(when cursor
		(setf cursor (max 0 cursor))
		 (let ((spos (se:calc-msgpos (cons cursor (floor (/ (cdr se:txtmax) 2))))))
			(set-text-color (cmt se:code_col '_to-16bit) (cmt se:emph_col '_to-16bit))
			;;(set-cursor (+ (car spos) 4) (+ (cdr spos) 4))
      (set-cursor (- (car spos) 2) (- (cdr spos) 2))
			(write-text (subseq mymsg cursor (1+ cursor)))
		)
	)
)

(defun se:input (mymsg default maxlen &optional chkanum)
	(keyboard-flush)
	(let* ((ibuf "") (newkey nil) 
		(lpos (1+ (cdr se:txtmax)))
		(istart (length mymsg))
		(iend (min maxlen (- (car se:txtmax) istart 1)))
		(ipos 0))

		(when default (setf ibuf default))
		(se:msg (concatenate 'string mymsg ibuf " ") nil istart)
		(loop
			(when (not newkey)
				(setf newkey (keyboard-get-key t))
			)
			(when newkey
				(case newkey
					(216 (when (> ipos 0) (decf ipos)))
					(215 (when (< ipos (length ibuf)) (incf ipos)))
					((or 10 13) (se:clr-msg) (keyboard-flush) (return ibuf))
					((or 8 127) (if (> ipos 0)
							(progn
								(decf ipos)
								(setf ibuf (concatenate 'string (subseq ibuf 0 ipos) (subseq ibuf (1+ ipos))))
							)
							(setf ibuf "")
						 ) 
					)
					(t
						(if chkanum 
							(when (and (< (length ibuf) maxlen) (se:alphnum newkey))
								(setf ibuf (concatenate 'string (subseq ibuf 0 ipos) (string (code-char newkey)) (subseq ibuf ipos (length ibuf))))
								(incf ipos)
							)
							(when (< (length ibuf) maxlen)
								(setf ibuf (concatenate 'string (subseq ibuf 0 ipos) (string (code-char newkey)) (subseq ibuf ipos (length ibuf))))
								(incf ipos)
							)
						)
					)
				)
				(se:clr-msg)
				(se:msg (concatenate 'string mymsg ibuf " ") nil (+ istart ipos))
				(setf newkey nil)
			)
		)
	)
)

(defun se:alphnum (chr)
	(if (or (= chr 31) (= chr 45) (= chr 95) 
      (and (> chr 64) (< chr 91)) 
      (and (> chr 47) (< chr 58)) 
      (and (> chr 96) (< chr 123)))
		t
		nil 
	)
)

(defun se:clr-msg ()
  (se:show-text)
)

(defun print-text-list (lst)
  (fill-rect 0 18 320 218 (cmt se:bg_col '_to-16bit))
  (draw-rect 0 18 320 218 (cmt se:border_col '_to-16bit))
  (let ((spos (se:calc-msgpos (cons 4 0))))
		(dotimes (x (length lst))
			(setf spos (se:calc-msgpos (cons 0 x)))
			(set-cursor 4 (cdr spos))
      (write-text (nth x lst))
			))
)

(defvar help-lists 
 (list (list "        ---Help 1/2--- " 
      "While holding the touchscreen"
      "c - quit" 
      "n - new file"
      "k - delete line at cursor"
      "scroll left - cursor to SOL"
      "scroll right - cursor to EOL"
      "scroll up or down - page up or down"
      "* - cursor to begin of buffer"
      "b - bind contents to symbol and quit"
      "d - delete a file on SD"
      "s - save buffer to SD"
      "l - load buffer from SD"
      "i - show dir"
      "1 - toggle bracket matching"
      "2 - highlight bracket"
      " down for more, any to quit"
    )
 (list "        ---Help 2/2--- " 
      "touchscreen alt characters"
      "k -> ` "
      "p -> ~ "
      "$ -> % "
      "a -> ^ "
      "q -> & "
      "o -> = "
      "t -> < "
      "y -> > "
      "u -> \\"
      "g -> | "
      "( -> [ "
      ") -> ] "
      "space  -> tab"
    )) )

(defun se:help ()
	(keyboard-flush)
  (print-text-list 
    (nth 0 help-lists ))
  (let ((lk nil))
		(loop
			(setf lk (keyboard-get-key t))
      (when lk
        (case lk
          ((= lk 217) (print-text-list (nth 1 help-lists )) )
          ((= lk 218) (print-text-list (nth 0 help-lists )))
          (t (return))
        )))
		(se:clr-msg)
		(keyboard-flush)
	)
)

(defun se:sedit (&optional myform myskin)
	(se:init myskin)
	(let* ((lkd nil)
		   (lku nil)
		   (lastkey nil)
		   (k_pr nil)
		   (numline 0)
		   (kdelay 500)
		   (repcnt nil)
		   (newkey nil)
		   (krepeat nil))
			(if myform
				(progn
					(setf se:funcname (prin1-to-string myform)) 
					(setq se:buffer (split-string-to-list (string #\Newline) (string (with-output-to-string (str) (pprint (eval myform) str)))) )
					(set-cursor (* 32 se:cwidth) 0)
					(set-text-color (cmt se:code_col '_to-16bit) (cmt se:cursor_col '_to-16bit))
					(if (> (length se:funcname) 13)
						(write-text (concatenate 'string "SYM: " (subseq se:funcname 0 10) "..."))
						(write-text (concatenate 'string "SYM: " se:funcname))
					)
				)
				(setq se:buffer (list ""))
			)
			(se:map-brackets)
			(se:show-text)
			(se:show-cursor)
			(loop
				(setf lastkey (keyboard-get-key))
				(when lastkey 
					(case lastkey
						((or 1 210) (se:linestart))
						((or 5 213) (se:lineend))
						((or 3 17) (when (se:alert "Exit") (se:cleanup) (setf se:exit t)) (keyboard-flush) (setf lastkey nil))
						((or 24 14 2) (se:flush-buffer) (setf lastkey nil))
						((or 11 12) (se:flush-line) (setf lastkey nil))
						(94 (se:docstart))
						(211 (se:prevpage))
						(214 (se:nextpage))
						(194 (se:toggle-match) (setf lastkey nil))
						(195 (se:checkbr) (setf lastkey nil))
						(198 (se:run) (setf lastkey nil))
						(202 (se:remove) (setf lastkey nil))
						(203 (se:save) (setf lastkey nil))
						(204 (se:load) (setf lastkey nil))
						(205 (se:show-dir) (setf lastkey nil))
						(216 (se:left))
						(215 (se:right))
						(218 (se:up))
						(217 (se:down))
						((or 13 10) (se:enter))
            (16 (se:help))
						(9 (se:tab) (setf lastkey nil))
						((or 8 127) (se:delete))
						(t (se:insert (code-char lastkey)))
					)
				)
				(when se:exit (fill-screen) (return t))
			)
	)
	(keyboard-flush)
	nil
)


;
; Helper functions
;
;
(defun constrain (value mini maxi)
  (min (max value mini) maxi)
)

(defun split-string-to-list (delim str)
	(unless (or (eq str nil) (not (stringp str))) 
		(let* ((start 0)
          (end (search-str delim str))
          (lst nil))
			(loop
        (if (eq end nil) 
          (return (append lst (list (subseq str start))))
				  (setq lst (append lst (list (subseq str start end)))))
        (setq start (1+ end))
        (setq end (search-str delim str start))
      )
    )
  )
)

(defun char-list-to-string (clist)
	(let ((str "")) 
		(dolist (c clist) (setq str (concatenate 'string str c))) 
		str
	)
)

(defun to-hex-char (i)
	(unless (equal i nil)
		(let ((i (abs i))) 
			(code-char (+ i (if (<= i 9) 48 55)) )
		)
	)
)

(defun byte-to-hexstr (i)
	(unless (equal i nil)
		(let ((i (abs i))) 
		 (concatenate 'string (string (to-hex-char (ash i -4))) (string (to-hex-char (logand i 15))) ) 
		)
	)
)

(defun word-to-hexstr (i)
	(unless (equal i nil)
		(let ((i (abs i))) 
		 (concatenate 'string (byte-to-hexstr (ash i -8)) (byte-to-hexstr (logand i 255))) 
		)
	)
)

(defun hexstr-to-int (s)
	(read-from-string (concatenate 'string "#x" s))
)

(defun remove-if (fn lis)
  (mapcan #'(lambda (item) (unless (funcall fn item) (list item))) lis)
 )

(defun remove-if-not (fn lis)
  (mapcan #'(lambda (item) (when (funcall fn item) (list item))) lis)
)

(defun remove_item (ele lis)
	(remove-if (lambda (item) (equal ele item)) lis)
)

(defun remove (place lis)
	(let ((newlis ()))
		(dotimes (i place)
			(push (pop lis) newlis)
		)
		(pop lis)
		(dotimes (i (length lis))
			(push (pop lis) newlis)
		)
		(reverse newlis)
	)
)

(defun assoc* (a alist test)
  (cond
   ((null alist) nil)
   ((funcall test a (caar alist)) (car alist))
   (t (assoc* a (cdr alist) test))
  )
)

(defun reverse-assoc* (a alist test)
  (cond
   ((null alist) nil)
   ((funcall test a (cdar alist)) (caar alist))
   (t (reverse-assoc* a (cdr alist) test))
  )
)

(defun get-obj (aname anum)
	(read-from-string (eval (concatenate 'string aname (string anum))))
)



; 
; class color
;
(defvar color
	(class nil '(
			red 0
			green 0
			blue 0

			_copy-color #'(lambda (self acol)
				(stv self 'red (gtv acol 'r))
				(stv self 'green (gtv acol 'g))
				(stv self 'blue (gtv acol 'b))
			)

			_set-rgb #'(lambda (self r g b)
				(stv self 'red r)
				(stv self 'green g)
				(stv self 'blue b)
			)

			_to-hex-color #'(lambda (self)
				(logior (ash (gtv self 'red) 16) (ash (gtv self 'green) 8) (gtv self 'red))
			)

			_to-hex-string  #'(lambda (self)
				(concatenate 'string (byte-to-hexstr (gtv self 'red)) (byte-to-hexstr (gtv self 'green)) (byte-to-hexstr (gtv self 'blue))) 
			)

			_to-16bit #'(lambda (self)
				(let* ((r (gtv self 'red)) (g (gtv self 'green)) (b (gtv self 'blue)))
					(logior (ash (logand r #xf8) 8) (ash (logand g #xfc) 3) (ash b -3))
				)
			)
		)
	)
)

)lisplibrary";

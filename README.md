# ulisp-sedit-tdeck
Port of ulisp-lispbox text editor for the LilyGO T-Deck. Orignal editor was written by https://github.com/ErsatzMoco


## How to install
Do all the things needed to set up the [t-deck](http://www.ulisp.com/show?4JAO). The t-deck library contains the [SensorsLib](https://github.com/Xinyuan-LilyGO/T-Deck/tree/master/lib/SensorsLib) folder which contains the touchscreen drivers. Move that folder into the arduino library folder. Add the lisplibrary.h and extensions.ino files to the folder which contains [ulisp-tdeck.ino](https://github.com/technoblogy/ulisp-tdeck). Then follow the setup instructions for [extensions](http://www.ulisp.com/show?19Q4) and [lisplibrary](http://www.ulisp.com/show?27OV) to enable both features. Add the `initTouch();` and `inittrackball();` functions to the `setup()` function in `ulisp-tdeck.ino` to enable the touchscreen and trackball.

## Known issues
- [superprint issue](http://forum.ulisp.com/t/packages-and-persistent-storage/1318/16) breaks the editing of existing functions by introducing escape characters into the string being edited
- sometimes the first letter of a line doesn't show up
- touchscreen and letter combination modifier can be finicky

## Usage
Thanks to innovative usage of the [touchscreen as a modifier for the t-decks keyboard output](https://github.com/hasn0life/ulisp-tdeck-touch-example) we can have all the necessary features for the [lispbox text editor](https://github.com/ErsatzMoco/ulisp-lispbox/tree/main) without having to reprogram the [T-deck's keyboard](https://github.com/hasn0life/t-deck-keyboard-ex). Also the trackball is used to move the cursor around. 

To invoke it type `(se:sedit)` or `(se:sedit 'symbol)` where "symbol" can be any symbol name already present in uLisp
The commands from the orignal lispbox editor work with slight modifications

- touchscreen-c --- quit editor and return to REPL

- touchscreen-n --- discard current text buffer (i.e. new file)

- touchscreen-k --- delete line starting at cursor position

- touchscreen-(scroll left) --- move cursor to start of line

- touchscreen-(scroll right) --- move cursor to end of line

- touchscreen-* (sym+a) --- move cursor to beginning of buffer

- touchscreen-(scroll up) / touchscreenscroll down) --- move one page up or down

- touchscreen-h --- help menu

- touchscreen-1 (sym+w) --- toggle bracket matching on/off

- touchscreen-2 (sym+e) --- check if bracket under the cursor has a matching bracket in the buffer. If so, they are temporarily highlighted. (Use when continuous bracket matching is off.)

- touchscreen-b --- bind contents of the text buffer to a symbol of your choice and quit editor

- touchscreen-d --- delete a file on the SD card

- touchscreen-s --- save text buffer to SD card

- touchscreen-l --- load text from SD card into buffer, discarding the present one

- touchscreen-i --- show directory of SD card

```
touchscreen alt characters
k -> `
p -> ~ 
$ -> % 
a -> ^ 
q -> & 
o -> = 
t -> < 
y -> > 
u -> \
g -> | 
( -> [ 
) -> ] 
space  -> tab
```

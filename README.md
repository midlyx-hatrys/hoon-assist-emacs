# hoon-assist-emacs
Provide official documentation for Hoon runes and commands under point. 

## Installation

Installation is manual.

`git clone https://github.com/poctyr-bidbes/hoon-assist-emacs.git`

Move the following files into your ~/.emacs.d/site-lisp/ directory:

hoon-assist-emacs.el<br>
hoon-dictionary.json

add (require 'hoon-assist-emacs) to your init.el file

## Operation

Restart emacs, open a hoon file.

Place point on the rune or command you would like defined. Press F7. Press F7 again to close the documentation window.

## Notes

hoon-assist-emacs will globally modify the function attached to the F7 function key. Inactivate or choose a different key by editing hoon-assist-emacs.el directly.

If your site-lisp directory differs from `~/.emacs.d/site-lisp` you will need to edit the `site-lisp-dir` variable at the top of hoon-assist-emacs.el

hoon-assist-emacs does not perform syntax highlighting. Consider [hoon-mode.el](https://github.com/urbit/hoon-mode.el).

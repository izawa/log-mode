# log-mode.el

Simple Text indentation support minor mode.

## How to install:
### Clone file into your emacs directory
    cd ~/.emacs.d
    git clone git@github.com:izawa/log-mode.git

### add load path subdirectory recursively
    (let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

### Add require into your .emacs. (or init.el)

    (require 'log-mode)

### Additional setting
 If you want to bind Hot-keys, add setting lines like below.

    ; Shift-Tab: back indent level
      	(define-key log-mode-map [S-tab] 'log-mode-tab-back)
    ; Ctrl-Return: raw CR
      	(define-key log-mode-map [C-return] 'log-mode-raw-return)
    ; set indent prefix as you like.
      	(setq log-mode-indent-prefix-list  [
           "* "                 ; level 1
           "  + "               ; level 2
           "    # "             ; level 3
           "      - "           ; level 4
           "        o "         ; level 5
           "          * "       ; level 6
           ])

### if you using filladapt.el, you can use log-mode-hook to regist indent-prefix into filladapt-token-table as "bullet".

    (add-hook 'log-mode-hook
    	      (lambda ()
	      	      (filladapt-mode)
	      	      (let ((len (length log-mode-indent-prefix-list))
		      	      (i 0))
			      	      (while (< i len)
				      	      (pushnew (cons (aref log-mode-indent-prefix-list i) (cons 'bullet nil)) filladapt-token-table)
					      	      (setq i (1+ i))))))

## How to use:
type
   M-x log-mode

###key bindings:
* Tab: insert indent prefix, or increment indent level (like a PowerPoint).
* Shift-Tab: decrement indent level.
* C-c C-a: insert log-header. 


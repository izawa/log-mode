;;; log-mode.el --- minor mode for text mode like a PowerPoint

;; Author:  Yukimitsu Izawa <izawa@izawa.org>
;; Version: 0.1
;; Created: Dec 22, 1999
;; Revised: Sep 13, 2001

;;; Commentary:

;; The updated version is available at:
;;      http://www.izawa.org/software/log-mode/
;;
;; Minimum setup:
;;      (autoload 'log-mode  "log-mode" nil t)

;;; Code:

(provide 'log-mode)

(if (fboundp 'defgroup)
    (defgroup log-mode nil
      "ログ取り君1号"
      :tag "logmode"
      :prefix "log-mode-"
      ))

(if (fboundp 'defcustom)
    (progn
(defcustom log-mode-indent-prefix-list  ["  + " "    + " "      + " "        + "]
  "ログモードのprefix"
  :group 'log-mode
  :type 'string)

(defcustom log-mode-hook nil
  "ログモード起動時のhook"
  :group 'log-mode
  :type 'hook)

(defcustom log-mode-off-hook nil
  "ログモード終了時のhook"
  :group 'log-mode
  :type 'hook)

(defcustom log-mode-insert-header-hook nil
  "ヘッダインサート時のhook"
  :group 'log-mode
  :type 'hook)

(defcustom log-mode-user-full-name nil
  "ヘッダインサートに用いるユーザ名"
  :group 'log-mode
  :type '(choice (const nil)
		 string))

(defcustom log-mode-user-mail-address nil
  "ヘッダインサートに用いるメールアドレス"
  :group 'log-mode
  :type '(choice (const nil)
		 string))

(defcustom log-mode-header-format "Date: %s %s %2d %02d:%02d:%02d %s %d\nLogger: %s <%s>\n\n"
  "ヘッダインサートに用いるフォーマット"
  :group 'log-mode
  :type 'string)

(defcustom log-mode-header-value '(Week Month day hour min sec TZ year Username Mailaddr)
  "ヘッダインサートに用いる変数"
  :group 'log-mode
  :type 'string)

(defcustom log-mode-insert-newline nil
  "改行時に空行を入れるかどうか"
  :group 'log-mode
  :type 'boolean)

(defcustom log-mode-header-insert-buffertop t
  "ヘッダをバッファの先頭にインサートするかどうか"
  :group 'log-mode
  :type 'boolean)

(defcustom log-mode-insert-newline-only-first-level nil
  "第一レベルのみ改行時に空行を入れるかどうか"
  :group 'log-mode
  :type 'boolean)
)
;;;
;;; don't have defcustom 
;;;
(defvar log-mode-indent-prefix-list  ["  + " "    + " "      + " "        + "]
  "ログモードのprefix")

(defvar log-mode-hook nil
  "ログモード起動時のhook")

(defvar log-mode-off-hook nil
  "ログモード終了時のhook")

(defvar log-mode-insert-header-hook nil
  "ヘッダインサート時のhook")

(defvar log-mode-user-full-name nil
  "ヘッダインサートに用いるユーザ名")

(defvar log-mode-user-mail-address nil
  "ヘッダインサートに用いるメールアドレス")

(defvar log-mode-header-format "Date: %s %s %2d %02d:%02d:%02d %s %d\nLogger: %s <%s>\n\n"
  "ヘッダインサートに用いるフォーマット")

(defvar log-mode-header-value '(Week Month day hour min sec TZ year Username Mailaddr)
  "ヘッダインサートに用いる変数")

(defvar log-mode-insert-newline nil
  "改行時に空行を入れるかどうか")

(defvar log-mode-insert-newline-only-first-level nil
  "第一レベルのみ改行時に空行を入れるかどうか")
)


(defvar log-mode nil)
(make-variable-buffer-local 'log-mode)

(defvar log-mode-map (make-sparse-keymap))

(defvar egg:*in-fence-mode* nil)
(defvar canna:*fence-mode* nil)
(defvar egg:henkan-mode-in-use nil)

(setq log-mode-kanjimap-aref '((egg:henkan-mode-in-use . henkan-mode-map )
			       (egg:*in-fence-mode*  . fence-mode-map )
			       (canna:*fence-mode* . canna-mode-map )))

(or (assoc 'log-mode minor-mode-alist)
    (setq minor-mode-alist
 	  (cons '(log-mode " ログ取り君1号") minor-mode-alist)))

(or (assoc 'log-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
 	  (cons (cons 'log-mode log-mode-map)
 		minor-mode-map-alist)))

;;
;; key binding
;;
(if (featurep 'xemacs)
    (progn
      (define-key log-mode-map [iso-left-tab] 'log-mode-tab-back)
      (define-key log-mode-map [(shift tab)] 'log-mode-tab-back))
  (define-key log-mode-map [S-iso-lefttab] 'log-mode-tab-back))
(if (featurep 'meadow)
    (define-key log-mode-map [(shift tab)] 'log-mode-tab-back))
(define-key log-mode-map "\C-i" 'log-mode-tab-fwd)
(define-key log-mode-map [(control shift tab)] 'log-mode-tab-back)
(if (and (boundp 'MULE) (boundp 'mule-version))
    (define-key log-mode-map "\M-\C-m" 'log-mode-raw-return)
  (define-key log-mode-map [(meta return)] 'log-mode-raw-return))
(define-key log-mode-map "\C-m" 'log-mode-return)
(define-key log-mode-map "\C-c\C-a" 'log-mode-insert-header)
(if (string= major-mode "mgp-mode")
    (define-key log-mode-map "\C-c\C-v" 'log-mode-run-mgp))


(defun log-mode (&optional arg)
  "これは Text mode 用のマイナーモードで、PowerPointと似たインターフェースを提供するものです。
tabで、itemize prefixを挿入します。続けて tab を押すことでitemize level が進みます。
Shift-tab で逆に、itemize level が戻ります。
return で改行後、自動的に itemize prefix が挿入されます。
M-return で単に改行します。"
  (interactive "P")
  ;; we set the mode on or off
  (setq log-mode (not (or (and (null arg) log-mode)
			  (<= (prefix-numeric-value arg) 0))))
  (if log-mode
      (log-mode-on)
    (log-mode-off))
  ;; we force the modeline re-printing
  (set-buffer-modified-p (buffer-modified-p)))

(defun log-mode-on ()
  ; if buffer killing run log-mode-off
  (add-hook 'kill-buffer-hook
            '(lambda ()
               (if log-mode
                   (log-mode-off))))
  ;; we end with the log-mode hooks
  (run-hooks 'log-mode-hook)
  (if (string= major-mode "mgp-mode")
      (define-key log-mode-map "\C-c\C-v" 'log-mode-run-mgp)))


(defun log-mode-off ()
  ;; we mark the mode as killed
  (run-hooks 'log-mode-off-hook)
  (setq log-mode nil))

(defun log-mode-tab-fwd ()
   (interactive)
   (if (not (is-kanji-mapped-key "\C-i"))
  (log-mode-rotate "fwd")))

(defun log-mode-tab-back ()
  (interactive)
  (log-mode-rotate "back"))

(defun log-mode-raw-return ()
  (interactive)
  (insert-string "\n"))

(defun log-mode-return ()
  (interactive)
  (if (not (is-kanji-mapped-key [return]))
      (progn 
	(let ((flag t)(cnt 0)(len (length log-mode-indent-prefix-list)))
	  (while (and (< cnt len) flag)
	    (if (log-mode-check-line-prefix-partial (aref log-mode-indent-prefix-list cnt))
		(progn
		  (setq flag nil)
		  (if (and (string= (log-mode-get_line)
				    (aref log-mode-indent-prefix-list 0)) 
			   log-mode-insert-newline-only-first-level)
		      (save-excursion
			(beginning-of-line)
			(insert-string "\n"))

		    (if log-mode-insert-newline
			(insert-string "\n"))
		    
		    (insert-string "\n")
		    (log-mode-insert_prefix cnt))))
	    (setq cnt (1+ cnt)))
	  (if  flag
	      (let ((p (point))
		    i)
		(progn 
		  (setq i (log-mode-rsearch-with-prefix))
		  (goto-char p)
		  (if (and log-mode-insert-newline i)
		      (insert-string "\n"))
		  (insert-string "\n")
		  (if i 
		      (insert-string i)))))))))

(defun log-mode-rotate (direction)
  (interactive)
  (let ((len (length log-mode-indent-prefix-list))
	(flag t)
	(mflag nil)
	(cnt 0))
    (while (and (< cnt len) flag)
      (if (not mflag)
	  (progn
	    (setq mflag (log-mode-check-line-prefix (aref log-mode-indent-prefix-list cnt)))))
      (if (and (log-mode-check-line-prefix (aref log-mode-indent-prefix-list cnt))
	       (<= (log-mode-convert_direction direction cnt) (1- (length log-mode-indent-prefix-list)))
	       (>= (log-mode-convert_direction direction cnt) -1))
	  (progn
	    (setq flag nil)
	    (log-mode-delete_prefix (aref log-mode-indent-prefix-list cnt))
            (end-of-line)
	    (log-mode-insert_prefix (log-mode-convert_direction direction cnt))))
      (setq cnt (1+ cnt)))
    (if (and flag 
	     (string= direction "fwd")
	     (not mflag))
	(log-mode-insert_prefix 0))))

(defun log-mode-convert_direction (dir cnt)
  (let ((a 0)(cur (point)))
    (if (string= dir "fwd")
	(setq a (1+ cnt)))
    (if (string= dir "back")
	(setq a (1- cnt)))
    (goto-char cur)
    a))

(defun log-mode-delete_prefix (prefix)
  (interactive)
  (log-mode-get_line)
  (let ((a 0))
	(beginning-of-line)
	(setq a (point))
	(delete-region a (+ a (length prefix)))))

(defun log-mode-insert_prefix (cnt)
  (interactive)
    (beginning-of-line)
    (if (>= cnt 0)
	(progn 
	  (insert-string (aref log-mode-indent-prefix-list  cnt)))))

(defun log-mode-check-line-prefix-partial (prefix)
  (interactive)
  (let ( (all (log-mode-get_line_partial)) )
    (if (and (>= (length all) (length prefix))
	     (string= prefix (substring all 0 (length prefix))))
	t nil)))

(defun log-mode-check-line-prefix (prefix)
  (interactive)
  (let ( (all (log-mode-get_line)) )
    (if (and (>= (length all) (length prefix))
	     (string= prefix (substring all 0 (length prefix))))
	t nil)))

(defun log-mode-get_line ()
  (interactive)
  (let ((a 0)(b 0)(cur (point)))
    (beginning-of-line)
    (setq a (point))
    (end-of-line)
    (setq b (point))
    (goto-char cur)
    (buffer-substring a b)))

(defun log-mode-get_line_partial ()
  (interactive)
  (let ((a 0)(cur (point)))
    (beginning-of-line)
    (setq a (point))
    (goto-char cur)
    (buffer-substring a cur)))

;(defun log-mode-in-fence-p ()
;   (or egg:*in-fence-mode* canna:*fence-mode*))

;(defun log-mode-in-henkan-p ()
;  egg:henkan-mode-in-use )

(defun is-kanji-mapped-key (key)
  (if (not (= 0 (setq n (length (memq t (mapcar 'eval (mapcar 'car log-mode-kanjimap-aref)))))))
      (let ((map-name (cdr (nth (1- (- (length log-mode-kanjimap-aref)(1- n))) log-mode-kanjimap-aref)))
	    (map (eval (cdr (nth (1- (- (length log-mode-kanjimap-aref)(1- n))) log-mode-kanjimap-aref)))))
	(if (and (keymapp map) (lookup-key map key))
	    (progn 
;	      (message "%s" (lookup-key map key))
	      (call-interactively (lookup-key map key))
	      t)
	  (if canna:*fence-mode*
	      (progn
		(call-interactively 'canna-functional-insert-command)
		t)
	    nil)
	  ))nil))

;------------------------------------------------------------------
; time string insertion functions
;------------------------------------------------------------------

(defvar log-mode-internal-date-list 
; ***WARNING*** DON'T MODIFY THIS VARIABLE.
; year = 2000, month = 1-12, day = 1-31, hour = 0-23,
; min = 0-59, sec=0-59, Jweek="日"-"土", Month = "Jan"-"Dec",
; Week = "Sun"-"Sat", TZ="JST"... Username = "Username", 
; Mailaddr = "yourmailaddr"
 '(year month day  hour min sec Jweek Month Week TZ Username Mailaddr))

(defun log-mode-make-time-string ()
  (let ((time (current-time-string)) year month day hour min sec week Month Week)
    (string-match
     "^\\([A-Z][a-z][a-z]\\) *\\([A-Z][a-z][a-z]\\) *\\([0-9]*\\) *\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\) *\\([0-9]*\\)$" time)
    (setq Week  (substring time (match-beginning 1) (match-end 1)))
    (setq Month (substring time (match-beginning 2) (match-end 2)))
    (setq day (string-to-int (substring time (match-beginning 3) (match-end 3))))
    (setq hour (string-to-int (substring time (match-beginning 4) (match-end 4))))
    (setq min (string-to-int (substring time (match-beginning 5) (match-end 5))))
    (setq sec (string-to-int (substring time (match-beginning 6) (match-end 6))))
    (setq year (string-to-int (substring time (match-beginning 7) (match-end 7))))
    (setq month (cdr (assoc Month
			    '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
			      ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
			      ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))))
    (setq week (cdr (assoc Week
			   '(("Sun" . "日") ("Mon" . "月") ("Tue" . "火")
			     ("Wed" . "水") ("Thu" . "木") ("Fri" . "金")
			     ("Sat" . "土")))))
    (mapcar 'eval (list year month day  hour min sec week Month Week (nth 1 (current-time-zone))))))

(defun log-mode-get-header-value ()
  (mapcar 'cdr (mapcar (lambda (x) 
			 (assoc x (mapcar* (quote cons) log-mode-internal-date-list (append (log-mode-make-time-string) (list (log-mode-get-user-full-name) (log-mode-get-user-mail-address))))))
		       log-mode-header-value)))

(defun log-mode-insert-header ()
  (interactive)
  (save-excursion 
    (if log-mode-header-insert-buffertop
	(goto-char 1))
    (insert (apply (function format) log-mode-header-format (log-mode-get-header-value)))
    (run-hooks 'log-mode-insert-header-hook)))

;(defun log-mode-time-string ()
;  (concat (substring (current-time-string) 0 20)
;	  (nth 1 (current-time-zone))
;	  " "
;	  (substring (current-time-string) -4)))

(defun log-mode-get-user-full-name ()
  (if log-mode-user-full-name
      (eval log-mode-user-full-name)
    (if (functionp 'user-full-name)
	(user-full-name)
      "Your Full Name")))

(defun log-mode-get-user-mail-address ()
  (if log-mode-user-mail-address
      (eval log-mode-user-mail-address)
    (if (functionp 'user-mail-address)
	(user-mail-address)
      "you@your.domain")))
;;
(defun log-mode-current-line ()
  "Return the vertical position of point..."
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun log-mode-get-prefix-match-length (mlength)
  (let ((c 0)
	(l '())
	(a-list-len (length log-mode-indent-prefix-list)))
    
    (while (< c a-list-len)
      (if (= (length (aref log-mode-indent-prefix-list c)) mlength)
	  (setq l (cons (aref log-mode-indent-prefix-list c) l)))
      (setq c (1+ c)))
    l))

(defun log-mode-getIndentString ()
  (let ((line (log-mode-get_line)))
    
    (string-match "^ *" line) 
    (match-string 0 line)))

(defun log-mode-rsearch-with-prefix ()
  (let* ((prefix_length (length (log-mode-getIndentString)))
	 (prefix_list (log-mode-get-prefix-match-length prefix_length))
	 (flag t)
	 line
	 subline
	 (ret nil))

    (while (and flag (<= 1 (log-mode-current-line)))
      (previous-line 1)
      (setq line (log-mode-get_line))
      (if (<=   prefix_length (length line))
	  (progn 
	    (setq subline (substring line 0 prefix_length))
	    (if (not (string= (make-string prefix_length ? ) subline))
		(progn
		  (setq ret (nth (- (length 
				     prefix_list)
				    (length (member subline  prefix_list)))
				 prefix_list))
		  (setq flag nil))))
	(setq flag nil)))
    ret))

;;
(defvar head1 "%prefix2tab = (\n")
(defvar head2 ");\n")
(defvar code1 
"# code section
while (<STDIN>) {
  my $line = $_;
  for my $i ( keys %prefix2tab) {
    my $qi = quotemeta $i;
    $line =~ s/$qi/$prefix2tab{$i}/;
  }
  print $line;
}

__END__
")
;;
;; some utility functions
;;

(defun basename (PATH)
  "return basename of PATH."
  (let ((rev  (string-reverse PATH))
	ret)
    (if (string= (substring rev 0 1) "/")
	(setq rev (substring rev 1)))
    (string-match "\\([^/]*\\)" rev)
    (setq ret (string-reverse (substring rev (match-beginning 0) (match-end 0))))
    (if (string= ret "")
	(setq ret "/"))
    ret))


(defun dirname (PATH)
  "return dirname of PATH."
  (let ((rev  (string-reverse PATH))
	ret)
    (if (string= (substring rev 0 1) "/")
	(setq rev (substring rev 1)))
    (string-match "\\([^/]*\\)" rev)
    (setq ret ".")
    (if (= (match-end 0) (length rev))
	(setq ret ".")
      (setq ret (string-reverse (substring rev  (1+ (match-end 0)))))
      (if (string= ret "")
	  (setq ret "/")))
    ret))

(defun string-reverse (str)
  " reverse string."
  (mapconcat 'char-to-string
	     (reverse (string-to-char-list str)) ""))


(defun copy-buffer (SRC DEST)
  "copy buffer text from SRC buffer to DEST buffer."
  (let (contents)
    (if (get-buffer DEST)
	(set-buffer (get-buffer DEST))
      (set-buffer (get-buffer-create DEST)))
    (erase-buffer)
    (setq buffer-read-only nil)
    (set-buffer-modified-p (buffer-modified-p))
    (set-buffer SRC)
    (setq contents (buffer-string))
    (set-buffer DEST)
    (insert-string contents)
    (set-buffer SRC)))
    

;;

(defun log-mode-run-mgp ()
  (interactive 

   (save-excursion
   (let (temp-file-name tempdir old-buffer)

; テンポラリふぁいる名
     (if buffer-file-name 
	 (setq tempdir (dirname buffer-file-name))
       (setq tempdir (temp-directory)))
     (setq temp-file-name (make-temp-name (expand-file-name "log-mode" tempdir)))
; ばっふぁのこぴー
     (setq old-buffer (current-buffer))
     (copy-buffer (current-buffer) "*log-mode-tmp*")


     (set-buffer "*log-mode-tmp*")
     (set-buffer-file-coding-system 'iso-2022-jp)
; バッファ内容のそうさ
     (let ( (c 0) 
	    (len (length log-mode-indent-prefix-list))
	    reg
	    tabs
	    )
       (while (< c len)
	 (setq reg (concat "^" (regexp-quote (aref log-mode-indent-prefix-list c))))
	 (setq tabs (make-string (1+ c) ?\t))
	 (goto-char 0)
	 (replace-regexp reg tabs)
	 (setq c (1+ c))))

; ふぁいるに書き出す
    (write-file temp-file-name)


; mgp はしらせる
    (call-interactively 'mgp-run-mgp)
    
; ふぁいるの削除
    (if (y-or-n-p (format "delete tempfile? " temp-file-name))
	  (delete-file temp-file-name)))))
  (message "Done."))


(defun log-mode-get-prefix-match-string (mstr)
   (interactive)
   (progn 
     (let ((cnt 0)
	   (len (length log-mode-indent-prefix-list))
 	  (rval nil))
       (while (< cnt len)
	 (if (string= mstr (aref log-mode-indent-prefix-list cnt))
 	    (setq rval cnt))
 	(setq cnt (1+ cnt)))
       rval)))

       

 

;;; Copyright Notice:

;;     Copyright (c) 1999-2001 Yukimitsu Izawa <izawa@izawa.org>
;;     All rights reserved.
;; 
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. All advertising materials mentioning features or use of this software
;;     must display the following acknowledgement:
;;       This product includes software developed by Yukimitsu Izawa.
;;  4. The name of Yukimitsu Izawa may not be used to endorse or promote products
;;     derived from this software without specific prior written permission.
;; 
;;  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; log-mode.el ends here

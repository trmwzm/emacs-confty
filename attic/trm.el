;; personalization on top of prelude

;; load the package system and add some repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents) (package-refresh-contents))

;; scheme
(prelude-ensure-module-deps '(highlight-symbol rect+ geiser))

;; lisp
;; in LISP
;; (quicklisp-quickstart:install)
;; (ql:add-to-init-file)
;; (ql:quickload "quicklisp-slime-helper")
(if (file-exists-p "~/quicklisp/slime-helper.el")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "sbcl")
)

;; not completely blind yet
(set-face-attribute 'default nil :height 100)

(setq flyspell-issue-welcome-flag nil) ;; fix flyspell problem

(setq whitespace-line-column 132)

;; C-mode
(custom-set-variables
 '(c-basic-offset 2))

;; on-the-fly columns
(defun fill-region-ask-width (fill-width)
  "Asks for a width and fills the region to it"
  (interactive "nFill region to column: ")
  (fill-region-width fill-width))

(defun fill-region-width-70 ()
  "Fills the region with width 70"
  (interactive)
  (fill-region-width 70))

(defun fill-region-width (fill-width)
  "Fills the region with the given width"
  (set-fill-column fill-width)
  (fill-region (region-beginning) (region-end)))

(defun unfill-paragraph ()
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(autoload 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload 'imath "imath" "Interactive Math mode" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)

(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(control meta f3)] 'highlight-symbol-query-replace)

(require 'rect+)
(define-key ctl-x-r-map "C" 'rectplus-copy-rectangle)
(define-key ctl-x-r-map "N" 'rectplus-insert-number-rectangle)
(define-key ctl-x-r-map "\M-c" 'rectplus-create-rectangle-by-regexp)
(define-key ctl-x-r-map "A" 'rectplus-append-rectangle-to-eol)
(define-key ctl-x-r-map "R" 'rectplus-kill-ring-to-rectangle)
(define-key ctl-x-r-map "K" 'rectplus-rectangle-to-kill-ring)
(define-key ctl-x-r-map "\M-l" 'rectplus-downcase-rectangle)
(define-key ctl-x-r-map "\M-u" 'rectplus-upcase-rectangle)

;; C-u M-x ps-print-buffer-with-faces
(setq ps-paper-type 'letter
     ps-font-size 8.0
     ps-print-header nil
;;     ps-landscape-mode t ; see pr-toggle-file-landscape
     ps-number-of-columns 2)

;;; page margins
(setq ps-left-margin (/ (* 72 0.7) 2.54)) ; 0.7 cm
(setq ps-right-margin (/ (* 72 0.7) 2.54)) ; 0.7 cm
(setq ps-inter-column (/ (* 72 1.0) 2.54)) ; 1 cm
(setq ps-bottom-margin (/ (* 72 0.5) 2.54)) ; 0.5 cm
(setq ps-bottom-margin (/ (* 72 0.9) 2.54)) ; 0.9 cm
(setq ps-top-margin (/ (* 72 0.7) 2.54)) ; 0.7 cm
(setq ps-header-offset (/ (* 72 0.3) 2.54)) ; 0.3 cm
(setq ps-header-line-pad 0.15)
(setq ps-n-up-margin (/ (* 72 0.5) 2.54)) ; 0.5 cm

(defun print-to-pdf ()
 (interactive)
 (load-theme 'leuven t)
 (setq ps-build-face-reference t)
 (ps-spool-buffer-with-faces)
 (switch-to-buffer "*PostScript*")
 (write-file "tmp.ps")
 (kill-buffer "tmp.ps")
 (setq cmd (concat "ps2pdf14 tmp.ps " (buffer-name) ".pdf"))
 (shell-command cmd)
 (shell-command "rm tmp.ps")
 (message (concat "File printed in : "(buffer-name) ".pdf"))
)

(defun print-to-pdf-batch ()
 (interactive)
 (load-theme 'tango t)
 (setq ps-build-face-reference t)
 (ps-spool-buffer-with-faces)
 (switch-to-buffer "*PostScript*")
 (write-file "tmp.ps")
 (kill-buffer "tmp.ps")
 (setq cmd (concat "ps2pdf14 tmp.ps " (buffer-name) ".pdf"))
 (shell-command cmd)
 (shell-command "rm tmp.ps")
 (kill-emacs t)
)

(global-set-key [(meta f5)] 'insert-quotes)
(defun insert-quotes ()
  "Inserts quotes (\") around the current region or work."
  (interactive)
  (let (start end bounds)
    (if (and transient-mark-mode mark-active)
        (setq start (region-beginning)
              end (region-end))
      (progn
        (setq bounds (bounds-of-thing-at-point 'symbol))
        (setq start (car bounds)
              end (cdr bounds))))
    (goto-char start)
    (insert "\"")
    (goto-char (+ end 1))
    (insert "\"")))

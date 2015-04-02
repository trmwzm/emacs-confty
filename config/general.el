;;; general.el
;;; General settings

;;; Some environment variables
(setenv "LANG" "C")
(setenv "LC_COLLATE" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "LC_MESSAGES" "en_US.UTF-8")

(setenv "PAGER" "cat")
(setenv "TERM" "xterm")
(setenv "TMPDIR" "/tmp")

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d"))

;; I dont what this means. but looks imp
(setq buffer-file-coding-system 'utf-8-unix)
(setq default-file-name-coding-system 'utf-8-unix)
(setq default-keyboard-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq default-sendmail-coding-system 'utf-8-unix)
(setq default-terminal-coding-system 'utf-8-unix)

(setq gc-cons-threshold 20000000)       ; increase gc
(setq undo-outer-limit  40000000)
(setq large-file-warning-threshold 100000000) ;; 95mb aprox

(add-to-list 'completion-styles 'substring t)

(setq message-log-max 5000              ; Increase message log
      echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      show-paren-delay 0
      inhibit-startup-screen t          ; Skip the startup screens
      initial-scratch-message nil
      require-final-newline t       ; Always end a file with a newline
      search-whitespace-regexp "[ \t\r\n]+"  ;
      next-line-add-newlines nil ; Stop at the end of the file, not just add lines
      browse-url-browser-function 'browse-url-firefox  ; my system needs this
      )

(setq completion-ignored-extensions
      (delete-dups (append completion-ignored-extensions
                           '(".bak" ".obj" ".map" ".mod" ".rel" ".out"))))

(setq-default
 frame-title-format `("%b       " ,(user-login-name) "@" ,(system-name) "     "
                      global-mode-string)
 ;; frame-title-format '(buffer-file-name "%f" "%b") ; I already know this is Emacs
 truncate-lines t                                 ; Truncate lines, don't wrap
 paren-mode 'sexp                                 ; Highlight parenthesis
 blink-cursor-alist '((t . hollow))     ; Cursor blinks solid and hollow
 disabled-command-function nil          ; Don't second-guess advanced commands
 kill-read-only-ok t                    ; Silently copy in read-only buffers
 tab-width 4                            ; Set tab stops
 tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84)
 indent-tabs-mode nil             ; Use spaces only, no tabs
 page-delimiter "^\\s *\n\\s *"   ; Page delim one or more blank lines
 minibuffer-max-depth nil         ; Mini-buffer settings
 display-time-day-and-date t      ; Display the time and date on the mode line
 case-fold-search t               ; Fold case on searches
 indicate-empty-lines t
 fill-column 78)

;;; damn IMPORTANT.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; using vcs
(setq make-backup-files nil)

;;; don't create "lock" links
(setq create-lockfiles nil)

;; split vertical in single frame
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;; Dont ask me when a process is alive while I kill a buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;; tag
(add-hook 'c-mode-common-hook
  (function (lambda ()
              (require 'gtags)
              (gtags-mode t))))

(add-hook 'gtags-mode-hook
          (function (lambda()
                      (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
                      (local-set-key (kbd "M-,") 'gtags-find-rtag)  ; reverse tag
                      (local-set-key (kbd "C-M-,") 'gtags-find-pattern)  ; reverse tag
                      (local-set-key (kbd "C-M-;") 'tm-gtags-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
                      ;;(local-set-key "\M-." 'gtags-find-tag) ;; M-. finds tag
                      )))
(defun tm-gtags-update ()
  "create the gnu global tag file"
  (interactive)
  (if (= 0 (call-process "global" nil nil nil " -p")) ; tagfile doesn't exist?
  	  ;;(start-process "gtags" "*Messages*" "gtags" "--single-update" (buffer-name))
  	  (start-process "gtags" "*Messages*" "global" "--update") ))

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (if (eq system-type 'windows-nt)
      (start-process "update-gtags" "update-gtags" "cmd" "/c" (concat "cd " (gtags-root-dir) " && gtags --single-update " filename ))
    (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename ))))

(defun gtags-update-current-file()
  (interactive)
  (let ((gtagsfilename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer)))))
    (gtags-update-single gtagsfilename)
    (message "Gtags updated for %s" gtagsfilename)))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))


;; (defun tm-gtags-global-update ()
;;   "If current directory is part of a GLOBAL database update it."
;;   (interactive)
;;   (when (tm-gtags-global-dir)
;;     (if (equal (call-process "global" nil nil nil "-vu") 0)
;;         (setq gtags-global-complete-list-obsolete-flag t)
;;       (error "global database update failed"))))

;; (defun tm-gtags-global-dir-p (dir)
;;   "Return non-nil if directory DIR contains a GLOBAL database."
;;   (and (file-exists-p (expand-file-name "GPATH" dir))
;;        (file-exists-p (expand-file-name "GRTAGS" dir))
;;        (file-exists-p (expand-file-name "GSYMS" dir))
;;        (file-exists-p (expand-file-name "GTAGS" dir))))

;; (defun tm-gtags-global-dir (&optional dir)
;;   "Return the nearest super directory that contains a GLOBAL database."
;;   (interactive)
;;   (when (null dir)
;;     (setq dir default-directory))
;;   (cond ((tm-gtags-global-dir-p dir) dir)
;;         ((equal (file-truename dir)
;;                 (file-truename "/")) nil)
;;         (t (tm-gtags-global-dir
;;             (file-name-as-directory
;;              (expand-file-name ".."  dir))))))

(defun tm-gtags-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (next-line)
           (gtags-select-it nil))
          ) ))

 (add-hook 'gtags-mode-hook
           (lambda ()
             ; (add-hook 'after-save-hook 'tm-gtags-update nil t)
  	    (add-hook 'after-save-hook 'gtags-update-hook nil t)
  	    ))

;;;---------------------------------------------------------------------
;;; make executable if shebang is present
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;---------------------------------------------------------------------
;;; change behavious of builtins

(defadvice kill-line (around kill-region-if-active activate)
  "kill region if active with C-k"
  (if (and (called-interactively-p 'any)
           (region-active-p))
      (kill-region (region-beginning) (region-end))
    ad-do-it))

(defadvice yank (after indent-region activate)
  "To make yank content indent automatically."
  (if (member major-mode '(emacs-lisp-mode
                           scheme-mode
                           lisp-mode
                           lisp-interaction-mode
                           c-mode
                           fortran-mode
                           f90-mode
                           yorick-mode
                           c++-mode
                           objc-mode
                           latex-mode
                           plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

;;; delete nasty hidden white spaces at the end of lines
;; le weng from emacs.help
(defun my-save-buffer-dtws (arg)
  "save buffer delete trailing white space, preserve white space
   before point if point is past text"
  (interactive "p")
  (let ((save (when (and (looking-at "\\s-*$")
                         (looking-back "\\s-+"
                                       (line-beginning-position) t))
                (match-string 0))))
    (delete-trailing-whitespace)
    (set-buffer-modified-p t)
    (save-buffer arg)
    (when (and (eq major-mode 'org-mode) save)
      (insert save)
      (set-buffer-modified-p nil))))

(global-set-key [remap save-buffer] 'my-save-buffer-dtws)
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; buttonize addresses
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;;; stop unwanted kill-emacs (while using orgmode)
;; (global-unset-key (kbd "C-x C-c"))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(when (fboundp 'copyright-update)
  (setq copyright-names-regexp "Thierry\\|Free Software")
  (add-hook 'before-save-hook 'copyright-update))

;;; init-general.el ends here
(provide 'general)

;; Custom Bindings
;; ---------------

(global-set-key (kbd "M-SPC") 'set-mark-command) ; was just-one-space

;;; C-M \ (indent) ;; M-; (comment & uncomment-region)
(global-set-key (kbd "C-c s") 'ispell-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)


;; emacs newlines and indents when enter key
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> s") 'yag/switch-to-scratch)
(global-set-key (kbd "<f9> S") 'ielm)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> v") 'visible-mode)

;; Window switching. (C-x O goes to the other window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Indentation help
(global-set-key (kbd "C-^") 'tm-top-join-line)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") 'proced)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'smex)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

;; hilight symbol
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(control meta f3)] 'highlight-symbol-query-replace)
(global-set-key [(meta f5)] 'insert-quotes)

;; double-quote symbol
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

;; a complement to the zap-to-char command, that doesn't eat up the target character
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'tm-kill-whole-line)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c j") 'ace-jump-mode)
(global-set-key (kbd "s-.") 'ace-jump-mode)
(global-set-key (kbd "C-c J") 'ace-jump-buffer)
(global-set-key (kbd "s->") 'ace-jump-buffer)

(global-set-key [remap other-window] 'ace-window)

(global-set-key [(shift return)] 'tm-smart-open-line)
(global-set-key (kbd "M-o") 'tm-smart-open-line)
(global-set-key [(control shift return)] 'tm-smart-open-line-above)
(global-set-key [(control shift up)]  'move-text-up)
(global-set-key [(control shift down)]  'move-text-down)
(global-set-key [(meta shift up)]  'move-text-up)
(global-set-key [(meta shift down)]  'move-text-down)
(global-set-key (kbd "C-c n") 'tm-cleanup-buffer-or-region)
(global-set-key (kbd "C-c f")  'tm-recentf-ido-find-file)
(global-set-key (kbd "C-M-z") 'tm-indent-defun)
(global-set-key (kbd "C-c D") 'tm-delete-file-and-buffer)
(global-set-key (kbd "C-c d") 'tm-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") 'tm-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c r") 'tm-rename-buffer-and-file)
(global-set-key (kbd "C-c t") 'tm-visit-term-buffer)
(global-set-key (kbd "C-c k") 'tm-kill-other-buffers)
(global-set-key (kbd "C-c TAB") 'tm-indent-rigidly-and-copy-to-clipboard)

(defun change-to-other-buffer ()
  "Change to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))
(global-set-key (kbd "<f1>") 'change-to-other-buffer)

;; C-k at beginning of line takes the whole line (no need to c-k twice)
(setq kill-whole-line t)

(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

(global-set-key (kbd "C-x f") 'ffap)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (revert-buffer t t t)
                                (message "buffer is reverted")))

(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-h a") 'apropos)

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

(global-set-key "\C-x\C-r" 'rgrep)
(global-set-key (kbd "C-|") (defun my-just-one-space ()
                                     (interactive)
                                     (just-one-space -1)))

;;; unset C-z first
(global-unset-key "\C-z")

;; C-z TAB cycles through buffers
(global-set-key [(control ?z) tab] 'bury-buffer)

(global-set-key [(control tab)] 'bury-buffer)
(provide 'custom-key-set)

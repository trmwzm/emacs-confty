;; Spaces, please.
(setq-default indent-tabs-mode nil)

;; No more new lines.
(setq require-final-newline nil)
(setq next-line-add-newlines nil)

;; Sets spaces to tabs
(setq-default indent-tabs-mode nil)

;; Backup to one place.
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-inhibited t)

;; Speed up closing-parenthesis blink
(setq blink-matching-delay 0.1)

;; Prevent autoscroll from jumping
(setq scroll-conservatively 10000)

; Don't wrap lines
(setq-default truncate-lines t)

;; Yanks go into system clipboard
(setq x-select-enable-clipboard t)

;; Accept 'y' and 'n' as answers to yes/no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Case-insensitive tab-completion
(setq dabbrev-case-distinction nil)

;; Other settings
(setq dabbrev-case-fold-search nil)
(setq windmove-wrap-around t)
(setq echo-keystrokes 0.1)
(setq delete-active-region nil)

;; Enable spelling.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "british")

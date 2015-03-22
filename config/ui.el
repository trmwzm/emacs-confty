;; disable startup screen and GUI tools.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Maximise by default.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; minor modes
(column-number-mode t)
(blink-cursor-mode -1)
(show-paren-mode 1)
(electric-pair-mode 1)

;; x-gtk-use-system-tooltips
;; (setq tooltip-use-echo-area t)

(provide 'ui)

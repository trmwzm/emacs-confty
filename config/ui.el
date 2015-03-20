;; Hide all the default GUI crap.
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Maximise by default.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; minor modes
(tool-bar-mode -1)
(column-number-mode t)
(blink-cursor-mode -1)
(show-paren-mode 1)
(electric-pair-mode 1)

;; Don't use the awful OSX full screen.
(setq ns-use-native-fullscreen nil)

;; Remove tooltips. Again, OSX fails with them.
;; Ugh, Apple.
;; (setq tooltip-use-echo-area t)

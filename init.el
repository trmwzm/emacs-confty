(defun load-config (file)
  "Prefixes the file name with the config file directory path and loads the
  file."
  (load-file (expand-file-name file "~/.emacs.d/config")))

(load-config "ui.el")
(load-config "editing.el")
(load-config "packages.el")
(load-config "fortran.el")
(load-config "general.el")
(load-config "custom-key-set.el")
(load-config "ido.el")
(load-config "yorick.el")
(load-config "hippie-expand.el")
(load-config "auto-complete.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;--------------------------------------------------------------------------------
;; color-theme
;;--------------------------------------------------------------------------------
(custom-set-variables
 '(custom-safe-themes (quote ("f07583bdbcca020adecb151868c33820dfe3ad5076ca96f6d51b1da3f0db7105" default))))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "theme/replace-colorthemes"))
(load-theme 'dark-laptop)
(custom-set-faces
 '(default ((t (:background "unspecified" :foreground "unspecified")))))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; linum-mode
;;--------------------------------------------------------------------------------
(setq linum-format "%3d ")
(set-face-attribute 'linum nil :background "black" :foreground "unspecified")
;;--------------------------------------------------------------------------------

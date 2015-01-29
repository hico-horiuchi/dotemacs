;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;--------------------------------------------------------------------------------
;; Dictionary.app
;;--------------------------------------------------------------------------------
(global-set-key (kbd "C-c w o")
 (lambda () (interactive)
   (let ((url (concat "dict://" (read-from-minibuffer "" (current-word)))))
     (browse-url url))))
(global-set-key (kbd "C-c w s")
 (lambda (key) (interactive "MSearch: ")
   (let ((url (concat "dict://" key)))
     (browse-url url))))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; テーマ
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

;;--------------------------------------------------------------------------------
;; hl-line-mode
;;--------------------------------------------------------------------------------
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "black"))
    (((class color)
      (background light))
     (:background "black"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;;--------------------------------------------------------------------------------

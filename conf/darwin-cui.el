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

;;--------------------------------------------------------------------------------
;; tabbar
;;--------------------------------------------------------------------------------
;; 色設定
(set-face-attribute 'tabbar-default nil    ; バー自体の色
  :background "gray75" :foreground nil :underline nil)
(set-face-attribute 'tabbar-selected nil   ; アクティブなタブ
  :background "unspecified" :foreground "unspecified" :weight 'bold)
(set-face-attribute 'tabbar-unselected nil ; 非アクティブなタブ
  :background "gray75" :foreground "black")
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; auto-complete
;;--------------------------------------------------------------------------------
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 0)          ; 補完までの時間
(setq ac-auto-show-menu 0) ; メニューが表示されるまでの時間
(setq ac-auto-start 1)     ; 補完が開始される文字数
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "elisp/auto-complete/dict"))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; undohist
;;--------------------------------------------------------------------------------
(require 'undohist)
(undohist-initialize)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; redo+
;;--------------------------------------------------------------------------------
(require 'redo+)
(global-set-key (kbd "C-M-/") 'redo)
(setq undo-no-redo t) ; 過去のUndoがRedoされないようにする
;; 大量のUndoに耐えられるようにする
(setq undo-limit 600000
      undo-strong-limit 900000)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; jaunte
;;--------------------------------------------------------------------------------
(autoload 'jaunte "jaunte")
(global-set-key (kbd "C-c j") 'jaunte)
(setq jaunte-hint-unit 'whitespace)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; popup-kill-ring
;;--------------------------------------------------------------------------------
(autoload 'popup-kill-ring "popup-kill-ring")
(global-set-key (kbd "M-y") 'popup-kill-ring)
(setq popup-kill-ring-popup-width 51)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; go-mode
;;--------------------------------------------------------------------------------
(autoload 'go-mode "go-mode")
(autoload 'gofmt-before-save "go-mode")
(autoload 'godoc "go-mode")
(autoload 'go-download-play "go-mode")
(global-set-key (kbd "C-c g o") 'go-mode)
(add-auto-mode "\\.go$" go-mode)
(add-hook-fn 'go-mode-hook
  (setq tab-width 2)
  (setq indent-tabs-mode t)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (let ((envs '("GOROOT" "GOPATH"))) (exec-path-from-shell-copy-envs envs)))
;; gocode
(add-to-list 'load-path "~/.go/src/github.com/nsf/gocode/emacs")
(require 'go-autocomplete)
;;--------------------------------------------------------------------------------

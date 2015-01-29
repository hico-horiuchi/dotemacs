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
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "theme/solarized"))
(load-theme 'solarized-dark)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; タイトルバー: バッファ名 - emacs@コンピュータ名
;;--------------------------------------------------------------------------------
(setq-default frame-title-format (format "%%b - emacs@%s" (system-name)))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; 対応する括弧をハイライト
;;--------------------------------------------------------------------------------
(setq show-paren-style 'expression)                     ; 括弧内もハイライト
(set-face-background 'show-paren-match-face nil)        ; 背景色: なし
(set-face-underline-p 'show-paren-match-face "#2aa198") ; 下線色
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; mark
;;--------------------------------------------------------------------------------
(defun th-activate-mark-init () (setq cursor-type 'bar))
(defun th-deactivate-mark-init () (setq cursor-type 'box))
(add-hook 'activate-mark-hook 'th-activate-mark-init)
(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; tabbar
;;--------------------------------------------------------------------------------
;; 色設定
(set-face-attribute 'tabbar-default nil    ; バー自体の色
  :background "#839496" :foreground nil :box nil)
(set-face-attribute 'tabbar-selected nil   ; アクティブなタブ
  :background "#002b36" :foreground "#829496" :box nil :weight 'bold)
(set-face-attribute 'tabbar-unselected nil ; 非アクティブなタブ
  :background "#829496" :foreground "#002b36" :box nil)
;; フォント
(defvar tabbar-font "Ricty Regular for Powerline")
(set-face-attribute 'tabbar-default nil :family tabbar-font :height 1.0)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; 環境変数
;;--------------------------------------------------------------------------------
(exec-path-from-shell-initialize)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; ダイアログ
;;--------------------------------------------------------------------------------
(setq use-dialog-box nil)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; フォント
;;--------------------------------------------------------------------------------
(let* ((size 16)
       (asciifont "Ricty")
       (jpfont "Ricty")
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil '(#x0080 . #x024F) fontspec)
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))
;;--------------------------------------------------------------------------------

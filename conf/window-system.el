;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;--------------------------------------------------------------------------------
;; テーマ
;;--------------------------------------------------------------------------------
(custom-set-variables
 '(custom-safe-themes (quote ("7392f213ece957a89580293fb7976359b33d5afd17709a3add22e098c19552a9" default))))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "theme/solarized"))
(load-theme 'solarized-dark)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; タイトルバー: バッファ名 - emacs@コンピュータ名
;;--------------------------------------------------------------------------------
(setq-default frame-title-format (format "%%b - emacs@%s" (system-name)))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; hl-line-mode
;;--------------------------------------------------------------------------------
(global-hl-line-mode)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; 対応する括弧をハイライト
;;--------------------------------------------------------------------------------
(setq show-paren-style 'expression)                     ; 括弧内もハイライト
(set-face-background 'show-paren-match-face nil)        ; 背景色: なし
(set-face-underline-p 'show-paren-match-face "#2aa198") ; 下線色
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; フォント
;;--------------------------------------------------------------------------------
(defvar japanese-font "Ricty Regular for Powerline")
(defvar tabbar-font "Ricty Regular for Powerline")
(set-default-font "Ricty Regular for Powerline-15")
(set-fontset-font nil 'jisx0201                 japanese-font)
(set-fontset-font nil 'latin-jisx0201           japanese-font)
(set-fontset-font nil 'japanese-jisx0208        japanese-font)
(set-fontset-font nil 'japanese-jisx0208-1978   japanese-font)
(set-fontset-font nil 'japanese-jisx0212        japanese-font)
(set-fontset-font nil 'japanese-jisx0213-1      japanese-font)
(set-fontset-font nil 'japanese-jisx0213-2      japanese-font)
(set-fontset-font nil 'japanese-jisx0213-a      japanese-font)
(set-fontset-font nil 'japanese-jisx0213.2004-1 japanese-font)
(set-fontset-font nil 'katakana-sjis            japanese-font)
(set-fontset-font nil 'cp932-2-byte             japanese-font)
(set-fontset-font nil 'cp932                    japanese-font)
(set-fontset-font nil '(#x3040 . #x309f)        japanese-font)
(set-fontset-font nil '(#x30a0 . #x30ff)        japanese-font)
(set-fontset-font nil '(#xff00 . #xffef)        japanese-font)
(set-fontset-font nil '(#x0370 . #x03ff)        japanese-font)
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
(set-face-attribute 'tabbar-default nil :family tabbar-font :height 1.0)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; mark
;;--------------------------------------------------------------------------------
(defun th-activate-mark-init () (setq cursor-type 'bar))
(defun th-deactivate-mark-init () (setq cursor-type 'box))
(add-hook 'activate-mark-hook 'th-activate-mark-init)
(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)
;;--------------------------------------------------------------------------------

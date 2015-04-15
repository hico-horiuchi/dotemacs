;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;--------------------------------------------------------------------------------
;; 言語設定
;;--------------------------------------------------------------------------------
;; デフォルトの文字コードと改行コード
(set-default-coding-systems 'utf-8-dos)
;; パスとファイル名はShift_JIS
(setq default-file-name-coding-system 'sjis-dos)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; テーマ
;;--------------------------------------------------------------------------------
(custom-set-variables
 '(custom-safe-themes (quote ("b2231b396c332cf82d255f6a882687cb3c97ac36ee4b918713883a8f148af966" default))))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "theme/solarized"))
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)
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
;; フォント
;;--------------------------------------------------------------------------------
(defvar japanese-font "Ricty Regular for Powerline")
(set-default-font "Ricty Regular for Powerline-12")
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
(set-fontset-font nil 'katakana-jisx0201        japanese-font)
(set-fontset-font nil 'cp932-2-byte             japanese-font)
(set-fontset-font nil 'cp932                    japanese-font)
(set-fontset-font nil '(#x3040 . #x309f)        japanese-font)
(set-fontset-font nil '(#x30a0 . #x30ff)        japanese-font)
(set-fontset-font nil '(#xff00 . #xffef)        japanese-font)
(set-fontset-font nil '(#x0370 . #x03ff)        japanese-font)
;;--------------------------------------------------------------------------------

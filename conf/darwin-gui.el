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
  :background "#829496" :foreground "#002b36" :box nil :weight 'bold)
(set-face-attribute 'tabbar-unselected nil ; 非アクティブなタブ
  :background "#002b36" :foreground "#829496" :box nil)
;; フォント
(defvar tabbar-font "Ricty")
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
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (let ((envs '("GOROOT" "GOPATH"))) (exec-path-from-shell-copy-envs envs)))
;; gocode
(add-to-list 'load-path "~/.go/src/github.com/nsf/gocode/emacs")
(require 'go-autocomplete)
;;--------------------------------------------------------------------------------

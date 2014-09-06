;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;--------------------------------------------------------------------------------
;; add-hook macro
;;--------------------------------------------------------------------------------
(defmacro add-hook-fn (name &rest body)
  `(add-hook ,name #'(lambda () ,@body)))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; add auto-mode-alist macro
;;--------------------------------------------------------------------------------
(defmacro add-auto-mode (name mode)
  `(setq auto-mode-alist (cons '(,name . ,mode) auto-mode-alist)))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; load-path
;;--------------------------------------------------------------------------------
(let ((default-directory (expand-file-name "~/.emacs.d/elisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; 基本設定
;;--------------------------------------------------------------------------------
(custom-set-variables
 '(auto-save-default nil)          ; 自動保存しない
 '(auto-save-list-file-name nil)   ; 自動保存ファイルの名前を記録しない
 '(auto-save-list-file-prefix nil) ; 自動保存ファイルリストを初期化しない
 '(delete-auto-save-files t)       ; 自動保存ファイルを削除
 '(inhibit-startup-screen t)       ; スタートアップ画面を非表示
 '(make-backup-files nil)          ; バックアップファイルを作成しない
 '(scroll-bar-mode nil)            ; スクロールバーなし
 '(transient-mark-mode t)          ; アクティブなリージョンをハイライト
 '(menu-bar-mode nil)              ; メニューバー非表示
 '(tool-bar-mode nil)              ; ツールバー非表示
 '(indent-tabs-mode nil))          ; タブを空白で入力
(set-face-foreground 'font-lock-function-name-face "brightblue")
(set-face-foreground 'minibuffer-prompt "brightblue")
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; モードライン
;;--------------------------------------------------------------------------------
(load "~/.emacs.d/conf/mode-line")
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; color-theme
;;--------------------------------------------------------------------------------
(load "~/.emacs.d/elisp/color-theme/themes/color-theme-library")
(require 'color-theme)
(color-theme-dark-laptop)
(global-font-lock-mode t)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; linum-mode
;;--------------------------------------------------------------------------------
(global-linum-mode)
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
(global-hl-line-mode)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; 補完
;;--------------------------------------------------------------------------------
;; バッファー名の問い合わせで大文字小文字の区別をしない
(setq read-buffer-completion-ignore-case t)
;; ファイル名の問い合わせで大文字小文字の区別をしない
(setq read-file-name-completion-ignore-case t)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; 対応する括弧をハイライト
;;--------------------------------------------------------------------------------
(show-paren-mode t)
(setq show-paren-delay 0) ; 表示までの秒数: 0秒
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; 対応する記号を自動補完
;;--------------------------------------------------------------------------------
(require 'skeleton)
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?『 _ ?』))
(global-set-key (kbd "『") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?【 _ ?】))
(global-set-key (kbd "【") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?[ _ ?]))
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?〔 _ ?〕))
(global-set-key (kbd "〔") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?〈 _ ?〉))
(global-set-key (kbd "〈") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?《 _ ?》))
(global-set-key (kbd "《") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?{ _ ?}))
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?「 _ ?」))
(global-set-key (kbd "「") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?( _ ?)))
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?‘ _ ’〕))
(global-set-key (kbd "‘") 'skeleton-pair-insert-maybe)
(add-to-list 'skeleton-pair-alist '(?“ _ ?”))
(global-set-key (kbd "“") 'skeleton-pair-insert-maybe)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; 保存時に最終行の改行を削除
;;--------------------------------------------------------------------------------
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; ウィンドウ移動
;;--------------------------------------------------------------------------------
(windmove-default-keybindings) ; Shift+カーソルキーで移動
(setq windmove-wrap-around t)  ; 端から反対側へ移動
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; yes/no選択
;;--------------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)                ; y/nで選択
(define-key query-replace-map [return] 'act) ; RETでyes選択
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; キーバインド
;;--------------------------------------------------------------------------------
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-cl" 'toggle-truncate-lines)
(global-set-key "\C-cws" 'whitespace-mode)
(global-set-key "\C-crs" 'replace-string)
(global-set-key "\C-crr" 'replace-regexp)
(global-set-key "\C-cfw" 'follow-delete-other-windows-and-split)
(global-set-key [zenkaku-hankaku] 'toggle-input-method)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; tabbar
;;--------------------------------------------------------------------------------
(require 'tabbar)
(tabbar-mode)
(global-set-key "\M-]" 'tabbar-forward)  ; 次のタブ
(global-set-key "\M-[" 'tabbar-backward) ; 前のタブ
;; タブ上でマウスホイールを使わない
(tabbar-mwheel-mode nil)
;; グループを使わない
(setq tabbar-buffer-groups-function nil)
;; 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
;; 色設定
(set-face-attribute 'tabbar-default nil    ; バー自体の色
  :background "white" :foreground nil :box nil)
(set-face-attribute 'tabbar-selected nil   ; アクティブなタブ
  :background "black" :foreground "white" :box nil :weight 'bold)
(set-face-attribute 'tabbar-unselected nil ; 非アクティブなタブ
  :background "white" :foreground "black" :box nil)
;; タブの間
(setq tabbar-separator '(1.0))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; direx
;;--------------------------------------------------------------------------------
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(require 'direx-project)
(setq direx:leaf-icon   "  "
      direx:open-icon   "- "
      direx:closed-icon "+ ")
(push '(direx:direx-mode :position left :width 30 :dedicated t)
  popwin:special-display-config)
(defun direx:jump-to-project-directory ()
  (interactive)
  (let ((result (ignore-errors
    (direx-project:jump-to-project-root-other-window) t)))
    (unless result (direx:jump-to-directory-other-window))))
(global-set-key "\C-cdx" 'direx:jump-to-project-directory)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; auto-complete
;;--------------------------------------------------------------------------------
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 0)          ; 補完までの時間
(setq ac-auto-show-menu 0) ; メニューが表示されるまでの時間
(setq ac-auto-start 1)     ; 補完が開始される文字数
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/dict")
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
(global-set-key "\C-cj" 'jaunte)
(setq jaunte-hint-unit 'whitespace)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; popup-kill-ring
;;--------------------------------------------------------------------------------
(autoload 'popup-kill-ring "popup-kill-ring")
(global-set-key "\M-y" 'popup-kill-ring)
(setq popup-kill-ring-popup-width 51)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; vc-git
;;--------------------------------------------------------------------------------
;; vc-gitをオフにする
(setq vc-handled-backends nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; diff-mode
;;--------------------------------------------------------------------------------
;; diffの表示方法を変更
(defun diff-mode-setup-faces ()
  ;; 追加された行は緑で表示
  (set-face-attribute 'diff-added nil
    :foreground "green" :background "black")
  ;; 削除された行は赤で表示
  (set-face-attribute 'diff-removed nil
    :foreground "red" :background "black"))
(add-hook-fn 'diff-mode-hook (diff-mode-setup-faces))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; magit
;;--------------------------------------------------------------------------------
(autoload 'magit-status         "magit")
(autoload 'magit-log            "magit")
(autoload 'magit-reflog         "magit")
(autoload 'magit-branch-manager "magit")
(autoload 'magit-browse         "magit")
(global-set-key "\C-cms" 'magit-status)         ; git status
(global-set-key "\C-cml" 'magit-log)            ; git log
(global-set-key "\C-cmr" 'magit-reflog)         ; git reflog
(global-set-key "\C-cmb" 'magit-branch-manager) ; git branch
(global-set-key "\C-cmw" 'magit-browse)
(add-hook-fn 'magit-mode-hook
  ;; diff用のfaceを設定する
  (diff-mode-setup-faces)
  ;; diffの表示設定が上書きされてしまうのでハイライトを無効にする
  (set-face-attribute 'magit-item-highlight nil :inherit nil)
  ;; ファイル名は青で表示
  (set-face-attribute 'magit-diff-file-header nil :foreground "dark blue"))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; c-mode
;;--------------------------------------------------------------------------------
(global-set-key "\C-cc" 'c-mode)
(add-hook-fn 'c-mode-common-hook
  (c-set-style "stroustrup")     ; インデントスタイル
  (c-toggle-auto-hungry-state t) ; DELで左側の空白を全削除
  (setq c-basic-offset 2)        ; インデント幅: 2
  ;; オートインデント
  (define-key c-mode-base-map "\C-m" 'newline-and-indent))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; php-mode
;;--------------------------------------------------------------------------------
(autoload 'php-mode "php-mode")
(add-auto-mode "\\.php$" php-mode)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; ruby-mode
;;--------------------------------------------------------------------------------
(autoload 'ruby-mode "ruby-mode")
(autoload 'run-ruby "inf-ruby")
(autoload 'inf-ruby-keys "inf-ruby")
;; (autoload 'inf-ruby-setup-keybindings "inf-ruby")
(autoload 'ruby-electric-mode "ruby-electric")
(global-set-key "\C-crb" 'ruby-mode)
(add-auto-mode "\\.rb" ruby-mode)
(add-hook-fn 'ruby-mode-hook
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  ;; (inf-ruby-setup-keybindings)
  ;; 括弧の自動挿入
  (ruby-electric-mode)
  ;; 改行時に自動インデント
  (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent))
;; インデント幅: 2
(add-hook-fn 'ruby-mode-hook (setq ruby-indent-level 2))
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; rhtml-mode
;;--------------------------------------------------------------------------------
(autoload 'rhtml-mode "rhtml-mode")
(add-auto-mode "\\.erb" rhtml-mode)
(add-hook-fn 'rhtml-mode-hook
  (set-face-background 'erb-face nil)
  (set-face-underline-p 'erb-face t)
  (set-face-background 'erb-exec-face nil)
  (set-face-underline-p 'erb-exec-face t))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; yaml-mode
;;--------------------------------------------------------------------------------
(autoload 'yaml-mode "yaml-mode")
(add-auto-mode "\\.yml$" yaml-mode)
(add-hook-fn 'yaml-mode-hook
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; slim-mode
;;--------------------------------------------------------------------------------
(autoload 'slim-mode "slim-mode")
(add-auto-mode "\\.slim" slim-mode)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; css-mode
;;--------------------------------------------------------------------------------
(setq css-indent-offset 2)
(setq cssm-indent-function #'cssm-c-style-indenter)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; less-css-mode
;;--------------------------------------------------------------------------------
(autoload 'less-css-mode "less-css-mode")
(add-auto-mode "\\.less" less-css-mode)
(setq c-basic-offset 2)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; markdown-mode
;;--------------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode")
(add-auto-mode "\\.md" markdown-mode)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; coffee-mode
;;--------------------------------------------------------------------------------
(autoload 'coffee-mode "coffee-mode")
(add-auto-mode "\\.coffee" coffee-mode)
(add-hook-fn 'coffee-mode-hook
  (setq coffee-tab-width 2)
  (define-key coffee-mode-map "\C-m" 'coffee-newline-and-indent))
;;--------------------------------------------------------------------------------

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
(let ((default-directory (expand-file-name (concat user-emacs-directory "elisp"))))
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
 '(initial-scratch-message nil)    ; scratchメッセージを非表示
 '(backup-inhibited t)             ; 保存時にバックアップファイルを作成しない
 '(make-backup-files nil)          ; バックアップファイルを作成しない
 '(scroll-bar-mode nil)            ; スクロールバーなし
 '(transient-mark-mode t)          ; アクティブなリージョンをハイライト
 '(menu-bar-mode nil)              ; メニューバー非表示
 '(tool-bar-mode nil)              ; ツールバー非表示
 '(indent-tabs-mode nil)           ; タブを空白で入力
 '(comment-empty-lines t)          ; 空行もコメントアウト
 '(blink-cursor-mode nil)          ; カーソルを点滅しない
 '(kill-whole-line t)              ; C-kで改行までまとめてカット
 '(require-final-newline t)        ; 最終行に必ず1行追加
 '(next-line-add-newlines nil))    ; 最終行での新規行の追加を禁止
(set-face-foreground 'font-lock-function-name-face "brightblue")
(set-face-foreground 'minibuffer-prompt "brightblue")
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; モードライン
;;--------------------------------------------------------------------------------
(load (concat user-emacs-directory "conf/mode-line"))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; テーマ
;;--------------------------------------------------------------------------------
(custom-set-variables
 '(custom-safe-themes (quote ("f07583bdbcca020adecb151868c33820dfe3ad5076ca96f6d51b1da3f0db7105" default))))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "theme/replace-colorthemes"))
(load-theme 'dark-laptop t)
(set-frame-parameter nil 'background-mode 'dark)
(enable-theme 'dark-laptop)
(custom-set-faces
 '(default ((t (:background "unspecified-bg" :foreground "unspecified-fg")))))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; linum-mode
;;--------------------------------------------------------------------------------
(global-linum-mode)
(setq linum-format "%3d ")
(set-face-attribute 'linum nil :background "black" :foreground "unspecified-fg")
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; git-gutter
;;--------------------------------------------------------------------------------
;; (require 'git-gutter)
;; (global-git-gutter-mode)
;; (git-gutter:linum-setup)
;; (global-set-key (kbd "C-c g h") 'git-gutter:popup-hunk)
;; (global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)
;; (global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)
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
(global-set-key (kbd "C-c l")   'toggle-truncate-lines)
(global-set-key (kbd "C-c w s") 'whitespace-mode)
(global-set-key (kbd "C-c r s") 'replace-string)
(global-set-key (kbd "C-c r r") 'replace-regexp)
(global-set-key (kbd "C-c f w") 'follow-delete-other-windows-and-split)
(global-set-key [zenkaku-hankaku] 'toggle-input-method)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; tabbar
;;--------------------------------------------------------------------------------
(require 'tabbar)
(tabbar-mode)
(global-set-key (kbd "M-]") 'tabbar-forward)  ; 次のタブ
(global-set-key (kbd "M-[") 'tabbar-backward) ; 前のタブ
;; タブ上でマウスホイールを使わない
(tabbar-mwheel-mode nil)
;; グループを使わない
(setq tabbar-buffer-groups-function nil)
;; タブの間隔
(setq tabbar-separator '(1.0))
;; 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
;; 色設定
(set-face-attribute 'tabbar-default nil    ; バー自体の色
  :background "gray75" :foreground nil :underline nil)
(set-face-attribute 'tabbar-selected nil   ; アクティブなタブ
  :background "unspecified-bg" :foreground "unspecified-fg" :weight 'bold)
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
;; vc-git
;;--------------------------------------------------------------------------------
;; vc-gitをオフにする
(setq vc-handled-backends nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; dash
;;--------------------------------------------------------------------------------
;; (require 'dash)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; exec-path-from-shell
;;--------------------------------------------------------------------------------
(autoload 'exec-path-from-shell-initialize "exec-path-from-shell")
(autoload 'exec-path-from-shell-copy-envs "exec-path-from-shell")
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; diff-mode
;;--------------------------------------------------------------------------------
;; diffの表示方法を変更
(defun diff-mode-setup-faces ()
  ;; 追加された行は緑で表示
  (set-face-attribute 'diff-added nil :foreground "green" :background "black")
  ;; 削除された行は赤で表示
  (set-face-attribute 'diff-removed nil :foreground "red" :background "black"))
(add-hook-fn 'diff-mode-hook (diff-mode-setup-faces))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; magit
;;--------------------------------------------------------------------------------
;; (autoload 'magit-status         "magit")
;; (autoload 'magit-log            "magit")
;; (autoload 'magit-reflog         "magit")
;; (autoload 'magit-branch-manager "magit")
;; (global-set-key (kbd "C-c m s") 'magit-status)         ; git status
;; (global-set-key (kbd "C-c m l") 'magit-log)            ; git log
;; (global-set-key (kbd "C-c m r") 'magit-reflog)         ; git reflog
;; (global-set-key (kbd "C-c m b") 'magit-branch-manager) ; git branch
;; (add-hook-fn 'magit-mode-hook
;;   (setq magit-auto-revert-mode nil)
;;   ;; diff用のfaceを設定する
;;   (diff-mode-setup-faces)
;;   ;; diffの表示設定が上書きされてしまうのでハイライトを無効にする
;;   (set-face-attribute 'magit-item-highlight nil :inherit nil)
;;   ;; ファイル名は青で表示
;;   (set-face-attribute 'magit-diff-file-header nil :foreground "dark blue"))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; c-mode
;;--------------------------------------------------------------------------------
(global-set-key (kbd "C-c c c") 'c-mode)
(add-hook-fn 'c-mode-common-hook
  (c-set-style "stroustrup")     ; インデントスタイル
  (c-toggle-auto-hungry-state t) ; DELで左側の空白を全削除
  (setq c-basic-offset 2)        ; インデント幅: 2
  ;; オートインデント
  (define-key c-mode-base-map "\C-m" 'newline-and-indent))
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

;;--------------------------------------------------------------------------------
;; crystal-mode
;;--------------------------------------------------------------------------------
(autoload 'crystal-mode "crystal-mode")
(global-set-key (kbd "C-c c r") 'crystal-mode)
(add-auto-mode "\\.cr$" crystal-mode)
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
(global-set-key (kbd "C-c r b") 'ruby-mode)
(add-auto-mode "\\.rb$" ruby-mode)
(add-auto-mode "\\.rake$" ruby-mode)
(add-auto-mode "Gemfile$" ruby-mode)
(add-auto-mode "Gomfile$" ruby-mode)
(add-auto-mode "Vagrantfile$" ruby-mode)
(add-hook-fn 'ruby-mode-hook
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  ;; (inf-ruby-setup-keybindings)
  ;; 括弧の自動挿入
  (ruby-electric-mode)
  ;; 改行時に自動インデント
  (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
  ;; magic commentを入れない
  (ruby-insert-encoding-magic-comment nil))
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
(add-auto-mode "\\.erb$" rhtml-mode)
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
(add-auto-mode "\\.yml.sample$" yaml-mode)
(add-hook-fn 'yaml-mode-hook
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; slim-mode
;;--------------------------------------------------------------------------------
(autoload 'slim-mode "slim-mode")
(add-auto-mode "\\.slim$" slim-mode)
(add-auto-mode "\\.ace$" slim-mode)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; css-mode
;;--------------------------------------------------------------------------------
(add-auto-mode "\\.scss$" css-mode)
(setq css-indent-offset 2)
(setq cssm-indent-function #'cssm-c-style-indenter)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; less-css-mode
;;--------------------------------------------------------------------------------
(autoload 'less-css-mode "less-css-mode")
(add-auto-mode "\\.less$" less-css-mode)
(setq c-basic-offset 2)
(defvar hexcolor-keywords
  '(("#[ABCDEFabcdef0-9]\\{3,6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))
(defun hexcolor-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolor-keywords))
(add-hook 'css-mode-hook 'hexcolor-add-to-font-lock)
(add-hook 'less-css-mode-hook 'hexcolor-add-to-font-lock)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; markdown-mode
;;--------------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode")
(add-auto-mode "\\.md$" markdown-mode)
(add-hook-fn 'markdown-mode-hook
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; js-mode
;;--------------------------------------------------------------------------------
(add-hook-fn 'js-mode-hook
  (setq js-indent-level 2)
  (define-key js-mode-map "\C-m" 'newline-and-indent))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; coffee-mode
;;--------------------------------------------------------------------------------
(autoload 'coffee-mode "coffee-mode")
(add-auto-mode "\\.coffee$" coffee-mode)
(add-auto-mode "\\.cjsx$" coffee-mode)
(add-hook-fn 'coffee-mode-hook
  (setq coffee-tab-width 2)
  (define-key coffee-mode-map "\C-m" 'coffee-newline-and-indent))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; dockerfile-mode
;;--------------------------------------------------------------------------------
(autoload 'dockerfile-mode "dockerfile-mode")
(add-auto-mode "Dockerfile$" dockerfile-mode)
;;--------------------------------------------------------------------------------

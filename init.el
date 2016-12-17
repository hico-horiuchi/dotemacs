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
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; モードライン
;;--------------------------------------------------------------------------------
(load (concat user-emacs-directory "conf/mode-line"))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; linum-mode
;;--------------------------------------------------------------------------------
(global-linum-mode t)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; hl-line-mode
;;--------------------------------------------------------------------------------
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
(add-hook 'diff-mode-hook 'diff-mode-setup-faces)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; magit
;;--------------------------------------------------------------------------------
(autoload 'magit-status         "magit")
(autoload 'magit-log            "magit")
(autoload 'magit-reflog         "magit")
(autoload 'magit-branch-manager "magit")
(global-set-key (kbd "C-c m s") 'magit-status)         ; git status
(global-set-key (kbd "C-c m l") 'magit-log)            ; git log
(global-set-key (kbd "C-c m r") 'magit-reflog)         ; git reflog
(global-set-key (kbd "C-c m b") 'magit-branch-manager) ; git branch
(add-hook-fn 'magit-mode-hook
  (setq magit-auto-revert-mode nil)
  ;; diff用のfaceを設定する
  (diff-mode-setup-faces)
  ;; ハイライトを無効
  (remove-hook 'magit-section-highlight-hook 'magit-diff-highlight)
  (remove-hook 'magit-section-highlight-hook 'magit-section-highlight)
  (set-face-attribute 'magit-diff-added nil :background "unspecified-bg")
  (set-face-attribute 'magit-diff-removed nil :background "unspecified-bg"))
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
;; groovy-mode
;;--------------------------------------------------------------------------------
(autoload 'groovy-mode "groovy-mode")
(add-auto-mode "\\.groovy$" groovy-mode)
(add-hook-fn 'groovy-mode-hook
  (require 'groovy-electric)
  (groovy-electric-mode))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; ruby-mode
;;--------------------------------------------------------------------------------
(autoload 'ruby-mode "ruby-mode")
(autoload 'ruby-electric-mode "ruby-electric")
(global-set-key (kbd "C-c r b") 'ruby-mode)
(add-auto-mode "\\.rb$" ruby-mode)
(setq ruby-deep-indent-paren-style nil)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-hook-fn 'ruby-mode-hook
  ;; 括弧の自動挿入
  (ruby-electric-mode)
  ;; インデント幅: 2
  (setq ruby-indent-level 2)
  ;; 改行時に自動インデント
  (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent))
;; defadviceで既存のインデント関数ruby-indent-lineに対する追加処理を定義する
;; after -> 既存の関数の処理の後に実行される
;; unindent-closing-paren -> このアドバイスの名前
;; activate -> このアドバイスがすぐに有効になる
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      ;; ポイント(カーソル)をインデントの位置に移動する
      (back-to-indentation)
      ;; syntax-ppssはparserの状態を表すリストを返す
      ;; 1番目の要素は括弧の深さ、2番目の要素は一番内側の開始括弧の位置を表す
      (let ((state (syntax-ppss)))
        ;; ポイントの初期状態とインデントの位置との差をoffsetとする
        (setq offset (- column (current-column)))
        ;; ポイントの位置の文字が')'で括弧の中にある場合
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          ;; 一番内側の'('に移動
          (goto-char (cadr state))
          ;; インデント幅を取得
          (setq indent (current-indentation)))))
    (when indent
      ;; インデントする
      (indent-line-to indent)
      ;; オフセットが存在する場合、その分だけポイントを移動する
      ;; つまり、インデント修正後のポイントのあるべき場所に戻る
      (when (> offset 0) (forward-char offset)))))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; inf-ruby
;;--------------------------------------------------------------------------------
(autoload 'inf-ruby-minor-mode "inf-ruby")
(setenv "PAGER" (executable-find "cat"))
(add-hook-fn 'ruby-mode-hook
  (inf-ruby-minor-mode)
  (inf-ruby-switch-setup))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; rspec-mode
;;--------------------------------------------------------------------------------
(autoload 'rspec-mode "rspec-mode")
(add-hook 'ruby-mode-hook 'rspec-mode)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; flycheck
;;--------------------------------------------------------------------------------
(autoload 'flycheck-mode "flycheck")
(add-hook 'ruby-mode-hook 'flycheck-mode)
(setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; slim-mode
;;--------------------------------------------------------------------------------
(autoload 'slim-mode "slim-mode")
(add-auto-mode "\\.slim$" slim-mode)
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; css-mode
;;--------------------------------------------------------------------------------
(setq css-indent-offset 2)
(setq cssm-indent-function #'cssm-c-style-indenter)
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
;; textile-mode
;;--------------------------------------------------------------------------------
(autoload 'textile-mode "textile-mode")
(add-auto-mode "\\.textile$" textile-mode)
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
(add-hook-fn 'coffee-mode-hook
  (setq coffee-tab-width 2)
  (define-key coffee-mode-map "\C-m" 'coffee-newline-and-indent))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; yatex
;;--------------------------------------------------------------------------------
;; (autoload 'yatex-mode "yatex")
;; (add-auto-mode "\\.tex$" yatex-mode)
;; ;; 自動改行を無効
;; (add-hook-fn 'yatex-mode-hook (setq auto-fill-function nil))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; org-mode
;;--------------------------------------------------------------------------------
(autoload 'org-install "org-install")
(autoload 'org-bullets-mode "org-bullets")
(global-set-key (kbd "C-c o r") 'org-mode)
(global-set-key (kbd "C-c o a") 'org-agenda)
(add-auto-mode "\\.org$" org-mode)
(add-hook-fn 'org-mode-hook
  (global-set-key (kbd "C-c o i") 'org-toggle-inline-images)
  ;; 日本語リンクのfont-lock対策
  (setq org-activate-links '(date bracket radio tag date footnote angle)
        org-return-follows-link t                  ; RETでカーソル下のリンクを開く
        org-startup-truncated nil                  ; 右端で折り返す
        org-src-fontify-natively t                 ; SRCブロックをハイライト表示
        org-log-done 'time                         ; DONE時にタイムスタンプ
        org-use-fast-todo-selection t              ; TODO項目の入力補助
        org-export-latex-coding-system 'euc-jp-dos ; TeX形式をEUC-JPでエクスポート
        org-export-latex-classes nil)
  ;; TODO項目
  (setq org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
     (sequence "APPT(a)"                           "|" "DONE(x)" "CANCEL(c)")))
  ;; org-agendaのディレクトリ
  (setq org-directory (concat user-emacs-directory "org")
        org-agenda-files (list org-directory))
  ;; LaTeXエクスポートの設定
  (load (concat user-emacs-directory "conf/ox-latex")))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; org-bullets
;;--------------------------------------------------------------------------------
(global-set-key (kbd "C-c o b m") 'org-bullets-mode)
(add-hook-fn 'org-bullets-mode-hook
  (global-set-key (kbd "C-c o b e") 'org-bullets-export)
  (global-set-key (kbd "C-c o b i") 'org-bullets-import))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; system-type & window-system
;;--------------------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (if (window-system)
      (load (concat user-emacs-directory "conf/windows-gui"))))
(when (eq system-type 'darwin)
  (if (window-system)
      (load (concat user-emacs-directory "conf/darwin-gui"))
    (load (concat user-emacs-directory "conf/darwin-cui"))))
;;--------------------------------------------------------------------------------

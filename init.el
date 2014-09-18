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
 '(make-backup-files nil)          ; バックアップファイルを作成しない
 '(scroll-bar-mode nil)            ; スクロールバーなし
 '(transient-mark-mode t)          ; アクティブなリージョンをハイライト
 '(menu-bar-mode nil)              ; メニューバー非表示
 '(tool-bar-mode nil)              ; ツールバー非表示
 '(indent-tabs-mode nil))          ; タブを空白で入力
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; モードライン
;;--------------------------------------------------------------------------------
(load (concat user-emacs-directory "conf/mode-line"))
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
;; 常時起動モード
;;--------------------------------------------------------------------------------
(global-linum-mode t)       ; 行番号表示
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
;; vc-git
;;--------------------------------------------------------------------------------
;; vc-gitをオフにする
(setq vc-handled-backends nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)
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
;; (setq tabbar-buffer-groups-function nil)
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
  (load (concat user-emacs-directory "conf/ox-latex"))
  ;; SRCブロックをハイライト表示
  (setq org-src-fontify-natively t))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; org-bullets
;;--------------------------------------------------------------------------------
(global-set-key (kbd "C-c o b") 'org-bullets-mode)
(add-hook-fn 'org-bullets-mode-hook
  (global-set-key (kbd "C-c o b e") 'org-bullets-export)
  (global-set-key (kbd "C-c o b i") 'org-bullets-import))
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
;; coffee-mode
;;--------------------------------------------------------------------------------
(autoload 'coffee-mode "coffee-mode")
(add-auto-mode "\\.coffee$" coffee-mode)
(add-hook-fn 'coffee-mode-hook
  (setq coffee-tab-width 2)
  (define-key coffee-mode-map "\C-m" 'coffee-newline-and-indent))
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
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; system-type
;;--------------------------------------------------------------------------------
(if (eq system-type 'windows-nt)
  (load (concat user-emacs-directory "conf/windows-nt")))
;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; window-system
;;--------------------------------------------------------------------------------
(when window-system
  (load (concat user-emacs-directory "conf/window-system")))
;;--------------------------------------------------------------------------------

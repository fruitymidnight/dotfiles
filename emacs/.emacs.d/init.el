                                        ; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;----------------------------------------------------------------------
;; @ Load Path
;; 
;;   ロードパスの追加

(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/site-elisp"
                   "~/.emacs.d/elisp"
                   )
                 load-path))

;;----------------------------------------------------------------------
;; @ macro

(load "initfuncs" nil t)

;;----------------------------------------------------------------------
;; @ Language

;; 環境変数の設定
(setenv "LANG" "ja_JP.UTF-8")
(setenv "LC_CTIME" "C") ;; find-dired の日付の設定が面倒なので

;; 言語を日本語にする
(set-language-environment 'Japanese)

;; 極力UTF-8とする
(prefer-coding-system 'utf-8-unix)

(when windows-p
  ;; 日本語入力のための設定
  (set-keyboard-coding-system 'cp932)

  (set-file-name-coding-system 'cp932)
  (setq default-process-coding-system '(cp932 . cp932))

  ;; 日本語をパラメータに取りうるコマンド関係のエンコードを指定
  (set-function-coding-system shell-command 'utf-8 'cp932)
  (set-function-coding-system grep 'utf-8 'cp932)
  )


;; ----------------------------------------------------------------------
;; @ ime

(cond
 (darwin-p
  ;; Input Method
  (mac-input-method-mode t)

  ;; Google Japanese Input
  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-type `box)
  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-color "magenta")
  )
 (windows-p
  ;; 標準IMEの設定
  (setq default-input-method "W32-IME")

  ;; IME状態のモードライン表示
  (setq-default w32-ime-mode-line-state-indicator "[Aa]")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

  ;; IMEの初期化
  (w32-ime-initialize)

  ;; IME OFF時の初期カーソルカラー
  (set-cursor-color "red")

  ;; IME ON/OFF時のカーソルカラー
  (add-hook 'input-method-activate-hook
            (lambda() (set-cursor-color "green")))
  (add-hook 'input-method-inactivate-hook
            (lambda() (set-cursor-color "red")))

  ;; バッファ切り替え時にIME状態を引き継ぐ
  (setq w32-ime-buffer-switch-p nil)
  )
 )

;; ----------------------------------------------------------------------
;; @ font

(cond
 ;; Mac OS
 (darwin-p
  (if window-system
      (when (x-list-fonts "Ricty")
        (let* ((size 14)
               (asciifont "Ricty")
               (jpfont "Ricty")
               (h (* size 10))
               (fontspec)
               (jp-fontspec))
          (set-face-attribute 'default nil :family asciifont :height h)
          (setq fontspec (font-spec :family asciifont))
          (setq jp-fontspec (font-spec :family jpfont))
          (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
          (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
          (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
          (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
          (set-fontset-font nil '(#x0080 . #x024F) fontspec)
          (set-fontset-font nil '(#x0370 . #x03FF) fontspec)))
    )
  )
 ;; Windows
 (windows-p
  ;; 標準フォントの設定
  (set-default-font "ricty-10"))
 )


;; ----------------------------------------------------------------------
;; @ frame

;; フレームタイトルの設定
(setq frame-title-format (concat "%b - emacs@" system-name))

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.85))

;; 起動時にウィンドウ最大化
;; http://www.emacswiki.org/emacs/FullScreen#toc12
(defun jbr-init ()
  "Called from term-setup-hook after the default
   terminal setup is
   done or directly from startup if term-setup-hook not
   used.  The value
   0xF030 is the command for maximizing a window."
  (interactive)
  (w32-send-sys-command #xf030)
  (ecb-redraw-layout)
  (calendar))

(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-frame-position (selected-frame) 0 0)
         (setq term-setup-hook 'jbr-init)
         (setq window-setup-hook 'jbr-init))
        ((eq ws 'ns)
         (set-frame-position (selected-frame) 0 0)

         ;; 横サイズは (frame-width) で調整すること
         (set-frame-size (selected-frame) 269   (- (/ (- (x-display-pixel-height) 40) (frame-char-height)) 1)))))

(cond
 (emacs24-p
  (load-theme 'misterioso t))
 )

;; スクロールバーを右側に
(set-scroll-bar-mode 'right)                   

;; ツールバーをす消
(tool-bar-mode 0)

;; メニューバーを消す
(menu-bar-mode 0)


;; ----------------------------------------------------------------------
;; @ buffer

;; 画面外文字への切り詰め表示
(setq truncate-lines nil)
;;(setq truncate-lines t)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファにディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffer-re "*[^*]+*")


;; ----------------------------------------------------------------------
;; @ modeline

;; 行番号の表示
;; (line-number-mode t)

;; 列番号の表示
;; (column-number-mode t)

;; 時刻の表示
(require 'time)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time)

;; モードラインに関数名
;; (which-function-mode 1)

;; cp932エンコード時の表示を「P」とする
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

(setq-default
 mode-line-position
 '(
   " "
   ;; Position, including warning for 80 columns
   (:propertize "%4l" face mode-line-position-face)
   (:propertize "/" face mode-line-delim-face-1)
   (:eval
    (number-to-string (count-lines (point-min) (point-max))))
   " "
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " "
   ))

(setq-default
 mode-line-format
 '("%e"
   mode-line-mule-info
   ;; emacsclient [default -- keep?]
   mode-line-client
   mode-line-remote
                                        ;evil-mode-line-tag
   mode-line-position
                                        ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "  ")))
   " "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b" face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n"
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%]"
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   "  "
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   ;; "  "
   ;; nyan-mode uses nyan cat as an alternative to %p
   ;; (:eval (when nyan-mode (list (nyan-create))))
   ))


;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))



(set-face-attribute 'mode-line nil
                    :foreground "gray80" :background "gray10"
                    :inverse-video nil
                    :weight 'normal
                    :height 120
                    :box '(:line-width 2 :color "gray10" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray80" :background "gray30"
                    :inverse-video nil
                    :weight 'extra-light
                    :height 120
                    :box '(:line-width 2 :color "gray30" :style nil))


;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(make-face 'mode-line-delim-face-1)

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :weight 'extra-light
                    :height 110
                    :foreground "gray90")
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#eab700"
                    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family "Menlo")
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "white")
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray60"
                    :height 100)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black" :background "#eab700")
(set-face-attribute 'mode-line-delim-face-1 nil
                    :inherit 'mode-line-face
                    :foreground "white")


;; ----------------------------------------------------------------------
;; @ cursor

;; カーソル点滅表示
(blink-cursor-mode 0)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ; five line at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-step 1) ; keyboard scroll one line at a time

;; ----------------------------------------------------------------------
;; @ default setting

;;起動時のメッセージの非表示
(setq inhibit-startup-message t)

;; スタートアップ時のエコー領域メッセージの非表示
(setq inhibit-startup-echo-area-message -1)

;; 対応する括弧を光らせる（グラフィック環境のみ作用）
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; 圧縮されたファイルも編集＆日本語infoの文字化け防止
(auto-compression-mode t)

;;"yes or no"を"y or n"にする
(fset 'yes-or-no-p 'y-or-n-p)

;;EmacsでXのクリップボードを使う
(setq x-select-enable-clipboard t)

;; インテンドにタブを使用しない
(setq-default indent-tabs-mode nil)


;; ----------------------------------------------------------------------
;; @ backup

;; 変更ファイルのバックアップ
(setq make-backup-files nil)

;; 変更ファイルの番号付きバックアップ
(setq version-control nil)

;; 編集ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ先
(setq auto-save-file-name-transform
      '((".*", temporary-file-directory t)))

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 30)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 500)

;; バックアップ世代数
(setq kept-old-versions 1)
(setq kept-new-versions 2)

;; 上書き時の警告表示
(setq trim-versions-without-asking nil)

;; 古いバックアップファイルの削除
(setq delete-old-versions t)

;; 履歴を次回Emacs起動時にも保存する
(setq savehist-file "~/.emacs.d/var/savehist/history")
(savehist-mode 1)

;; auto-save を無効
(setq delete-auto-save-files t)
(setq auto-save-default nil)

;;; Backupを禁止
(setq backup-inhibiteD t)

;; ----------------------------------------------------------------------
;; @ History

(setq history-length 10000)
(savehist-mode 1)
(setq recentf-max-saved-items 10000)


;; ----------------------------------------------------------------------
;; @ key bind

(keyboard-translate ?\C-h ?\C-?) ; 削除
;; (define-key global-map (kbd "C-h") 'delete-backward-char) ; 削除
(define-key global-map (kbd "C-o") 'toggle-input-method)     ; 日本語入力切替
(define-key global-map (kbd "C-z") 'undo)                    ; undo
(define-key global-map (kbd "M-C-g") 'grep)                  ; grep
(define-key global-map (kbd "C-M-n") 'next-multiframe-window) ; 次のウィンドウへ移動
(define-key global-map (kbd "C-M-p") 'previous-multiframe-window) ; 前のウィンドウへ移動
(global-set-key "\C-a" 'beginning-of-visual-indented-line)
(global-set-key "\C-e" 'end-of-visual-line)

;; ----------------------------------------------------------------------
;; @ mac setting

(when darwin-p
  ;; Full-Screen mode
  ;;(ns-toggle-fullscreen)

  ;; command と option の役割を逆にする
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))

  ;; ファイルをドラッグオンドロップ時に開くようにする(デフォルトはinsert)
  (define-key global-map [ns-drag-file] 'ns-find-file)
  (setq ns-pop-up-frames nil)

  (x-focus-frame nil)
  )

;; ----------------------------------------------------------------------
;; @ windows setting

(when windows-p

  (when (require 'setup-cygwin nil t)
    ;; gtagsが動かなくなるのでコメントアウト
    ;;(require 'cygwin-mount)
    ;;(cygwin-mount-activate)

    )

  ;; shell設定

  (when (require 'shell nil t)
    (setq explicit-shell-file-name "bash.exe")
    (setq shell-command-switch "-c")
    (setq shell-file-name "bash.exe")

    ;; (M-! and M-| and compile.el)
    (setq shell-file-name "bash.exe")
    (modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

    ;; shellモードの時の^M抑制
    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

    ;; shell-modeでの補完 (for drive letter)
    (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

    ;; エスケープシーケンス処理の設定
    (autoload 'ansi-color-for-comint-mode-on "ansi-color"
      "Set `ansi-color-for-comint-mode' to t." t)

    (setq shell-mode-hook
          (function
           (lambda ()

             ;; シェルモードの入出力文字コード
             (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)e
             (set-buffer-file-coding-system    'sjis-unix)
             )))
    )
  )


;; ----------------------------------------------------------------------
;; @ load setting

;;; Load other setting file
(load "config/builtins" nil t)
(load "config/packages" nil t)

;; 個別の設定があったら読み込む
(condition-case err
    (load "config/local" nil t)
  (error))

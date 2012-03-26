;;============================================================
;; init.el

;; ----------------------------------------------------------------------
;; @ macro
;;
;;   環境別に場合分け
;;   emacs-version predicates
;;   http://d.hatena.ne.jp/tomoya/20090807/1249601308

(defun x->bool (elt) (not (not elt)))

(setq emacs22-p (string-match "^22" emacs-version)
      emacs23-p (string-match "^23" emacs-version)
      emacs23.0-p (string-match "^23\.0" emacs-version)
      emacs23.1-p (string-match "^23\.1" emacs-version)
      emacs23.2-p (string-match "^23\.2" emacs-version))

;; system-type predicates
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (eq window-system 'ns)
      carbon-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      colinux-p (when linux-p
                  (let ((file "/proc/modules"))
                    (and
                     (file-readable-p file)
                     (x->bool
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (re-search-forward "^cofuse\.+" nil t))))))
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

;; ----------------------------------------------------------------------
;; @ macro
;;  autoload
;;  http://d.hatena.ne.jp/tomoya/20090811/1250006208
(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))


;; ----------------------------------------------------------------------
;; @ macro
;; 関数用コーディングシステム定義マクロ
(defmacro set-function-coding-system (function decoding encoding)
  `(defadvice ,function (around ,(intern (concat
                                          "ad-"
                                          (symbol-name function)
                                          "-coding-setup"))
                                activate)
     (let ((coding-system-for-read ,decoding)
           (coding-system-for-write ,encoding))
       ad-do-it)))

;;----------------------------------------------------------------------
;; @ Load Path
(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/site-elisp")
                 load-path))

(defconst my-elisp-directory "~/.emacs.d/elisp" "The directory for my elisp file.")

(dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
               (list dir (format "%s%d" dir emacs-major-version))))
  (when (and (stringp dir) (file-directory-p dir))
    (let ((default-directory dir))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

;;----------------------------------------------------------------------
;; @ Language

;; 言語を日本語にする
(set-language-environment 'Japanese)

;; 極力UTF-8とする
(prefer-coding-system 'utf-8)

;; Input Method
(setq default-input-method "MacOSX")

;; Google Japanese Input
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "漢")
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-type `box)
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-color "magenta")


;; ----------------------------------------------------------------------
;; @ font

(when darwin-p
  (if window-system
      (when (>= emacs-major-version 23)
        (set-face-attribute 'default nil
                            :family "monaco"
                            :height 140)
        (set-fontset-font
         (frame-parameter nil 'font)
         'japanese-jisx0208
         '("Hiragino Maru Gothic Pro" . "iso10646-1"))
        (set-fontset-font
         (frame-parameter nil 'font)
         'japanese-jisx0212
         '("Hiragino Maru Gothic Pro" . "iso10646-1"))
        (set-fontset-font
         (frame-parameter nil 'font)
         'mule-unicode-0100-24ff
         '("monaco" . "iso10646-1"))
        (setq face-font-rescale-alist
              '(("^-apple-hiragino.*" . 1.2)
                (".*osaka-bold.*" . 1.2)
                (".*osaka-medium.*" . 1.2)
                (".*courier-bold-.*-mac-roman" . 1.0)
                (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                (".*monaco-bold-.*-mac-roman" . 0.9)
                ("-cdac$" . 1.3))))
    )
  )

;; ----------------------------------------------------------------------
;; @ frame

;; フレームタイトルの設定
(setq frame-title-format (concat "%b - emacs@" system-name))

;; フレームサイズ、透明度の設定
(modify-all-frames-parameters
 (list (cons 'alpha  '(nil nil nil nil))))
(setq initial-frame-alist
      (append (list
               '(width . 100)
               '(height . 50)
               '(top . 0)
               '(left . 0)
               )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; 色設定
(require 'color-theme)
(require 'zenburn)
(color-theme-initialize)
(color-theme-zenburn)

;; スクロールバーを右側に
(set-scroll-bar-mode 'right)                   

;; ツールバーを消す
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
;; @ fringe

;; org-mode では無効にする(相性がクソ悪いから)
(defadvice linum-on (around my-linum-on () activate)
  (unless (and (fboundp 'org-mode-p) (org-mode-p))
    ad-do-it))

;; バッファ中の行番号表示
(global-linum-mode t)

;; 行番号のフォーマット
(set-face-attribute 'linum nil  :foreground "red"  :height 0.8)
(setq linum-format "%4d")


;; ----------------------------------------------------------------------
;; @ modeline

;; 行番号の表示
(line-number-mode t)

;; 列番号の表示
(column-number-mode t)

;; 時刻の表示
(require 'time)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time)

;; モードラインに関数名
(which-function-mode 1)

;; cp932エンコード時の表示を「P」とする
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)


;; ----------------------------------------------------------------------
;; @ cursor

;; カーソル点滅表示
(blink-cursor-mode 0)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;; スクロール行数（一行ごとのスクロール）
(setq vertical-centering-font-regexp ".*")
;;(setq scroll-conservatively 35)
(setq scroll-conservatively 0)
;;(setq scroll-margin 0)
(setq scroll-margin 1)
(setq scroll-step 1)

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)


;; ----------------------------------------------------------------------
;; @ default setting

;;起動時のメッセージの非表示
(setq inhibit-startup-message t)

;; スタートアップ時のエコー領域メッセージの非表示
(setq inhibit-startup-echo-area-message -1)


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
;; (setq trim-versions-without-asking nil)

;; 古いバックアップファイルの削除
(setq delete-old-versions t)

;; 履歴を次回Emacs起動時にも保存する
(setq savehist-file "~/.emacs.d/var/savehist/history")
(savehist-mode 1)

;; (setq delete-auto-save-files t)
;; (setq auto-save-default nil)

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

)

;; ----------------------------------------------------------------------
;; @ load setting

;;; Load other setting file
(load "config/builtins")
(load "config/packages")

;; 個別の設定があったら読み込む
(condition-case err
    (load "config/local")
  (error))

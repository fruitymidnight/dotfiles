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
;; 
;;   ロードパスの追加
;;   http://www.sodan.org/~knagano/emacs/dotemacs.html#autoload-if-found
;;   http://masutaka.net/chalow/2009-07-05-3.html

(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/site-elisp"
                   "~/.emacs.d/elisp"
                   "~/.emacs.d/elisp/auto-complete"
                   "~/.emacs.d/elisp/auto-install"
                   "~/.emacs.d/elisp/calfw"
                   "~/.emacs.d/elisp/color-theme"
                   "~/.emacs.d/elisp/elscreen"
                   "~/.emacs.d/elisp/gtags"
                   "~/.emacs.d/elisp/lua"
                   "~/.emacs.d/elisp/migemo"
                   "~/.emacs.d/elisp/misc"
                   "~/.emacs.d/elisp/mozc"
                   "~/.emacs.d/elisp/session"
                   "~/.emacs.d/elisp/win-apel"
                   "~/.emacs.d/elisp/yasnippet"
                   )
                 load-path))

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

(when darwin-p
  ;; Input Method
  (setq default-input-method "MacOSX")

  ;; Google Japanese Input
  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "漢")
  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-type `box)
  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-color "magenta")
)

(when windows-p
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

;; ----------------------------------------------------------------------
;; @ encode

;; 機種依存文字
(when (require 'cp5022x nil t)
  
  (define-coding-system-alias 'euc-jp 'cp51932)

  ;; decode-translation-table の設定
  (coding-system-put 'euc-jp :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'iso-2022-jp :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'utf-8 :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

  ;; encode-translation-table の設定
  (coding-system-put 'euc-jp :encode-translation-table
                     (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
  (coding-system-put 'iso-2022-jp :encode-translation-table
                     (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
  (coding-system-put 'cp932 :encode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'utf-8 :encode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

  ;; charset と coding-system の優先度設定
  (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                        'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
  (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

  ;; PuTTY 用の terminal-coding-system の設定
  (apply 'define-coding-system 'utf-8-for-putty
         "UTF-8 (translate jis to cp932)"
         :encode-translation-table 
         (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
         (coding-system-plist 'utf-8))
  (set-terminal-coding-system 'utf-8-for-putty)

  ;; East Asian Ambiguous
  (defun set-east-asian-ambiguous-width (width)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (let ((table (make-char-table nil)))
      (dolist (range 
               '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
                        (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
                        #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
                        (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0 
                        (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
                        #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
                        (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
                        (#x0148 . #x014B) #x014D (#x0152 . #x0153)
                        (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
                        #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
                        (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
                        #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
                        (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401 
                        (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
                        (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
                        (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
                        #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
                        #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
                        #x212B (#x2153 . #x2154) (#x215B . #x215E) 
                        (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
                        (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
                        (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
                        #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
                        (#x2227 . #x222C) #x222E (#x2234 . #x2237)
                        (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
                        (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
                        (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
                        #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
                        (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595) 
                        (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
                        (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
                        (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1) 
                        (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
                        (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
                        #x2642 (#x2660 . #x2661) (#x2663 . #x2665) 
                        (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
                        (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F) 
                        #xFFFD
                        ))
        (set-char-table-range table range width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table)))
  (set-east-asian-ambiguous-width 2)
  )

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

(when windows-p
  ;; 標準フォントの設定
  (set-default-font "ricty-10")
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
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (when (require 'zenburn nil t)
    (color-theme-zenburn)))

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
             (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
             (set-buffer-file-coding-system    'sjis-unix)
             )))
    )
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

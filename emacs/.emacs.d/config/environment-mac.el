;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; environment-mac.el --- My Emacs configuration file

;;; Commentary:

;; 環境依存の設定

;;; Code:


;;; -------------------------------------------------------------------
;;;; environment variables
;;; -------------------------------------------------------------------

;; 環境変数の設定
(setenv "LANG" "ja_JP.UTF-8")
(setenv "LC_CTIME" "C") ;; find-dired の日付の設定が面倒なので


;;; -------------------------------------------------------------------
;;;; language - coding system
;;; -------------------------------------------------------------------

;; 
;; set-language-environment を設定すると
;; 環境にあわせて文字コードは設定される
;; Windowsなどで細かな挙動を変更したいときは個別に設定していく
;; @see http://yohshiy.blog.fc2.com/blog-entry-273.html
;;

;; 言語環境を日本語にする
(set-language-environment 'Japanese)

;; デフォルトの文字コード
;; (set-default-coding-systems 'utf-8-unix)

;; テキストファイル／新規バッファの文字コード
;;(prefer-coding-system 'utf-8-unix)

;; ファイル名の文字コード
;; (set-file-name-coding-system 'utf-8-unix)

;; キーボード入力の文字コード
;; (set-keyboard-coding-system 'utf-8-unix)

;; サブプロセスのデフォルト文字コード
;; (setq default-process-coding-system '(undecided-dos . utf-8-unix))


;;; -------------------------------------------------------------------
;;;; language - input method
;;; -------------------------------------------------------------------

;; Input Method
(mac-input-method-mode t)

;; Google Japanese Input
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-type `box)
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-color "magenta")


;;; -------------------------------------------------------------------
;;;; language - font set
;;; -------------------------------------------------------------------

;; フォント設定については下記が詳しい
;; @see http://d.hatena.ne.jp/setoryohei/20110117/1295336454
(if window-system
    (when (x-list-fonts "Ricty")
      (let* ((size 12)           ; asciiフォントのサイズ [9/10/12/14/15/17/19/20/...]
             (asciifont "Ricty") ; asciiフォント
             (jpfont "Ricty")    ; 日本語フォント
             (h (* size 10))
             (fontspec)
             (jp-fontspec))
        ;; デフォルト フォント
        (set-face-attribute 'default nil :family asciifont :height h)
        ;; プロポーショナル フォント
        (set-face-attribute 'variable-pitch nil :family asciifont :height h)
        ;; 等幅フォント
        (set-face-attribute 'fixed-pitch nil :family asciifont :height h)
        ;; ツールチップ表示フォント
        (set-face-attribute 'tooltip nil :family asciifont :height h)
        (setq fontspec (font-spec :family asciifont))
        (setq jp-fontspec (font-spec :family jpfont))
        (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
        (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
        (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
        (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
        (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)   ; 半角カナ
        (set-fontset-font nil '(#x0080 . #x024F) fontspec)      ; 分音符付きラテン
        (set-fontset-font nil '(#x0370 . #x03FF) fontspec)      ; ギリシャ文字
        ))
  )

;; フォントサイズ調整
(global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))

;; フォントサイズ リセット
(global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))


;;; -------------------------------------------------------------------
;;;; cocoa emacs
;;; -------------------------------------------------------------------

;; Full-Screen mode
;;(ns-toggle-fullscreen)

;; command と option の役割を逆にする
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; ファイルをドラッグオンドロップ時に開くようにする(デフォルトはinsert)
(define-key global-map [ns-drag-file] 'ns-find-file)
(setq ns-pop-up-frames nil)

(x-focus-frame nil)



;;; environment-mac.el ends here

;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; edit.el --- My Emacs configuration file

;;; Commentary:

;;; Code:


;;; -------------------------------------------------------------------
;;;; key-bind
;;; -------------------------------------------------------------------

(keyboard-translate ?\C-h ?\C-?) ; 削除
;; (define-key global-map (kbd "C-h") 'delete-backward-char) ; 削除
(define-key global-map (kbd "C-o") 'toggle-input-method)     ; 日本語入力切替
(define-key global-map (kbd "C-z") 'undo)                    ; undo
(define-key global-map (kbd "M-C-g") 'grep)                  ; grep
(define-key global-map (kbd "C-M-n") 'next-multiframe-window) ; 次のウィンドウへ移動
(define-key global-map (kbd "C-M-p") 'previous-multiframe-window) ; 前のウィンドウへ移動
(global-set-key "\C-a" 'beginning-of-visual-indented-line)
(global-set-key "\C-e" 'end-of-visual-line)


;;; -------------------------------------------------------------------
;;;; mini-buffer
;;; -------------------------------------------------------------------

;; "yes or no"を"y or n"にする
(fset 'yes-or-no-p 'y-or-n-p)


;;; -------------------------------------------------------------------
;;;; cut & paste
;;; -------------------------------------------------------------------

;; EmacsでXのクリップボードを使う
(setq x-select-enable-clipboard t)


;;; -------------------------------------------------------------------
;;;; file - edit
;;; -------------------------------------------------------------------

;; 圧縮されたファイルも編集＆日本語infoの文字化け防止
(auto-compression-mode t)

;; インテンドにタブを使用しない
(setq-default indent-tabs-mode nil)


;;; -------------------------------------------------------------------
;;;; file - backup
;;; -------------------------------------------------------------------

;; ファイルオープン時のバックアップ（~）
(setq make-backup-files   t)  ;; 自動バックアップの実行有無
(setq version-control     t)  ;; バックアップファイルへの番号付与
(setq kept-new-versions   3)  ;; 最新バックアップファイルの保持数
(setq kept-old-versions   0)  ;; 最古バックアップファイルの保持数
(setq delete-old-versions t)  ;; バックアップファイル削除の実行有無

;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
            backup-directory-alist))

;; 編集中ファイルの自動バックアップ
(setq backup-inhibited nil)

;; 終了時に自動バックアップファイルを削除
(setq delete-auto-save-files nil)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 3)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 100)

;; 編集中ファイル（##）の格納ディレクトリ
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))


;;; -------------------------------------------------------------------
;;;; file - lockfile
;;; -------------------------------------------------------------------

;; ロックファイルの生成を抑止
(setq create-lockfiles nil)


;;; -------------------------------------------------------------------
;;;; file - history
;;; -------------------------------------------------------------------

;; 履歴を次回Emacs起動時にも保存する
(setq savehist-file "~/.emacs.d/var/savehist/history")
(setq history-length 10000)
(setq recentf-max-saved-items 10000)
(savehist-mode 1)


;;; edit.el ends here

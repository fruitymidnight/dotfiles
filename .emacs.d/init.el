;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; init.el --- My Emacs configuration file

;;; Commentary:

;; 技術メモ 

;;;; Emacs のインストール方法

;; - Mac OSX
;;   + Homebrew からインストールする場合(24.3)
;;     : brew install --cocoa --srgb --with-gnutls --japanese emacs -v

;; - Windows
;;   + gnupack 12.01の場合
;;     - gnupack-devel をダウンロードして適当なディレクトリに展開
;;     

;;;; 本設定ファイルの配置方法

;; - Mac OSX
;;   + 
;; 

;;;; ディレクトリ構成

;; 

;;; Code:


;;; -------------------------------------------------------------------
;;;; emacs-version predicates
;;; -------------------------------------------------------------------

;; Emacs の種類/バージョンを判別するための変数を定義
;; 下記のものを抜粋
;; @see http://d.hatena.ne.jp/tomoya/20090807/1249601308
;; @see http://www.gfd-dennou.org/member/uwabami/cc-env/EmacsBasic.html

(defvar oldemacs-p (<= emacs-major-version 22)) ; 22 以下
(defvar emacs23-p (<= emacs-major-version 23))  ; 23 以下
(defvar emacs24-p (>= emacs-major-version 24))  ; 24 以上

;; system-type predicates
(defvar darwin-p (eq system-type 'darwin))      ; Mac OS
(defvar ns-p (eq window-system 'ns))            ; Cocoa
(defvar linux-p (eq system-type 'gnu/linux))    ; Linux
(defvar cygwin-p (eq system-type 'cygwin))      ; Cygwin
(defvar nt-p (eq system-type 'windows-nt))      ; Windows-NT
(defvar meadow-p  (featurep 'meadow))           ; Meadow
(defvar windows-p (or cygwin-p nt-p meadow-p))  ; Windows


;;; -------------------------------------------------------------------
;;;; directory setting                                             
;;; -------------------------------------------------------------------


;;; ディレクトリ構成を決めるための変数
;;
;; Emacs 22 以下用に user-emacs-directory を定義する.
;; 他にも以下の変数を定義
;; - my:user-emacs-config-directory    → ~/.emacs.d/config
;; - my:user-emacs-temporary-directory → ~/.emacs.d/tmp
;; - my:user-emacs-share-directory     → ~/.emacs.d/share
;;

(when oldemacs-p
  (defvar user-emacs-directory
    (expand-file-name (concat (getenv "HOME") "/.emacs.d/"))))
(defconst my:user-emacs-config-directory
  (expand-file-name (concat user-emacs-directory "config/")))
(defconst my:user-emacs-temporary-directory
  (expand-file-name (concat user-emacs-directory "tmp/")))
(defconst my:user-emacs-share-directory
  (expand-file-name (concat user-emacs-directory "share/")))



;;; -------------------------------------------------------------------
;;;; load-path
;;; -------------------------------------------------------------------

;;
;; 最後に add したものが先頭にくるようになっている。
;; 読み込みたくないファイルは, 先頭に "." や "_" をつけると良い.
;; @see http://www.gfd-dennou.org/member/uwabami/cc-env/EmacsBasic.html
;; (defun add-to-load-path (&rest paths)
;;   (let (path)
;;     (dolist (path paths paths)
;;       (let ((default-directory
;;               (expand-file-name (concat user-emacs-directory path))))
;;         (add-to-list 'load-path default-directory)
;;         (if (fboundp 'normal-top-level-add-subdirs-to-loadpath)
;;             (normal-top-level-add-subdirs-to-load-path))))))

;; ;; ロードパスの追加
;; (add-to-load-path
;;  "~/.emacs.d"
;;  "config"                  ; 分割した設定群の置き場所.
;;  "site-elisp"              ; el-get 本体
;;  )

(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/site-elisp"
                   "~/.emacs.d/elisp"
                   )
                 load-path))

;;; -------------------------------------------------------------------
;;;; load setting
;;; -------------------------------------------------------------------

;; 見た目の設定
(load "initfuncs" nil t)

;; 各環境の設定ファイル読み込み
(cond
 (ns-p
  (load "config/environment-mac" nil t))
 (windows-p
  (load "config/environment-win" nil t))
)

;; 見た目の設定
(load "config/face" nil t)

;; 編集環境の設定
(load "config/edit" nil t)

;; 組込みのelispの設定 
(load "config/builtins" nil t)

;; サードパーティのelispの設定
(load "config/packages" nil t)

;; ローカル設定があったら読み込む
(condition-case err
    (load "config/local" nil t)
  (error))

;;; init.el ends here

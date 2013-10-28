; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;------------------------------------------------------------------------------
;; @ package.el の設定
(when (require 'package nil t)
  ;; package.elでelispを入れるdirectoryの設定
  (setq package-user-dir "~/.emacs.d/elpa")

  ;; パッケージリポジトリにMarmaladeを追加
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  ;; インストールしたパッケージにロードパスを通してロードする
  (package-initialize))


;;------------------------------------------------------------------------------
;; @ el-get.el の設定
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; レシピ置き場
(add-to-list 'el-get-recipe-path
             (expand-file-name
              (concat user-emacs-directory "config/el-get/recipes")))

;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path
             (expand-file-name
              (concat user-emacs-directory "config/el-get/local-recipes")))


;;------------------------------------------------------------------------------
;; @ package のインストール

;; http://shibayu36.hatenablog.com/entry/2013/04/30/175740
;; Packages to install from MELPA
(defvar my/packages
  '(
    yasnippet
    helm
    helm-c-yasnippet
    helm-git
    helm-migemo
    helm-gtags
    auto-complete
    popwin
    magit
    markdown-mode
    lua-mode
;;    calfw
;;    japanese-holidays
    session
    sr-speedbar
    gtags
;;    elscreen
;;    elscreen-gf
;;    elscreen-dired
;;    elscreen-server
;;    elscreen-color-theme
;;    elscreen-color-dnd
    auto-async-byte-compile
    tabbar
    color-moccur
    cygwin-mount
    )
  "A list of packages to install from MELPA at launch.")

;; Install Melpa packages
(dolist (package my/packages)
  (when (or (not (package-installed-p package)))
    (package-install package)))


;; Packages to install from el-get
(defvar my/el-get-packages
  '(
    sdic
    text-translator
    dmacro
    cp5022x
    setup-cygwin
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync my/el-get-packages)

;;------------------------------------------------------------------------------
;; @ インストールしたelispの設定

(load "config/packages/init-helm" nil t)
;; (load "config/packages/init-anything" nil t)
(load "config/packages/init-autocomplete" nil t)
(load "config/packages/init-magit" nil t)
(load "config/packages/init-c" nil t)
(load "config/packages/init-lua" nil t)
(load "config/packages/init-markdown" nil t)
;; (load "config/packages/init-org" nil t)
;; (load "config/packages/init-calfw" nil t)
(load "config/packages/init-session" nil t)
(load "config/packages/init-srspeedbar" nil t)
(load "config/packages/init-dmacro" nil t)
(load "config/packages/init-sdic" nil t)
(load "config/packages/init-texttranslator" nil t)
(load "config/packages/init-cp5022x" nil t)
(load "config/packages/init-auto-async-byte-complile" nil t)
;; (load "config/packages/init-elscreen" nil t)
(load "config/packages/init-tabbar" nil t)
(load "config/packages/init-migemo" nil t)
(load "config/packages/init-popwin" nil t)


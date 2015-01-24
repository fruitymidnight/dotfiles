;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; init-yasnippet.el --- 

;;; Commentary:

;; snippet の作成方法は下記頁を参考にする
;; @see http://yasnippet-doc-jp.googlecode.com/svn/trunk/doc-jp/snippet-development.html

;;; Code:


;;; -------------------------------------------------------------------
;;;; Yasnippet
;;; -------------------------------------------------------------------




(when (locate-library "helm-c-yasnippet")
  (require 'helm-c-yasnippet)
  (global-set-key (kbd "C-c y") 'helm-c-yas-complete)
  )

(lazyload (helm-c-yas-complete) "yasnippet"          
          (setq yas-snippet-dirs
                '(
                  "~/.emacs.d/var/snippets"                        ; personal snippets
                  yas-installed-snippets-dir                       ; default collection
                  ))
          (yas-global-mode 1)
          )


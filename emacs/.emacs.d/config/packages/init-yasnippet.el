;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; init-yasnippet.el --- 

;;; Commentary:

;; snippet の作成方法は下記頁を参考にする
;; @see http://yasnippet-doc-jp.googlecode.com/svn/trunk/doc-jp/snippet-development.html

;;; Code:


;;; -------------------------------------------------------------------
;;;; Yasnippet
;;; -------------------------------------------------------------------

(use-package helm-c-yasnippet
  :ensure t
  :defer t
  :bind (("C-c y" . helm-c-yas-complete))
  )

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (progn
    (setq yas-snippet-dirs
          '(
            "~/.emacs.d/var/snippets"                        ; personal snippets
            yas-installed-snippets-dir                       ; default collection
            ))
    (yas-global-mode 1)
    ))

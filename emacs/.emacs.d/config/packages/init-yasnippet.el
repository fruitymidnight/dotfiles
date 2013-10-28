; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;------------------------------------------------------------------------------
;; @ yasnippet

(when (locate-library "helm-c-yasnippet")
  (require 'helm-c-yasnippet)
  (global-set-key (kbd "C-c y") 'helm-c-yas-complete)
  )

(lazyload (helm-c-yas-complete) "yasnippet"
          (yas-global-mode 1)
          (setq yas-snippet-dirs
                '(
                  "~/.emacs.d/var/snippets"            ; personal snippets
                  ))
          )

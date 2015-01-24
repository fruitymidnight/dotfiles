; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ srspeedbar

(global-set-key (kbd "<f9>") 'sr-speedbar-toggle)

(lazyload (sr-speedbar-toggle) "sr-speedbar"
          (setq sr-speedbar-right-side nil)  ;;左側に表示
          )

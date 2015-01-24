; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ auto-complile

;; ファイルを保存したときに自動的にバイトコンパイルする
(when (locate-library "auto-async-byte-compile")
  (require 'auto-async-byte-compile)
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
  )

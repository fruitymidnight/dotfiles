; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ auto-complete

(when (locate-library "auto-complete")
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
  (ac-config-default)

  ;; 補完推測機能のデータを永続化するファイル名を指定
  (setq ac-comphist-file "~/.emacs.d/var/auto-complete/ac-comphist.dat")
  )

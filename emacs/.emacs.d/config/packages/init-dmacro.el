; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ dmacro
;;   繰り返し操作をおこなうことができる

(defconst *dmacro-key* [f7] "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

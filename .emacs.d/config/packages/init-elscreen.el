; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ elscreen

(when (locate-library "elscreen")
  (setq elscreen-prefix-key "\C-t")
  (setq elscreen-display-tab 8) ; タブの幅（６以上じゃないとダメ）
  (setq elscreen-tab-display-kill-screen nil) ; タブの左端の×を非表示
  (load "elscreen" "ElScreen" t)

  ;; plugin
  (when (locate-library "elscreen-gf")
    (require 'elscreen-gf)
    )
  (when (locate-library "elscreen-dired")
    (require 'elscreen-dired)
    )
  (when (locate-library "elscreen-color-theme")
    (require 'elscreen-color-theme)  
    )
  (when (locate-library "elscreen-server")
    (require 'elscreen-server)
    )
  (when (locate-library "elscreen-dnd")
    (require 'elscreen-dnd)
    )
  )

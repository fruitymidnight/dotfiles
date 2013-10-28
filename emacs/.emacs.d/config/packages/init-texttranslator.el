; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ text-translator

;;翻訳キー設定
(global-set-key "\C-ct" 'text-translator)
(global-set-key "\C-c\M-T" 'text-translator-translate-last-string)
(when (locate-library "text-translator-load")
  (require 'text-translator-load)

  ;; popup.el がある場合は popup で表示
  (when (locate-library "popup")
    (setq text-translator-display-popup t)
    ))


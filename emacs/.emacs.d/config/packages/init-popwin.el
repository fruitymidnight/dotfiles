; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ popwin

(when (locate-library "popwin")
  (require 'popwin)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)

  ;; Browse-Kill-Ring
  (push '("*Kill Ring*") popwin:special-display-config)
  ;; anything
  (push '("*anything*") popwin:special-display-config)
  ;; helm
  (push '("^\*helm .+\*$" :regexp t :height 0.4) popwin:special-display-config)
  ;; sdic
  (push '("*sdic*") popwin:special-display-config)
  ;; Completions
  (push '("*Completions*") popwin:special-display-config)
  ;; auto-async-byte-compile
  (push '(" *auto-async-byte-compile*" :noselect t) popwin:special-display-config)
  (push '(" *auto-async-byte-compile*" :height 14 :position bottom :noselect t) popwin:special-display-config)

  ;; sdic-display-buffer 書き換え 
  (defadvice sdic-display-buffer (around sdic-display-buffer-normalize activate)
    "sdic のバッファ表示を普通にする。"
    (setq ad-return-value (buffer-size))
    (let ((p (or (ad-get-arg 0)
                 (point))))
      (and sdic-warning-hidden-entry
           (> p (point-min))
           (message "この前にもエントリがあります。"))
      (goto-char p)
      (display-buffer (get-buffer sdic-buffer-name))
      (set-window-start (get-buffer-window sdic-buffer-name) p)))

  (defadvice sdic-other-window (around sdic-other-normalize activate)
    "sdic のバッファ移動を普通にする。"
    (other-window 1))

  (defadvice sdic-close-window (around sdic-close-normalize activate)
    "sdic のバッファクローズを普通にする。"
    (bury-buffer sdic-buffer-name))
  )

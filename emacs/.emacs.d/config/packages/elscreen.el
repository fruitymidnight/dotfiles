;;; elscreen config file
;;; =====================
;;; CHANGE LOG
;;; 2010/08/31 create.
;;;

;apel
; for elscreen
(when windows-p
  (add-to-list 'load-path "~/.emacs.d/elisp/win-apel/")
)

;====================================
; elscreen.el
;====================================
(add-to-list 'load-path "~/.emacs.d/elisp/elscreen/")
(setq elscreen-prefix-key "\C-t")
(setq elscreen-display-tab 8) ; タブの幅（６以上じゃないとダメ）
(setq elscreen-tab-display-kill-screen nil) ; タブの左端の×を非表示
(load "elscreen" "ElScreen" t)

(require 'elscreen-gf)
(require 'elscreen-dired)
(require 'elscreen-color-theme)
(require 'elscreen-howm)
(require 'elscreen-server)
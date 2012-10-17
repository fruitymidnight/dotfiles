;;; elscreen config file

(unless (locate-library "elscreen")
  (el-get 'sync 'elscreen))


(setq elscreen-prefix-key "\C-t")
(setq elscreen-display-tab 8) ; タブの幅（６以上じゃないとダメ）
(setq elscreen-tab-display-kill-screen nil) ; タブの左端の×を非表示
(load "elscreen" "ElScreen" t)

;; (require 'elscreen-gf)
;; (require 'elscreen-dired)
;; (require 'elscreen-color-theme)
;; (require 'elscreen-howm)
;; (require 'elscreen-server)

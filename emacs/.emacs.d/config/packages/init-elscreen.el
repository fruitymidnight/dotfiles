;;; elscreen config file

(unless (locate-library "elscreen")
  (el-get 'sync 'elscreen))


(setq elscreen-prefix-key "\C-t")
(setq elscreen-display-tab 8) ; タブの幅（６以上じゃないとダメ）
(setq elscreen-tab-display-kill-screen nil) ; タブの左端の×を非表示
(load "elscreen" "ElScreen" t)

(unless (locate-library "elscreen-gf")
  (el-get 'sync 'elscreen-gf))
(require 'elscreen-gf)

(unless (locate-library "elscreen-dired")
  (el-get 'sync 'elscreen-dired))
(require 'elscreen-dired)

(unless (locate-library "elscreen-color-theme")
  (el-get 'sync 'elscreen-color-theme))
(require 'elscreen-color-theme)

(unless (locate-library "elscreen-server")
  (el-get 'sync 'elscreen-server))
(require 'elscreen-server)

(unless (locate-library "elscreen-dnd")
  (el-get 'sync 'elscreen-dnd))
(require 'elscreen-dnd)


;;; init-dmacro.el

;; ----------------------------------------------------------------------
;; @ dmacro
;;   繰り返し操作をおこなうことができる

(unless (locate-library "dmacro")
  (el-get 'sync 'dmacro))

(defconst *dmacro-key* [f7] "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

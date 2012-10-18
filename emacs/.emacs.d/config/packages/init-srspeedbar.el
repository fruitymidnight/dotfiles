(unless (locate-library "sr-speedbar")
  (el-get 'sync 'sr-speedbar))

(setq sr-speedbar-right-side nil)  ;;左側に表示
(global-set-key (kbd "<f11>") 'sr-speedbar-toggle)


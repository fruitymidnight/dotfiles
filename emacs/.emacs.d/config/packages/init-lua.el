;;; lua-init.el

(unless (locate-library "lua-mode")
  (el-get 'sync 'lua-mode))

(when (autoload-if-found 'lua-mode "lua-mode" "Lua editing mode." t)
  (eval-after-load "lua-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
     (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
     ))
  )

;;; lua-init.el

(when (autoload-if-found 'lua-mode "lua-mode" "Lua editing mode." t)
  (eval-after-load "lua-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
     (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
     ))
  )

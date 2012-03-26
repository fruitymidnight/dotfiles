;;; lua config file
;;; =====================
;;; CHANGE LOG
;;; 2011/12/12 create.
;;;

;====================================
; anything
;====================================
(add-to-list 'load-path "~/.emacs.d/elisp/lua/")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

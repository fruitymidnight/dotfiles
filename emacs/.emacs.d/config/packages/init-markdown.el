
(unless (locate-library "markdown-mode")
  (el-get 'sync 'markdown-mode))

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
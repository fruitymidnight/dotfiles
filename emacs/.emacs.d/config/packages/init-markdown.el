; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ markdown

(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(lazyload (markdown-mode) "markdown-mode"
          (setq markdown-css-path (expand-file-name "~/.emacs.d/var/css/markdown.css"))
          )

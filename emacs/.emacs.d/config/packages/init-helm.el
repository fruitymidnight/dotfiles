; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;------------------------------------------------------------------------------
;; @ init-helm.el 

(when (require 'helm-config nil t)
  (define-key global-map (kbd "C-l") 'helm-mini)
  (define-key global-map (kbd "M-r") 'helm-resume)
  (define-key global-map (kbd "C-x b") 'helm-for-files)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "C-M-o") 'helm-occur)
  (define-key global-map (kbd "C-c i") 'helm-imenu)

  (when (require 'helm-migemo nil t)
    (setq helm-use-migemo t))
  (when (require 'helm-git nil t)
    (global-set-key (kbd "C-x C-g") 'helm-git-find-files)
    )
  )


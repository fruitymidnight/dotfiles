;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; init-helm.el --- 

;;; Commentary:

;; 

;;; Code:

(use-package helm
  :ensure t
  :defer t
  :idle ()
  :bind (("C-l" . helm-mini)
         ("M-r" . helm-resume)
         ("C-x b" . helm-for-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-M-o" . helm-occur)
         ("C-c i" . helm-imenu))
  :config
  (progn
    (use-package helm-config)
    )
  )

(use-package helm-ag
  :ensure t
  :defer t
  :if (executable-find "ag")
  :bind (("C-c g" . helm-ag))
  )


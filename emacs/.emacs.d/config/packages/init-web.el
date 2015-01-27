;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; init-web.el --- 

;;; Commentary:

;; 

;;; Code:


;;; -------------------------------------------------------------------
;;;; web-mode
;;; -------------------------------------------------------------------
(use-package web-mode
  :ensure t
  :mode  (("\\.html?\\'" . web-mode)
          ("\\.phtml\\'" . web-mode)
          ("\\.tpl\\.php\\'" . web-mode)
          ("\\.[agj]sp\\'" . web-mode)
          ("\\.as[cp]x\\'" . web-mode)
          ("\\.erb\\'" . web-mode)
          ("\\.mustache\\'" . web-mode)
          ("\\.djhtml\\'" . web-mode)
          )
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)  ; HTML offset indentation
    (setq web-mode-css-indent-offset 2)     ; CSS offset indentation
    (setq web-mode-code-indent-offset 2)    ; Script offset indentation (for JavaScript, Java, PHP, etc.)
    
    )
  )

;;; -------------------------------------------------------------------
;;;; js2-mode
;;; -------------------------------------------------------------------

(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
         )
  :config
  (progn
    ;; js2-mode の設定値は setq ではなく custom-set-variables を使うこと
    ;; setq では設定した値が反映されなかった
    (custom-set-variables '(js2-basic-offset 2))  ; indent
    )
)

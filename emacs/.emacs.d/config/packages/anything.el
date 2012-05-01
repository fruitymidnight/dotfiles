;;; anything-init.el 

(when (require 'anything-startup nil t)
  (define-key global-map  (kbd "C-l") 'anything)
  )


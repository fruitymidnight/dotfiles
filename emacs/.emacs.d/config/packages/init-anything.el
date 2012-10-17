;;; init-anything.el 

(unless (locate-library "anything")
  (el-get 'sync 'anything))

(define-key global-map  (kbd "C-l") 'anything)


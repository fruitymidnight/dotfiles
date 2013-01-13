;;; init-anything.el 

(unless (locate-library "anything")
  (el-get 'sync 'anything))

(require 'anything-startup)
(define-key global-map (kbd "C-l") 'anything)
(define-key global-map (kbd "C-x b") 'anything-for-files)
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)
(define-key global-map (kbd "M-x") 'anything-M-x)


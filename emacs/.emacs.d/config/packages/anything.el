;;; anything config file
;;; =====================
;;; CHANGE LOG
;;; 2010/08/31 create.
;;;

;====================================
; anything
;====================================
(add-to-list 'load-path "~/.emacs.d/elisp/anything/")
(require 'anything-startup)

;(require 'anything)
;; (require 'anything-config)
;; (setq anything-sources
;;       (list anything-c-source-buffers
;;             anything-c-source-recentf
;;             anything-c-source-file-name-history
;;             anything-c-source-emacs-commands
;;             anything-c-source-man-pages
;;             anything-c-source-info-pages
;;             ))
;; (define-key anything-map (kbd "C-p") 'anything-previous-line)
;; (define-key anything-map (kbd "C-n") 'anything-next-line)
;; (define-key anything-map (kbd "C-v") 'anything-next-source)
;; (define-key anything-map (kbd "M-v") 'anything-previous-source)
(define-key global-map  (kbd "C-l") 'anything)

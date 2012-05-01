;;; ryby config file
;;; =====================
;;; CHANGE LOG
;;; 2010/08/31 create.
;;;

;====================================
; ruby
;====================================
;; ruby-mode
;; (autoload 'ruby-mode "ruby-mode"
;;   "Mode for editing ruby source files" t)
;; (setq auto-mode-alist
;;       (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
;; (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
;;                                      interpreter-mode-alist))
;; (autoload 'run-ruby "inf-ruby"
;;   "Run an inferior Ruby process")
;; (autoload 'inf-ruby-keys "inf-ruby"
;;   "Set local key defs for inf-ruby in ruby-mode")
;; (add-hook 'ruby-mode-hook
;;           '(lambda () (inf-ruby-keys)))

;; ;; ruby-electric
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;; ;; refe
;; (require 'refe)
;; ;(require 'anything-show-completion)

;; ;; rails.el
;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))
;; (setq rails-use-mongrel t)
;; (require 'rails)
;; ;; 対応するファイルへの切り替え(C-c C-p)
;; (define-key rails-minor-mode-map "\C-c\C-p" 'rails-lib:run-primary-switch)
;; ;; 行き先を選べるファイル切り替え(C-c C-n)
;; (define-key rails-minor-mode-map "\C-c\C-n" 'rails-lib:run-secondary-switch)

;; (setq auto-mode-alist  (cons '("\\.rhtml$" . html-mode) auto-mode-alist))

;; ;====================================
;; ; rcodetools
;; ;====================================
;; (require 'anything-rcodetools)
;; (setq rct-get-all-methods-command "PAGER=cat fri -l")
;; ;; See docs
;; (define-key anything-map "\C-e" 'anything-execute-persistent-action)

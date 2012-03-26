;;; c config file
;;; =====================
;;; CHANGE LOG
;;; 2010/11/24 create.
;;; 2011/04/20 

;====================================
; Cモードの設定
;====================================
(add-to-list 'load-path "~/.emacs.d/elisp/gtags/")
(autoload 'gtags-mode "gtags" "" t)
;(require 'gtags)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\M-p" 'gtags-pop-stack)
         (setq gtags-path-style 'relative)
         ))


(defun pew-c-mode-hook ()
    "C mode with adjusted defaults for use with the PEW Style."
   (c-set-style "gnu")
;   (setq-default indent-tabs-mode t) ; インデントはTABでおこなう
   (setq tab-width 4)
   (setq c-basic-offset tab-width)
   (linum-mode t)
   (gtags-mode 1)
   (setq truncate-lines t)
;   (define-key c-mode-base-map [backspace] 'backward-delete-char)
)
;; C++ style
(add-hook 'c-mode-hook 'pew-c-mode-hook)

;; flymake の設定
;; http://moimoitei.blogspot.com/2010/05/flymake-in-emacs.html
;;-----------------
;;Makefile で以下を追加する
;;PHONY: check-syntax
;;check-syntax:
;;    $(CC) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)
;;-----------------
;(require 'flymake)

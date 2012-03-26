;====================================
; Misc
;====================================

;==================================
;; mcomplete （anything で代替可）
;==================================
;(require 'mcomplete)
;(turn-on-mcomplete-mode)

;====================================
; minibuf-isearch
;====================================
;;   minibufでisearchを使えるようにする
(require 'minibuf-isearch nil t)


; isearch中でもC-gで問答無用にisearchを抜けて欲しい
; http://blog.livedoor.jp/tek_nishi/archives/4866943.html
(define-key isearch-mode-map "\C-g" 'isearch-cancel)


;====================================
; session.el
;   kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する
;====================================
(setq load-path
      (cons (expand-file-name "~/.emacs.d/elisp/session/lisp/") load-path))
(when (require 'session nil t)
  (setq session-save-file "~/.emacs.d/var/session/.session")
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  ; ミニバッファ履歴リストの最大長：tなら無限
  (setq history-length t)
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  (setq session-undo-check -1)
  )


;=============================
; info
;============================
; infoファイルを置くディレクトリを設定する
(setq Info-directory-list
      (append
    Info-default-directory-list
       (list
     (expand-file-name "~/.emacs.d/share/info")
        (expand-file-name "~/.emacs.d/share/info/python2.5")
     (expand-file-name "~/.emacs.d/share/info/EmacsFAQ")
        )
    ))

;============================
; dmacro
;============================
; 繰り返し操作をおこなうことができる
(defconst *dmacro-key* [f7] "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

;============================
; popwin.el
;============================
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)


;============================
; gtags.el
;============================
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))


;============================
; sr-speedbar
;============================
(require 'sr-speedbar)
;(setq sr-speedbar-right-side nil);;左側に表示
(global-set-key (kbd "<f11>") 'sr-speedbar-toggle)



;; ----------------------------------------------------------------------
;; @ session.el
;;   kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する

(unless (locate-library "session")
  (el-get 'sync 'session))

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


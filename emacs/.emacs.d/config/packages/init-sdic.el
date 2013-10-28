; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ sdic

(global-set-key "\C-cw" 'sdic-describe-word)

(lazyload (sdic-describe-word) "sdic"
          (setq sdic-default-coding-system 'utf-8-unix)
          (setq sdicf-array-command "/usr/local/bin/sary") ; コマンドパス
          (setq sdic-eiwa-dictionary-list
                '((sdicf-client "~/.emacs.d/var/dict/eijiro.sdic" (strategy array)))
                sdic-waei-dictionary-list
                '((sdicf-client "~/.emacs.d/var/dict/waeijiro.sdic" (strategy array))))

          ;; saryを直接使用できるように sdicf.el 内に定義されているarrayコマンド用関数を強制的に置換
          (fset 'sdicf-array-init 'sdicf-common-init)
          (fset 'sdicf-array-quit 'sdicf-common-quit)
          (fset 'sdicf-array-search
                (lambda (sdic pattern &optional case regexp)
                  (sdicf-array-init sdic)
                  (if regexp
                      (signal 'sdicf-invalid-method '(regexp))
                    (save-excursion
                      (set-buffer (sdicf-get-buffer sdic))
                      (delete-region (point-min) (point-max))
                      (apply 'sdicf-call-process
                             sdicf-array-command
                             (sdicf-get-coding-system sdic)
                             nil t nil
                             (if case
                                 (list "-i" pattern (sdicf-get-filename sdic))
                               (list pattern (sdicf-get-filename sdic))))
                      (goto-char (point-min))
                      (let (entries)
                        (while (not (eobp)) (sdicf-search-internal))
                        (nreverse entries))))))

          (defadvice sdic-forward-item (after sdic-forward-item-always-top activate)
            (recenter 0))
          (defadvice sdic-backward-item (after sdic-backward-item-always-top activate)
            (recenter 0))
          )

; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ magit

(lazyload (magit-status) "magit"
          ;; diffを表示しているときに文字単位での変更箇所も強調表示する
          ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
          (setq magit-diff-refine-hunk 'all)
          ;; diff用のfaceを設定する
          (diff-mode-setup-faces)
          ;; diffの表示設定が上書きされてしまうのでハイライトを無効にする
          (set-face-attribute 'magit-item-highlight nil :inherit nil)

          ;; 同一ウィンドウ内でコミットアクションを完結
          ;; http://qiita.com/dtan4/items/658a8a7ca06aa8c2da4c
          (cond
           (darwin-p
            (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient")
            ))
          )


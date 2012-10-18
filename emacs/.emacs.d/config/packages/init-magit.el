(unless (locate-library "magit")
  (el-get 'sync 'magit))

(when (require 'magit nil t)
  ;; requireに成功場合
  
  ;; diff関連の設定
  (defun magit-setup-diff ()
  ;; diffを表示しているときに文字単位での変更箇所も強調表示する
  ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
    (setq magit-diff-refine-hunk 'all)
    ;; diff用のfaceを設定する
    (diff-mode-setup-faces)
    ;; diffの表示設定が上書きされてしまうのでハイライトを無効にする
    (set-face-attribute 'magit-item-highlight nil :inherit nil))
  (add-hook 'magit-mode-hook 'magit-setup-diff)
  
)

; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------
;; @ 組込みelispの設定

;; ----------------------------------------------------------------------
;; @ Apache などの設定ファイルに色をつける

(require 'generic-x)

;; ----------------------------------------------------------------------
;; @ Diff

;; diffの表示方法を変更
(defun diff-mode-setup-faces ()
  ;; 追加された行は緑で表示
  (set-face-attribute 'diff-added nil
                      :foreground "white" :background "dark green")
  ;; 削除された行は赤で表示
  (set-face-attribute 'diff-removed nil
                      :foreground "white" :background "dark red")
  ;; 文字単位での変更箇所は色を反転して強調
  (set-face-attribute 'diff-refine-change nil
                      :foreground nil :background nil
                      :weight 'bold :inverse-video t))
(add-hook 'diff-mode-hook 'diff-mode-setup-faces)

;; diffを表示したらすぐに文字単位での強調表示も行う
(defun diff-mode-refine-automatically ()
  (diff-auto-refine-mode t))
(add-hook 'diff-mode-hook 'diff-mode-refine-automatically)


;; ----------------------------------------------------------------------
;; @ cua-mode

;; リージョン選択中に C-<enter> で矩形選択モードになります。 
;; sense-regionと同様に C-w や M-w が使えます。また、矩形選択中に次のコマンドを打つと、矩形領域に対して操作を行うことができます。
;; コマンド 動作
;; 適当なキー 矩形領域の前（または後ろ）にそのまま挿入。 <enter> で挿入位置を変えられる
;; M-p 矩形の幅を固定
;; M-b 空白文字で埋める。 open-rectangle と同等
;; M-s 文字列で置き換える。 string-rectangle と同等
;; M-f 1種類の文字で埋める。 string-rectangle で1文字指定したときと同等
;; M-i 矩形領域内の数字をインクリメントする
;; M-n 矩形領域を連番で埋める。フォーマット指定可
;; http://tech.kayac.com/archive/emacs-rectangle.html
(cua-mode t)
(setq cua-enable-cua-keys nil) ; そのままだと C-x が切り取りになってしまったりするので無効化


;; ----------------------------------------------------------------------
;; @ isswitch （anything で代替可）

;;; iswitchb は、バッファ名の一部の文字を入力することで、
;;; 選択バッファの絞り込みを行う機能を実現します。
;;; バッファ名を先頭から入力する必要はなく、とても使いやすくなります。
;(iswitchb-mode 1) ;;iswitchb
;; C-f, C-b, C-n, C-p で候補を切り替えることができるように。
;(add-hook 'iswitchb-define-mode-map-hook
;      (lambda ()
        ;; (define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
        ;; (define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
        ;; (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
        ;; (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))


;; ----------------------------------------------------------------------
;; @ emacsclient

(require 'server)
(unless (server-running-p)
  (server-start))

;; ----------------------------------------------------------------------
;; @ whitespace

;; Tab や 全角スペースを可視化
;; emacs23 より標準の elisp 
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style
      '(face tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        ;; tabを文字で置き換えるとタブがずれるので無効化
        ;; (tab-mark   ?\t   [?\xBB ?\t])
        ))
(set-face-foreground 'whitespace-space "LightSlateGray")
;; タブのフェイス設定
(set-face-attribute 'whitespace-tab nil
                    :background "#2f373a"
                    ;; :background "#2d3743"
                    :strike-through nil
                    :underline nil)



;; ----------------------------------------------------------------------
;; @ linum
;; http://d.hatena.ne.jp/kitokitoki/20100714/p1

;; メジャーモード/マイナーモードでの指定
(setq my-linum-hook-name '(emacs-lisp-mode-hook slime-mode-hook sh-mode-hook
                           c-mode-hook c++-mode-hook
                           php-mode-hook python-mode-hook ruby-mode-hook
                           css-mode-hook js2-mode-hook javascript-mode-hook
                           html-helper-mode-hook markdown-mode-hook 
                           ))

;; メジャーモード/マイナーモードの hook の指定
(defvar my-linum-hook-name nil)
(mapc (lambda (hook-name)
          (add-hook hook-name (lambda () (linum-mode t))))
       my-linum-hook-name)

;; ファイル名での判定
(defvar my-linum-file nil)
(defun my-linum-file-name ()
  (when (member (buffer-name) my-linum-file)
                (linum-mode t)))
(add-hook 'find-file-hook 'my-linum-file-name)

;; 拡張子での判定
(defvar my-linum-file-extension nil)
(defun my-linum-file-extension ()
  (when (member (file-name-extension (buffer-file-name)) my-linum-file-extension)
                (linum-mode t)))
(add-hook 'find-file-hook 'my-linum-file-extension)


(eval-after-load 'linum
  '(progn
     ;; 行番号のフォーマット
     (set-face-attribute 'linum nil  :foreground "gray")
     (setq linum-format"%4d ")
     ))


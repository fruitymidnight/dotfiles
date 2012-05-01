;; ----------------------------------------------------------------------
;; @ 組込みelispの設定

;; 対応する括弧を光らせる（グラフィック環境のみ作用）
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; 圧縮されたファイルも編集＆日本語infoの文字化け防止
(auto-compression-mode t)

;;"yes or no"を"y or n"にする
(fset 'yes-or-no-p 'y-or-n-p)

;;EmacsでXのクリップボードを使う
(setq x-select-enable-clipboard t)

;; インテンドにタブを使用しない
(setq-default indent-tabs-mode nil)

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
;; @ recentf

(setq recentf-max-saved-items 500)
(recentf-mode 1)


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

(if (locate-library "server")
    (progn
;;      (setq server-auth-dir "~/.emacs.d/var/server/")
      (load-library "server")
      (server-start)
      ))


;; ----------------------------------------------------------------------
;; @ whitespace

;; Tab や 全角スペースを可視化
;; emacs23 より標準の elisp 
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(set-face-foreground 'whitespace-space "LightSlateGray")
;(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
;(set-face-background 'whitespace-tab "DarkSlateGray")


;; ----------------------------------------------------------------------
;; @ grep

(when windows-p
  ;; shell-quote-argumentの問題回避 
  (defvar quote-argument-for-windows-p t "enables `shell-quote-argument' workaround for windows.") 
  (defadvice shell-quote-argument (around shell-quote-argument-for-win activate) 
    "workaround for windows." 
    (if quote-argument-for-windows-p 
        (let ((argument (ad-get-arg 0))) 
          (setq argument (replace-regexp-in-string "\\\\" "\\\\" argument nil t)) 
          (setq argument (replace-regexp-in-string "'" "'\\''" argument nil t)) 
          (setq ad-return-value (concat "'" argument "'"))) 
      ad-do-it)) 

  ;; lgrep で utf-8 を使うように設定 
  (setq grep-host-defaults-alist nil) ;; これはおまじないだと思ってください 
  (setq grep-use-null-device nil)
  (setq grep-command "lgrep -Du8 -Ku8 -Ou8 -n '' {*,.*}")
  (setq grep-template "lgrep -Du8 -Ku8 -Ou8 <C> -n <R> <F> <N>") 
  (setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep -Du8 -Ku8 -Ou8 <C> -n <R> <N>") 
  
  ;; http://d.hatena.ne.jp/whitypig/20111216/1324024458
  ;; (defadvice compilation-start (around compilation-start-message-coding-ad activate)
  ;;   (let ((coding-system-for-read 'utf-8)
  ;;         (coding-system-for-write 'sjis-dos))
  ;;     ad-do-it))
)

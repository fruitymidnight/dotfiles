;; ---------------------------------------------------
;; text-translator
;; ---------------------------------------------------

(require 'text-translator)
;;翻訳キー設定
(global-set-key "\C-ct" 'text-translator)
(global-set-key "\C-c\M-T" 'text-translator-translate-last-string)

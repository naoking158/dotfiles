;;; -*- lexical-binding: t no-byte-compile: t -*-

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold (* 1024 1024 1024))))

;; basic
(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)
(setq debug-on-error t
      init-file-debug t
      user-full-name "Naoki Sakamoto"
      user-mail-address "naoki@bbo.cs.tsukuba.ac.jp"
      user-login-name "naoking158"
      package-enable-at-startup nil
      package-native-compile t)

(setq inhibit-splash-screen t
      frame-inhibit-implied-resize t
      byte-compile-warnings '(cl-functions))

;; Avoid popup Async buffer window
(add-to-list 'display-buffer-alist
             '("^*Async Shell Command*" . (display-buffer-no-window)))

;; map backspace [delete-backward-char] to C-h
(define-key key-translation-map [?\C-?] [?\C-h])

;; map M-backspace [backward-kill-word] to M-h
(define-key key-translation-map [?\M-\d] [?\M-h])

;; map C-h to backspace
;; (define-key key-translation-map [?\C-h] [?\C-?])

;; map M-h [mark-paragraph] to M-backspace
(define-key key-translation-map [?\M-h] [?\M-\d])


(package-initialize)

(defun init-skk nil
  (require 'skk)

  (setq default-input-method "japanese-skk"
        skk-jisyo-code 'utf-8
        skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.Huge.utf8"
        skk-backup-jisyo "~/.skk-jisyo.BAK"
        skk-save-jisyo-instantly t
        skk-share-private-jisyo t
        skk-server-report-response nil
        skk-preload nil
        skk-isearch-mode-enable 'always
        skk-kutouten-type 'en
        skk-use-auto-kutouten t
        skk-show-inline 'vertical
        skk-inline-show-face nil
        skk-egg-like-newline t  ;; skk-kakutei by RET
        skk-auto-okuri-process nil
        skk-henkan-strict-okuri-precedence t
        skk-auto-insert-paren t
        skk-use-auto-enclose-pair-of-region t
        skk-sticky-key ";"
        skk-dcomp-activate t
        skk-dcomp-multiple-activate t
        skk-status-indicator 'minor-mode
        ;; skk-inline-show-face '( :foreground "#ECEFF4"
        ;;                         :background "#4C566A"
        ;;                         :inherit 'normal)
        )  
  )

(defun enable-wl-copy-process nil
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste)
  )


(defun create-empty-buffer-with-skk nil
  (interactive)  
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (enable-wl-copy-process)
    (text-mode)
    (fido-vertical-mode)
    (init-skk)
    (skk-mode)
    (setq confirm-kill-processes nil
          select-enable-primary t)
    ))

(create-empty-buffer-with-skk)

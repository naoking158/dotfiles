;;; early-init.el --- Early Initialization. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

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
(setq debug-on-error nil
      init-file-debug t
      user-full-name "Naoki Sakamoto"
      user-mail-address "naoki@bbo.cs.tsukuba.ac.jp"
      user-login-name "naoking158"
      package-enable-at-startup nil
      package-native-compile t)

(when-let ((fish (executable-find "fish")))
  (setq shell-file-name fish))

;; GUI
(setq default-frame-alist '((line-spacing . 2)
                            (vertical-scroll-bars)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (left-fringe . 10)
                            (right-fringe . 10)
                            (width . 150)
                            (height . 80)))

(setq inhibit-splash-screen t
      frame-inhibit-implied-resize t
      byte-compile-warnings '(cl-functions))

;; Avoid popup Async buffer window
(add-to-list 'display-buffer-alist
             '("^*Async Shell Command*" . (display-buffer-no-window)))

(provide 'early-init)

;;; early-init.el ends here

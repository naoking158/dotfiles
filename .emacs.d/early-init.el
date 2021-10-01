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
			package-native-compile t)
;; (native-compile-async "~/.emacs.d/elisp/" 'recursively)
(dolist (item (split-string
							 (shell-command-to-string "which fish") "\n"))
	(when (string-match "/usr/" item)
		(setq shell-file-name item)))

;; GUI
(setq default-frame-alist (append '((line-spacing . 4)
																		(vertical-scroll-bars)
																		(menu-bar-lines . 0)
																		(tool-bar-lines . 0)
																		(left-fringe . 10)
																		(right-fringe . 10))
																	initial-frame-alist))

;; (push '(fullscreen . maximized) default-frame-alist)
;; (push initial-frame-alist default-frame-alist)
(setq inhibit-splash-screen t
	  frame-inhibit-implied-resize t
	  byte-compile-warnings '(cl-functions))

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here

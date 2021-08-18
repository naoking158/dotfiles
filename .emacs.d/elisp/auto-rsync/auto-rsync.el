;;; auto-rsync-mode -- minor mode for auto rsync
;;
;; Author: @l3msh0
;; Editted by: dizzyvn

;;; Example

;;; Customize
;;
(defgroup auto-rsync nil "Auto rsync")
(defcustom auto-rsync-command "rsync" "rsync command path" :group 'auto-rsync)
(defcustom auto-rsync-command-option "-avzq" "rsync command option" :group 'auto-rsync)


;;; TODO
;;
;; open remote counterpart
;;
(defvar auto-rsync-dir-alist nil "Pair of rsync source and destination dir")
(defvar auto-rsync-normalized-alist nil)

;;; Code
(require 'cl)
(defun alist-keys (alist)
  (mapcar 'car alist))

(defun sync-pairs (alist)
  ;; Run commands
  (setq target-dirs
	(apply #'concatenate 'string (mapcar (lambda (x) (format "%s, " (cdr x))) alist)))
  (message "Syncing to %s" target-dirs)
  (setq command-list (mapcar (lambda (x) (format "%s %s %s %s&;" auto-rsync-command auto-rsync-command-option (car x) (cdr x))) alist))
  (setq command-str (apply #'concatenate 'string command-list))
  (call-process-shell-command command-str nil 0))

(defun auto-rsync-exec-rsync ()
  "execute rsync if editing file path matches src dir"
  (interactive)
  (setq normalized-alist
	(mapcar (lambda (x) (cons (file-name-as-directory (expand-file-name (car x)))
                                  (file-name-as-directory (cdr x))))
                auto-rsync-dir-alist))
  (setq filtered-normalized-alist (remove-if-not #'(lambda (x) (string-match (car x) buffer-file-name)) normalized-alist))
  (sync-pairs filtered-normalized-alist))

(define-minor-mode auto-rsync-mode
  "automatically execute rsync when editing file's path matches `auto-rsync-dir-alist`"
  :lighter " rsync"
  :global t
  (cond (auto-rsync-mode
         (add-hook 'after-save-hook 'auto-rsync-exec-rsync))
        (t
         (remove-hook 'after-save-hook 'auto-rsync-exec-rsync))))

(provide 'auto-rsync)

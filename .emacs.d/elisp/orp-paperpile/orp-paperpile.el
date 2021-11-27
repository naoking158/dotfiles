;;; orp-paperpile.el --- Org-Roam-Protocol Paperpile -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;
;; orp-paperpile; Org-Roam-Protocol Paperpile is an interface
;; to comunicate between org-mode and paperpile using org-roam-protocol.
;;
;;; Code:

(require 'ol)  ;; for org-link-decode
(require 'org-protocol)
(require 'org-roam)
(require 'org-roam-protocol)

(defgroup orp-paperpile nil
  "Org-Roam-Protocol Paperpile."
  :prefix "orp-paperpile-"
  :group 'orp-paperpile)

(defcustom orp-paperpile-local-pdf-dir
  "/Volumes/GoogleDrive/My Drive/Paperpile/"
  "Local PDFs managed by Paperpile are here. If your OS is macOS and
    Google-Drive-Desktop has been introduced, the PDFs are located under
    `/Volumes/GoogleDrive/My Drive/Paperpile/'."
  :type 'string
  :group 'orp)

(defun org-roam-visit-pdf-online ()
      (interactive)
      (setq pdf-string (replace-regexp-in-string "\"" "" (org-entry-get 0 "PDF")))
      (setq pdf-string-list (split "-----" pdf-string))
      (org-link-open-from-string (nth 0 pdf-string-list)))

;;;###autoload
(defun orp-paperpile--open-paper (info)
  (unless (plist-get info :ref)
    (user-error "No ref key provided"))
  (org-roam-plist-map! (lambda (k v)
                         (org-link-decode
                          (if (equal k :ref)
                              (org-protocol-sanitize-uri v)
                            v))) info)
  (when org-roam-protocol-store-links
    (push (list (plist-get info :ref)
                (plist-get info :title)) org-stored-links))
  (org-link-store-props :type (and (string-match org-link-plain-re
                                                 (plist-get info :ref))
                                   (match-string 1 (plist-get info :ref)))
                        :link (plist-get info :ref)
                        :annotation (org-link-make-string (plist-get info :ref)
                                                          (or (plist-get info :title)
                                                              (plist-get info :ref)))
                        :initial (or (plist-get info :body) ""))
  (raise-frame)
  (org-roam-capture-
   :keys (plist-get info :template)
   :node (org-roam-node-create :title (plist-get info :title))
   :info (list :ref (plist-get info :ref)
               :cite (plist-get info :cite)
               :file (plist-get info :file)
               :pdf (plist-get info :pdf)
               :permalink (plist-get info :permalink)
               :abstract (plist-get info :abstract))
   :templates org-roam-capture-ref-templates)
  nil)

(defun orp-paperpile--try-capture-to-ref-h ()
  "Try to capture to an existing node that match the ref. This overrides org-roam-protocol--try-capture-to-ref-h"
  (when-let ((node (and (plist-get org-roam-capture--info :ref)
                        (org-roam-node-from-ref
                         (plist-get org-roam-capture--info :ref)))))
    (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
    (goto-char (org-roam-node-point node))
    (widen)
    (org-roam-node-id node)
    (orp-paperpile--update-org-roam-paper org-roam-capture--info)))


;; Capture implementation
(defun orp-paperpile--insert-captured-ref-h ()
  "This overrides org-roam-protocol--insert-captured-ref-h."
  (orp-paperpile--update-org-roam-paper org-roam-capture--info))


;; Capture implementation
(defun orp-paperpile--update-org-roam-paper (info)
  (message "%s" info)

  (when-let ((ref (plist-get info :ref)))
    (org-roam-ref-add ref))

  (when-let ((cite (plist-get info :cite)))
    (org-entry-delete 0 "CITE")
    (org-roam-add-property cite "CITE"))

  (when-let ((pdf (plist-get info :pdf))
             (file (plist-get info :file))
             (permalink (plist-get info :permalink)))
    (org-entry-delete 0 "PAPERPILE")
    (org-entry-delete 0 "PDF")
    (org-roam-add-property
     (concat (format "[[%s][online]]-----" pdf)
             (format "[[file:%s][offline]]-----"
                     (expand-file-name file orp-paperpile-local-pdf-dir))
             (format "[[%s][paperpile]]" permalink))
     "PDF"))

  (when-let ((abstract (plist-get info :abstract)))
    (when (org-find-exact-headline-in-buffer "Abstract")
      (goto-char (org-find-exact-headline-in-buffer "Abstract"))
      (let (this-command (inhibit-message t)) (org-cut-subtree)))
    (orp-paperpile-insert-abstract-to-top abstract)))

;;;###autoload
(defun orp-paperpile-insert-abstract-to-top (abstract)
  (interactive)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (insert (format "* Abstract\n%s\n\n" abstract)))

;;;###autoload
(defun orp-activate nil
  (interactive)
  (advice-add #'org-roam-protocol--try-capture-to-ref-h
              :override #'orp-paperpile--try-capture-to-ref-h)
  (advice-add #'org-roam-protocol--insert-captured-ref-h
              :override #'orp-paperpile--insert-captured-ref-h)

  (custom-set-variables
   '(org-protocol-protocol-alist '(("org-roam-node"
                                    :protocol "roam-node"
                                    :function org-roam-protocol-open-node)
                                   ("org-roam-ref"
                                    :protocol "roam-ref"
                                    :function org-roam-protocol-open-ref)
                                   ("org-roam-paper"
                                    :protocol "roam-paper"
                                    :function orp-paperpile--open-paper)))))

;;;###autoload
(defun orp-deactivate nil
  (interactive)
  (advice-remove #'org-roam-protocol--try-capture-to-ref-h
                 #'orp-paperpile--try-capture-to-ref-h)
  (advice-remove #'org-roam-protocol--insert-captured-ref-h
                 #'orp-paperpile--insert-captured-ref-h))

(provide 'orp-paperpile)

;; orp-paperpile.el ends here

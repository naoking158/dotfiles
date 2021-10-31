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

(defcustom orp-paperpile-ref-templates
  '(("r" "ref" plain "%?"
     :target (file+head "${slug}.org"
                        "#+title: ${title}")
     :unnarrowed t))
  "See org-roam-capture-ref-templates."
  :group 'orp
  :type '(repeat
          (choice (list :tag "Multikey description"
                        (string :tag "Keys       ")
                        (string :tag "Description"))
                  (list :tag "Template entry"
                        (string :tag "Keys           ")
                        (string :tag "Description    ")
                        (choice :tag "Capture Type   " :value entry
                                (const :tag "Org entry" entry)
                                (const :tag "Plain list item" item)
                                (const :tag "Checkbox item" checkitem)
                                (const :tag "Plain text" plain)
                                (const :tag "Table line" table-line))
                        (choice :tag "Template       "
                                (string)
                                (list :tag "File"
                                      (const :format "" file)
                                      (file :tag "Template file"))
                                (list :tag "Function"
                                      (const :format "" function)
                                      (function :tag "Template function")))
                        (plist :inline t
                               ;; Give the most common options as checkboxes
                               :options (((const :format "%v " :target)
                                          (choice :tag "Node location"
                                                  (list :tag "File"
                                                        (const :format "" file)
                                                        (string :tag "  File"))
                                                  (list :tag "File & Head Content"
                                                        (const :format "" file+head)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content"))
                                                  (list :tag "File & Outline path"
                                                        (const :format "" file+olp)
                                                        (string :tag "  File")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))
                                                  (list :tag "File & Head Content & Outline path"
                                                        (const :format "" file+head+olp)
                                                        (string :tag "  File")
                                                        (string :tag "  Head Content")
                                                        (list :tag "Outline path"
                                                              (repeat (string :tag "Headline"))))))
                                         ((const :format "%v " :prepend) (const t))
                                         ((const :format "%v " :immediate-finish) (const t))
                                         ((const :format "%v " :jump-to-captured) (const t))
                                         ((const :format "%v " :empty-lines) (const 1))
                                         ((const :format "%v " :empty-lines-before) (const 1))
                                         ((const :format "%v " :empty-lines-after) (const 1))
                                         ((const :format "%v " :clock-in) (const t))
                                         ((const :format "%v " :clock-keep) (const t))
                                         ((const :format "%v " :clock-resume) (const t))
                                         ((const :format "%v " :time-prompt) (const t))
                                         ((const :format "%v " :tree-type) (const week))
                                         ((const :format "%v " :unnarrowed) (const t))
                                         ((const :format "%v " :table-line-pos) (string))
                                         ((const :format "%v " :kill-buffer) (const t))))))))

(defun orp-paperpile-org-roam-visit-paperpile ()
  (interactive)
  (org-link-open-from-string (org-entry-get 0 "PAPERPILE")))

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
   :templates orp-paperpile-ref-templates)
  nil)

;;;###autoload
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
;;;###autoload
(defun orp-paperpile--insert-captured-ref-h ()
  "This overrides org-roam-protocol--insert-captured-ref-h."
  (orp-paperpile--update-org-roam-paper org-roam-capture--info))

;; Capture implementation
;;;###autoload
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

(defun orp-paperpile-insert-abstract-to-top (abstract)
  (interactive)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (insert (format "* Abstract\n%s\n\n" abstract)))

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

(defun orp-deactivate nil
  (interactive)
  (advice-remove #'org-roam-protocol--try-capture-to-ref-h
                 #'orp-paperpile--try-capture-to-ref-h)
  (advice-remove #'org-roam-protocol--insert-captured-ref-h
                 #'orp-paperpile--insert-captured-ref-h))

;; (orp-activate)

(provide 'orp-paperpile)

;; orp-paperpile.el ends here

;;; consult-tramp.el --- Tramp vertico interface for ssh -*- lexical-binding: t; -*-


;;; Commentary:

;; consult-tramp provides interfaces of Tramp
;; You can also use tramp with consult interface as root


;;; Code:

(require 'consult)
(require 'tramp)
(require 'cl-lib)

(defgroup consult-tramp nil
  "Tramp with vertico interface for ssh, docker, vagrant"
  :group 'consult)

(defcustom consult-tramp-default-method "ssh"
  "Default method when use tramp multi hop."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-localhost-directory "/"
  "Initial directory when connecting with /sudo:root@localhost:."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-control-master nil
  "If you want to put out a candidate for completion from ssh controlmaster, please set to t."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-control-master-path "~/.ssh/"
  "Path where ssh controlmaster exists."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-control-master-prefix "master-"
  "Prefix of ssh controlmaster."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-pre-command-hook nil
  "Hook run before `consult-tramp'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom consult-tramp-post-command-hook nil
  "Hook run after `consult-tramp'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom consult-tramp-quit-hook nil
  "Hook run when `consult-tramp-quit'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom consult-tramp-custom-connections '()
  "A list to manually add extra connections.
E.g.: '(\"/ssh:domain|sudo:user@localhost:/\")."
  :type 'string)

(defun consult-tramp-quit ()
  "Quit consult-tramp.
Kill all remote buffers."
  (interactive)
  (run-hooks 'consult-tramp-quit-hook)
  (tramp-cleanup-all-buffers))

(defun consult-tramp--candidates (&optional file)
  "Collect candidates for consult-tramp from FILE."
  (let ((source (split-string
                 (with-temp-buffer
                   (insert-file-contents (or file "~/.ssh/config"))
                   (buffer-string))
                 "\n"))
        (hosts (if file '() consult-tramp-custom-connections)))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
				(setq host (match-string 1 host))
				(if (string-match "[ \t\n\r]+\\'" host)
						(replace-match "" t t host))
				(if (string-match "\\`[ \t\n\r]+" host)
						(replace-match "" t t host))
        (unless (string= host "*")
					(if (string-match "[ ]+" host)
							(let ((result (split-string host " ")))
								(while result
									(push
									 (concat "/" tramp-default-method ":" (car result) ":")
									 hosts)
									(push
									 (concat "/" consult-tramp-default-method ":"
													 (car result) "|sudo:root@" (car result) ":/")
									 hosts)
									(pop result)))
						(push
						 (concat "/" tramp-default-method ":" host ":")
						 hosts)
						(push
						 (concat "/" consult-tramp-default-method ":"
										 host "|sudo:root@" host ":/")
						 hosts))))
      (when (string-match "Include +\\(.+\\)$" host)
        (setq include-file (match-string 1 host))
        (when (not (file-name-absolute-p include-file))
          (setq include-file (concat (file-name-as-directory "~/.ssh")
																		 include-file)))
        (when (file-exists-p include-file)
          (setq hosts (append hosts (consult-tramp--candidates include-file))))))
    (when consult-tramp-control-master
      (let ((files (consult-tramp--directory-files
										(expand-file-name
										 consult-tramp-control-master-path)
										consult-tramp-control-master-prefix))
						(hostuser nil)
						(hostname nil)
						(port nil))
				(dolist (controlmaster files)
					(let ((file (file-name-nondirectory controlmaster)))
						(when (string-match
									 (concat consult-tramp-control-master-prefix
													 "\\(.+?\\)@\\(.+?\\):\\(.+?\\)$")
									 file)
							(setq hostuser (match-string 1 file))
							(setq hostname (match-string 2 file))
							(setq port (match-string 3 file))
							(push
							 (concat "/" tramp-default-method ":"
											 hostuser "@" hostname "#" port ":")
							 hosts)
							(push
							 (concat "/" consult-tramp-default-method ":"
											 hostuser "@" hostname "#" port "|sudo:root@" hostname ":/")
							 hosts))))))
    (push (concat "/sudo:root@localhost:" consult-tramp-localhost-directory)
					hosts)
    (reverse hosts)))

(defun consult-tramp--directory-files (dir regexp)
  "Return list of all files under DIR that have file names matching REGEXP."
  (let ((result nil)
				(files nil)
				(tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
												'string<))
      (unless (member file '("./" "../"))
				(if (not (consult-tramp--directory-name-p file))
						(when (string-match regexp file)
							(push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))

(defsubst consult-tramp--directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (lastc ?.))
    (if (> len 0)
        (setq lastc (aref name (1- len))))
    (or (= lastc ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= lastc ?\\)))))

;;;###autoload
(defun consult-tramp ()
  "Open your ~/.ssh/config with consult interface.
You can connect your server with tramp"
  (interactive)
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
  (when (require 'docker-tramp nil t)
    (unless (executable-find "docker")
      (error "'docker' is not installed")))
  (when (require 'vagrant-tramp nil t)
    (unless (executable-find "vagrant")
      (error "'vagrant' is not installed")))
  (run-hooks 'consult-tramp-pre-command-hook)
  (find-file (completing-read "Tramp: " (consult-tramp--candidates)))
  (run-hooks 'consult-tramp-post-command-hook))

(provide 'consult-tramp)

;;; consult-tramp.el ends here

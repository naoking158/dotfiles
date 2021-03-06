;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el

(setq exec-profile nil)

(when exec-profile
  (defvar setup-tracker--level 0)
  (defvar setup-tracker--parents nil)
  (defvar setup-tracker--times nil)

  (when load-file-name
    (push load-file-name setup-tracker--parents)
    (push (current-time) setup-tracker--times)
    (setq setup-tracker--level (1+ setup-tracker--level)))

  (add-variable-watcher
   'load-file-name
   (lambda (_ v &rest __)
     (cond ((equal v (car setup-tracker--parents))
            nil)
           ((equal v (cadr setup-tracker--parents))
            (setq setup-tracker--level (1- setup-tracker--level))
            (let* ((now (current-time))
                   (start (pop setup-tracker--times))
                   (elapsed (+ (* (- (nth 1 now) (nth 1 start)) 1000)
                               (/ (- (nth 2 now) (nth 2 start)) 1000))))
              (with-current-buffer (get-buffer-create "*setup-tracker*")
                (save-excursion
                  (goto-char (point-min))
                  (dotimes (_ setup-tracker--level) (insert "> "))
                  (insert
                   (file-name-nondirectory (pop setup-tracker--parents))
                   " (" (number-to-string elapsed) " msec)\n")))))
           (t
            (push v setup-tracker--parents)
            (push (current-time) setup-tracker--times)
            (setq setup-tracker--level (1+ setup-tracker--level))))))


  (defun efs/display-startup-time()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))
  (add-hook 'emacs-startup-hook #'efs/display-startup-time))

(prog1 'leaf-setup
  (eval-and-compile
    ;; Setup straight
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

    ;; Install leaf and leaf-keywords
    (straight-use-package 'leaf)
    (straight-use-package 'leaf-keywords)
    (straight-use-package 'leaf-convert)
    (straight-use-package 'hydra)
    (straight-use-package 'blackout)
    
    (leaf leaf-keywords
      :require t
      :config (leaf-keywords-init)
      ;; :init
      ;; (leaf leaf-convert :straight t)
      ;; (leaf hydra :straight t)
      ;; (leaf blackout :straight t)

      ;; (leaf key-chord
      ;;   :disabled t
      ;;   :straight t
      ;;   :hook (emacs-startup-hook . (lambda () (key-chord-mode 1)))
      ;;   :custom ((key-chord-one-keys-delay . 0.02)
      ;;            (key-chord-two-keys-delay . 0.03))
      ;;   :config
      ;;   (key-chord-define-global "x0" '"\C-x0")
      ;;   (key-chord-define-global "x1" '"\C-x1")
      ;;   (key-chord-define-global "x2" '"\C-x2")
      ;;   (key-chord-define-global "x3" '"\C-x3")
      ;;   (key-chord-define-global "x5" '"\C-x52"))
      )
    )
  
  ;; (eval-and-compile
  ;;   (custom-set-variables
  ;;    '(warning-suppress-types '((comp)))
  ;;    '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
  ;;                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
  ;;                         ("melpa" . "https://melpa.org/packages/"))))
  ;;   (package-initialize)
  ;;   (unless (package-installed-p 'leaf)
  ;;     (package-refresh-contents)
  ;;     (package-install 'leaf))

  ;;   (leaf leaf-keywords
  ;;     :straight t
  ;;     :config (leaf-keywords-init)
  ;;     :init
  ;;     (leaf leaf-convert :straight t)
  ;;     (leaf hydra :straight t)
  ;;     (leaf blackout :straight t)

  ;;     (leaf key-chord
  ;;       :straight t
  ;;       :hook (emacs-startup-hook . (lambda () (key-chord-mode 1)))
  ;;       :custom ((key-chord-one-keys-delay . 0.02)
  ;;                (key-chord-two-keys-delay . 0.03))
  ;;       :config
  ;;       (key-chord-define-global "x0" '"\C-x0")
  ;;       (key-chord-define-global "x1" '"\C-x1")
  ;;       (key-chord-define-global "x2" '"\C-x2")
  ;;       (key-chord-define-global "x3" '"\C-x3")
  ;;       (key-chord-define-global "x5" '"\C-x52"))

  ;;     (leaf straight
  ;;       :config
  ;;       )
  ;;     ))
  )

(leaf *keep-clean
  :config
  ;; Use no-littering to automatically set common paths to the new user-emacs-directory
  (leaf no-littering
    :straight t
    :leaf-defer nil
    :config
    ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d

    (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
          url-history-file (expand-file-name "url/history" user-emacs-directory))
    (setq no-littering-etc-directory
          (expand-file-name "etc/" user-emacs-directory))
    (setq no-littering-var-directory
          (expand-file-name "var/" user-emacs-directory)))

  ;; Keep customization settings in a temporary file
  (leaf cus-edit
    :doc "tools for customizing Emacs and Lisp packages"
    :tag "builtin" "faces" "help"
    :config
    (setq custom-file
          (if (boundp 'server-socket-dir)
              (expand-file-name "custom.el" server-socket-dir)
            (expand-file-name
             (format "emacs-custom-%s.el" (user-uid))
             temporary-file-directory)))
    (load custom-file t)
    )

  (leaf recentf
    :require no-littering
    :custom ((recentf-exclude . `(".recentf"
                                  "bookmarks"
                                  "org-recent-headings.dat"
                                  "^/tmp\\.*"
                                  "^/private\\.*"
                                  "/TAGS$"
                                  ,no-littering-var-directory
                                  ,no-littering-etc-directory))
             (recentf-save-file . "~/.emacs.d/.recentf")
             (recentf-max-saved-items . 2000)
             (recentf-auto-cleanup . 'never))
    :global-minor-mode t)

  (leaf *auto-save
    :config
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))

(leaf *general-configrations
  :config
  (leaf cus-start
    :doc "define customization properties of builtins"
    :tag "builtin" "internal"
    :url "http://handlename.hatenablog.jp/entry/2011/12/11/214923"
    :leaf-defer nil
    :bind (("C-M-<backspace>" . delete-region)
           ("C-x C-p" . switch-to-prev-buffer)
           ("C-x C-n" . switch-to-next-buffer))
    :hook (after-init-hook . general-init-hook)
    :preface
    (defun general-init-hook nil
      (menu-bar-mode -1)
      (when-let ((gls (executable-find "gls")))
        (setq insert-directory-program gls dired-use-ls-dired t)
        (setq dired-listing-switches "-al --group-directories-first")))
    :custom '((fill-column . 82)
              (tab-width . 2)             
              (frame-resize-pixelwise . t)
              (enable-recursive-minibuffers . t)
              (create-lockfiles)
              (use-dialog-box)
              (use-file-dialog)
              (history-length . 1000)
              (history-delete-duplicates . t)
              (scroll-preserve-screen-position . t)
              (scroll-conservatively . 100)
              (mouse-wheel-scroll-amount quote (1 ((control). 5)))
              (ring-bell-function . 'ignore)
              (text-quoting-style . 'straight)
              (truncate-lines . t)
              (fringe-mode . 10)
              (blink-cursor-mode . t)
              (show-paren-mode . 1)
              (confirm-kill-emacs . 'y-or-n-p)
              (recentf-auto-cleanup . 'never)
              (save-place-mode . 1)
              (save-interprogram-paste-before-kill . t)
              (indent-tabs-mode . nil))
    :config
    (defalias 'yes-or-no-p 'y-or-n-p)
    (keyboard-translate 8 127)
    (mapc (lambda (fn)
            (put fn 'disabled nil))
          (list 'upcase-region
                'downcase-region
                'narrow-to-region
                'narrow-to-page
                'narrow-to-defun
                'list-timers)))

  (leaf exec-path-from-shell
    :doc "Get environment variables such as $PATH from the shell"
    :tag "environment" "unix"
    :url "https://github.com/purcell/exec-path-from-shell"
    :straight t
    :leaf-defer nil
    :when window-system
    :custom
    (;; (exec-path-from-shell-check-startup-files)
     (exec-path-from-shell-variables . '("PATH"
                                         "SSH_AUTH_SOCK"
                                         "SSH_AGENT_PID"
                                         "GPG_AGENT_INFO"
                                         "LANG"
                                         "LC_CTYPE"
                                         "NIX_SSL_CERT_FILE"
                                         "NIX_PATH"
                                         "PYTHONPATH"
                                         "NEPTUNE_API_TOKEN")))
    :config
    (exec-path-from-shell-initialize))

  (leaf eldoc
    :doc "Show function arglist or variable docstring in echo area"
    :tag "builtin"
    :custom (eldoc-idle-delay . 0.1)))

(leaf change-system-configuration
  :leaf-defer nil
  :bind (("M-o" . finder-current-dir-open)
         ("s-w" . kill-buffer)
         ("s-q" . save-buffers-kill-emacs)
         ("s-v" . yank)
         ("s-c" . kill-ring-save))
  :preface
  (defun finder-current-dir-open nil
    (interactive)
    (shell-command "open ."))
  :config
  ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (leaf mac
    :doc "implementation of gui terminal on macos"
    :doc "each symbol can be `control', `meta', `alt', `hyper', or `super'"
    :doc "`left' meens same value setting its left key"
    :when (eq 'darwin window-system)
    :custom ((mac-control-modifier . 'control)
             (mac-option-modifier . 'meta)
             (mac-command-modifier . 'super)
             (mac-right-control-modifier . 'control)
             (mac-right-option-modifier . 'meta)
             (mac-right-command-modifier . 'super)))

  (leaf ns
    :doc "next/open/gnustep / macos communication module"
    :when (eq 'ns window-system)
    :custom ((ns-control-modifier . 'control)
             (ns-option-modifier . 'meta)
             (ns-command-modifier . 'super)
             (ns-right-control-modifier . 'control)
             (ns-right-option-modifier . 'meta)
             (ns-right-command-modifier . 'super)
             (ns-use-proxy-icon . nil))))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1)
           (global-auto-revert-non-file-buffers . t))
  :config (global-auto-revert-mode 1))

(leaf super-save
  :doc "Auto-save buffers, based on your activity."
  :req "emacs-24.4"
  :url "https://github.com/bbatsov/super-save"
  :straight t ace-window
  :require t ace-window
  :blackout
  :custom ((super-save-auto-save-when-idle . t)
           (super-save-idle-duration . 7))
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1))

(leaf undo-tree
  :disabled t
  :straight t
  :hook (emacs-startup-hook . global-undo-tree-mode)
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ([remap undo-tree-restore-state-from-register] . nil)
         ([remap undo-tree-save-state-to-register] . nil))
  :custom
  ((undo-tree-limit . 1000000)
   (undo-tree-strong-limit . 4000000)
   (undo-tree-outer-limit . 12000000)
   (undo-tree-auto-save-history . t)
   (undo-tree-history-directory-alist . `(("." . ,(no-littering-expand-etc-file-name
                                                   "undo"))))))

(leaf vundo
  :straight t
  :bind ("C-c u" . vundo))

;; Compilation deny package
(setq comp-deferred-compilation-deny-list (list "jupyter"))

;; native-compile all Elisp files under a directory
(native-compile-async (file-truename "~/.emacs.d/elisp/") 'recursively)

(defun my/toggle-modeline (&optional arg)
  (interactive)
  (if (or (null mode-line-format) arg)
      (kill-local-variable 'mode-line-format)
    (setq-local mode-line-format nil)
    (force-mode-line-update)))

(defun my/trim-newline-from-string (string)
  (let ((len (length string)))
    (cond
     ((and (> len 0) (eql (aref string (- len 1)) ?\n))
      (substring string 0 (- len 1)))
     (t string))))

;; https://github.com/alphapapa/unpackaged.el#sort-sexps
(defun my-sort-sexps (beg end)
  "Sort sexps in region (from BEG to END)."
  (interactive "r")
  (cl-flet ((skip-whitespace () (while (looking-at (rx (1+ (or space "\n"))))
                                  (goto-char (match-end 0))))
            (skip-both () (while (cond ((or (nth 4 (syntax-ppss))
                                            (ignore-errors
                                              (save-excursion
                                                (forward-char 1)
                                                (nth 4 (syntax-ppss)))))
                                        (forward-line 1))
                                       ((looking-at (rx (1+ (or space "\n"))))
                                        (goto-char (match-end 0)))))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (skip-both)
        (cl-destructuring-bind (sexps markers)
            (cl-loop do (skip-whitespace)
                     for start = (point-marker)
                     for sexp = (ignore-errors
                                  (read (current-buffer)))
                     for end = (point-marker)
                     while sexp
                     ;; Collect the real string, then one used for sorting.
                     collect (cons (buffer-substring (marker-position start) (marker-position end))
                                   (save-excursion
                                     (goto-char (marker-position start))
                                     (skip-both)
                                     (buffer-substring (point) (marker-position end))))
                     into sexps
                     collect (cons start end)
                     into markers
                     finally return (list sexps markers))
          (setq sexps (sort sexps (lambda (a b)
                                    (string< (cdr a) (cdr b)))))
          (cl-loop for (real . sort) in sexps
                   for (start . end) in markers
                   do (progn
                        (goto-char (marker-position start))
                        (insert-before-markers real)
                        (delete-region (point) (marker-position end)))))))))

(leaf command-log-mode
  :straight t
  :commands command-log-mode)

(defun my/toggle-presentation (fontsize)
  (interactive (list
                (read-number "Fontsize: " (if (null mode-line-format) 16 35))))

  (let ((my/presentation (null mode-line-format)))
    (my/toggle-modeline my/presentation)
    (tab-bar-mode (if my/presentation 1 'toggle))
    (my/set-font-size fontsize)))

(defun my/bibtex-indent-in-buffer nil
  "Align indent and equal symbol"
  (interactive)
  (setq bibtex-align-at-equal-sign t)
  (save-excursion
    (goto-char (point-min))
    (let ((is-failed nil))
      (while (not is-failed)
        (condition-case nil
            (progn
              (re-search-forward "\@")
              (bibtex-fill-entry))
          (error (setq is-failed t)))))))

(leaf ui
  :leaf-defer nil
  :hook
  ((prog-mode-hook latex-mode-hook) . (lambda ()
                                        (display-line-numbers-mode 1)))

  :config
  (leaf dashboard
    :doc "A startup screen extracted from Spacemacs"
    :req "emacs-25.3" "page-break-lines-0.11"
    :tag "dashboard" "tools" "screen" "startup" "emacs>=25.3"
    :url "https://github.com/emacs-dashboard/emacs-dashboard"
    :straight t
    :require dashboard-widgets
    :leaf-defer nil
    :init
    (custom-set-variables
     '(dashboard-projects-backend (if (<= emacs-major-version 27)
                                      'projectile
                                    'project-el)))
    :custom ((dashboard-items . '((recents . 5)
                                  (projects . 5)
                                  (bookmarks . 5))))
    :config
    (when window-system
      (setq dashboard-startup-banner "~/.emacs.d/banner/coffee.png"))
    (dashboard-setup-startup-hook))

  (leaf set-title-bar
    :when window-system
    :config
    ;; This shoud be set before exec `display-time`. 
    (setq display-time-string-forms '((format "%s %s %s" dayname monthname day)
                                      (format "  %s:%s" 24-hours minutes))
          frame-title-format '(" - " display-time-string " - Emacs"))
    (display-time))
  
  (leaf visual-line-mode
    :tag "builtin"
    :hook (text-mode-hook . visual-line-mode)))

(leaf *frame-transparency
  :when window-system
  :preface
  (defun my/change-transparency (&optional num)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive (list
                  (read-number "Transparency Value 0 - 100 opaque: " 100)))
    (let ((diff-active-and-inactive-frame 4))
      (set-frame-parameter nil 'alpha
                           (cons num (- num diff-active-and-inactive-frame)))
      (add-to-list 'default-frame-alist
                   `(alpha . (,num . ,(- num diff-active-and-inactive-frame))))))

  :config
  (my/change-transparency 91))

(leaf font
  :when window-system
  :leaf-defer nil
  :hook (emacs-startup-hook . (lambda () (my/set-font my-fontsize)))
  :advice (:after load-theme my/set-font-after-load-theme)
  :preface
  (setq-default text-scale-remap-header-line t)
  (setq-default my-fontsize (if (eq 'darwin system-type) 17 13))

  (defun my/set-font-size (fontsize)
    (interactive (list
                  (read-number "Fontsize: " my-fontsize)))
    (set-face-attribute 'default (selected-frame) :height (* fontsize 10))
    (set-face-attribute 'fixed-pitch (selected-frame) :height (* fontsize 10))
    (set-face-attribute 'variable-pitch (selected-frame) :height (* fontsize 10)))

  (defun my/set-font (&optional fontsize)
    (interactive)
    (let ((ascii-font "UDEV Gothic NF")
          (variable-font "Iosevka Aile")
          (japanese-font "UDEV Gothic NF")
          (emoji-font (if (eq 'darwin system-type)
                          "Apple Color Emoji"
                        "Noto Color Emoji")))

      ;; ascii
      (set-face-attribute 'default nil :font ascii-font)

      ;; Set the fixed pitch face
      (set-face-attribute 'fixed-pitch nil :font ascii-font)

      ;; Set the variable pitch face
      (set-face-attribute 'variable-pitch nil :font variable-font)

      ;; japanese
      (set-fontset-font t 'unicode japanese-font nil 'append)

      ;; emoji
      (set-fontset-font t 'unicode emoji-font nil 'prepend))

    ;; set font height
    (when fontsize (my/set-font-size fontsize)))

  (defun my/set-font-weight (weight)
    (interactive
     (list (intern (completing-read "Choose weight:"
                                    '(light normal medium bold)))))
    (set-face-attribute 'default nil :weight weight)
    (set-face-attribute 'fixed-pitch nil :weight weight)
    (set-face-attribute 'variable-pitch nil :weight weight))

  (defun my/set-font-after-load-theme (&rest args)
    (let* ((str-theme (symbol-name (car args)))
           (weight (cond
                    ((string-match "\\(light\\|operandi\\)" str-theme) 'medium)
                    ((and (string-match "bespoke" str-theme)
                          (eq 'light bespoke-set-theme))
                     'medium)
                    (t 'normal))))
      (my/set-font-weight weight))
    (my/set-font-size my-fontsize)))

(leaf themes
  :hook (emacs-startup-hook . my/default-theme)
  :advice (:before load-theme (lambda (&rest args)
                                (mapc #'disable-theme custom-enabled-themes)))
  :preface
  (leaf doom-themes
    :doc "an opinionated pack of modern color-themes"
    :req "emacs-25.1" "cl-lib-0.5"
    :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
    :url "https://github.com/hlissner/emacs-doom-theme"
    :straight t neotree all-the-icons
    :custom ((doom-themes-enable-italic . nil)
             (doom-themes-enable-bold . t))
    :config
    (defun my/load-doom-theme (sym-theme)
      (require 'neotree)
      (require 'all-the-icons)
      (load-theme sym-theme t)
      (doom-themes-neotree-config)
      (doom-themes-org-config)
      (doom-themes-treemacs-config)))
  
  (leaf modus-themes
    :straight t
    :config
    (defun my/load-modus-theme (sym-theme)
      (setq modus-themes-bold-constructs t
            modus-themes-region '(bg-only no-extend)
            modus-themes-org-blocks 'gray-background
            modus-themes-mixed-fonts t
            modus-themes-subtle-line-numbers t
            modus-themes-variable-pitch-headings t
            modus-themes-variable-pitch-ui t
            modus-themes-fringes nil
            modus-themes-prompts '(intense gray)
            modus-themes-completions '((matches . (extrabold))
                                       (selection . (semibold accented))
                                       (popup . (accented intense)))
            modus-themes-paren-match '(underline)
            ;; this is an alist: read the manual or its doc string
            modus-themes-org-agenda '((header-block . (variable-pitch scale-title))
                                      (header-date . (grayscale workaholic bold-today))
                                      (scheduled . uniform)
                                      (habit . traffic-light-deuteranopia))
            modus-themes-headings '((1 . (bold overline variable-pitch  1.5))
                                    (2 . (bold overline variable-pitch 1.3))
                                    (3 . (bold variable-pitch 1.2))
                                    (4 . (bold variable-pitch 1.15))
                                    (t . (bold variable-pitch 1.1))))
      ;; Load the theme files before enabling a theme
      (modus-themes-load-themes)
      ;; Load choiced theme
      (pcase sym-theme
        ('modus-dark (modus-themes-load-vivendi))
        ('modus-light (modus-themes-load-operandi)))
      
      (defvar my-rainbow-region-colors
        (modus-themes-with-colors
          `((red . ,red-subtle-bg)
            (green . ,green-subtle-bg)
            (yellow . ,yellow-subtle-bg)
            (blue . ,blue-subtle-bg)
            (magenta . ,magenta-subtle-bg)
            (cyan . ,cyan-subtle-bg)))
        "Sample list of color values for `my-rainbow-region'.")

      (defun my-rainbow-region (color)
        "Remap buffer-local attribute of `region' using COLOR."
        (interactive
         (list
          (completing-read "Pick a color: " my-rainbow-region-colors)))
        (face-remap-add-relative
         'region
         `( :background ,(alist-get (intern color) my-rainbow-region-colors)
            :foreground ,(face-attribute 'default :foreground))))

      (defun my-rainbow-region-red ()
        (my-rainbow-region "red"))

      (add-hook 'prog-mode-hook #'my-rainbow-region-red)
      (add-hook 'text-mode-hook #'my-rainbow-region-red)

      (defun my--tab-bar-format (tab i)
        (propertize
         (format
          (concat
           (if (eq (car tab) 'current-tab)
               "???? " "")
           "%s")
          (alist-get 'name tab))
         'face (list (append
                      (if (eq (car tab) 'current-tab)
                          '(:inherit modus-themes-tab-active :box t)
                        '(:inherit modus-themes-tab-inactive))))))
      (setq tab-bar-tab-name-format-function #'my--tab-bar-format)

      (leaf *flycheck-set-face
        :straight flycheck-inline
        :custom-face
        ((flycheck-inline-info . '((t (:inherit modus-themes-intense-cyan))))
         (flycheck-inline-warning . '((t (:inherit modus-themes-intense-yellow))))
         (flycheck-inline-error . '((t (:inherit modus-themes-intense-red))))))
      ))

  (leaf bespoke-themes
    :straight (bespoke-themes
               :type git
               :host github
               :repo "mclear-tools/bespoke-themes"
               :branch "main")
    :require t bespoke-theme
    :custom (;; Set initial theme variant
             (bespoke-set-italic-comments . nil)
             (bespoke-set-italic-keywords . nil))
    :config
    (defun my/load-bespoke-theme (sym-theme)
      (funcall sym-theme)
       ;; (set-face-attribute 'org-block nil
       ;;                    :inherit 'fixed-pitch
       ;;                    :foreground nil
       ;;                    :background bespoke-subtle)
      ;; (custom-theme-set-faces
      ;;  `user
      ;;  `(org-agenda-clocking ((t :foreground ,bespoke-salient)))
      ;;  `(org-agenda-done ((t :foreground ,bespoke-faded :strike-through nil))))
      ))

    ;;; utils
  (setq my/theme-list '(doom-nord
                        doom-solarized-light
                        modus-light
                        modus-dark
                        bespoke/dark-theme
                        bespoke/light-theme))

  (defun my/load-theme-func-for (sym-theme)
    (let* ((str-theme (symbol-name sym-theme)))
      (cond
       ((string-match "doom" str-theme) #'my/load-doom-theme)
       ((string-match "modus" str-theme) #'my/load-modus-theme)
       ((string-match "bespoke" str-theme) #'my/load-bespoke-theme)
       (t #'(lambda (arg)
              (message "The theme ``%s'' is not implemented." arg)
              (message "Check the argument of ``my/load-theme''.")
              nil)))))

  (defun my/load-theme (sym-theme)
    (interactive
     (list
      (intern (completing-read "Choose one:" my/theme-list))))
    (setq my-load-theme-func (my/load-theme-func-for sym-theme))
    (funcall my-load-theme-func sym-theme))

  (defun my/default-theme nil
    (let ((time
           (string-to-number
            (format-time-string "%H"))))
      (if (and (> time 5) (< time 18))
          (my/load-theme 'modus-light)
        (my/load-theme 'modus-dark))))

  :config
  (column-number-mode)
  (setq inhibit-compacting-font-caches t))

(leaf *modelines
  :hook (emacs-startup-hook . (lambda nil
                                (line-number-mode 1)
                                (column-number-mode 1)
                                (if window-system
                                    (my/modeline-moody)
                                  (my/modeline-doom))))
  :preface
  (leaf moody
    :when window-system
    :straight t
    :config
    (setq my--modeline-gui-rw-symbol "????"
          my--modeline-tty-rw-symbol "RW"

          my--modeline-gui-ro-symbol "????"
          my--modeline-tty-ro-symbol "RO"
          
          my--modeline-gui-mod-symbol "??????"
          my--modeline-tty-mod-symbol "**")
    
    (defun my--modeline-status ()
      "Return buffer status: default symbols are read-only (????)/(RO),
modified (??????)/(**), or read-write (????)/(RW)"
      (let ((read-only   buffer-read-only)
            (modified    (and buffer-file-name (buffer-modified-p))))
        ;; Use status letters for TTY display
        (cond
         (modified (if (display-graphic-p)
                       my--modeline-gui-mod-symbol
                     my--modeline-tty-mod-symbol))
         (read-only (if (display-graphic-p)
                        my--modeline-gui-ro-symbol
                      my--modeline-tty-ro-symbol))
         (t (if (display-graphic-p)
                my--modeline-gui-rw-symbol
              my--modeline-tty-rw-symbol)))))
    
    (defun my/modeline-moody nil
      (interactive)
      (setq x-underline-at-descent-line t
            moody-mode-line-height 30)
      (moody-replace-mode-line-buffer-identification)
      (moody-replace-vc-mode)
      (moody-replace-eldoc-minibuffer-message-function)
      (moody-replace-element 'mode-line-frame-identification
                             '(:eval (meow-indicator)))
      (moody-replace-element 'mode-line-mule-info '(""))
      (moody-replace-element 'mode-line-client '(""))
      (moody-replace-element 'mode-line-remote '(""))
      (moody-replace-element 'mode-line-modified
                             '(:eval (my--modeline-status)))))

  (leaf bespoke-modeline
    :straight (bespoke-modeline
               :type git
               :host github
               :repo "mclear-tools/bespoke-modeline")
    ;; :custom (;; Set header line
    ;;          (bespoke-modeline-position . 'top)
    ;;          ;; Set mode-line height
    ;;          (bespoke-modeline-size . 3)
    ;;          ;; Show diff lines in mode-line
    ;;          (bespoke-modeline-git-diff-mode-line . t)
    ;;          ;; Set mode-line cleaner
    ;;          (bespoke-modeline-cleaner . t)
    ;;          ;; Use mode-line visual bell
    ;;          (bespoke-modeline-visual-bell . t)
    ;;          ;; Set vc symbol
    ;;          (bespoke-modeline-vc-symbol . "G:"))
    :config
    (defun my/modeline-bespoke-modeline nil
      (interactive)
      ;; Set header line
      (setq bespoke-modeline-position 'top)
      ;; Set mode-line height
      (setq bespoke-modeline-size 3)
      ;; Show diff lines in mode-line
      (setq bespoke-modeline-git-diff-mode-line t)
      ;; Set mode-line cleaner
      (setq bespoke-modeline-cleaner t)
      ;; Use mode-line visual bell
      (setq bespoke-modeline-visual-bell t)
      ;; Set vc symbol
      (setq  bespoke-modeline-vc-symbol "G:")
      (bespoke-modeline-mode)))

  (leaf doom-modeline
    :when (not window-system)
    :doc "A minimal and modern mode-line"
    :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
    :tag "mode-line" "faces" "emacs>=25.1"
    :url "https://github.com/seagle0128/doom-modeline"
    :straight t
    :custom ((doom-modeline-buffer-file-name-style . 'truncate-except-project)
             (doom-modeline-project-detection . 'auto)
             (doom-modeline-icon . t)
             (doom-modeline-major-mode-icon . nil)
             (doom-modeline-minor-modes . nil)
             (doom-modeline-hud . t)
             (doom-modeline-env-version . t)
             (doom-modeline-height . 16)
             (doom-modeline-bar-width . 7)
             (doom-modeline-lsp . t)
             (doom-modeline-github . nil)
             (doom-modeline-persp-name . nil)
             (doom-modeline-buffer-state-icon . t)
             (doom-modeline-env-enable-python . t))
    :config
    (defun my/modeline-doom nil
      (interactive)
      (doom-modeline-mode)))

  (leaf minions
    :straight t
    :hook (emacs-startup-hook . minions-mode)
    :custom ((minions-mode-line-lighter . ";")
             (minions-prominent-modes . '(defining-kbd-macro ;; flymake-mode
                                                             )))))

(leaf meow
  :straight t
  :after consult
  :leaf-defer nil
  :load-path "~/.emacs.d/elisp/meow-config/"
  :config
  (require 'meow-keybindings)
  (meow-setup)
  (meow-global-mode)
  (setq meow-cursor-type-region-cursor '(bar . 3)
        meow-cursor-type-insert '(bar . 3))
  :hook
  ((meow-insert-exit-hook . (lambda nil
                              (if skk-mode (skk-latin-mode-on))))
   (eshell-mode-hook . meow-append)
   (after-change-major-mode-hook . (lambda nil
                                     (if (and (featurep 'magit)
                                              (magit-commit-message-buffer))
                                         (meow-insert)))))
  :bind ((meow-normal-state-keymap
          ("C-j" . (lambda ()
                     (interactive)
                     (if skk-mode (skk-j-mode-on))
                     (meow-append))))))

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :url "https://github.com/justbur/emacs-which-key"
  :straight t
  :blackout t
  :custom ((which-key-idle-delay . 1)
           (which-key-replacement-alist quote
                                        (((nil . "Prefix Command")
                                          nil . "prefix")
                                         ((nil . "\\`\\?\\?\\'")
                                          nil . "lambda")
                                         (("<left>")
                                          "???")
                                         (("<right>")
                                          "???")
                                         (("<\\([[:alnum:]-]+\\)>")
                                          "\\1"))))
  :global-minor-mode t)

(leaf transient
  :doc "Transient commands"
  :req "emacs-25.1"
  :url "https://github.com/magit/transient"
  :straight t
  :custom ((transient-detect-key-conflicts . t))
  :config
  (leaf transient-dwim
    :doc "Useful preset transient commands"
    :req "emacs-26.1" "transient-0.1.0"
    :tag "conao3" "conao3-dev" "out-of-MELPA"
    :url "https://github.com/conao3/transient-dwim.el"
    :straight t
    :bind ("M-=" . transient-dwim-dispatch)))

;; (leaf dired
;;   :straight dired-collapse
;;   :require dired-x
;;   :hook (dired-mode-hook . (lambda ()
;;                              (dired-collapse-mode 1)
;;                              (dired-omit-mode)
;;                              (dired-hide-details-mode 1)))
;;   :bind (dired-mode-map
;;          ("o" . dired-display-file))
;;   :custom ((dired-listing-switches . "-agho --group-directories-first")
;;            (dired-omit-files . "^\\.[^.].*")
;;            (dired-omit-verbose . nil)
;;            (dired-hide-details-hide-symlink-targets . nil)
;;            (delete-by-moving-to-trash . t)
;;            (dired-dwim-target . t)))

(leaf dirvish
  :straight t
  :custom
  ((dired-kill-when-opening-new-dired-buffer . t) ; added in emacs 28
   (dired-clean-confirm-killing-deleted-buffers . nil)
   (dired-recursive-copies . 'always)
   (dired-recursive-deletes . 'always)
   (delete-by-moving-to-trash . t)
   (dired-dwim-target . t)
   dired-listing-switches . "-AGhlv --group-directories-first --time-style=long-iso")
  )

(leaf crux
  :straight t
  :bind ("C-S-k" . crux-top-join-line))

(leaf neotree
  :straight t all-the-icons
  :require all-the-icons
  ;; :bind ("C-c c" . neotree-show)
  :custom ((neo-show-hidden-files . t)
           (neo-smart-open . t)
           (neo-window-fixed-size . nil)
           (neo-confirm-create-file . 'y-or-n-p)
           (neo-confirm-create-directory . 'y-or-n-p)))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :url "https://github.com/magit/magit"
  :straight t
  :bind ("C-c m" . magit-status)
  :custom ((magit-bury-buffer-function quote magit-mode-quit-window)
           (magit-refresh-verbose . t)
           (magit-commit-ask-to-stage quote stage)
           (magit-clone-set-remote\.pushDefault . t)
           (magit-clone-default-directory . "~/src/github.com/")
           (magit-remote-add-set-remote\.pushDefault quote ask)
           (magit-log-margin-show-committer-date . t)
           (magit-log-margin . '(t "%m/%d/%Y %H:%M " magit-log-margin-width t 12)))
  :config
  (leaf emoji-cheat-sheet-plus
    :straight t
    :commands magit-status
    :hook (magit-log-mode-hook . emoji-cheat-sheet-plus-display-mode))
)

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter"
  :req "emacs-24.3"
  :url "https://github.com/emacsorphanage/git-gutter"
  :straight t
  :custom
  ((git-gutter:modified-sign . "~")
   (git-gutter:added-sign . "+")
   (git-gutter:deleted-sign . "-"))
  :custom-face
  ((git-gutter:modified . '((t (:background "#f1fa8c"))))
   (git-gutter:added . '((t (:background "#50fa7b"))))
   (git-gutter:deleted . '((t (:background "#ff79c6"))))))

(leaf dap-mode
  :when window-system
  :straight t
  ;; :after exec-path-from-shell
  :custom (;; (dap-python-debugger . 'debugpy)
           ;; (dap-python-executable . path-to-venv-python)
           (dap-auto-configure-features . '(sessions locals tooltip))
           ;; (lsp-enable-dap-auto-configure . nil)
           )
  :hook
  (python-mode-hook . (lambda nil
                        (require 'dap-mode)
                        (require 'dap-python)
                        (dap-mode)
                        (dap-ui-mode)
                        (dap-tooltip-mode)
                        (add-hook 'dap-stopped-hook
                                  #'(lambda (arg)
                                      (call-interactively #'dap-hydra)))))
  ;; ((dap-stopped-hook . (lambda (arg) (call-interactively #'dap-hydra)))
  ;;  (python-mode-hook . dap-mode)
  ;;  (python-mode-hook . dap-ui-mode)
  ;;  (python-mode-hook . dap-tooltip-mode))
  )

(leaf projectile
  :when (version< emacs-version "28")
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1" "pkg-info-0.4"
  :url "https://github.com/bbatsov/projectile"
  :straight t
  :custom (projectile-enable-caching . t)
  :global-minor-mode t)


(leaf project
  :when (version<= "28" emacs-version)
  :straight t)

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-25.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :url "https://github.com/emacs-lsp/lsp-mode#supported-languages"
  :url "https://github.com/MaskRay/ccls/wiki/lsp-mode#find-definitionsreferences"
  :emacs>= 25.1
  :straight t
  :after orderless cape
  :commands lsp lsp-deferred
  :hook ((lsp-mode-hook . lsp-enable-which-key-integration)
         (lsp-managed-mode-hook . lsp-modeline-diagnostics-mode)
         (lsp-completion-mode-hook . my/lsp-mode-setup-completion))
  :custom `((lsp-keymap-prefix . "C-c l")        
            (read-process-output-max . ,(* 1 1024 1024))  ;; 1MB
            ;; debug
            (lsp-auto-guess-root . nil)
            (lsp-headerline-breadcrumb-enable . t)
            (lsp-log-io . nil)
            (lsp-trace . nil)
            (lsp-print-performance . nil)
            ;; general
            (lsp-idle-delay . 0.5)
            (lsp-document-sync-method . 2)
            (lsp-response-timeout . 5)
            (lsp-prefer-flymake . nil)
            (lsp-completion-enable . t)
            (lsp-enable-indentation . nil)
            (lsp-restart . 'ignore)
            (lsp-completion-provider . :none))
  :preface
  (defun my/orderless-dispatch-flex-first (pattern index _total)
    (and (eq index 0) 'orderless-flex pattern))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  :config
  (dolist (regexp '("[\\d\\D]*\\.dat\\'"
                    "[\\d\\D]*\\.pth\\'"
                    "[\\d\\D]*\\.npy\\'"
                    "[/\\\\]\\output\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories regexp)))


(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :req "emacs-25.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :straight t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :preface
  (defun ladicle/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))

  :bind (lsp-mode-map
         :package lsp-mode
         ("C-c C-r" . lsp-ui-peek-find-references)
         ("C-c C-j" . lsp-ui-peek-find-definitions)
         ("C-c s"   . lsp-ui-sideline-mode)
         ("C-c d"   . ladicle/toggle-lsp-ui-doc)
         ("C-c i"   . lsp-ui-doc-focus-frame))
  :custom (;; lsp-ui-doc
           (lsp-ui-doc-header . t)
           (lsp-ui-doc-delay . 2)
           (lsp-ui-doc-include-signature . t)
           (lsp-ui-doc-alignment . 'window)
           (lsp-ui-doc-max-height . 30)
           (lsp-ui-doc-show-with-mouse . nil)
           (lsp-ui-doc-show-with-cursor . t)
           ;; lsp-ui-sideline
           (lsp-ui-sideline-enable . nil)
           (lsp-ui-sideline-ignore-duplicate . t)
           (lsp-ui-sideline-show-symbol . t)
           (lsp-ui-sideline-show-hover . t)
           (lsp-ui-sideline-show-diagnostics . nil)
           (lsp-ui-sideline-show-code-actions . nil)
           ;; lsp-ui-imenu
           (lsp-ui-imenu-enable . nil)))

(leaf lsp-latex :straight t)

(leaf helpful
  :straight t
  :bind* (("C-c h f" . helpful-function)
          ("C-c h s" . helpful-symbol)
          ("C-c h v" . helpful-variable)
          ("C-c h c" . helpful-command)
          ("C-c h k" . helpful-key)))

(leaf macrostep
  :straight t
  :bind (("C-c e" . macrostep-expand)))

(leaf lua-mode
  :straight t)

(when-let* ((miniconda-path
             (my/trim-newline-from-string
              (shell-command-to-string
               "find $HOME -maxdepth 1 -type d -name 'miniconda*' | head -n 1")))
            (path-to-venv (expand-file-name "envs/venv" miniconda-path)))
  (setq path-to-miniconda miniconda-path)
  (setq path-to-venv-python (expand-file-name "bin/python" path-to-venv))
  (custom-set-variables '(org-babel-python-command path-to-venv-python))
  )

(leaf python-mode
  :doc "Python major mode"
  :url "https://gitlab.com/groups/python-mode-devs"
  :straight t
  :mode "\\.py\\'"
  :custom `((py-keep-windows-configuration . t)
            (python-indent-guess-indent-offset . t)
            (python-indent-guess-indent-offset-verbose . nil)
            (py-python-command . ,(if (executable-find "python3") "python3"
                                "python"))
            (python-shell-virtualenv-root . ,(expand-file-name "envs/venv"
                                                               path-to-miniconda))
            )
  :hook (python-mode-hook . my/python-basic-config)
  :preface
  (defun my/python-basic-config ()
    (setq indent-tabs-mode nil
          python-indent 4
          tab-width 4)))

(leaf conda
  :doc "Work with your conda environments"
  :req "emacs-24.4" "pythonic-0.1.0" "dash-2.13.0" "s-1.11.0" "f-0.18.2"
  :url "http://github.com/necaris/conda.el"
  :when (length> path-to-miniconda 0)
  :straight t
  :commands conda-env-activate
  :custom ((conda-anaconda-home . path-to-miniconda)
           (conda-env-home-directory . path-to-miniconda))
  :config
  (conda-env-initialize-eshell)
  (conda-env-initialize-interactive-shells))

(leaf lsp-pyright
  :doc "Python LSP client using Pyright"
  :req "emacs-26.1" "lsp-mode-7.0" "dash-2.18.0" "ht-2.0"
  :url "https://github.com/emacs-lsp/lsp-pyright"
  :when (length> path-to-miniconda 0)
  :straight t
  :custom
  `((lsp-pyright-venv-path . ,(expand-file-name "envs"
                                                path-to-miniconda)))
  :hook
  ((conda-postactivate-hook . my/lsp-pyright-setup-when-conda)
   (conda-postdeactivate-hook . my/lsp-pyright-setup-when-conda))
  :preface
  (defun my/lsp-pyright-setup-when-conda ()
    (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
    (if (bound-and-true-p lsp-mode)
        (lsp-restart-workspace)
      (require 'lsp-pyright)
      (lsp))))

(leaf python-isort :straight t)

(leaf blacken
  :straight t
  :custom ((blacken-line-length . 91)
           (blacken-skip-string-normalization . t)))

(leaf web-mode
  :straight t
  :custom ((web-mode-markup-indent-offset . 2)
           (web-mode-css-indent-offset . 2)
           (web-mode-code-indent-offset . 2))
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"))

(leaf *javascript
  :hook
  (js-mode-hook . (lambda nil
                    (make-local-variable 'js-indent-level)
                    (setq js-indent-level 2))))

(leaf json-mode
  :straight t
  :custom
  ((json-mode-standard-file-ext . '(".json" ".jsonc" ".jsonld"))))

(leaf sh-mode
  :hook (sh-mode-hook . lsp-deferred))

(leaf flymake
  :disabled t
  :doc "A universal on-the-fly syntax checker"
  :tag "builtin"
  :custom (flymake-gui-warnings-enabled . t)
  :bind (flymake-mode-map
         ("C-c C-n" . flymake-goto-next-error)
         ("C-c C-p" . flymake-goto-prev-error))
  :config
  (leaf flymake-diagnostic-at-point
    :doc "Display flymake diagnostics at point"
    :req "emacs-26.1" "popup-0.5.3"
    :tag "tools" "languages" "convenience" "emacs>=26.1"
    :url "https://github.com/meqif/flymake-diagnostic-at-point"
    :straight t
    :after flymake
    :custom ((flymake-diagnostic-at-point-timer-delay . 0.8)
             (flymake-diagnostic-at-point-error-prefix . " ??? ")
             (flymake-diagnostic-at-point-display-diagnostic-function
              quote flymake-diagnostic-at-point-display-minibuffer))
    :hook (flymake-mode-hook . flymake-diagnostic-at-point-mode))

  
  (leaf flymake-posframe
    :straight (flymake-posframe
               :type git
               :host github
               :repo "ladicle/flymake-posframe")
    :hook (flymake-mode-hook . flymake-posframe-mode)
    :custom ((flymake-posframe-warning-prefix . "?????? ")
             (flymake-posframe-error-prefix . "???? ")
             (flymake-posframe-note-prefix . "?????? ")))
  )

(leaf flyspell
  ;; :hook (LaTeX-mode-hook org-mode-hook markdown-mode-hook text-mode-hook)
  :config
  (leaf ispell
    :doc "interface to spell checkers"
    :tag "builtin"
    :custom ((ispell-program-name . "aspell")
             (ispell-local-dictionary . "en_US"))
    :hook (emacs-startup-hook . (lambda ()
                                  ;; for text mixed English and Japanese
                                  (add-to-list 'ispell-skip-region-alist
                                               '("[^\000-\377]+"))))))

(leaf flycheck
  :straight t
  :bind (("C-c C-n" . flycheck-next-error)
         ("C-c C-p" . flycheck-previous-error)
         (:python-mode-map
          :package python-mode
          ("C-c C-n" . flycheck-next-error)
          ("C-c C-p" . flycheck-previous-error)))
  :hook (python-mode-hook . flycheck-mode)
  :custom (flycheck-display-errors-delay . 0.3)
  :config
  (leaf flycheck-inline
    :straight t
    :hook (flycheck-mode-hook . flycheck-inline-mode)
    :advice
    (:override flycheck-inline--error-face my--flycheck-inline--error-face)
    (:override flycheck-inline--error-message my--flycheck-inline--error-message)
    :preface
    (defun my--flycheck-inline--error-face (err)
      "Return the face used to display ERR."
      (pcase (flycheck-error-level err)
        (`info 'flycheck-inline-info)
        (`warning 'flycheck-inline-warning)
        (`error 'flycheck-inline-error)
        (_ 'flycheck-inline-info)))

    (defun my--flycheck-inline--error-message (err)
      "Return the message to display for ERR."
      (let ((filename (flycheck-error-filename err))
            (id (flycheck-error-id err))
            (prefix (pcase (flycheck-error-level err)
                      (`info "?????? ")
                      (`warning "?????? ")
                      (`error "???? ")
                      (_ "?????? "))))
        (concat
         prefix
         (when (and filename (not (equal filename (buffer-file-name))))
           (format "In \"%s\":\n" (file-relative-name filename default-directory)))
         (flycheck-error-message err)
         (when (and id flycheck-inline-display-error-id)
           (format " [%s]" id))))))
  
  (leaf *flycheck-gui
    :disabled t
    :when window-system
    :config
    ;; checker for textlint
    (flycheck-define-checker textlint
      "A linter for text."
      :command ("~/.config/textlint/textlint.sh" source)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": "
                (id (one-or-more (not (any " "))))
                (message (one-or-more not-newline)
                         (zero-or-more "
" (any " ") (one-or-more not-newline)))
                line-end))
      :modes (latex-mode latex-extra-mode))
    
    (add-hook 'latex-extra-mode-hook #'(lambda nil
                                         (setq flycheck-checker 'textlint)
                                         (flycheck-mode 1))))

  (leaf *flycheck-tty
    :when (not window-system)
    :custom (flycheck-indication-mode . 'left-margin)
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)))

(leaf highlight-indent-guides
  :blackout
  :doc "Minor mode to highlight indentation"
  :req "emacs-24.1"
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :straight t
  :hook prog-mode-hook
  :custom
  ((highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-responsive . t)
   (highlight-indent-guides-method . 'character)
   (highlight-indent-guides-suppress-auto-error . t)))

(leaf *indent-region-custom
  :doc "This should be used in GUI Emacs to avoid inserting weired characters in CUI Emacs."
  :when window-system
  :preface
  (defun indent-region-custom(numSpaces)
    (progn
      ;; default to start and end of current line
      (setq regionStart (line-beginning-position))
      (setq regionEnd (line-end-position))
      ;; if there's a selection, use that instead of the current line
      (when (use-region-p)
        (setq regionStart (region-beginning))
        (setq regionEnd (region-end))
        )

      (save-excursion ; restore the position afterwards
        (goto-char regionStart) ; go to the start of region
        (setq start (line-beginning-position)) ; save the start of the line
        (goto-char regionEnd) ; go to the end of region
        (setq end (line-end-position)) ; save the end of the line

        (indent-rigidly start end numSpaces) ; indent between start and end
        (setq deactivate-mark nil) ; restore the selected region
        )))
  :config
  (leaf *untab-region
    :bind (("C-M-[" . untab-region))
    :preface
    (defun untab-region nil
      (interactive)
      (indent-region-custom -4)))

  (leaf *tab-region
    :bind ("C-M-]" . tab-region)
    :preface
    (defun tab-region nil
      (interactive)
      (if (active-minibuffer-window)
          (minibuffer-complete)    ; tab is pressed in minibuffer window -> do completion
        (if (use-region-p)    ; tab is pressed is any other buffer -> execute with space insertion
            (indent-region-custom 4) ; region was selected, call indent-region-custom
          (insert "    ") ; else insert four spaces as expected
          ))
      )))

(leaf paren
  :hook (emacs-startup-hook . show-paren-mode)
  :custom-face
  (show-paren-match . '((nil
                         (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  ((show-paren-style . 'mixed)
   (show-paren-when-point-inside-paren . t)
   (show-paren-when-point-in-periphery . t)))

(leaf smartparens
  :straight t
  :init (require 'smartparens-config)
  :hook ((prog-mode-hook . turn-on-smartparens-mode)
         ((python-mode-hook
           emacs-lisp-mode
           lisp-interaction-mode
           LaTeX-mode-hook) . turn-on-smartparens-strict-mode))
  :bind (smartparens-mode-map
         ("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)

         ("C-S-f" . sp-forward-symbol)
         ("C-S-b" . sp-backward-symbol)

         ("C-<right>" . sp-forward-slurp-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("M-<left>" . sp-backward-slurp-sexp)
         ("M-<right>" . sp-backward-barf-sexp)

         ("C-M-k" . sp-kill-sexp)
         ("C-k" . sp-kill-hybrid-sexp)
         ("M-k" . sp-backward-kill-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("C-M-d" . sp-delete-region)

         ("M-<backspace>" . backward-kill-word)
         ;; ([remap sp-backward-kill-word] . backward-kill-ward)

         ;; ("M-s" . sp-unwrap-sexp)
         ("M-s" . sp-splice-sexp) ;; depth-changing commands
         ("M-<up>" . sp-splice-sexp-killing-backward)
         ("M-<down>" . sp-splice-sexp-killing-forward)
         ("M-r" . sp-splice-sexp-killing-around)

         ("C-c (" . wrap-with-parens)
         ("C-c [" . wrap-with-brackets)
         ("C-c {" . wrap-with-braces)
         ("C-c '" . wrap-with-single-quotes)
         ("C-c \"" . wrap-with-double-quotes)
         ("C-c _" . wrap-with-underscores)
         ("C-c `" . wrap-with-back-quotes)
         )
  :preface
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(cl-loop for (key . val) in pairs
                  collect
                  `(defun ,(read (concat
                                  "wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`"))))

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf rainbow-mode
  :doc "Colorize color names in buffers"
  :tag "faces"
  :url "http://elpa.gnu.org/packages/rainbow-mode.html"
  :straight t
  :blackout t
  :commands rainbow-mode
  :custom ((rainbow-html-colors-major-mode-list . '(css-mode
                                                    html-mode
                                                    php-mode
                                                    nxml-mode
                                                    xml-mode))
           (rainbow-x-colors-major-mode-list . '(emacs-lisp-mode
                                                 lisp-interaction-mode
                                                 c-mode
                                                 c++-mode
                                                 java-mode))
           (rainbow-latex-colors-major-mode-list . '(latex-mode))
           (rainbow-ansi-colors-major-mode-list . '(sh-mode c-mode c++-mode))
           (rainbow-r-colors-major-mode-list . '(ess-mode)))
  :hook (prog-mode-hook))

(leaf volatile-highlights
  :doc "Minor mode for visual feedback on some operations."
  :url "http://www.emacswiki.org/emacs/download/volatile-highlights.el"
  :straight t
  :blackout
  :hook emacs-startup-hook
  :custom-face
  (vhl/default-face quote
                    ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

(leaf lin
  :straight t
  :custom ((lin-mode-hooks . '(text-mode-hook prog-mode-hook notmuch-search-mode-hook))
           (lin-face . 'lin-cyan))
  :config
  (global-hl-line-mode)
  (lin-global-mode 1))

(leaf yasnippet
  :straight t
  :hook (emacs-startup-hook . yas-global-mode)
  :custom ((yas-indent-line . 'fixed)
           (yas-snippet-dirs . `(,(file-truename "~/.emacs.d/snippets/"))))
  :bind ((yas-keymap
          ("<tab>" . nil))  ;; conflict with company/coruf
         (yas-minor-mode-map
          ("C-c y i" . yas-insert-snippet)
          ("C-c y n" . yas-new-snippet)
          ("C-c y v" . yas-visit-snippet-file)
          ("C-c y l" . yas-describe-tables)
          ("C-c y g" . yas-reload-all))))

(leaf google-translate
  :straight t popup
  :bind ("C-c t" . google-translate-smooth-translate)
  :custom
  (google-translate-translation-directions-alist . '(("en" . "ja")
                                                     ("ja" . "en")))
  :advice (:override google-translate--search-tkk
                     my--google-translate--search-tkk-override-advice)
  :preface
  (defun my--google-translate--search-tkk-override-advice ()
    "Search TKK." (list 430675 2721866130)))

(leaf wgrep
  :straight t
  :bind (grep-mode-map
         ("e" . wgrep-change-to-wgrep-mode)))

(leaf skk
  :straight ddskk
  :hook
  ((text-mode-hook occur-edit-mode-hook) . (lambda nil
                                             (skk-mode)
                                             (skk-latin-mode-on)))
  :custom ((default-input-method . "japanese-skk")
           (skk-jisyo-code . 'utf-8)
           (skk-large-jisyo . nil)
           ;; (skk-jisyo . "~/.skk-jisyo")
           (skk-backup-jisyo . "~/.skk-jisyo.BAK")
           (skk-save-jisyo-instantly . t)
           (skk-share-private-jisyo . t)
           (skk-server-host . "localhost")
           (skk-server-portnum . 1178)
           (skk-server-report-response . nil)
           (skk-byte-compile-init-file . t)
           (skk-preload . nil)
           (skk-isearch-mode-enable . 'always)
           (skk-kutouten-type . 'jp)
           (skk-use-auto-kutouten . t)
           (skk-show-inline . 'vertical)
           (skk-inline-show-face . nil)
           (skk-egg-like-newline . t)  ;; skk-kakutei by RET
           (skk-auto-okuri-process . nil)
           (skk-henkan-strict-okuri-precedence . t)
           (skk-auto-insert-paren . t)
           (skk-use-auto-enclose-pair-of-region . t)
           (skk-sticky-key . ";")
           (skk-dcomp-activate . t)
           (skk-dcomp-multiple-activate . t)
           (skk-status-indicator . 'minor-mode)
           (skk-inline-show-face . '( :foreground "#ECEFF4"
                                      :background "#4C566A"
                                      :inherit 'normal))))

(leaf whitespace
  :straight t
  :commands whitespace-mode
  :custom ((whitespace-style . '(face
                                trailing
                                tabs
                                spaces
                                empty
                                space-mark
                                tab-mark))
           (whitespace-display-mappings . '((space-mark ?\u3000 [?\u25a1])
                                            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
           (whitespace-space-regexp . "\\(\u3000+\\)")
           (whitespace-global-modes . '(emacs-lisp-mode shell-script-mode sh-mode python-mode org-mode))
           (global-whitespace-mode . t)
           )
)

(leaf winner
  :doc "Restore old window configurations"
  :tag "builtin"
  :bind (("C-x <right>" . winner-redo)
         ("C-x <left>" . winner-undo)
         ("C-x w r")
         ("C-x w u"))
  :hook (emacs-startup-hook . winner-mode))

(leaf ace-window
  :doc "Quickly switch windows."
  :req "avy-0.5.0"
  :tag "location" "window"
  :url "https://github.com/abo-abo/ace-window"
  :straight t
  :bind* ("C-t" . ace-window)
  :custom (aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  ((aw-leading-char-face . '((t (:height 4.0 :foreground "#f1fa8c")))))
  :config
  (defun my--switch-window (&optional num)
    (interactive "P")
    (unless (integerp num)
      (let ((key (event-basic-type last-command-event)))
        (setq num (- key ?0))))

    (let* ((wnd-list (aw-window-list))
           (wnd-num (1- (min num (length wnd-list))))
           (wnd (nth wnd-num wnd-list)))
      (aw-switch-to-window wnd)))

  (dotimes (i 9)
    (global-set-key (vector (append '(super) (list (+ i 1 ?0))))
                    #'my--switch-window)))

(defun my--switch-window (num)
  (let* ((wnd-list (aw-window-list))
         (wnd-num (- (min num (length wnd-list)) 1))
         (wnd (nth wnd-num wnd-list)))
    (aw-switch-to-window wnd)))

(eval
 `(progn
    ,@(mapcar
       (lambda (elm)
         `(global-set-key
           (kbd ,(format "s-%s" elm))
           (lambda nil (interactive) (my--switch-window ,elm))))
       (number-sequence 1 9))))

(leaf *windmove
  :bind (("s-h" . windmove-left)
         ("s-j" . windmove-down)
         ("s-k" . windmove-up)
         ("s-l" . windmove-right)))

(leaf my-window-resizer
  :doc "Control window size and position."
  :bind (("C-x w s" . my-window-resizer)
         ("C-x w m" . maximize-window)
         ("C-x w b" . balance-windows)
         ("C-x w d" . delete-frame)
         ("C-x w t" . tear-off-window))
  :preface
  (defun my-window-resizer()
    "Control window size and position."
    (interactive)
    (let ((window-obj (selected-window))
          (current-width (window-width))
          (current-height (window-height))
          (dx (if (= (nth 0 (window-edges)) 0) 1
                -1))
          (dy (if (= (nth 1 (window-edges)) 0) 1
                -1))
          action c)
      (catch 'end-flag
        (while t
          (setq action
                (read-key-sequence-vector (format "size[%dx%d]"
                                                  (window-width)
                                                  (window-height))))
          (setq c (aref action 0))
          (cond ((= c ?l)
                 (enlarge-window-horizontally dx))
                ((= c ?h)
                 (shrink-window-horizontally dx))
                ((= c ?j)
                 (enlarge-window dy))
                ((= c ?k)
                 (shrink-window dy))
                ;; otherwise
                (t
                 (let ((last-command-char (aref action 0))
                       (command (key-binding action)))
                   (when command
                     (call-interactively command)))
                 (message "Quit")
                 (throw 'end-flag t))))))))

(defun split-and-follow-horizontally ()
  "Split and follow horizontally."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "Split and follow vertically."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(leaf tab-bar
  :doc "frame-local tabs with named persistent window configurations"
  :tag "builtin"
  :bind (("C-x t n" . tab-new)
         ("C-x t r" . tab-bar-rename-tab)
         ("s-]" . tab-bar-switch-to-next-tab)
         ("s-[" . tab-bar-switch-to-prev-tab))
  :custom ((tab-bar-show . 1)
           (tab-bar-new-button-show . nil)
           (tab-bar-close-button-show . nil)
           (tab-bar-select-tab-modifiers . '(meta)))

  :hook (emacs-startup-hook . my--init-tab-bar)
  :preface
  (defun my/tab-new-with-name (&optional name)
    (interactive "sName: ")
    (tab-new)
    (if name (tab-bar-rename-tab name)))

  (defun my--init-tab-bar nil
    (tab-bar-mode 1)
    (tab-bar-rename-tab "Work")
    
    (my/tab-new-with-name "Env")
    (my/tab-new-with-name "Mail")
    (my/tab-new-with-name "Any")))

(leaf rotate
  :doc "Rotate the layout of emacs"
  :url "https://github.com/daichirata/emacs-rotate"
  :straight t
  :bind (("C-x w l"))
  :chord (("rl" . rotate-layout)
          ("rw" . rotate-window)))

(leaf visual-fill-column
  :straight t
  :custom ((visual-fill-column-width . 88)
           (visual-fill-column-center-text . t))
  :hook (org-mode-hook . visual-fill-column-mode))

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

;; (setq split-height-threshold nil)
;; (setq split-width-threshold nil)

(leaf duplicate-thing
  :doc "Duplicate current line & selection"
  :tag "selection" "line" "duplicate" "command" "convenience"
  :url "https://github.com/ongaeshi/duplicate-thing"
  :straight t
  :bind ("M-c" . duplicate-thing))

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs."
  :req "cl-lib-0.5"
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M-SPC" . mc/mark-all-dwim-or-mark-sexp))
  :preface
  (defun mc/edit-lines-or-string-rectangle (s e)
    "C-x r t????????????????????????mc/edit-lines (C-u M-x mc/mark-all-dwim)"
    (interactive "r")
    (if (eq (save-excursion (goto-char s) (current-column))
            (save-excursion (goto-char e) (current-column)))
        (call-interactively 'mc/edit-lines)
      (call-interactively 'string-rectangle)))

  (defun mc/mark-all-dwim-or-mark-sexp (arg)
    "C-u C-M-SPC???mc/mark-all-dwim, C-u C-u C-M-SPC???C-u M-x mc/mark-all-dwim"
    (interactive "p")
    (cl-case arg
      (16 (mc/mark-all-dwim t))
      (4 (mc/mark-all-dwim nil))
      (1 (mark-sexp 1)))))

(leaf affe
  :straight t
  :after orderless
  :bind (("C-c C-g" . affe-grep)
         ("C-c C-f" . affe-find))
  :custom
  ;; Use Orderless
  ((affe-find-command . "fd --color=never --full-path --no-ignore --hidden --exclude \".git\"")
   (affe-grep-command . "rg --null --color=never --max-columns=1000 --no-heading --no-ignore --hidden --smart-case --line-number -v ^$ --glob=\"!.git\" ."))
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(leaf embark
  :straight t embark-consult
  :bind (("C-," . embark-act)
         ("C-;" . embark-dwim)
         ("C-. b" . embark-bindings)
         (:minibuffer-local-map
          ("C-c C-e" . embark-export)))
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode)
  :init (require 'embark-consult)
  :config
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(leaf consult
  :straight t
  :require t
  :chord ("gl" . consult-goto-line)
  :hook
  ((shell-mode-hook eshell-mode-hook) . (lambda ()
                                          (setq completion-in-region-function
                                                #'consult-completion-in-region)))
  :bind* ("C-x C-b b" . consult-bookmark)
  :bind
  (([remap switch-to-buffer] . consult-buffer) ; C-x b
   ([remap yank-pop] . consult-yank-pop)       ; M-y
   ([remap goto-line] . consult-goto-line)     ; M-g g
   ([remap repeat-complex-command] . consult-complex-command) ; C-x M-: or C-x Esc Esc
   ([remap org-open-at-point] . consult-outline)
   ("C-s" . my-consult-line)
   ("C-M-r" . consult-recent-file)
   ("C-x C-o" . consult-file-externally)
   ("C-S-s" . consult-imenu)
   ;; ("C-c C-g" . consult-grep)
   ("C-c C-j" . consult-mark))
  :preface
  (defun my-consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line)))
  :advice (;; Optionally tweak the register preview window.
           ;; This adds thin lines, sorting and hides the mode line of the window.
           (:override register-preview consult-register-window)
           ;; Optionally replace `completing-read-multiple' with an enhanced version.
           (:override completing-read-multiple consult-completing-read-multiple))
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.4 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-.")
   ;; :preview-key (list :debounce 0.7 (kbd "M-."))
   )

  (leaf consult-dir
    :after consult
    :straight t
    :bind (("C-c d" . consult-dir)
           (:vertico-map
            ("C-c d" . consult-dir)
            ("C-x j" . consult-dir-jump-file))))

  (leaf consult-ghq
    :after consult
    :straight t
    :bind (("C-s-f" . consult-ghq-find)
           ("C-s-g" . consult-ghq-grep)))

  (leaf consult-lsp
    :after lsp-mode
    :straight t
    :bind (lsp-mode-map
           ([remap xref-find-apropos] . consult-lsp-symbols)))

  (leaf consult-tramp
    :load-path "~/.emacs.d/elisp/consult-tramp/"
    :custom ((tramp-default-method . "ssh"))
    :commands consult-tramp
    :config
    (tramp-set-completion-function "ssh"
                                   '((tramp-parse-sconfig "~/.ssh/config")))))

(leaf consult-ls-git
  :straight t)

(if (not (executable-find "cmigemo"))
    (leaf orderless
      :straight t
      :require t
      :custom
      '((completion-styles . '(orderless))
        (completion-category-defaults . nil)
        (completion-category-overrides . ((file (styles '(orderless-prefixes
                                                          orderless-initialism
                                                          orderless-regexp)))))))

  (leaf orderless
    :straight t migemo
    :require t migemo
    :leaf-defer nil
    :bind (:minibuffer-local-completion-map
           ("SPC" . nil)
           ("?" . nil))
    :custom
    '((completion-styles . '(basic substring initials flex orderless))
      (completion-cycle-threshold . 2)
      (completion-flex-nospace . nil)
      (completion-category-defaults . nil)
      (completion-category-overrides
       quote ((file (styles orderless-migemo-style))
              ;; for consult-line
              (citar-reference (styles orderless-migemo-style))
              (command (styles orderless-default-style))
              (consult-location (styles orderless-migemo-style))
              (consult-multi (styles orderless-default-style))
              (imenu (styles orderless-migemo-style))
              (org-roam-node (styles orderless-migemo-style))
              (unicode-name (styles orderless-migemo-style))
              )))
    :config
    (setq my--orderless-default-styles
          '(orderless-prefixes
            orderless-initialism
            orderless-regexp))

    (defun my--orderless-literal-dispatcher (pattern _index _total)
      "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
      (when (string-suffix-p "=" pattern)
        `(orderless-literal . ,(substring pattern 0 -1))))

    (defun my--orderless-initialism-dispatcher (pattern _index _total)
      "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
      (when (string-suffix-p "," pattern)
        `(orderless-initialism . ,(substring pattern 0 -1))))

    (defun my--orderless-flex-dispatcher (pattern _index _total)
      "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
      (when (string-suffix-p "~" pattern)
        `(orderless-flex . ,(substring pattern 0 -1))))

    (setq orderless-matching-styles my--orderless-default-styles)
    (setq orderless-style-dispatchers '(my--orderless-literal-dispatcher
                                        my--orderless-initialism-dispatcher
                                        my--orderless-flex-dispatcher))

    
    (defun orderless-migemo (component)
      (let ((pattern (migemo-get-pattern component)))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
    
    (orderless-define-completion-style
     orderless-default-style
     (orderless-matching-styles '(orderless-literal
                                  orderless-prefixes
                                  orderless-initialism
                                  orderless-regexp)))

    (orderless-define-completion-style
     orderless-migemo-style
     (orderless-matching-styles '(orderless-literal
                                  orderless-prefixes
                                  orderless-initialism
                                  orderless-regexp
                                  orderless-migemo)))))

(leaf migemo
  :when (executable-find "cmigemo")
  :straight t
  :hook (emacs-startup-hook . migemo-init)
  :custom
  `((migemo-user-dictionary  . nil)
    (migemo-regex-dictionary . nil)
    (migemo-coding-system    . 'utf-8)
    (migemo-dictionary . ,(cond
                           ((file-exists-p "/usr/local/share/migemo/utf-8/migemo-dict")
                            "/usr/local/share/migemo/utf-8/migemo-dict")
                           ((file-exists-p "/opt/homebrew/opt/cmigemo/share/migemo/utf-8/migemo-dict")
                            "/opt/homebrew/opt/cmigemo/share/migemo/utf-8/migemo-dict")))
    (migemo-isearch-enable-p . t)))

(leaf marginalia
  :straight t
  :require t
  :after vertico
  :global-minor-mode t)

(leaf vertico
  :straight (vertico
             :files (:defaults "extensions/*")
             :includes (vertico-buffer
                        vertico-directory
                        vertico-multiform))
  :custom ((vertico-count . 10)
           (vertico-cycle . t))
  :hook (emacs-startup-hook . (lambda ()
                                (vertico-mode)
                                (savehist-mode))))

(leaf vertico-directory
  :after vertico
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :bind (:vertico-map
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("C-w"   . vertico-directory-delete-word)
         ("RET"   . vertico-directory-enter)))

(leaf corfu
  :straight t
  :global-minor-mode global-corfu-mode
  :custom
  ((corfu-excluded-modes . '(shell-mode eshell-mode))
   (corfu-auto . t)
   (corfu-auto-delay . 0.2)
   (corfu-auto-prefix . 2)
   (corfu-preselect-first . nil)

   ;; Enable indentation+completion using the TAB key.
   ;; `completion-at-point' is often bound to M-TAB.
   (tab-always-indent . 'complete)
   (tab-first-completion . 'word))
  :hook ((java-mode-hook . my/corfu-remap-tab-command)
         (minibuffer-setup-hook . my/corfu-enable-in-minibuffer))

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:corfu-map
         ("<tab>" . corfu-complete))
  :init
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))

  (defun my/corfu-remap-tab-command ()
    (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command))

  ;; https://github.com/minad/corfu/wiki#auto-commit
  (defun my/corfu-commit-predicate ()
    "Auto-commit candidates if:
    1. A `.' is typed, except after a SPACE.
    2. A selection was made, aside from entering SPACE.
    3. Just one candidate exists, and we continue to non-symbol info.
    4. The 1st match is exact."
    (cond
     ((seq-contains-p (this-command-keys-vector) ?.)
      (or (string-empty-p (car corfu--input))
          (not (string= (substring (car corfu--input) -1) " "))))

     ((/= corfu--index corfu--preselect) ; a selection was made
      (not (seq-contains-p (this-command-keys-vector) ? )))

     ((eq corfu--total 1) ;just one candidate
      (seq-intersection (this-command-keys-vector) [?: ?, ?\) ?\] ?\( ? ]))

     ((and corfu--input ; exact 1st match
           (string-equal (substring (car corfu--input) corfu--base)
                         (car corfu--candidates)))
      (seq-intersection (this-command-keys-vector) [?: ?. ?, ?\) ?\] ?\" ?' ? ]))))
  (setq corfu-commit-predicate #'my/corfu-commit-predicate))

(leaf cape
  :straight t
  :require t dabbrev
  :custom (cape-dabbrev-min-length . 2)
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line))
  :hook ((prog-mode-hook . my--set-basic-capf)
         (text-mode-hook . my--set-basic-capf)
         (lsp-completion-mode-hook . my--set-lsp-capf))
  :init
  (defun my--convert-super-capf (arg-capf)
    (list (cape-capf-buster
           (cape-super-capf arg-capf #'cape-dabbrev))
          #'cape-file))

  (defun my--set-basic-capf ()
    (setq-local completion-at-point-functions
                (my--convert-super-capf (car completion-at-point-functions))))

  (defun my--set-lsp-capf ()
    (setq-local completion-at-point-functions
                (my--convert-super-capf #'lsp-completion-at-point)))

  :config
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t))


;; Dabbrev works with Corfu
(leaf dabbrev
  :doc """Cited from Sec. 3.1.8.2 at https://protesilaos.com/dotemacs/#h:675ebef4-d74d-41af-808d-f9579c2a5ec4

    ```
    Whereas dabbrev-completion benefits from minibuffer interactivity and the pattern matching styles in effect (Completion framework and extras). With the help of Corfu, the completion candidates are displayed in a pop-up window near point (Corfu for in-buffer completion).

    The dabbrev-abbrev-char-regexp is configured to match both regular words and symbols (e.g. words separated by hyphens). This makes it equally suitable for code and ordinary language.

    While the dabbrev-abbrev-skip-leading-regexp is instructed to also expand words and symbols that start with any of these: $, *, /, =, ~, '. This regexp may be expanded in the future, but the idea is to be able to perform completion in contexts where the known word/symbol is preceded by a special character. For example, in the org-mode version of this document, all inline code must be placed between the equals sign. So now typing the =, then a letter, will still allow me to expand text based on that input.w
    ```
  """
  :leaf-defer nil
  :require t
  :custom ((dabbrev-abbrev-char-regexp . "\\sw\\|\\s_")
           (dabbrev-abbrev-skip-leading-regexp . "[$*/=~']")
           (dabbrev-backward-only . nil)
           (dabbrev-case-distinction . 'case-replace)
           (dabbrev-case-fold-search . nil)
           (dabbrev-case-replace . 'case-replace)
           (dabbrev-check-other-buffers . t)
           (dabbrev-eliminate-newlines . t)
           (dabbrev-upcase-means-case-search . t))
  :bind* (("M-/" . dabbrev-expand)
          ("C-M-/" . dabbrev-completion)))

(leaf kind-icon
  :straight t
  :after corfu
  :custom
  ;; to compute blended backgrounds correctly
  (kind-icon-default-face . 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf citar
  :straight t
  :bind (("C-c b" . citar-insert-citation)
         (:minibuffer-local-map
          ("M-b" . citar-insert-preset)))
  :custom
  ((citar-bibliography . '("~/org/braindump/preferences/ref.bib"))
   (citar-latex-cite-commands . '((("cite" "citet" "citep") . (t))))))

(leaf avy
  :doc "Jump to arbitrary positions in visible text and select text quickly."
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "location" "point" "emacs>=24.1"
  :url "https://github.com/abo-abo/avy"
  :straight t
  :bind* ("C-q" . avy-goto-char-timer)
  ;; :init (add-to-list 'avy-styles-alist '(avy-goto-char-timer . pre))
  ;; :init (add-to-list 'avy-styles-alist '(avy-goto-migemo-timer . pre))
  :custom ((avy-styles-alist . '(avy-goto-char-timer . pre))
           (avy-timeout-seconds . 0.5)
           (avy-keys . '( ?q ?e ?r ?u ?o ?p
                          ?a ?s ?d ?f ?g ?h ?j ?l ?'
                          ?c ?v ?b ?n ?, ?/)))
  ;; :preface
  ;; (defun avy-goto-migemo-timer (&optional arg)
  ;;   (interactive "P")
  ;;   (let ((avy-all-windows (if arg
  ;;                              (not avy-all-windows)
  ;;                            avy-all-windows)))
  ;;     (avy-with avy-goto-migemo-timer
  ;;       (setq avy--old-cands (avy--read-candidates #'migemo-get-pattern))
  ;;       (avy-process avy--old-cands))))
  :config
  ;; orverride avy function
  (defun avy-show-dispatch-help ()
    "Display action shortucts in echo area."
    (let* ((len (length "avy-action-"))
           (fw (frame-width))
           (raw-strings (mapcar
                         (lambda (x)
                           (format "%2s: %-19s"
                                   (propertize
                                    (char-to-string (car x))
                                    'face 'aw-key-face)
                                   (substring (symbol-name (cdr x)) len)))
                         avy-dispatch-alist))
           (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
           (strings-len (length raw-strings))
           (per-row (floor fw max-len))
           display-strings)
      (cl-loop for string in raw-strings
               for N from 1 to strings-len do
               (push (concat string " ") display-strings)
               (when (= (mod N per-row) 0) (push "\n" display-strings)))
      (message "%s" (apply #'concat (nreverse display-strings)))))

  ;; Kill text
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  ;; Copy text
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)

  ;; Yank text
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  ;; Transpose/Move text
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  ;; Mark text
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  ;; Flyspell words
  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  ;; Bind to semicolon (flyspell uses C-;)
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)


  ;; Get Elisp Help
  ;; Replace with your package manager or help library of choice
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

  ;; Embark
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t))

(leaf mwim
  :doc "Switch between the beginning/end of line or code"
  :tag "convenience"
  :url "https://github.com/alezost/mwim.el"
  :straight t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(leaf eshell
  :bind* ("C-x m" . eshell)
  :config
  (leaf em-alias
    :defer-config
    (eshell/alias "dua" "du -shc * | sort -h")
    (eshell/alias "ll" "ls -l")
    (eshell/alias "bd" "cd ../")
    (eshell/alias "e" "emacsclient $1"))
  
  (leaf eshell-p10k
    :disabled t
    :straight (eshell-p10k
               :type git
               :host github
               :repo "elken/eshell-p10k")
    :config
    (eshell-p10k-def-segment time
                             "???"
                             (format-time-string "%H:%M" (current-time))
                             'eshell-p10k-distro-face)
    (defun eshell-p10k-prompt-function ()
      "Prompt defining function."
      (eshell-p10k-def-prompt '(distro dir git prompt-num time)))

    (setq eshell-prompt-function #'eshell-p10k-prompt-function
          eshell-prompt-regexp eshell-p10k-prompt-string)))

(leaf fish-mode
  :doc "Major mode for fish shell scripts"
  :req "emacs-24"
  :tag "shell" "fish" "emacs>=24"
  :straight t)

(leaf nix-mode
  :straight t
  :mode "\\.nix\\'")

(leaf vterm
  :straight t
  :custom (vterm-max-scrollback . 10000)
  :config
  (leaf vterm-toggle
    :straight t
    :bind (("C-M-'" . vterm-toggle)
           (vterm-mode-map
            ("C-<return>" . vterm-toggle-insert-cd)))
    :custom ((vterm-toggle-reset-window-configration-after-exit . nil)
             (vterm-toggle-hide-method . 'reset-window-configration)
             )
    ))

(leaf gcmh
  :straight t
  :blackout
  :custom (gcmh-verbose . nil)
  :hook after-init-hook)

(leaf org
  :when window-system
  :doc "Export Framework for Org Mode"
  :tag "builtin"
  :mode "\\.org\\'"
  :custom
  ((org-directory . "~/org/")
   (org-ellipsis . " ??? ")

   (org-hide-emphasis-markers . t)
   (org-src-window-setup . 'current-window)
   (org-src-fontify-natively . t)
   (org-fontify-quote-and-verse-blocks . t)
   (org-hide-block-startup . nil)
   (org-startup-folded . 'content)
   
   ;; (org-adapt-indentation . t)
   ;; (org-indent-indentation-per-level . 2)
   (org-edit-src-content-indentation . 0)
   (org-startup-indented . t)

   (org-use-speed-commands . t)
   (org-enforce-todo-dependencies . t)
   (org-log-done . t)
   (org-return-follows-link . t)
   (org-highlight-latex-and-related . '(latex script entities))
   (org-confirm-babel-evaluate . nil)
   (org-catch-invisible-edits . 'show)
   (org-preview-latex-image-directory . "~/tmp/ltximg/")
   (search-whitespace-regexp . ".*?")
   (isearch-lazy-count . t)
   (lazy-count-prefix-format . " (%s/%s) ")
   (isearch-yank-on-move . 'shift)
   (isearch-allow-scroll . 'unlimited)
   (org-structure-template-alist . '(("sh" . "src shell")
                                     ("bash" . "src bash")
                                     ("c" . "center")
                                     ("C" . "comment")
                                     ("conf" . "src conf")
                                     ("el" . "src emacs-lisp")
                                     ("ex" . "example")
                                     ("ht" . "export html")
                                     ("lua" . "src lua")
                                     ("tex" . "export latex")
                                     ("q" . "quote")
                                     ("s" . "src")
                                     ("py" . "src python :session :results value")
                                     ("jp" . "src jupyter-python :session py :async yes :kernel torch")
                                     ("js" . "src javascript")
                                     ("d" . "definition")
                                     ("t" . "theorem")
                                     ("mc" . "quoting")
                                     ("mq" . "question")
                                     ("mt" . "todo")
                                     ("ms" . "summary"))))

  :custom-face (org-document-title . '((t (:inherit t :weight bold :height 1.6))))
  :defun my/set-org-face
  :preface
  (defun my/set-org-face (&rest sym-theme)
    ;; Increase the size of various headings
    (interactive)
    (set-face-attribute 'org-document-title nil
                        :weight 'bold :height 1.6)
    (set-face-attribute 'org-level-1 nil
                        :weight 'bold :slant 'normal :height 1.35)
    (dolist (face '((org-level-2 . 1.3)
                    (org-level-3 . 1.2)
                    (org-level-4 . 1.15)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil
                          :weight 'medium
                          :slant 'normal
                          :height (cdr face)))

    ;; variable pitch
    (face-remap-add-relative 'default :inherit 'variable-pitch)

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way    
    (set-face-attribute 'org-table nil						:inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil					:inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil							:inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-indent t							:inherit '(org-hide fixed-pitch))
    (set-face-attribute 'org-verbatim nil					:inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil	:inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil				:inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil					:inherit 'fixed-pitch)

    ;; Get rid of the background on column views
    (set-face-attribute 'org-column nil :background nil)
    (set-face-attribute 'org-column-title nil :background nil))

  (setq org-format-latex-options
        '( :foreground default
           :background default
           :scale 1.7
           :html-foreground "Black"
           :html-background "Transparent"
           :html-scale 1.0
           :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  (defun my/org-mode-hook ()
    (my/set-org-face))

  :config
  (require 'org-tempo)   ;; need for org-template
  (require 'org-indent)  ;; Make sure org-indent face is available

  (leaf org-fragtog
    :straight t
    :hook (org-mode-hook . org-fragtog-mode)))

(leaf org-agenda
  :when window-system
  :bind* (("C-c C-a" . my/org-agenda-cache)
          ("C-c C-m" . jethro/org-inbox-capture))
  :custom
  `((org-agenda-window-setup . 'other-window)
    (org-agenda-block-separator . nil)
    (org-agenda-start-with-log-mode . t)
    ;; speed up techniques
    (org-agenda-dim-blocked-tasks . nil)
    (org-agenda-use-tag-inheritance . '(search timeline agenda))
    (org-agenda-ignore-drawer-properties . '(effort appt category))
    ;; show agenda from today
    (org-agenda-start-on-weekday . nil)
    (org-agenda-current-time-string . "??? now")
    (org-agenda-time-grid quote ;; Format is changed from 9.1
                          ((daily today require-timed)
                           (0700 1200 1700 2200)
                           "-"
                           "????????????????????????????????????????????????"))
    (org-columns-default-format
     quote
     "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)"))
  :preface
  (defun jethro/set-todo-state-next ()
    "Visit each parent task and change NEXT states to TODO"
    (org-todo "NEXT"))

  (defun my/org-agenda-cache (&optional regenerate)
    "Show agenda buffer without updating if it exists"
    (interactive "P")
    (if (or regenerate (null (get-buffer "*Org Agenda*")))
        (progn
          (setq current-prefix-arg nil)
          (org-agenda nil "a"))
      (org-switch-to-buffer-other-window "*Org Agenda*")))

  (defun jethro/org-inbox-capture ()
    (interactive)
    "Capture a task in agenda mode."
    (org-capture))
  
  :defer-config
  (leaf org-agenda
    :hook ((kill-emacs-hook . ladicle/org-clock-out-and-save-when-exit)
           (org-clock-in-hook . jethro/set-todo-state-next)
           (org-clock-in-hook . (lambda ()
                                  (add-to-list 'frame-title-format
                                               '(:eval org-mode-line-string) t)))
           ;; (org-capture-after-finalize-hook . (lambda ()
           ;;                                      (setq org-agenda-files
           ;;                                            (directory-files-recursively
           ;;                                             org-directory "\\.org$"))))
           )
    :bind (org-agenda-mode-map
           ("i" . org-agenda-clock-in)
           ("r" . jethro/org-agenda-process-inbox-item)
           ("R" . org-agenda-refile)
           ("c" . jethro/org-inbox-capture)
           ("q" . quit-window))
    :preface
    (defvar jethro/org-current-effort "1:00"
      "Current effort for agenda items.")

    (defun jethro/my-org-agenda-set-effort (effort)
      "Set the effort property for the current headline."
      (interactive
       (list (read-string (format "Effort [%s]: " jethro/org-current-effort)
                          nil nil jethro/org-current-effort)))
      (setq jethro/org-current-effort effort)
      (org-agenda-check-no-diary)
      (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                           (org-agenda-error)))
             (buffer (marker-buffer hdmarker))
             (pos (marker-position hdmarker))
             (inhibit-read-only t)
             newhead)
        (org-with-remote-undo buffer
          (with-current-buffer buffer
            (widen)
            (goto-char pos)
            (org-show-context 'agenda)
            (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
            (end-of-line 1)
            (setq newhead (org-get-heading)))
          (org-agenda-change-all-lines newhead hdmarker))))

    (defun jethro/org-agenda-process-inbox-item ()
      "Process a single item in the org-agenda."
      (interactive)
      (org-with-wide-buffer
       (org-agenda-set-tags)
       (org-agenda-priority)
       (call-interactively 'jethro/my-org-agenda-set-effort)
       (org-agenda-refile nil nil t)))

    (defvar jethro/org-agenda-bulk-process-key ?f
      "Default key for bulk processing inbox items.")

    (defun jethro/bulk-process-entries ()
      (if (not (null org-agenda-bulk-marked-entries))
          (let ((entries (reverse org-agenda-bulk-marked-entries))
                (processed 0)
                (skipped 0))
            (dolist (e entries)
              (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
                (if (not pos)
                    (progn (message "Skipping removed entry at %s" e)
                           (cl-incf skipped))
                  (goto-char pos)
                  (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
                  ;; `post-command-hook' is not run yet.  We make sure any
                  ;; pending log note is processed.
                  (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                            (memq 'org-add-log-note post-command-hook))
                    (org-add-log-note))
                  (cl-incf processed))))
            (org-agenda-redo)
            (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
            (message "Acted on %d entries%s%s"
                     processed
                     (if (= skipped 0)
                         ""
                       (format ", skipped %d (disappeared before their turn)"
                               skipped))
                     (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

    (defun jethro/org-process-inbox ()
      "Called in org-agenda-mode, processes all inbox items."
      (interactive)
      (org-agenda-bulk-mark-regexp "inbox:")
      (jethro/bulk-process-entries))

    (defun ladicle/org-clock-out-and-save-when-exit ()
      "Save buffers and stop clocking when kill emacs."
      (ignore-errors (org-clock-out) t)
      (save-some-buffers t))

    :defvar (org-capture-templates)
    :config
    (require 'org-habit)
    (require 'org-capture)
    (setq
     gtd/org-agenda-directory (file-truename "~/org/gtd/")
     org-agenda-files (directory-files-recursively
                       gtd/org-agenda-directory "\\.org")
     org-outline-path-complete-in-steps nil
     org-log-done 'time
     org-log-into-drawer t
     org-log-state-notes-insert-after-drawers nil
     org-tag-alist '(("@errand" . ?e)
                     ("@private" . ?p)
                     ("@work" . ?w)
                     (:newline)
                     ("CANCELLED" . ?c))
     org-fast-tag-selection-single-key nil
     org-todo-keywords '((sequence
                          "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                         (sequence
                          "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
     org-refile-use-outline-path 'file
     org-refile-allow-creating-parent-nodes 'confirm
     org-refile-targets '((org-agenda-files . (:level . 1)))
     org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key
                                         jethro/org-agenda-process-inbox-item)))

    (setq journal-directory (expand-file-name "journal" org-directory)
          journal-daily-file (expand-file-name "daily.org" journal-directory)
          journal-weekly-file (expand-file-name "weekly.org" journal-directory)
          journal-monthly-file (expand-file-name "monthly.org" journal-directory)
          journal-template-directory (expand-file-name "templates" journal-directory)
          journal-template-daily (expand-file-name "daily.org" journal-template-directory)
          journal-template-weekly (expand-file-name "weekly.org" journal-template-directory)
          journal-template-monthly (expand-file-name "monthly.org" journal-template-directory))

    (setq org-capture-templates
          `(("i" "inbox" entry
             (file ,(concat gtd/org-agenda-directory "inbox.org"))
             "* TODO %?")
            ;; ("d" "daily" entry
            ;;  (file+headline ,journal-daily-file ,(format-time-string "%Y-%m-%d %A"))
            ;;  (file ,journal-template-daily))
            ))

    (defun jethro/org-archive-done-tasks ()
      "Archive all done tasks."
      (interactive)
      (org-map-entries 'org-archive-subtree "/DONE" 'file))

    (defun jethro/is-project-p ()
      "Any task with a todo keyword subtask"
      (save-restriction
        (widen)
        (let ((has-subtask)
              (subtree-end (save-excursion (org-end-of-subtree t)))
              (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
          (save-excursion
            (forward-line 1)
            (while (and (not has-subtask)
                        (< (point) subtree-end)
                        (re-search-forward "^\*+ " subtree-end t))
              (when (member (org-get-todo-state) org-todo-keywords-1)
                (setq has-subtask t))))
          (and is-a-task has-subtask))))

    (defun jethro/skip-projects ()
      "Skip trees that are projects"
      (save-restriction
        (widen)
        (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
          (cond
           ((org-is-habit-p)
            next-headline)
           ((jethro/is-project-p)
            next-headline)
           (t
            nil)))))

    (setq org-agenda-custom-commands
          `(("a" "Agenda"
             ((agenda ""
                      ((org-agenda-span 'week)
                       (org-deadline-warning-days 365)
                       (org-agenda-prefix-format " %i %-12:c%?- t % s % e")
                       ))
              (todo "TODO"
                    ((org-agenda-overriding-header "Inbox")
                     (org-agenda-files '(,(concat gtd/org-agenda-directory
                                                  "inbox.org")))))
              (todo "NEXT"
                    ((org-agenda-overriding-header "In Progress")
                     (org-agenda-files '(,(concat gtd/org-agenda-directory
                                                  "projects.org")
                                         ))))
              (todo "TODO"
                    ((org-agenda-overriding-header "Active Projects")
                     (org-agenda-skip-function #'jethro/skip-projects)
                     (org-agenda-files '(,(concat gtd/org-agenda-directory
                                                  "projects.org")
                                         ))))
              (todo "TODO"
                    ((org-agenda-overriding-header "One-off Tasks")
                     (org-agenda-files '(,(concat gtd/org-agenda-directory
                                                  "next.org")))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                 'deadline))))))))))

(setq org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (latex . t)
                                 (shell . t)
                                 (lua . t)))

(leaf ob-emacs-lisp
  :straight org
  :commands (org-babel-execute:emacs-lisp))

(leaf ob-python
  :straight org
  :commands (org-babel-execute:python org-babel-load-session:python))

(leaf ob-latex
  :straight org
  :commands (org-babel-execute:latex))

(leaf ob-shell
  :straight org
  :commands
  org-babel-execute:sh
  org-babel-expand-body:sh
  org-babel-execute:shell
  org-babel-expand-body:shell)

(leaf ob-lua
  :straight org
  :commands (org-babel-execute:lua))

(leaf org-present
  :when window-system
  :straight t
  :bind (org-present-mode-keymap
         ("C-c C-n" . dw/org-present-next)
         ("C-c C-p" . dw/org-present-prev))
  :hook ((org-present-mode-hook . dw/org-present-hook)
         (org-present-mode-quit-hook . dw/org-present-quit-hook))
  :preface
  (defun dw/org-present-prepare-slide ()
    (org-overview)
    (org-show-entry)
    (org-show-children))

  (defun dw/org-present-hook ()
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.5) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))
    (setq header-line-format " ")
    (org-appear-mode -1)
    (org-display-inline-images)
    (dw/org-present-prepare-slide))

  (defun dw/org-present-quit-hook ()
    (setq-local face-remapping-alist '((default variable-pitch default)))
    (setq header-line-format nil)
    (org-present-small)
    (org-remove-inline-images)
    (org-appear-mode 1))

  (defun dw/org-present-prev ()
    (interactive)
    (org-present-prev)
    (dw/org-present-prepare-slide))

  (defun dw/org-present-next ()
    (interactive)
    (org-present-next)
    (dw/org-present-prepare-slide)))

(leaf xref
  :doc "Cross-referencing commands"
  :req "emacs-26.3"
  :url "http://elpa.gnu.org/packages/xref.html"
  :straight t
  :after org)

(leaf ox
  :doc "Export Framework for Org Mode"
  :custom (org-export-backends . '(ascii html latex beamer odt org extra)))

(leaf ox-hugo
  :when window-system
  :doc "Hugo Markdown Back-End for Org Export Engine"
  :straight t
  :require t
  :after ox
  :custom ((org-hugo-front-matter-format . "yaml")
           (org-hugo-link-desc-insert-type . t))
  ;; :defer-config
  ;; (defun c/ox-hugo-add-lastmod nil
  ;;   "Add `lastmod' property with the current time."
  ;;   (interactive)
  ;;   (org-set-property "EXPORT_HUGO_LASTMOD"
  ;;                     (format-time-string "[%Y-%m-%d %a %H:%M]")))

  ;; (leaf *ox-hugo-capture
  ;;     :require org-capture
  ;;     :after org
  ;;     :defvar (org-capture-templates)
  ;;     :config
  ;;     (add-to-list 'org-capture-templates
  ;;                  '("b" "Create new blog post" entry
  ;;                    (file+headline "~/src/omgithub.com/naoking158/blog-src/org/naoki.org" "blog")
  ;;                    "** TODO %?
  ;; :PROPERTIES:
  ;; :EXPORT_FILE_NAME: %(apply #'format \"%s-%s-%s\"
  ;;         (format-time-string \"%Y %m %d\")
  ;; :EXPORT_HUGO_TAGS:
  ;; :EXPORT_HUGO_LASTMOD:
  ;; :END:
  ;; -
  ;; ")
  ;;                  'append))
  )

(leaf ox-latex
  :when window-system
  :doc "LaTeX Back-End for Org Export Engine"
  :tag "out-of-MELPA" "wp" "calendar" "hypermedia" "outlines"
  :preface
  (defun my-latexmk-command (latex options &optional target output)
    "Generate LatexMk command for LATEX, (LatexMk-)OPTIONS, TARGET and OUTPUT directory."
    (let* ((latex-options
            '("-f" "-src-specials" "-file-line-error" "-interaction=nonstopmode"
              "-shell-escape" "-synctex=1"))
           (luatex-option
            (mapconcat (lambda (opt) (concat "-" opt)) latex-options " "))
           (latex-option
            (mapconcat 'identity latex-options " ")))
      (concat "latexmk -gg " options " "
              (cl-case latex
                ('euptex "-pdfdvi -latex='uplatex "))
              (cl-case latex
                ('luatex luatex-option)
                (t latex-option))
              "' "
              (if output (concat "-output-directory=" output " "))
              target)))
  :config
  (setq TeX-engine 'euptex)

  (add-hook 'org-export-before-processing-hook 'my-ox-latex-tex-engine-setup)

  (defun my-ox-latex-tex-engine-setup (backend)
    (message "backend=%s" backend)
    (when (equal backend 'latex)
      (my-ox-latex-engine-set TeX-engine)))

  (defun my-ox-latex-engine-set (latex)
    "Set up LATEX environments."

    (setq org-latex-default-class "jsarticle")
    (add-to-list 'org-latex-classes
                 '("research-note"
                   "\\documentclass[openany]{report}\n
\\input{../preferences/header.tex}\n
\\input{..//preferences/preamble_research_note.tex}\n
\\usepackage[whole]{bxcjkjatype}
%% \\usepackage{amsmath,amsthm,amssymb}
%% \\usepackage{mynotestyle}
%% \\usepackage{preamble}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                   ("\\datechapter{%s}" . "\\datechapter{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                   ))
    (add-to-list 'org-latex-classes
                 '("article"
                   "\\RequirePackage{plautopatch}\n
\\documentclass[a4p,uplatex,dvipdfmx]{article}\n
\\input{../preferences/header.tex}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("jsarticle"
                   "\\RequirePackage{plautopatch}\n
\\documentclass[a4p,uplatex,dvipdfmx]{jsarticle}\n
\\input{../preferences/header.tex}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (setq org-latex-pdf-process (list (my-latexmk-command TeX-engine "-pv" "%f" "%o")))
    ))

(leaf org-roam
  :when window-system
  :doc "Roam Research replica with Org-mode"
  :url "https://github.com/org-roam/org-roam"
  :straight t
  ;; This is necessary for variables to be initialized correctly.
  ;; :require t
  :bind* (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n c" . org-roam-capture))
  :custom
  `((org-roam-v2-ack . t)
    (org-roam-directory . ,(file-truename "~/org/braindump/"))
    (org-roam-db-location . ,(expand-file-name
                              "org-roam.db"
                              (file-truename "~/org/braindump/")))
    (org-roam-db-gc-threshold . most-positive-fixnum)
    (org-id-link-to-org-use-id . t)
    (org-roam-capture-templates
     quote
     (("c" "Concept" plain "%?"
       :target (file+head "concepts/${slug}.org"
                          "#+title: ${title}\n#+date: %U")
       :unnarrowed t)
      ("l" "Literature" plain
       (file "~/org/braindump/preferences/LiteratureTemplate.org")
       :target (file+head "lit/${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: Literature")
       :unnarrowed t)
      ("m" "Meeting" plain "%?"
       :target (file+head "work/${slug}.org"
                          "#+title: ${title}\n#+filetags: Meeting\n#+options: toc:nil")
       :unnarrowed t)
      ("w" "Working" plain "%?"
       :target (file+head "work/${slug}.org"
                          "#+title: ${title}\n#+filetags: Working\n#+options: toc:nil")
       :unnarrowed t)
      ("p" "Private" plain "%?"
       :target (file+head "private/${slug}.org"
                          "#+title: ${title}\n#+date: %U\n")
       :unnarrowed t))))

  :defer-config
  (leaf org-roam-dailies
    :bind-keymap ("C-c n d" . org-roam-dailies-map)
    :bind
    (:org-roam-dailies-map
     ("Y" . org-roam-dailies-capture-yesterday)
     ("T" . org-roam-dailies-capture-tomorrow)))

  ;; for org-roam-buffer-toggle
  ;; Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (org-roam-db-autosync-mode))


(leaf org-roam-ui
  :disabled t
  :after org-roam
  :load-path "~/.emacs.d/elisp/org-roam-ui/"
  :straight simple-httpd websocket
  :commands (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(leaf orp-paperpile
  :when window-system
  :after org-roam
  :doc "orp-paperpile; Org-Roam-Protocol Paperpile is an interface
        to comunicate between org-mode and paperpile using org-roam-protocol."
  :load-path "~/.emacs.d/elisp/orp-paperpile/"
  :require t
  :defer-config (orp-activate)
  :advice (:around org-link-open advice-around-org-link-open)
  :custom
  ((orp-paperpile-local-pdf-dir . "~/drive/Paperpile/")
   (org-roam-capture-ref-templates . '(("r" "ref" plain "%?"
                                        :target (file+head "lit/${slug}.org"
                                                           (concat
                                                            "#+date: %U\n"
                                                            "#+filetags: Literature\n"
                                                            "#+title: ${title}"))
                                        :unnarrowed t))))
  
  :preface
  (defun open-external (path)
    (interactive)
    (cond
     ((eq system-type 'darwin)
      (let ((cmd-list (list
                       (if (string-prefix-p "chrome-extension" path)
                           "brave"
                         "open")
                       (concat "'" path "'")
                       "&")))
        (shell-command (c-concat-separated cmd-list " "))))
     ((eq system-type 'gnu/linux)
      (let ((process-connection-type nil))
        (start-process "" nil "xdg-open" path)))))

  ;; for open paperpile link in external browser
  (defun advice-around-org-link-open (f link &optional arg)
    (let ((path (org-element-property :raw-link link))
          (type (org-element-property :type link)))
      (if (or (string-match "paperpile" path)
              (string-match "chrome-extension" path))
          (let ((path (if (string-equal "file" type)
                          (cadr (split-string path ":"))
                        path)))
            (open-external path)
            (message "Open: %s" path))
        (apply f link arg)))))

(leaf org-superstar
  :disabled t
  :straight t
  :after org
  :hook (org-mode-hook . org-superstar-mode)
  :custom
  ((org-superstar-remove-leading-stars . nil)
   (org-superstar-headline-bullets-list . '( "???" "???" "???" "???" "???" "???" " " ))
   (org-superstar-item-bullet-alist . '((?+ . ????)
                                        (?* . ?-)
                                        (?- . ????)))))

(setq paste-cmd (cond
                 ((executable-find "pngpaste")
                  "pngpaste ")
                 ((executable-find "xclip")
                  "xclip -selection clipboard -t image/png -out > ")
                 (t nil)))

(leaf *org-insert-clipboard-image
  :when paste-cmd
  :after org
  :bind ("C-M-y" . org-insert-clipboard-image)
  :preface
  (defun org-insert-clipboard-image ()
    "Generate png file from a clipboard image and insert a link to current buffer."
    (interactive)
    (let* ((filename
            (concat "imgs/"
                    (file-name-sans-extension (file-name-nondirectory
                                               (buffer-file-name)))
                    (format-time-string "_%Y%m%d_%H%M%S")
                    ".png")))
      (unless (file-exists-p (file-name-directory filename))
        (make-directory (file-name-directory filename)))
      (shell-command (concat paste-cmd filename))
      (if (file-exists-p filename)
          (insert (concat "[[file:" filename "]]")))
      (org-display-inline-images))))

(leaf *org-image-size-adjuster
  :advice
  (:before
   org-toggle-inline-images
   (lambda (&rest args)
     (setq-local org-image-actual-width
                 (truncate (* 0.8 (window-text-width nil t))))))
  )

(leaf org-appear
  :straight t
  :hook org-mode-hook)

(leaf *auto-tangle-configuration
  :hook (org-mode-hook . (lambda ()
                           (add-hook 'after-save-hook
                                     #'my/org-babel-tangle-config)))
  :defun my/org-babel-tangle-config
  :preface
  ;; Automatically tangle my Emacs.org Config file when I save it
  (defun my/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (file-truename "~/.emacs.d/Emacs.org"))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle)))))

(leaf org-make-toc
  :straight t
  :hook (org-babel-pre-tangle-hook . org-make-toc-mode))

(leaf *latex
  :when window-system
  :config
  (leaf auctex
    :straight t
    :custom ((TeX-master . nil)
             (TeX-auto-save . t)
             (TeX-parse-self . t)
             (TeX-source-correlate-method . 'synctex)
             (TeX-source-correlate-start-server . t)
             (TeX-source-correlate-mode . t)
             (TeX-PDF-mode . t))
 
    :config
    (defun my/latex-mode-hook nil
      (visual-fill-column-mode t)
      (let*
          ((latexmk-cmd "latexmk -synctex=1 -interaction=nonstopmode -pv -f ")
           (ja-compile-cmd (concat latexmk-cmd "%s.tex"))
           (en-compile-cmd (concat latexmk-cmd "-e $bibtex=q/bibtex/ -pdf %s.tex"))
           (displayline-cmd
            (if (eq 'darwin system-type)
                "/Applications/Skim.app/Contents/SharedSupport/displayline %n %s.pdf %b"
              "okular --unique %o#src:%n%b")))
        (setq TeX-command-list
              `(("Clean" "TeX-clean" TeX-run-function nil t
                 :help "Delete generated intermediate files")
                ("Clean All" "(TeX-clean t)" TeX-run-function nil t
                 :help "Delete generated intermediate and output files")
                ("ja-uptex" ,ja-compile-cmd TeX-run-command t nil
                 :help "Japanese compile with uptex")
                ("en-pdflatex" ,en-compile-cmd TeX-run-command t nil
                 :help "English compile with pdflatex")
                ("PDF view" "open %s.pdf" TeX-run-command t nil
                 :help "View in PDF")
                ("Displayline" ,displayline-cmd TeX-run-command t nil
                 :help "Show selected line in PDF")))))
    (add-hook 'LaTeX-mode-hook  #'my/latex-mode-hook))

  (leaf latex-extra
    :doc "Adds several useful functionalities to LaTeX-mode."
    :url "http://github.com/Malabarba/latex-extra"
    :straight t
    :hook (LaTeX-mode-hook . latex-extra-mode)
    :bind (:latex-extra-mode-map
           ("C-M-f" . forward-paragraph)
           ("C-M-b" . backward-paragraph)))

  (leaf reftex
    :doc "Built-in function"
    :hook (LaTeX-mode-hook . reftex-mode)
    :bind (:reftex-mode-map
           ("C-c r" . reftex-reference)
           ("C-c l" . reftex-label)
           ("C-c c" . reftex-citation))
    :custom
    ((reftex-ref-style-default-list . '("Cleveref"))
     (reftex-cite-format . 'natbib))))

(leaf oj
  :when window-system
	:doc "Competitive programming tools client for AtCoder, Codeforces"
	:req "emacs-26.1" "quickrun-2.2"
	:url "https://github.com/conao3/oj.el"
	:straight t
	:commands oj-prepare oj-test oj-submit
	:custom ((oj-default-online-judge quote atcoder)
					 (oj-compiler-python . "cpython")
					 (oj-home-dir . "~/drive/work/coder/AtCoder")
					 (oj-submit-args quote
													 ("-y" "--wait=0"))))

(leaf elfeed
  :when window-system
  :straight t
  :commands elfeed
  :advice (:after elfeed (lambda nil (visual-line-mode -1)))
  :custom
  ((elfeed-search-filter . "@2-days-ago +unread")
   (elfeed-search-title-max-width . 80)
   (elfeed-search-title-min-width . 80)
   (elfeed-feeds
    quote
    (;; programming
     ("https://news.ycombinator.com/rss" Hacker)
     ("https://www.reddit.com/r/programming.rss" Programming)
     ("https://www.reddit.com/r/learnprogramming.rss" LearnProgramming)
     ("https://www.reddit.com/r/emacs.rss" Emacs)
     ("https://www.reddit.com/r/planetemacs.rss" PlanetEmacs)
     ("https://www.reddit.com/r/orgmode.rss" Org-mode)

     ;; programming languages
     ("https://www.reddit.com/r/python.rss" Python)

     ;; Apple
     ("https://www.reddit.com/r/apple.rss" Apple)
     ("https://www.reddit.com/r/mac.rss" Mac)
     ("https://www.reddit.com/r/AppleWatch.rss" AppleWatch)))))

(leaf mail-general
  :require t
  :load-path "~/src/github.com/naoking158/envs/config-mail/")

;;; Notmuch (mail indexer and mail user agent (MUA))
;; I install notmuch from the distro's repos because the CLI program is
;; not dependent on Emacs.  Though the package also includes notmuch.el
;; which is what we use here (they are maintained by the same people).
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(leaf notmuch
  ;; :straight t
  :commands notmuch notmuch-hello
  :config
  (load-file "~/src/github.com/naoking158/envs/config-mail/config-notmuch.el")

;;; General UI
  (setq notmuch-show-logo nil)
  (setq notmuch-column-control t)
  (setq notmuch-hello-auto-refresh t)
  (setq notmuch-hello-recent-searches-max 50)
  (setq notmuch-hello-thousands-separator "")
  (setq notmuch-show-all-tags-list nil)

;;; Search
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ("flag" . notmuch-search-flagged-face)))
  (setq notmuch-show-empty-saved-searches t)

;;; Tags
  (setq notmuch-archive-tags '("-inbox" "-del" "-todo" "+archived"))
  (setq notmuch-message-replied-tags '("+replied"))
  (setq notmuch-message-forwarded-tags '("+forwarded"))
  (setq notmuch-show-mark-read-tags '("-unread"))
  (setq notmuch-draft-tags '("+draft"))
  (setq notmuch-draft-folder "drafts")
  (setq notmuch-draft-save-plaintext 'ask)
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged))))
  (setq notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
          (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

;;; Email composition
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil) ; TODO 2021-05-12: Review hidden headers
  ;; (setq notmuch-address-command nil)    ; FIXME 2021-05-13: Make it work with EBDB
  (setq notmuch-address-use-company nil)
  (setq notmuch-address-internal-completion '(received nil))
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)

;;; Reading messages
  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-save-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (setq notmuch-wash-citation-lines-prefix 6)
  (setq notmuch-wash-citation-lines-suffix 6)
  (setq notmuch-wash-wrap-lines-length 100)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

;;; Hooks and key bindings
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (add-hook 'notmuch-show-hook (lambda () (setq-local header-line-format nil)))

  (leaf *notmuch-config
    :leaf-defer nil
    :advice
    (:after notmuch-show-mode meow-insert-mode)
    (:after notmuch-tree-mode meow-insert-mode)
    (:after notmuch-hello-mode meow-insert-mode)
    (:after notmuch-search-mode meow-insert-mode)
    (:after notmuch-message-mode meow-insert-mode)
    
    :hook (notmuch-mua-send-hook . notmuch-mua-attachment-check)
    :bind (:notmuch-search-mode-map
           :package notmuch
           ("." . nil)
           ("." . notmuch-tag-jump))
    :config
    (add-to-list 'corfu-excluded-modes 'notmuch-message-mode)
    (add-to-list 'corfu-excluded-modes 'org-msg-edit-mode)
    
    (setq mark-complete-tags '("+archived" "-inbox" "-todo" "-unread")
          mark-delete-tags '("+del" "-inbox" "-archived" "-unread")
          mark-flag-tags '("+flag" "-unread")
          mark-spam-tags '("+spam" "+del" "-inbox" "-unread")
          mark-todo-tags '("+todo" "-unread" "-inbox"))

    (setq notmuch-tagging-keys
          `((,(kbd "a") notmuch-archive-tags "Archive (remove from inbox)")
            (,(kbd "c") mark-complete-tags "Complete and archive")
            (,(kbd "d") mark-delete-tags "Mark for deletion")
            (,(kbd "f") mark-flag-tags "Flag as important")
            (,(kbd "s") mark-spam-tags "Mark as spam")
            (,(kbd "t") mark-todo-tags "To-do")
            (,(kbd "r") ("-unread") "Mark as read")
            (,(kbd "u") ("+unread") "Mark as unread")))
    )
  )

(leaf notmuch-transient
  :straight t
  :after notmuch
  :bind
  ((:notmuch-search-mode-map :package notmuch
                             ("=" . nil)
                             ("=" . notmuch-search-mode-transient))
   (:notmuch-tree-mode-map :package notmuch
                           ("=" . nil)
                           ("=" . notmuch-tree-mode-transient))
   (:notmuch-hello-mode-map :package notmuch
                            ("=" . nil)
                            ("=" . notmuch-hello-mode-transient))
   (:notmuch-show-mode-map :package notmuch
                           ("=" . nil)
                           ("=" . notmuch-show-mode-transient))))

(leaf ol-notmuch
  :straight t
  :after notmuch)

(leaf *config-mu4e
  :disabled t
  :config
  (load-file "~/src/github.com/naoking158/envs/config-mail/config-mu4e.el"))

(leaf server
  :doc "Lisp code for GNU Emacs running as server process"
  :tag "builtin"
  :require t
  :bind ("C-x C-c" . server-edit)
  :hook (emacs-startup-hook . server-start))

(leaf tree-sitter
  :straight t tree-sitter-langs
  :hook
  (python-mode-hook . (lambda nil
                        (require 'tree-sitter)
                        (require 'tree-sitter-langs)
                        (tree-sitter-mode)
                        (tree-sitter-hl-mode))))

(leaf xwwp
  :disabled t
  :when (or (<= emacs-major-version 27)
            (memq window-system '(ns darwin)))
  :straight t
  :custom (browse-url-browser-function . 'xwidget-webkit-browse-url)
  :bind (("C-c s" . xwwp)
         (xwidget-webkit-mode-map
          ("v" . xwwp-follow-link)
          ([remap kill-ring-save] . xwidget-webkit-copy-selection-as-kill)
          ([remap xwidget-webkit-browse-url] . xwwp)))
  :advice (:override xwwp-browse-url-other-window
                     my/xwwp-browse-url-other-window)
  :preface
  (defun my/xwwp-browse-url-other-window (url &optional new-session)
    "Ask xwidget-webkit to browse URL.
NEW-SESSION specifies whether to create a new xwidget-webkit session.
Interactively, URL defaults to the string looking like a url around point."
    (interactive (progn
                   (require 'browse-url)
                   (browse-url-interactive-arg "xwidget-webkit URL: "
                                               ;;(xwidget-webkit-current-url)
                                               )))
    (or (featurep 'xwidget-internal)
        (user-error "Your Emacs was not compiled with xwidgets support"))
    (when (stringp url)
      (if new-session
          (xwidget-webkit-new-session url)
        (progn (xwidget-webkit-goto-url url)
               (switch-to-buffer (xwidget-buffer
                                  (xwidget-webkit-current-session))))))))

(leaf webkit
  :disabled t
  :when (and (eq 28 emacs-major-version)
             (memq window-system '(x pgtk)))
  :load-path "~/.emacs.d/elisp/emacs-webkit/"
  :require t webkit-ace webkit-dark
  ;; :init
  ;; ;; This must be set before webkit.el is loaded so certain hooks aren't installed
  ;; (setq webkit-own-window t)
  :bind (("C-c s" . webkit))
  :config
  ;; If you don't care so much about privacy and want to give your data to google
  (setq webkit-search-prefix "https://google.com/search?q=") 

  ;; Specify a different set of characters use in the link hints
  ;; For example the following are more convienent if you use dvorak
  (setq webkit-ace-chars "asdfjkl;")

  ;; If you want history saved in a different place or
  ;; Set to `nil' to if you don't want history saved to file (will stay in memory)
  (setq webkit-history-file
        (expand-file-name "webkit-history" no-littering-etc-directory))

  ;; If you want cookies saved in a different place or
  ;; Set to `nil' to if you don't want cookies saved
  (setq webkit-cookie-file
        (expand-file-name "cookies" no-littering-etc-directory)) 

  ;; Set webkit as the default browse-url browser
  ;; (setq browse-url-browser-function 'webkit-browse-url)
  ;; (setq browse-url-browser-function 'browse-url-default-browser)

  ;; Force webkit to always open a new session instead of reusing a current one
  (setq webkit-browse-url-force-new t)

  ;; Globally disable javascript
  ;; (add-hook 'webkit-new-hook #'webkit-enable-javascript)

  ;; Override the "loading:" mode line indicator with an icon from `all-the-icons.el'
  ;; You could also use a unicode icon like ???
  (defun webkit--display-progress (progress)
    (setq webkit--progress-formatted
          (if (equal progress 100.0)
              ""
            (format "%s%.0f%%  " (all-the-icons-faicon "spinner") progress)))
    (force-mode-line-update))

  ;; Set action to be taken on a download request. Predefined actions are
  ;; `webkit-download-default', `webkit-download-save', and `webkit-download-open'
  ;; where the save function saves to the download directory, the open function
  ;; opens in a temp buffer and the default function interactively prompts.
  (setq webkit-download-action-alist '(("\\.pdf\\'" . webkit-download-open)
                                       ("\\.png\\'" . webkit-download-save)
                                       (".*" . webkit-download-default)))

  ;; Globally use a proxy
  ;; (add-hook 'webkit-new-hook (lambda () (webkit-set-proxy "socks://localhost:8000")))

  ;; Globally use the simple dark mode
  (setq webkit-dark-mode t))

(leaf pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:pdf-view-mode-map
         ("j" . pdf-view-next-line-or-next-page)
         ("k" . pdf-view-previous-line-or-previous-page)
         ("h" . image-backward-hscroll)
         ("l" . image-forward-hscroll)
         ("/" . pdf-occur))
  :hook ((TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
         (pdf-view-mode-hook . (lambda () (set-buffer-multibyte t))))
  :custom (pdf-view-display-size . 'fit-width)
  :config
  (pdf-tools-install :no-query)
  (leaf pdf-annot
    :require t
    :after pdf-tools
    :custom `(pdf-annot-minor-mode-map-prefix . ,(kbd "a"))
    :bind
    (:pdf-annot-minor-mode-map
     ("d" . pdf-annot-delete)
     ("h" . pdf-annot-add-highlight-markup-annotation)
     ("s" . pdf-annot-add-strikeout-markup-annotation)
     ("u" . pdf-annot-add-underline-markup-annotation))))

(leaf applescript-mode :straight t)

(leaf jupyter
  :disabled t
  :straight jupyter websocket
  :after org
  :config
  (leaf ob-jupyter
    :require t
    :preface (dolist (lang '(python jupyter))
               (add-to-list 'org-babel-load-languages
                            (cons lang t) t))
    :advice ((:before org-babel-execute:jupyter (lambda (&rest args)
                                                  (require 'zmq)))
             (:before org-babel-expand-body:jupyter (lambda (&rest args)
                                                      (require 'zmq))))
    :config
    (define-key jupyter-org-interaction-mode-map
                [remap jupyter-org-hydra/body] nil)
    (define-key jupyter-org-interaction-mode-map
                (kbd "C-c C-.") #'jupyter-org-hydra/body)))

(leaf org-babel
  :after org python-mode
  :config
  (defun org-babel-edit-prep:jupyter-python (babel-info)
    (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
    (my/python-basic-config))
  ;; (defun org-babel-edit-prep:python (babel-info)
  ;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  ;;   (my/python-basic-config))
  )

(leaf sie-brow
  :disabled t
  :doc "Sie-Brow; Search in external browser with keywords
          - at point with prefix `C-u',
          - in selected region,
          - killed latest, or
          - input by user."
  :bind (("C-c s" . sie-brow/search-in-google)
         ("C-c p" . sie-brow/search-in-google-scholar))
  :custom (browse-url-browser-function . 'browse-url-default-browser)
  :preface
  (defgroup sie-brow nil
    "Search in external browser with keywords."
    :prefix "sie-brow/"
    :group 'sie-brow)

  (defcustom sie-brow/prefix-for-google-search "https://www.google.com/search"
    "Prefix for google search."
    :type 'string
    :group 'sie-brow)

  (defcustom sie-brow/prefix-for-google-scholar "https://scholar.google.com/scholar"
    "Prefix for google scholar."
    :type 'string
    :group 'sie-brow)

  (defcustom sie-brow/url-suffix "&ie=UTF-8"
    "Suffix of the URL."
    :type 'string
    :group 'sie-brow)

  (defun sie-brow/keyword-suitable-for-url-format (&optional at-point)
    "Return a search keyword suitable for the URL format."
    (let* ((default-keyword (cond
                             (at-point (thing-at-point 'symbol))
                             ((use-region-p) (buffer-substring-no-properties
                                              (mark) (point)))
                             (t (if kill-ring
                                    (substring-no-properties (car kill-ring))
                                  nil))))
           (keywords (read-from-minibuffer (if default-keyword
                                               (format "Search keywords (%s): "
                                                       default-keyword)
                                             "Search keywords: "))))
      (replace-regexp-in-string "[ \n\t\r\f ]"
                                "+"
                                (if (length> keywords 0)
                                    keywords
                                  default-keyword))))

  (defun sie-brow/search-in-external-browser (prefix &optional at-point)
    "Search in external browser with keywords
          - at point with prefix `C-u',
          - in selected region,
          - that are latest killed words, or
          - input by user."
    (let* ((search-keyword (sie-brow/keyword-suitable-for-url-format at-point)))
      (browse-url (concat prefix
                          "?q=" search-keyword
                          sie-brow/url-suffix))))

  (defun sie-brow/search-in-google (&optional at-point)
    "Search in Google."
    (interactive "P")
    (sie-brow/search-in-external-browser sie-brow/prefix-for-google-search at-point))

  (defun sie-brow/search-in-google-scholar (&optional at-point)
    "Search in Google Scholar."
    (interactive "P")
    (sie-brow/search-in-external-browser sie-brow/prefix-for-google-scholar at-point)))

(leaf eaf
  :disabled t
  :when (memq window-system '(x))
  :load-path "~/.emacs.d/elisp/emacs-application-framework/"
  :commands
  (eaf-search-it eaf-open eaf-open-browser eaf-open-browser-with-history eaf-open-pdf-from-history)
  :custom
                                        ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  ((eaf-python-command . "/usr/bin/python")
   (eaf-browser-continue-where-left-off . t)
   (eaf-browser-enable-adblocker . t)
   (browse-url-browser-function . 'eaf-open-browser))
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding))

(leaf browse-at-remote
  :straight t
  :commands browse-at-remote-get-url
  :custom (browse-at-remote-prefer-symbolic . nil)
  :bind ("M-g r" . browse-at-remote))

(leaf org-inline-anim
  :straight t
  :hook (org-mode-hook . org-inline-anim-mode))

(leaf burly
  :straight t
  :bind* (("C-x C-b w" . burly-bookmark-windows)
          ("C-x C-b f" . burly-bookmark-frames)))

(leaf atomic-chrome
  :straight t
  :hook (emacs-startup-hook . atomic-chrome-start-server)
  :custom ((atomic-chrome-enable-auto-update . nil)
           (atomic-chrome-default-major-mode . 'org-mode)
           (atomic-chrome-url-major-mode-alist . '(("overleaf\\.com" . tex-mode)))
           (atomic-chrome-buffer-open-style . 'split)
           (atomic-chrome-extension-type-list . '(ghost-text))))

(leaf org-modern
  :straight t
  :hook (org-mode-hook . org-modern-mode)
  :custom-face
  (org-modern-label . '((t :height 0.9 :inherit t)))
  :custom
  ((org-modern-star . ["???" "???" "???" "???" "???" "???" " "])
   (org-modern-list . '((?+ . "???")
                        (?* . "-")
                        (?- . "???"))))
  )

(leaf vertico-posframe
  :straight t
  :after vertico
  :when (display-graphic-p)
  :hook
  ((vertico-mode-hook . my--enable-vertico-posframe-mode)
   (before-make-frame-hook . my--disable-vertico-posframe-mode))
  :custom
  `((vertico-posframe-poshandler . 'posframe-poshandler-frame-bottom-center)
    (vertico-posframe-width . 155)
    (vertico-posframe-border-width . 5)
    (vertico-multiform-categories . '((file posframe)
                                      (citar-reference posframe)
                                      (command posframe)
                                      (consult-location posframe)
                                      (consult-multi posframe)
                                      (org-roam-node posframe)
                                      (completing-read posframe)
                                      ))
    (vertico-multiform-commands . '((consult-imenu buffer)
                                    (execute-extended-command posframe)
                                    (helpful-function posframe)
                                    (helpful-symbol posframe)
                                    (helpful-variable posframe))))
  :config
  (defun my--enable-vertico-posframe-mode ()
    (vertico-posframe-mode)
    (vertico-multiform-mode))

  (defun my--disable-vertico-posframe-mode ()
    (when (not (display-graphic-p))
      (vertico-posframe-mode -1)
      (vertico-multiform-mode -1))))

(leaf know-your-http-well
  :doc
  """
  HTTP encodings, headers, media types, methods, relations and status codes, all summarized and linking to their specification.

  M-x http-header ;; content-type
  M-x http-method ;; post | POST
  M-x http-relation ;; describedby
  M-x http-status-code ;; 500
  M-x http-status-code ;; not_found | NOT_FOUND
  """
  :straight t)

(leaf csv-mode :straight t)

(leaf markdown-preview-mode
  :straight t
  :defer-config
  (setq markdown-preview-stylesheets
        (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
  
  (dolist (elm (list
                "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))
    (add-to-list 'markdown-preview-javascript elm))
  )

(leaf mermaid-mode
  :straight t
  :defer-config
  (setq mermaid-mode-map
        (let ((map mermaid-mode-map))
          (define-key map (kbd "C-c C-c") nil)
          (define-key map (kbd "C-c C-f") nil)
          (define-key map (kbd "C-c C-b") nil)
          (define-key map (kbd "C-c C-r") nil)
          (define-key map (kbd "C-c C-o") nil)
          (define-key map (kbd "C-c C-d") nil)
          (define-key map (kbd "C-c C-d c") 'mermaid-compile)
          (define-key map (kbd "C-c C-d c") 'mermaid-compile)
          (define-key map (kbd "C-c C-d f") 'mermaid-compile-file)
          (define-key map (kbd "C-c C-d b") 'mermaid-compile-buffer)
          (define-key map (kbd "C-c C-d r") 'mermaid-compile-region)
          (define-key map (kbd "C-c C-d o") 'mermaid-open-browser)
          (define-key map (kbd "C-c C-d d") 'mermaid-open-doc)
          map)))

(provide 'init)

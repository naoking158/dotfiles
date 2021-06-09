;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el

(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)

(require 'cl-lib)
(setq default-directory "~/")

(cl-case system-type
  ('darwin
    (custom-set-variables '(shell-file-name "/usr/local/bin/fish")))
  ('gnu/linux
    (custom-set-variables '(shell-file-name "/usr/bin/fish")))
  )

(prog1 'emacs
  (eval-and-compile
    (when (or load-file-name byte-compile-current-file)
      (setq user-emacs-directory (expand-file-name
                                  (file-name-directory
                                   (or load-file-name byte-compile-current-file))))))

  (eval-and-compile
    (prog1 "install leaf"
      (custom-set-variables
        '(warning-suppress-types '((comp)))
        '(package-archives
           '(("celpa" . "https://celpa.conao3.com/packages/")
              ("melpa" . "https://melpa.org/packages/")
              ("org" . "https://orgmode.org/elpa/")
              ("gnu" . "https://elpa.gnu.org/packages/"))))
      (package-initialize)
      (unless (package-installed-p 'leaf)
        (package-refresh-contents)
        (package-install 'leaf)))

    (leaf leaf
      :config
      (leaf leaf-convert
        :ensure t
        )
      (leaf leaf-tree
        :ensure t
        :custom ((imenu-list-size . 30)
                  (imenu-list-position . 'left))))

    (leaf leaf-keywords
      :ensure t
      :init
      (leaf package
        :config
        (leaf *elpa-workaround
          :emacs>= 26.1
          :emacs<= 26.2
          :custom ((gnutls-algorithm-priority . "NORMAL:-VERS-TLS1.3"))))

      (leaf hydra
	      :doc "Make bindings that stick around."
	      :req "cl-lib-0.5" "lv-0"
	      :tag "bindings"
	      :url "https://github.com/abo-abo/hydra"
	      :ensure t
	      :after lv)

      (leaf feather
        :doc "Parallel thread modern package manager"
        :req "emacs-26.3" "async-1.9" "async-await-1.0" "ppp-1.0" "page-break-lines-0.1"
        :tag "package" "convenience" "emacs>=26.3"
        :url "https://github.com/conao3/feather.el"
        :emacs>= 26.3
        :ensure t
        :after ppp page-break-lines)

      (leaf blackout
        :doc "Better mode lighter overriding"
        :req "emacs-26"
        :tag "extensions" "emacs>=26"
        :url "https://github.com/raxod502/blackout"
        :emacs>= 26
        :ensure t)

      :config
      (leaf-keywords-init)))

  (leaf *initialize-emacs
    :config
    (leaf cus-edit
      :doc "tools for customizing Emacs and Lisp packages"
      :tag "builtin" "faces" "help"
      :custom `((custom-file \,
                             (locate-user-emacs-file "custom.el"))))

    (leaf cus-start
      :doc "define customization properties of builtins"
      :tag "builtin" "internal"
      :url "http://handlename.hatenablog.jp/entry/2011/12/11/214923"
      :preface
      (defun c/redraw-frame nil
        (interactive)
        (redraw-frame))

      (defun c/garbage-collect nil
        (interactive)
        (garbage-collect))

      (defun finder-current-dir-open nil
        (interactive)
        (shell-command "open ."))

      :bind (("M-ESC ESC" . c/redraw-frame)
              ("M-ESC g" . c/garbage-collect))
      :custom '((fill-column . 85)
                 (tab-width . 4)
                 (tool-bar-mode . nil)
                 (user-full-name . "Naoki Sakamoto")
                 (user-mail-address . "naoki@bbo.cs.tsukuba.ac.jp")
                 (user-login-name . "naoking158")
                 (create-lockfiles)
                 (use-dialog-box)
                 (use-file-dialog)
                 (debug-on-error . nil)
                 (init-file-debug . t)
                 (frame-resize-pixelwise . t)
                 (enable-recursive-minibuffers . t)
                 (history-length . 1000)
                 (history-delete-duplicates . t)
                 (scroll-preserve-screen-position . t)
                 (scroll-conservatively . 100)
                 (mouse-wheel-scroll-amount quote
                   (1
                     ((control)
                       . 5)))
                 (ring-bell-function quote ignore)
                 (text-quoting-style quote straight)
                 (truncate-lines . t)
                 (menu-bar-mode . nil)
                 (tool-bar-mode)
                 (scroll-bar-mode)
                 (fringe-mode . 10)
                 (indent-tabs-mode)
                 (frame-title-format . "%f")
                 (blink-cursor-mode . nil)
                 (show-paren-mode . 1)
                 (confirm-kill-emacs . 'y-or-n-p)
                 (recentf-auto-cleanup . 'never)
                 (save-place-mode . 1)
                 )
      :config
      (let ((gls "/usr/local/bin/gls"))
        (if (file-exists-p gls) (setq insert-directory-program gls)))

      (leaf hl-line
        :doc "highlight the current line"
        :tag "builtin"
        :require t
        :config
        ;;; hl-lineを無効にするメジャーモードを指定する
        (defvar global-hl-line-timer-exclude-modes '(todotxt-mode))
        (defun global-hl-line-timer-function ()
          (unless (memq major-mode global-hl-line-timer-exclude-modes)
            (global-hl-line-unhighlight-all)
            (let ((global-hl-line-mode t))
              (global-hl-line-highlight))))
        (setq global-hl-line-timer
              (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
        )


      (defalias 'yes-or-no-p 'y-or-n-p)
      (keyboard-translate 8 127)
      (mapc
       (lambda (fn)
         (put fn 'disabled nil))
       (list 'upcase-region 'downcase-region 'narrow-to-region 'narrow-to-page 'narrow-to-defun 'list-timers)))

    (leaf bookmark
      :doc "set bookmarks, maybe annotate them, jump to them later"
      :tag "builtin"
      :bind (("C-c b d" . counsel-bookmarked-directory)
             ("C-c b j" . bookmark-jump)))

    (leaf exec-path-from-shell
      :doc "Get environment variables such as $PATH from the shell"
      :tag "environment" "unix"
      :url "https://github.com/purcell/exec-path-from-shell"
      :ensure t
      :when (memq window-system
              '(mac ns x))
      :defun (exec-path-from-shell-initialize)
      :custom ((exec-path-from-shell-check-startup-files)
               (exec-path-from-shell-variables quote
                                               ("PATH" "PYTHONPATH")))
      :config
      (exec-path-from-shell-initialize))

    (leaf benchmark-init
      :disabled t
      :doc "Benchmarks Emacs require and load calls"
      :tag "benchmark"
      :ensure t
      :require t
      :leaf-defer nil
      :hook ((after-init-hook . benchmark-init/deactivate)))

    (leaf *ui
      :config
      (leaf mac
        :doc "implementation of gui terminal on macos"
        :doc "each symbol can be `control', `meta', `alt', `hyper', or `super'"
        :doc "`left' meens same value setting its left key"
        :when window-system
        :bind (("M-o" . finder-current-dir-open)
                ("s-w" . kill-buffer)
                ("s-q" . save-buffers-kill-emacs)
                ("s-v" . yank)
                ("s-c" . copy-region-as-kill))
        :custom ((mac-control-modifier quote control)
                  (mac-option-modifier quote meta)
                  (mac-command-modifier quote super)
                  (mac-right-control-modifier quote control)
                  (mac-right-option-modifier quote meta)
                  (mac-right-command-modifier quote super)
                  (initial-frame-alist . '((width . 110)
                                            (height . 65)))
                  (line-spacing . 4)
                  )
        )

      (leaf doom-themes
        :doc "an opinionated pack of modern color-themes"
        :req "emacs-25.1" "cl-lib-0.5"
        :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
        :url "https://github.com/hlissner/emacs-doom-theme"
        :emacs>= 25.1
        :ensure t
        :custom
        ((doom-themes-enable-italic . t)
          (doom-themes-enable-bold . t))
        :config
        ;; (load-theme 'doom-one t)
        (load-theme 'doom-nord t)
        (doom-themes-neotree-config)
        (doom-themes-org-config)

        (leaf doom-modeline
          :doc "A minimal and modern mode-line"
          :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
          :tag "mode-line" "faces" "emacs>=25.1"
          :url "https://github.com/seagle0128/doom-modeline"
          :emacs>= 25.1
          :ensure t
          :require t all-the-icons shrink-path
          :custom
          ((doom-modeline-buffer-file-name-style . 'truncate-from-project)
            (doom-modeline-icon . t)
            (doom-modeline-major-mode-icon . nil)
            (doom-modeline-minor-modes . nil))
          ;; :custom-face
          ;; (
          ;;  ;; (doom-modeline-bar . '((t (:background "#6272a4"))))
          ;;  (mode-line . '((t (:background "dark slate gray" :foreground "white")))))
          :hook (after-init-hook . doom-modeline-mode)
          :defun (doom-modeline-def-modeline)
          :config
          (line-number-mode 1)
          (column-number-mode 1)
          (doom-modeline-def-modeline 'main
            '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
            '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
          )
        )

      (leaf hide-mode-line
        :doc "minor mode that hides/masks your modeline"
        :req "emacs-24.4"
        :tag "mode-line" "frames" "emacs>=24.4"
        :url "https://github.com/hlissner/emacs-hide-mode-line"
        :emacs>= 24.4
        :ensure t
        :hook
        ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

      (leaf ns
        :doc "next/open/gnustep / macos communication module"
        :when (eq 'ns window-system)
        :custom ((ns-control-modifier quote control)
                  (ns-option-modifier quote meta)
                  (ns-command-modifier quote super)
                  (ns-right-control-modifier quote control)
                  (ns-right-option-modifier quote meta)
                  (ns-right-command-modifier quote super)
                  (ns-use-proxy-icon . nil)
                  (frame-title-format . nil)
                  (default-frame-alist quote
                    (
                      (inhibit-double-buffering . t)
                      (ns-transparent-titlebar . t)
                      (ns-appearance . dark)
                      ))
                  )
        )
      )

    (leaf solarized-theme
      :disabled t
      :doc "The Solarized color theme"
      :req "emacs-24.1" "dash-2.16"
      :tag "solarized" "themes" "convenience" "emacs>=24.1"
      :url "http://github.com/bbatsov/solarized-emacs"
      :emacs>= 24.1
      :leaf-defer nil
      :ensure t
      :commands (solarized-create-theme-file-with-palette solarized-color-blend)
      :custom ((solarized-use-less-bold . t)
                (solarized-scale-org-headlines)
                (solarized-distinct-fringe-background . t))
      :config
      (defun c/reload-theme nil
        (interactive)
        (eval-buffer)
        (load-theme 'solarized-wombat-dark t))

      (solarized-create-theme-file-with-palette 'dark 'solarized-wombat-dark
        '("#2a2a29" "#f6f3e8" "#e5c06d" "#ddaa6f" "#ffb4ac" "#e5786d" "#834c98" "#a4b5e6" "#7ec98f" "#8ac6f2")
        '((custom-theme-set-faces theme-name
            `(default
               ((,class
                  (:foreground ,base3 :background ,base03))))
            `(vertical-border
               ((,class
                  (:foreground ,base03))))
            `(mode-line
               ((,class
                  (:foreground ,base2 :background ,base02))))
            `(mode-line-inactive
               ((,class
                  (:foreground ,base0 :background ,base03))))
            `(font-lock-builtin-face
               ((,class
                  (:foreground ,magenta)))))))
      (load-theme 'solarized-wombat-dark t))

    (leaf which-key
      :diminish which-key-mode
      :doc "Display available keybindings in popup"
      :req "emacs-24.4"
      :tag "emacs>=24.4"
      :url "https://github.com/justbur/emacs-which-key"
      :emacs>= 24.4
      :ensure t
      :blackout t
      :custom ((which-key-idle-delay . 2)
                (which-key-replacement-alist quote
                  (((nil . "Prefix Command")
                     nil . "prefix")
                    ((nil . "\\`\\?\\?\\'")
                      nil . "lambda")
                    (("<left>")
                      "←")
                    (("<right>")
                      "→")
                    (("<\\([[:alnum:]-]+\\)>")
                      "\\1"))))
      :global-minor-mode t)

    (leaf which-key-posframe
      :disabled t
      :doc "Using posframe to show which-key"
      :req "emacs-26.0" "posframe-0.4.3" "which-key-3.3.2"
      :tag "tooltip" "bindings" "convenience" "emacs>=26.0"
      :url "https://github.com/yanghaoxie/which-key-posframe"
      :emacs>= 26.0
      :ensure t
      :after posframe which-key
      :global-minor-mode t)

    (leaf dashboard
      :doc "A startup screen extracted from Spacemacs"
      :req "emacs-25.3" "page-break-lines-0.11"
      :tag "dashboard" "tools" "screen" "startup" "emacs>=25.3"
      :url "https://github.com/emacs-dashboard/emacs-dashboard"
      :emacs>= 25.3
      :ensure t
      :custom ((dashboard-items quote
                 ((agenda . 10)
                   (recents . 15)
                   (projects . 5)
                   (bookmarks . 5))))
      :defun (dashboard-setup-startup-hook)
      :config
      (dashboard-setup-startup-hook)))

  (leaf projectile
    :doc "Manage and navigate projects in Emacs easily"
    :req "emacs-25.1" "pkg-info-0.4"
    :tag "convenience" "project" "emacs>=25.1"
    :url "https://github.com/bbatsov/projectile"
    :emacs>= 25.1
    :ensure t
    :config
    (leaf neotree
      :doc "A tree plugin like NerdTree for Vim"
      :req "cl-lib-0.5"
      :url "https://github.com/jaypei/emacs-neotree"
      :ensure t
      :custom
      ((neo-theme . 'nerd2))
      :defun
      (neotree-show neotree-hide neotree-dir neotree-find projectile-project-root)
      :defvar (neo-smart-open)
      :preface
      (defun neotree-projectile-toggle ()
        (interactive)
        (let ((project-dir
                (ignore-errors
         ;;; Pick one: projectile or find-file-in-project
                  (projectile-project-root)
                  ))
               (file-name (buffer-file-name))
               (neo-smart-open t))
          (if (and (fboundp 'neo-global--window-exists-p)
                (neo-global--window-exists-p))
            (neotree-hide)
            (progn
              (neotree-show)
              (if project-dir
                (neotree-dir project-dir))
              (if file-name
                (neotree-find file-name)))))))
    :bind
    (("M-t" . neotree-projectile-toggle))
    )
  )

(leaf ace-window
  :doc "Quickly switch windows."
  :req "avy-0.5.0"
  :tag "location" "window"
  :url "https://github.com/abo-abo/ace-window"
  :ensure t
  :bind* ("C-t" . ace-window)
  :custom ((aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  :custom-face
  ((aw-leading-char-face . '((t (:height 4.0 :foreground "#f1fa8c")))))
  )

(leaf auctex
  :doc "Integrated environment for *TeX*"
  :req "emacs-24.3" "cl-lib-1.0"
  :tag "preview-latex" "doctex" "context" "texinfo" "latex" "tex" "emacs>=24.3"
  :emacs>= 24.3
  :ensure t
  :setq-default ((TeX-master . nil))
  :custom
  ((TeX-auto-save . t)
   (TeX-parse-self . t)
   (TeX-source-correlate-method . 'synctex)
   (TeX-source-correlate-start-server . t)
   (TeX-PDF-mode . t)
   ;; RefTeX
   (reftex-plug-into-AUCTeX . t)
   (reftex-insert-label-flags . '("s" "sfte"))
   (reftex-label-alist . '((nil ?e nil "\\eqref{%s}" nil nil)))
   (reftex-default-bibliography . '("~/org/preferences/ref.bib"))
   (reftex-bibliography-commands . '("bibliography" "nobibliography" "addbibresource"))
   (fill-column . 86)
   )
  :defvar (TeX-command-list)
  :config
  (leaf latex-extra
    :doc "Adds several useful functionalities to LaTeX-mode."
    :req "auctex-11.86.1" "cl-lib-0.5"
    :tag "tex"
    :url "http://github.com/Malabarba/latex-extra"
    :ensure t)

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (add-to-list 'TeX-command-list
                                     '("ja"
                                       "sh ~/drive/lab/latextemplate/ja_latex.sh '%s'"
                                       TeX-run-command t nil))
                        (add-to-list 'TeX-command-list
                                     '("en"
                                       "sh ~/drive/lab/latextemplate/en_latex.sh '%s'"
                                       TeX-run-command t nil))
                        (add-to-list 'TeX-command-list
                                     '("pdfview" "open '%s.pdf' "
                                       TeX-run-command t nil))
                        )))
  ;; SyncTeX
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (add-to-list 'TeX-command-list
                                     '("Displayline" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %s.pdf %b" TeX-run-command t nil))
                        )))
  ;; RefTeX
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; Change key bindings
  (add-hook 'reftex-mode-hook
            '(lambda ()
               (define-key reftex-mode-map (kbd "\C-cr") 'reftex-reference)
               (define-key reftex-mode-map (kbd "\C-cl") 'reftex-label)
               (define-key reftex-mode-map (kbd "\C-cc") 'reftex-citation)
               ))
  ) ;; end of auctex

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :ensure t
  :require t
  :custom (auto-revert-interval . 0.1)
  )

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :require t
  :blackout t
  :leaf-defer nil
  :bind (;; ("<tab>" . company-indent-or-complete-common)
         (company-active-map
          ("M-n")
          ("M-p")
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ;; ("<tab>" . company-complete-selection)
)
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-tooltip-limit . 15)
           (company-idle-delay . 0.2)
           (company-dabbrev-downcase . 0)
           (company-minimum-prefix-length . 2)
           (company-transformers quote
                                 (company-sort-by-occurrence))
           (company-require-match . 'never)
           (completion-ignore-case . nil)
           (company-math-allow-latex-symbols-in-faces . t)
           (company-math-allow-unicode-symbols-in-faces . (quote (tex-math font-latex-math-face)))
           )

  :global-minor-mode global-company-mode
  :config
  (leaf company-org-block
    :ensure t
    :custom
    (company-org-block-edit-style . 'auto) ;; 'auto, 'prompt, or 'inline
    :hook ((org-mode-hook . (lambda ()
                              (setq-local company-backends '(company-org-block))
                              (company-mode +1)))))

  (leaf company-yasnippet
    :doc "company-mode completion backend for Yasnippet"
    :tag "out-of-MELPA"
    :after yasnippet
    :defvar (company-backends)
    :preface
    (defun c/company-mode-with-yas nil
      (setq company-backends (mapc
                              (lambda (elm)
                                (if (and
                                     (listp elm)
                                     (member 'company-yasnippet elm))
                                    elm
                                  (append
                                   (if (consp elm)
                                       elm
                                     (list elm))
                                   '(:with company-yasnippet))))
                              company-backends)))

    :hook ((prog-mode-hook . c/company-mode-with-yas))
    )
  ;; using child frame
  (leaf company-posframe
    :when window-system
    :doc "Use a posframe as company candidate menu"
    :req "emacs-26.0" "company-0.9.0" "posframe-0.1.0"
    :tag "matching" "convenience" "abbrev" "emacs>=26.0"
    :url "https://github.com/tumashu/company-posframe"
    :emacs>= 26.0
    :ensure t
    :global-minor-mode t
    :diminish t
    :hook (company-mode-hook . company-posframe-mode)
    )

  ;; Show pretty icons
  (leaf company-box
    :disabled t
    :diminish
    :hook (company-mode-hook . company-box-mode)
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :disabled (eq window-system 'x)
    :doc "Company front-end with icons"
    :url "https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el"
    :req "emacs-26.0.91" "dash-2.13" "dash-functional-1.2.0" "company-0.9.6"
    :tag "convenience" "front-end" "completion" "company" "emacs>=26.0.91"
    :url "https://github.com/sebastiencs/company-box"
    :emacs>= 26.0
    :ensure t
    :custom ((company-box-max-candidates . 20)
             (company-box-backends-colors . nil)
              (company-box-show-single-candidate . t))
    :require t
    :config
    (when (memq window-system
                '(ns mac))
      (defun company-box-icons--elisp (candidate)
        (when (derived-mode-p 'emacs-lisp-mode)
          (let ((sym (intern candidate)))
            (cond ((fboundp sym) 'Function)
                  ((featurep sym) 'Module)
                  ((facep sym) 'Color)
                  ((boundp sym) 'Variable)
                  ((symbolp sym) 'Text)
                  (t . nil)))))

      (with-eval-after-load 'all-the-icons
        (declare-function all-the-icons-faicon 'all-the-icons)
        (declare-function all-the-icons-fileicon 'all-the-icons)
        (declare-function all-the-icons-material 'all-the-icons)
        (declare-function all-the-icons-octicon 'all-the-icons)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
                (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
                (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
                (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
                (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
                (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
                (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
                (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
                (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
                (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
                (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
                (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
                (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
                (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
                (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
                (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
                (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
                (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
                (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
                (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
                (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
                (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))
    )

  (leaf company-math
    :doc "Completion backends for unicode math symbols and latex tags"
    :req "company-0.8.0" "math-symbol-lists-1.3"
    :tag "completion" "symbols" "unicode"
    :url "https://github.com/vspinu/company-math"
    :ensure t
    :preface
    (defun c/latex-mode-setup nil
      (setq-local company-backends
                  (append
                   '((company-math-symbols-latex company-math-symbols-unicode company-latex-commands))
                   company-backends)))

    :hook ((org-mode-hook . c/latex-mode-setup)
           (TeX-mode-hook . c/latex-mode-setup))
    )

  (leaf company-tabnine
    :ensure t
    :require t
    :config
    (add-to-list 'company-backends #'company-tabnine)
    ;; workaround for company-transformers
    (setq company-tabnine--disable-next-transform nil)
    (defun my-company--transform-candidates (func &rest args)
      (if (not company-tabnine--disable-next-transform)
          (apply func args)
        (setq company-tabnine--disable-next-transform nil)
        (car args)))

    (defun my-company-tabnine (func &rest args)
      (when (eq (car args) 'candidates)
        (setq company-tabnine--disable-next-transform t))
      (apply func args))

    (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
    (advice-add #'company-tabnine :around #'my-company-tabnine)
    )
  )  ;; end company

(leaf avy
  :doc "Jump to arbitrary positions in visible text and select text quickly."
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "location" "point" "emacs>=24.1"
  :url "https://github.com/abo-abo/avy"
  :emacs>= 24.1
  :ensure t
  :bind (("C-c a w" . avy-goto-word-1)
         ("C-c a c" . avy-goto-char-2)))

(leaf beacon
  :doc "Highlight the cursor whenever the window scrolls"
  :req "seq-2.14"
  :tag "convenience"
  :url "https://github.com/Malabarba/beacon"
  :ensure t
  :custom
  ((beacon-color . "yellow"))
  :config
  (beacon-mode 1))

(leaf cl-lib
  :doc "Common Lisp extensions for Emacs"
  :tag "builtin"
  :added "2021-02-06"
  :leaf-defer t)

(leaf diminish
  :doc "Diminished modes are minor modes with no modeline display"
  :req "emacs-24.3"
  :tag "codeprose" "minor" "diminish" "extensions" "emacs>=24.3"
  :url "https://github.com/myrjola/diminish.el"
  :emacs>= 24.3
  :ensure t)

(leaf dumb-jump
  :doc "Jump to definition for 40+ languages without configuration"
  :req "emacs-24.3" "s-1.11.0" "dash-2.9.0" "popup-0.5.3"
  :tag "programming" "emacs>=24.3"
  :url "https://github.com/jacktasia/dumb-jump"
  :emacs>= 24.3
  :ensure t
  :custom ((dumb-jump-selector 'ivy)
           (dumb-jump-use-visible-window))
  :bind
  (("s-." . dumb-jump-go)
   ("s-," . dumb-jump-back))
  :global-minor-mode t)

(leaf duplicate-thing
  :doc "Duplicate current line & selection"
  :tag "selection" "line" "duplicate" "command" "convenience"
  :url "https://github.com/ongaeshi/duplicate-thing"
  :ensure t
  :bind ("M-c" . duplicate-thing))

(leaf editorconfig
  ;; :disabled t
  :doc "EditorConfig Emacs Plugin"
  :req "cl-lib-0.5" "emacs-24"
  :tag "emacs>=24"
  :url "https://github.com/editorconfig/editorconfig-emacs#readme"
  :emacs>= 24
  :ensure t
  ;; :global-minor-mode t
  :config
  (setq editorconfig--enable-20210221-testing t)
  (editorconfig-mode 1))

(leaf eldoc
  :doc "Show function arglist or variable docstring in echo area"
  :tag "builtin"
  :blackout t
  :custom ((eldoc-idle-delay . 0.3)))

(leaf tab-bar
  :doc "frame-local tabs with named persistent window configurations"
  :tag "builtin"
  :bind (("C-<tab>" . tab-bar-switch-to-next-tab))
  :config
  (tab-bar-mode)
  (tab-bar-new-tab))

(leaf ein
    :doc "Emacs IPython Notebook"
    :req "emacs-25" "websocket-20190620.338" "anaphora-20180618" "request-20200117.0" "deferred-0.5" "polymode-20190714.0" "dash-2.13.0"
    :tag "emacs>=25"
    :emacs>= 25
    :ensure t
    :custom ((ein:output-area-inlined-images . t)))

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :tag "builtin"
  :bind (flymake-mode-map
         ("C-c C-n" . flymake-goto-next-error)
          ("C-c C-p" . flymake-goto-prev-error))
  :config
  (leaf flymake-posframe
    :when window-system
    :hook (flymake-mode-hook . flymake-posframe-mode)
    :tag "out-of-MELPA"
    ;; :url "https://github.com/Ladicle/flymake-posframe"
    ;; :el-get Ladicle/flymake-posframe
    :load-path "/Users/naoki/.emacs.d/el-get/flymake-posframe"
    :require t)

  (leaf flymake-diagnostic-at-point
    :doc "Display flymake diagnostics at point"
    :req "emacs-26.1" "popup-0.5.3"
    :tag "tools" "languages" "convenience" "emacs>=26.1"
    :url "https://github.com/meqif/flymake-diagnostic-at-point"
    :emacs>= 26.1
    :ensure t
    :after flymake
    :defvar (flymake-diagnostic-at-point-error-prefix)
    :custom ((flymake-diagnostic-at-point-timer-delay . 0.1)
              (flymake-diagnostic-at-point-error-prefix . " ► ")
              (flymake-diagnostic-at-point-display-diagnostic-function quote flymake-diagnostic-at-point-display-popup))
    :hook ((flymake-mode-hook . flymake-diagnostic-at-point-mode))
    :defvar (flymake-posframe-hide-posframe-hooks)
    :defun (flymake-posframe-hide-posframe my/flymake-diagnostic-at-point-display-popup package-lint-setup-flymake posframe-hide flymake--diag-text)
    :config
    ;; (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
    ;; (add-hook 'emacs-lisp-mode-hook #'package-lint-setup-flymake)
    (set-face-attribute 'popup-tip-face nil
		  :background "dark slate gray" :foreground "white" :underline nil)
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
    ;; flymake-posframe
    (defvar flymake-posframe-hide-posframe-hooks
      '(pre-command-hook post-command-hook focus-out-hook)
      "The hooks which should trigger automatic removal of the posframe.")

    (defun flymake-posframe-hide-posframe ()
      "Hide messages currently being shown if any."
      (posframe-hide " *flymake-posframe-buffer*")
      (dolist (hook flymake-posframe-hide-posframe-hooks)
        (remove-hook hook #'flymake-posframe-hide-posframe t)))

    (when window-system
      (defun my/flymake-diagnostic-at-point-display-popup (text)
        "Display the flymake diagnostic TEXT inside a posframe."
        (posframe-show " *flymake-posframe-buffer*"
		      :string (concat flymake-diagnostic-at-point-error-prefix
				            (flymake--diag-text
				              (get-char-property (point) 'flymake-diagnostic)))
		      :position (point)
		      :foreground-color "cyan"
		      :internal-border-width 2
		      :internal-border-color "red"
		      :poshandler 'posframe-poshandler-window-bottom-left-corner)
        (dolist (hook flymake-posframe-hide-posframe-hooks)
          (add-hook hook #'flymake-posframe-hide-posframe nil t)))
      (advice-add 'flymake-diagnostic-at-point-display-popup :override 'my/flymake-diagnostic-at-point-display-popup))
    )
  ) ;; end of flymake

(leaf gcmh
  :ensure t
  :diminish t
  :custom
  (gcmh-verbose . t)
  :config
  (gcmh-mode 1))

(leaf global-visual-line-mode
  :defun (global-visual-line-mode . t)
  :config
  (global-visual-line-mode t))

(leaf helm-org-rifle
  :doc "Rifle through your Org files"
  :req "emacs-24.4" "dash-2.12" "f-0.18.1" "helm-1.9.4" "s-1.10.0"
  :tag "outlines" "hypermedia" "emacs>=24.4"
  :url "http://github.com/alphapapa/helm-org-rifle"
  :emacs>= 24.4
  :ensure t)

(leaf highlight-indent-guides
  :diminish
  :doc "Minor mode to highlight indentation"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :emacs>= 24.1
  :ensure t
  :hook (((prog-mode-hook yaml-mode) . highlight-indent-guides-mode))
  :custom
  ((highlight-indent-guides-auto-enabled . t)
    (highlight-indent-guides-responsive . t)
    (highlight-indent-guides-method . 'character)) ; column
  :config
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  )

(leaf indent-region-custom
  :defun (indent-region-custom)
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
  (leaf untab-region
    :defun (untab-region . nil)
    :bind (("M-[" . untab-region))
    :preface
    (defun untab-region nil
      (interactive)
      (indent-region-custom -4))
    )

  (leaf tab-region
    :defun (tab-region . nil)
    :bind (("M-]" . tab-region))
    :preface
    (defun tab-region nil
      (interactive)
      (if (active-minibuffer-window)
          (minibuffer-complete)    ; tab is pressed in minibuffer window -> do completion
        (if (use-region-p)    ; tab is pressed is any other buffer -> execute with space insertion
            (indent-region-custom 4) ; region was selected, call indent-region-custom
          (insert "    ") ; else insert four spaces as expected
          )))
    )
  )

(leaf ispell
  :doc "interface to spell checkers"
  :tag "builtin"
  :setq-default (ispell-program-name . "aspell"))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2020-03-25"
  :emacs>= 24.5
  :ensure t
  :require t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . '((org-agenda-refile . "^")
                                         (org-capture-refile . "^")
                                         (counsel-describe-function . "^")
                                         (counsel-describe-variable . "^")
                                         (Man-completion-table . "^")
                                         (woman . "^")))
           (ivy-re-builders-alist quote
                                  ((t . ivy--regex-plus)
                                   (swiper . ivy--regex-plus)))
           (ivy-use-selectable-prompt . t)
           (ivy-prescient-retain-classic-highlighting . t)
           (ivy-read-action-function . #'ivy-hydra-read-action)
           (ivy-use-virtual-buffers . nil)
           (ivy-height . 30)
           (ivy-extra-directories . nil))
  :global-minor-mode t
  :config
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1))
  (leaf ivy-hydra
    :doc "Additional key bindings for Ivy"
    :req "emacs-24.5" "ivy-0.13.0" "hydra-0.15.0"
    :tag "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t)

  (leaf ivy-prescient
    :doc "prescient.el + Ivy"
    :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
    :tag "extensions" "emacs>=25.1"
    :url "https://github.com/raxod502/prescient.el"
    :emacs>= 25.1
    :ensure t
    :after prescient
    :custom ((ivy-prescient-retain-classic-highlighting . t))
    :global-minor-mode t)

  (leaf ivy-rich
    :disabled t
    :doc "More friendly display transformer for ivy."
    :req "emacs-24.5" "ivy-0.8.0"
    :tag "ivy" "emacs>=24.5"
    :emacs>= 24.5
    :ensure t
    :global-minor-mode t
    :config
    (plist-put ivy-rich-display-transformers-list
               'ivy-switch-buffer
               '(:columns
                 ((ivy-switch-buffer-transformer (:width 60))
                  (ivy-rich-switch-buffer-size (:width 7))
                  (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                  (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                  (ivy-rich-switch-buffer-project (:width 15 :face success))
                  (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                 :predicate
                 (lambda (cand) (get-buffer cand)))))


  (leaf ivy-xref
    :doc "Ivy interface for xref results"
    :req "emacs-25.1" "ivy-0.10.0"
    :tag "emacs>=25.1"
    :url "https://github.com/alexmurray/ivy-xref"
    :added "2020-03-25"
    :emacs>= 25.1
    :ensure t
    :after ivy
    :custom ((xref-show-xrefs-function quote ivy-xref-show-xrefs)))

  (leaf ivy-yasnippet
    :disabled t
    :doc "Preview yasnippets with ivy"
    :req "emacs-24" "ivy-0.10.0" "yasnippet-0.12.2" "dash-2.14.1" "cl-lib-0"
    :tag "convenience" "emacs>=24"
    :added "2020-06-23"
    :url "https://github.com/mkcms/ivy-yasnippet"
    :emacs>= 24
    :ensure t
    :after ivy yasnippet
    :bind (("C-c i" . ivy-yasnippet)))

  (leaf smex
    :doc "M-x interface with Ido-style fuzzy matching."
    :req "emacs-24"
    :tag "usability" "convenience" "emacs>=24"
    :url "http://github.com/nonsequitur/smex/"
    :emacs>= 24
    :ensure t
    :custom ((smex-history-length . 35)
             (smex-completion-method . 'ivy)))

  (leaf counsel
    :diminish
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    ;; コマンド	効果
    ;; M-o	選択項目に対するアクションを選択できる
    ;; C-o	ミニバッファを表示したままコマンドを発行．jk移動．gプレビュー．
    ;; C-M-n	候補を切り替えるとバッファも切り替わりプレビューできる（順方向）
    ;; C-M-p	候補を切り替えるとバッファも切り替わりプレビューできる（逆方向）
    ;; C-M-m	候補リストの表示を維持したまま，現在の選択候補をプレビューする
    ;; C-M-j	選択候補を無視して，入力中の値を使う
    :bind (("C-s" . counsel-grep-or-swiper)
           ("M-s M-s" . swiper-thing-at-point)
           ("M-x" . counsel-M-x)
           ("M-y" . counsel-yank-pop)
           ("C-M-z" . counsel-fzf)
           ("C-M-r" . counsel-recentf)
           ("C-x C-b" . counsel-ibuffer)
           ("C-M-f" . counsel-rg)
           ("C-x C-f" . counsel-find-file)
            )
    :custom '((counsel-yank-pop-separator . "----------")
              (counsel-find-file-ignore-regexp \,
                                               (rx-to-string
                                                '(| "./" "../")
                                                'no-group))
              (counsel-describe-function-function . 'helpful-callable)
              (counsel-describe-variable-function . 'helpful-variable)
              )
    :global-minor-mode t
    ;; :commands with-ivy-window
    :config
    (leaf counsel-tramp
      :doc "Tramp ivy interface for ssh, docker, vagrant"
      :req "emacs-24.3" "counsel-0.10"
      :tag "emacs>=24.3"
      :added "2021-01-09"
      :url "https://github.com/masasam/emacs-counsel-tramp"
      :emacs>= 24.3
      :ensure t
      :bind ("C-c s" . counsel-tramp)
      :custom
      ((tramp-default-method . "ssh")
       (tramp-ssh-controlmaster-options . nil)
       (make-backup-files . nil)
       (create-lockfiles . nil)
       (counsel-tramp-localhost-directory . "/home/naoki/"))
      :config
      (leaf aggressive-indent
        :doc "Minor mode to aggressively keep your code always indented"
        :req "emacs-24.3"
        :tag "tools" "maint" "lisp" "indent" "emacs>=24.3"
        :url "https://github.com/Malabarba/aggressive-indent-mode"
        :emacs>= 24.3
        :ensure t)
      (leaf editorconfig
        :doc "EditorConfig Emacs Plugin"
        :req "cl-lib-0.5" "emacs-24"
        :tag "emacs>=24"
        :url "https://github.com/editorconfig/editorconfig-emacs#readme"
        :emacs>= 24
        :ensure t)
      (add-hook 'counsel-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
				                                   (projectile-mode 0)
				                                   (editorconfig-mode 0)))
      (add-hook 'counsel-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
			                                (projectile-mode 1)
			                                (editorconfig-mode 1)))
      (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
      (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
      )

    ;; フィルタを指定
    ;; ivy--regex-ignore-order はデフォルトでは ivy--regex-plus
    (setf (alist-get 'counsel-M-x ivy-re-builders-alist) #'ivy--regex-ignore-order)

    ;; ソーティング方法
    (defun ivy--sort-by-len (name candidates)
      "Sort CANDIDATES based on similarity of their length with NAME."
      (let ((name-len (length name))
            (candidates-count (length candidates)))
        (if (< 500 candidates-count)
            candidates
          (seq-sort-by #'length
                       (lambda (a b)
                         (< (abs (- name-len a))
                            (abs (- name-len b))))
                       candidates))))

    (setf (alist-get 'counsel-M-x ivy-sort-matches-functions-alist)
      #'ivy--sort-by-len)

    ;; ;; Disable counsel-find-file
    ;; ;; https://emacs.stackexchange.com/questions/45929/disable-ivy-for-find-file
    ;; (defun my-disable-counsel-find-file (&rest args)
    ;;   "Disable `counsel-find-file' and use the original `find-file' with ARGS."
    ;;   (let ((completing-read-function #'completing-read-default)
    ;;         (completion-in-region-function #'completion--in-region))
    ;;     (apply #'read-file-name-default args)))
    ;; (setq read-file-name-function #'my-disable-counsel-find-file)
    ;; (define-key counsel-mode-map [remap find-file]  nil)

    ;; オリジナルを非インタラクティブ化
    (when (require 'find-func nil t)
      (defun find-library (library)
        "Override the original `find-library' to hide in command list."
        (prog1
            (switch-to-buffer (find-file-noselect (find-library-name library)))
          (run-hooks 'find-function-after-hook))))

    ;; counsel-rg
    ;; https://github.com/abo-abo/swiper/blob/7e4c56776f811f78b8eb95210156f8fbbdba67e7/counsel.el#L3156
    (when (memq system-type '(ms-dos mac))
      (setq counsel-rg-base-command
            (append counsel-rg-base-command
                    '("--path-separator" "/" "."))))

    (defun ad:counsel-rg (f &optional initial-input initial-directory extra-rg-args rg-prompt caller)
      (apply f (or initial-input (ivy-thing-at-point))
             (unless current-prefix-arg
               (or initial-directory default-directory))
             extra-rg-args rg-prompt caller))
    (advice-add 'counsel-rg :around #'ad:counsel-rg)

    (defun my-counsel-rg-in-dir (_arg)
      "Search again with new root directory."
      (let ((current-prefix-arg '(4)))
        (counsel-rg ivy-text nil ""))) ;; also disable extra-rg-args
    (ivy-add-actions
     'counsel-rg
     '(("r" my-counsel-rg-in-dir "search in directory")))

;; (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
;;       (apply f (or initial-input (ivy-thing-at-point))
;;              (unless current-prefix-arg
;;                (or initial-directory default-directory))
;;              extra-ag-args ag-prompt caller))
;;     (advice-add 'counsel-ag :around #'ad:counsel-ag)

;;     (defun my-counsel-ag-in-dir (_arg)
;;       "Search again with new root directory."
;;       (let ((current-prefix-arg '(4)))
;;         (counsel-ag ivy-text nil ""))) ;; also disable extra-ag-args
;;     (ivy-add-actions
;;      'counsel-ag
;;      '(("r" my-counsel-ag-in-dir "search in directory")))

    (defun ad:counsel-fzf (f &optional initial-input initial-directory fzf-prompt)
      (apply f (or initial-input
                   (ivy-thing-at-point))
             (or initial-directory default-directory)
             fzf-prompt))

    (advice-add 'counsel-fzf :around #'ad:counsel-fzf)

    ;; Common actions for counsel-ag, counsel-fzf, and counsel-recentf
    (defun my-counsel-fzf-in-default-dir (_arg)
      "Search the current directory with fzf."
      (counsel-fzf ivy-text default-directory))
    (defun my-counsel-fzf-in-dir (_arg)
      "Search again with new root directory."
      (counsel-fzf ivy-text
                   (read-directory-name
                    (concat (car (split-string counsel-fzf-cmd))
                            " in directory: "))))

    (ivy-add-actions
     'counsel-fzf
     '(("r" my-counsel-fzf-in-dir "search in directory")))

    ;; 発動するコマンドを限定
    (defcustom my-nocand-then-fzf-commands '(counsel-find-flie
                                             counsel-recentf
                                             counsel-projectile-find-file
                                             counsel-projectile-switch-project)
      "List of commands for applying extension no candidates then `counsel-fzf'."
      :group 'ivy
      :type '(list symbol))

    ;; 無応答の時[y/n]を出す．待ち時間を[s]で指定
    (defcustom my-nocand-then-fzf-idle-time 0.8
      "Idle time for showing prompt."
      :group 'ivy
      :type 'float)

    (defvar my--nocand-then-fzf t)
    (defun my-nocand-then-fzf-reset ()
      (setq my--nocand-then-fzf t))

    (defun my-nocand-then-fzf (prompt)
      (when (= ivy--length 0)
        (if (eq (read-char prompt) ?y) ;; y-or-n-p is not applicable
            (ivy-exit-with-action
             (lambda (x)
               (counsel-fzf ivy-text default-directory)))
          (setq my--nocand-then-fzf nil))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; プロンプトをカスタマイズ（モードライン非表示派向け）
    (defvar my-ivy-prompt-prefix t) ;; ime-on/-off に合わせて変えてもOK．
    (defun my-toggle-ivy-prompt-prefix ()
      "Toggle showing a header line before the ivy prompt."
      (interactive)
      (setq my-ivy-prompt-prefix (not my-ivy-prompt-prefix)))
    (defun my-ivy-prompt-prefix ()
      "Return a header line for the ivy prompt."
      (when my-ivy-prompt-prefix
        (if window-system
            (format "%s\n%s "
                    (make-string (frame-width) ?\x5F) ;; "__"
                    (all-the-icons-faicon "sort-amount-asc")) ;; ""
          (format "%s\n" (make-string (1- (frame-width)) ?\x2D))))) ;; "--"


    (defun ad:fzf:ivy--insert-prompt ()
      (when (and my--nocand-then-fzf
                 (memq (ivy-state-caller ivy-last) my-nocand-then-fzf-commands)
                 (= ivy--length 0))
        (let* ((std-props '(front-sticky t rear-nonsticky t field t read-only t))
               (prompt (concat (my-ivy-prompt-prefix) "Switch to Counsel-fzf? [y/n] ")))
          (set-text-properties 0 (length prompt)
                               `(face minibuffer-prompt ,@std-props) prompt)
          (run-with-idle-timer my-nocand-then-fzf-idle-time
                               nil #'my-nocand-then-fzf prompt))))

    ;; ivy--insert-prompt を adivce する．
    (advice-add 'ivy--insert-prompt :before #'ad:fzf:ivy--insert-prompt)

    ;; セッションを抜けたらフラグを戻す
    (add-hook 'minibuffer-setup-hook #'my-nocand-then-fzf-reset)
    (add-hook 'minibuffer-exit-hook #'my-nocand-then-fzf-reset)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ;; counsel-recentf のリストを "~/" から始める．
    (require 'ivy)
    ;; (defun ad:counsel-recentf ()
    ;;   "Find a file on `recentf-list'."
    ;;   (interactive)
    ;;   (require 'recentf)
    ;;   (recentf-mode)
    ;;   (ivy-read "Recentf: "
    ;;             (progn
    ;;               (mapc #'substring-no-properties recentf-list)  ;; no need?
    ;;               (mapc #'abbreviate-file-name recentf-list)) ;; ~/
    ;;             :action (lambda (f)
    ;;                       (with-ivy-window
    ;;                         (find-file f)))
    ;;             :require-match t
    ;;             :caller 'counsel-recentf))
    ;; (advice-add 'counsel-recentf :override #'ad:counsel-recentf)

    ;; 以下の関数は，counsel-projectile-* と counsel-recentf にぶら下げる．
    (defun my-counsel-fzf-in-default-dir (_arg)
      "Search the current directory with fzf."
      (counsel-fzf ivy-text default-directory))

    ;; add an action for counsel-recentf (M-o z)
    (ivy-add-actions
     'counsel-recentf
     '(("z" my-counsel-fzf-in-default-dir "switch to fzf")))

    (custom-set-variables
     '(recentf-exclude
       '(".recentf" "bookmarks" "org-recent-headings.dat" "^/tmp\\.*"
         "^/private\\.*" "/TAGS$")))
;; "^/var/folders\\.*"

    ;; 余分なメッセージを削除しておきましょう
    (defmacro with-suppressed-message (&rest body)
      "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
      (declare (indent 0))
      (let ((message-log-max nil))
        `(with-temp-message (or (current-message) "") ,@body)))

    (setq recentf-save-file "~/.emacs.d/.recentf")
    (setq recentf-max-saved-items 300)             ;; recentf に保存するファイルの数
    (setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
    (setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
    (run-with-idle-timer 30 t '(lambda () (with-suppressed-message (recentf-save-list))))
    ))

(leaf json-rpc
  :doc "JSON-RPC library"
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "emacs>=24.1"
  :url "https://github.com/skeeto/elisp-json-rpc"
  :emacs>= 24.1
  :ensure t)

(leaf jump-back!
  :defun (jump-back! . nil)
  :require ring edmacro
  :preface
  (defun jump-back!--ring-update nil
    (let ((marker (point-marker)))
      (unless jump-back!--marker-ring
        (setq jump-back!--marker-ring (make-ring 30)))
      (ring-insert jump-back!--marker-ring marker)))
  (defun jump-back! nil
    (interactive)
    (if (ring-empty-p jump-back!--marker-ring)
        (error "No further undo information")
      (let ((marker (ring-ref jump-back!--marker-ring 0))
            (repeat-key (vector last-input-event)))
        (ring-remove jump-back!--marker-ring 0)
        (if (=
             (point-marker)
             marker)
            (jump-back!)
          (goto-char marker)
          (message "(Type %s to repeat)"
                   (edmacro-format-keys repeat-key))
          (;; set-temporary-overlay-map
           set-transient-map
           (let ((km (make-sparse-keymap)))
             (define-key km repeat-key 'jump-back!)
             km))))))
  :config
  (defvar-local jump-back!--marker-ring nil)
  (run-with-idle-timer 1 t 'jump-back!--ring-update))

(leaf jupyter
  :disabled t
  ;; :when (executable-find "jupyter")
  :doc "Jupyter"
  :req "emacs-26" "zmq-0.10.3" "cl-lib-0.5" "simple-httpd-1.5.0" "websocket-1.9"
  :tag "emacs>=26"
  :url "https://github.com/dzop/emacs-jupyter"
  :emacs>= 26
  :ensure t
  :config
  (leaf ob-jupyter
    :after ox
    :require t ob-python jupyter
    :commands (org-babel-jupyter-override-src-block)
    :custom ((org-babel-default-header-args:jupyter-julia quote
                                                          ((:async . "yes")
                                                           (:session . "jl")
                                                           (:kernel . "julia-1.0")
                                                           (:cache . "yes")))
             (org-babel-default-header-args:jupyter-python quote
                                                           ((:async . "yes")
                                                            (:session . "py")
                                                            (:kernel . "python3")
                                                            (:cache . "yes"))))
    :config
    (org-babel-jupyter-override-src-block "python")))

(leaf key-chord
  :doc "map pairs of simultaneously pressed keys to commands"
  :req "emacs-24"
  :tag "input" "chord" "keyboard" "emacs>=24"
  :emacs>= 24
  :ensure t
  :global-minor-mode t
  :custom
  ((key-chord-one-keys-delay . 0.02)
   (key-chord-two-keys-delay . 0.03))
  :config
  ;; (key-chord-mode 1)
  (key-chord-define-global "gl" 'goto-line)
  (key-chord-define-global "fk" 'counsel-recentf)
  (key-chord-define-global "fj" 'counsel-M-x)
  (key-chord-define-global "x0" '"\C-x0")
  (key-chord-define-global "x1" '"\C-x1")
  (key-chord-define-global "x2" '"\C-x2")
  (key-chord-define-global "x3" '"\C-x3")
  (key-chord-define-global "x5" '"\C-x52")
  (key-chord-define-global "gr" 'counsel-ag)
  (key-chord-define-global "rl" 'rotate-layout)
  (key-chord-define-global "rw" 'rotate-window)
  ;; (key-chord-define-global "dj" 'dumb-jump-go)
  ;; (key-chord-define-global "db" 'dumb-jump-back)
  (key-chord-define-global "jb" 'jump-back!))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-25.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=25.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :url "https://github.com/emacs-lsp/lsp-mode#supported-languages"
  :url "https://github.com/MaskRay/ccls/wiki/lsp-mode#find-definitionsreferences"
  :emacs>= 25.1
  :ensure t
  :require t tramp
  :custom `((lsp-keymap-prefix . "s-l")
             (gc-cons-threshold \,
               (* 3 1024 1024 1024)
               )  ;; 3GB
             (gcmh-low-cons-threshold \,
               (* 512 1024 1024)
               )  ;; 512MB
             (read-process-output-max \,
               (* 1 1024 1024)
               )  ;; 1MB
             (lsp-diagnostics-modeline-scope . :project)
             ;; debug
             (lsp-auto-guess-root . t)
             (lsp-print-io . nil)
             (lsp-log-io . nil)
             (lsp-trace . nil)
             (lsp-print-performance . nil)
             ;; general
             (lsp-idle-delay . 0.5)
             ;; (lsp-pyls-plugins-flake8-enabled . t)
             (lsp-document-sync-method . 2)
             (lsp-response-timeout . 5)
             (lsp-prefer-flymake . t)
             (lsp-prefer-capf . t)
             (lsp-enable-completion-at-point . nil)
             (lsp-enable-indentation . nil)
             (lsp-restart . 'ignore)
             ;; (lsp-pyls-plugins-flake8-ignore . (E203 E226 E302 E41 E501 E503))
             ;; (lsp-pyls-plugins-flake8-max-line-length . 93)
             )
  :hook (;; (prog-mode-hook . lsp-deferred)
          (lsp-mode-hook . lsp-enable-which-key-integration)
          (lsp-managed-mode-hook . lsp-modeline-diagnostics-mode))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\AtCoder\\'")
  (advice-add 'lsp
    :before (lambda (&rest _args)
              (eval '(setf (lsp-session-server-id->folders
                             (lsp-session))
                       (ht))))
    )

  (leaf lsp-latex
    :doc "lsp-mode client for LaTeX, on texlab"
    :req "emacs-25.1" "lsp-mode-6.0"
    :tag "tex" "languages" "emacs>=25.1"
    :url "https://github.com/ROCKTAKEY/lsp-latex"
    :emacs>= 25.1
    :ensure t
    :config
    (add-hook 'TeX-mode-hook 'lsp)
    ;; (add-hook 'LaTeX-mode-hook 'lsp)
    )

  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-25.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "lsp" "emacs>=25.1"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :emacs>= 25.1
    :ensure t
    :custom (;; lsp-ui-doc
             (lsp-ui-doc-enable . t)
             (lsp-ui-doc-header . t)
             (lsp-ui-doc-include-signature . t)
             (lsp-ui-doc-position . 'top) ;; top, bottom, or at-point
             (lsp-ui-doc-max-width . 150)
             (lsp-ui-doc-max-height . 30)
             (lsp-ui-doc-use-childframe . t)
             (lsp-ui-doc-use-webkit . t)
             ;; lsp-ui-flycheck
             (lsp-ui-flycheck-enable . nil)
             ;; lsp-ui-sideline
             (lsp-ui-sideline-enable . nil)
             (lsp-ui-sideline-ignore-duplicate . t)
             (lsp-ui-sideline-show-symbol . t)
             (lsp-ui-sideline-show-hover . t)
             (lsp-ui-sideline-show-diagnostics . nil)
             (lsp-ui-sideline-show-code-actions . nil)
             ;; lsp-ui-imenu
             (lsp-ui-imenu-enable . nil)
             (lsp-ui-imenu-kind-position . 'top)
             ;; lsp-ui-peek
             (lsp-ui-peek-enable . t)
             (lsp-ui-peek-peek-height . 20)
             (lsp-ui-peek-list-width . 50)
             (lsp-ui-peek-fontify . 'on-demand) ;; never, on-demand, or always
             )
    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    ((lsp-mode-map
      ("C-c C-r" . lsp-ui-peek-find-references)
      ("C-c C-j" . lsp-ui-peek-find-definitions)
      ("C-c i"   . lsp-ui-peek-find-implementation)
      ("C-c m"   . lsp-ui-imenu)
      ("C-c s"   . lsp-ui-sideline-mode)
      ("C-c d"   . ladicle/toggle-lsp-ui-doc)))
    :hook
    (lsp-mode-hook . lsp-ui-mode)
    :config
    ;; (defun disable-flycheck-mode ()
    ;;   (interactive)
    ;;   (flycheck-mode -1))
    ;; (add-hook 'prog-mode-hook 'disable-flycheck-mode)
    )

  (leaf lsp-ivy
    :doc "LSP ivy integration"
    :req "emacs-25.1" "dash-2.14.1" "lsp-mode-6.2.1" "ivy-0.13.0"
    :tag "debug" "languages" "emacs>=25.1"
    :url "https://github.com/emacs-lsp/lsp-ivy"
    :emacs>= 25.1
    :ensure t
    :after ivy)
  )

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :emacs>= 25.1
  :ensure t
  :bind (("C-c m" . magit-status))
  :custom ((magit-bury-buffer-function quote magit-mode-quit-window)
            (magit-buffer-name-format . "%x%M%v: %t%x")
            (magit-refresh-verbose . t)
            (magit-commit-ask-to-stage quote stage)
            (magit-clone-set-remote\.pushDefault . t)
            (magit-clone-default-directory . "~/drive/github.com/")
            (magit-remote-add-set-remote\.pushDefault quote ask))
  ;; :config
  ;; (set-default 'magit-stage-all-confirm nil)
  ;; (set-default 'magit-unstage-all-confirm nil)
  ;; (remove-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  ;; ;; Restores "normal" behavior in branch view (when hitting RET)
  ;; (setq magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))
  ;; (setq git-commit-finish-query-functions nil)
  ;; (setq magit-visit-ref-create 1)
  ;; (setq magit-revision-show-gravatars nil)
  )

(leaf window-numbering
  :when window-system
  :doc "Numbered window shortcuts"
  :tag "matching" "faces"
  :url "http://nschum.de/src/emacs/window-numbering-mode/"
  :ensure t
  :config
  (window-numbering-mode 1))

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs."
  :req "cl-lib-0.5"
  :tag "cursors" "editing"
  :ensure t
  :preface
  (defun mc/edit-lines-or-string-rectangle (s e)
    "C-x r tで同じ桁の場合にmc/edit-lines (C-u M-x mc/mark-all-dwim)"
    (interactive "r")
    (if (eq (save-excursion (goto-char s) (current-column))
            (save-excursion (goto-char e) (current-column)))
        (call-interactively 'mc/edit-lines)
      (call-interactively 'string-rectangle)))

  (defun mc/mark-all-dwim-or-mark-sexp (arg)
    "C-u C-M-SPCでmc/mark-all-dwim, C-u C-u C-M-SPCでC-u M-x mc/mark-all-dwim"
    (interactive "p")
    (cl-case arg
      (16 (mc/mark-all-dwim t))
      (4 (mc/mark-all-dwim nil))
      (1 (mark-sexp 1))))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M-SPC" . mc/mark-all-dwim-or-mark-sexp)
         )
  )

(leaf mwim
  :doc "Switch between the beginning/end of line or code"
  :tag "convenience"
  :url "https://github.com/alezost/mwim.el"
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(leaf my-window-resizer
  :defun (my-window-resizer . nil)
  :defvar (last-command-char
           current-height
           current-width
           window-obj)
  :doc "Control window size and position."
  :bind (("C-x r" . my-window-resizer))
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

(leaf oj
  :doc "Competitive programming tools client for AtCoder, Codeforces"
  :req "emacs-26.1" "quickrun-2.2"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/conao3/oj.el"
  :emacs>= 26.1
  :ensure t
  :custom ((oj-default-online-judge quote atcoder)
           (oj-compiler-python . "cpython")
           (oj-home-dir . "~/drive/work/coder/AtCoder")
           (oj-submit-args quote
             ("-y" "--wait=0"))))

(leaf org
  :doc "Export Framework for Org Mode"
  :tag "builtin"
  :ensure org-plus-contrib
  :custom
  ((org-directory . "~/org/")
    (org-ellipsis . " ▼ ")
    (org-adapt-indentation . nil)
    (org-habit-show-habits-only-for-today . t)
    (org-startup-indented . nil)
    (org-use-speed-commands . t)
    (org-enforce-todo-dependencies . t)
    (org-log-done . t)
    (org-return-follows-link . t)
    (org-image-actual-width)
    (org-highlight-latex-and-related quote
      (latex script entities))
    (org-src-window-setup . 'current-window)
    (org-return-follows-link . t)
    (org-babel-load-languages . '((emacs-lisp . t)
                                   (python . t)
                                   (latex . t)
                                   ;; (ipython . t)
                                   ))
    (org-confirm-babel-evaluate . nil)
    (org-catch-invisible-edits . 'show)
    (org-preview-latex-image-directory . "~/tmp/ltximg/")
    (org-structure-template-alist . '(("b" . "src sh")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("p" . "src python :results output
")
                                       ("d" . "definition")
                                       ("t" . "theorem")
                                       ("mc" . "quoting")
                                       ("mq" . "question")
                                       ("mt" . "todo")
                                       ("ms" . "summary")
                                       ))
    (search-highlight . t)
    (search-whitespace-regexp . ".*?")
    (isearch-lax-whitespace . t)
    (isearch-regexp-lax-whitespace . nil)
    (isearch-lazy-highlight . t)
    (isearch-lazy-count . t)
    (lazy-count-prefix-format . " (%s/%s) ")
    (lazy-count-suffix-format . nil)
    (isearch-yank-on-move . 'shift)
    (isearch-allow-scroll . 'unlimited)
    (org-show-notification-handler . '(lambda (msg) (timed-notification nil msg)))
    ) ;; end custom
  :commands (org-with-remote-undo)
  :config
  (custom-theme-set-faces
    'user
    '(org-block ((t (:inherit fixed-pitch))))
    '(org-code ((t (:inherit (shadow fixed-pitch)))))
    '(org-document-info ((t (:foreground "dark orange"))))
    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
    '(org-link ((t (:foreground "royal blue" :underline t))))
    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-property-value ((t (:inherit fixed-pitch))) t)
    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.9))))
    '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
    '(org-agenda-current-time ((t (:foreground "chartreuse"))))
    '(org-agenda-done ((t (:foreground "gray" :weight book))))
    '(org-scheduled-today ((t (:foreground "orange" :weight book))))
    '(org-agenda-date ((t (:foreground "forest green" :height 1.1))))
    '(org-agenda-date-today ((t (:foreground "#98be65" :height 1.1))))
    )

  (custom-set-faces
    '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
    '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
    '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
    '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
    '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
    )

  ;; ;; fix color handling in org-preview-latex-fragment
  ;; (let ((dvipng--plist (alist-get 'dvipng org-preview-latex-process-alist)))
  ;;   (plist-put dvipng--plist :use-xcolor t)
  ;;   (plist-put dvipng--plist :image-converter '("dvipng -D %D -T tight -o %O %f")))
  (setq org-format-latex-options
    '(:foreground default
       :background default
       :scale 1.7
       :html-foreground "Black"
       :html-background "Transparent"
       :html-scale 1.0
       :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
    )

  (when (fboundp 'mac-toggle-input-method)
    (run-with-idle-timer 1 t 'ns-org-heading-auto-ascii))

  (defun ns-org-heading-auto-ascii ()
    (when (and window-focus-p
	           (eq major-mode 'org-mode)
	           (or (looking-at org-heading-regexp)
		           (equal (buffer-name) org-agenda-buffer-name)))
      (my:ime-off)))

  (defun my:ime-off ()
    (interactive)
    (mac-toggle-input-method nil)
    (run-hooks 'my:ime-off-hook))

  (leaf *terminal-notifier
    :when window-system
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Terminal notifier
    ;; requires 'brew install terminal-notifier'
    ;; stolen from erc-notifier
    :preface
    (defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")

    ;; (terminal-notifier-notify "Emacs notification" "Something amusing happened")

    (defun terminal-notifier-notify (title message)
      "Show a message with
        terminal-notifier-command
    ."
      (start-process "terminal-notifier"
                     "terminal-notifier"
                     terminal-notifier-command
                     "-title" title
                     "-message" message
                     "-activate" "org.gnu.Emacs"))

    (defun timed-notification (time msg)
      (interactive "sNotification when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
      (run-at-time time nil (lambda (msg) (terminal-notifier-notify "Emacs" msg)) msg))
    )


  (defun jethro/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))

  (setq jethro/org-agenda-directory (file-truename "~/org/gtd/"))
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
           "* TODO %?")
          ("w" "Weekly Review" entry (file+olp+datetree ,(concat jethro/org-agenda-directory "reviews.org"))
           (file ,(concat jethro/org-agenda-directory "templates/weekly_review.org")))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil)

  (setq org-tag-alist '(("@errand" . ?e)
                        ("@office" . ?o)
                        ("@home" . ?h)
                        ("@private" . ?p)
                        (:newline)
                        ("CANCELLED" . ?c)))

  (setq org-fast-tag-selection-single-key nil)
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files . (:level . 1))))

  (defvar jethro/org-agenda-bulk-process-key ?f
    "Default key for bulk processing inbox items.")

  (defun jethro/org-process-inbox ()
    "Called in org-agenda-mode, processes all inbox items."
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
    (jethro/bulk-process-entries))

  (defvar jethro/org-current-effort "1:00"
    "Current effort for agenda items.")


  (defun jethro/my-org-agenda-set-effort (effort)
    "Set the effort property for the current headline."
    (interactive
     (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
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

  (defun jethro/org-inbox-capture ()
    (interactive)
    "Capture a task in agenda mode."
    (org-capture nil "i"))

  (setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

  (defun jethro/set-todo-state-next ()
    "Visit each parent task and change NEXT states to TODO"
    (org-todo "NEXT"))

  (add-hook 'org-clock-in-hook 'jethro/set-todo-state-next 'append))

(leaf org-agenda
  :require t
  :preface
  (defun jethro/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :bind* (("C-c C-a" . jethro/switch-to-agenda)
          ("C-c C-m" . jethro/org-inbox-capture))
  :bind (org-agenda-mode-map
          :package org-agenda
          ("i" . org-agenda-clock-in)
          ("r" . jethro/org-agenda-process-inbox-item)
          ("R" . org-agenda-refile)
          ("c" . jethro/org-inbox-capture)
          ("q" . quit-window))
  :custom (org-agenda-window-setup . 'current-window)
  :config
  (defun org-agenda-cache (&optional regenerate)
    "agendaを更新せずに表示する。"
    (interactive "P")
    (when (or regenerate (null (get-buffer "*Org Agenda*")))
      ;; "a" は org-agenda-custom-commands で常用する文字
      (setq current-prefix-arg nil)
      (org-agenda nil "a"))
    (switch-to-buffer "*Org Agenda*")
    (delete-other-windows))

  (require 'org-habit)
  (add-to-list 'org-export-backends 'latex)

  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t
        ;; 今日から予定を表示させる
        org-agenda-start-on-weekday nil)

  (setq org-agenda-current-time-string "← now")
  (setq org-agenda-time-grid ;; Format is changed from 9.1
        '((daily today require-timed)
          (0800 01000 1200 1400 1600 1800 2000 2200 2400)
          "-"
          "────────────────"))

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

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ;; ((org-agenda-prefix-format
                                      ;;   '((agenda . " %i %-12:c%?- t % s % e"))))
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)
                                                (org-agenda-prefix-format " %i %-12:c%?- t % s % e")
                                                ))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Inbox")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "inbox.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                               (org-agenda-files '(,(concat jethro/org-agenda-directory "projects.org")
                                                                    ,(concat org-directory "braindump/org/concepts/research.org")
                                                                    ,(concat jethro/org-agenda-directory "daily.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-skip-function #'jethro/skip-projects)
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "projects.org")
                                                                    ,(concat org-directory "braindump/org/concepts/research.org")
                                                                    ,(concat jethro/org-agenda-directory "daily.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "next.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))))))))
  )


(leaf org-journal
  :disabled t
  :doc "a simple org-mode based journaling mode"
  :req "emacs-25.1" "org-9.1"
  :tag "emacs>=25.1"
  :url "http://github.com/bastibe/org-journal"
  :emacs>= 25.1
  :ensure t
  :after org
  ;; :init
  ;;  ;; Change default prefix key; needs to be set before loading org-journal
  :bind (("C-c j n" . org-journal-new-entry)
          ("C-c j s" . org-journal-search))
  :custom ((org-journal-dir . jethro/org-agenda-directory)
             (org-journal-date-format . "%Y-%m-%d, %A")
             (org-journal-file-format . "daily.org")
             (org-journal-enable-agenda-integration . t)
             ;; org-journal-dir (concat org-directory "journal/")
             ;; org-journal-file-format "%Y%m%d.org"
             )
  ;;
  ;; (defun org-journal-file-header-func (time)
  ;;   "Custom function to create journal header."
  ;;   (concat
  ;;     (pcase org-journal-file-type
  ;;       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
  ;;       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
  ;;       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
  ;;       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

  ;; (setq org-journal-file-header 'org-journal-file-header-func)
  )

(leaf org-download
  :when window-system
  :doc "Image drag-and-drop for Org-mode."
  :req "emacs-24.3" "async-1.2"
  :tag "download" "screenshots" "images" "multimedia" "emacs>=24.3"
  :url "https://github.com/abo-abo/org-download"
  :emacs>= 24.3
  :ensure t
  :custom
  ((org-download-image-dir . "imgs"))
  :config
  (with-eval-after-load "org"
    (defun org-insert-clipboard-image ()
      "Generate png file from a clipboard image and insert a link to current buffer."
      (interactive)
      (let* ((filename
               (concat (file-name-nondirectory (buffer-file-name))
                 "imgs/"
                 (format-time-string "%Y%m%d_%H%M%S")
                 ".png")))
        (unless (file-exists-p (file-name-directory filename))
          (make-directory (file-name-directory filename)))
        (shell-command (concat "pngpaste " filename))
        (if (file-exists-p filename)
          (insert (concat "[[file:" filename "]]")))
        (org-display-inline-images)))

    (global-set-key (kbd "C-M-y") 'org-insert-clipboard-image))

  (defcustom org-limit-image-size '(0.8 . 0.4) "Maximum image size") ;; integer or float or (width-int-or-float . height-int-or-float)

  (defun org-limit-image-size--get-limit-size (width-p)
    (let ((limit-size (if (numberp org-limit-image-size)
                        org-limit-image-size
                        (if width-p (car org-limit-image-size)
                          (cdr org-limit-image-size)))))
      (if (floatp limit-size)
        (ceiling (* limit-size (if width-p (frame-text-width) (frame-text-height))))
        limit-size)))

  (defvar org-limit-image-size--in-org-display-inline-images nil)

  (defun org-limit-image-size--create-image
    (old-func file-or-data &optional type data-p &rest props)

    (if (and org-limit-image-size--in-org-display-inline-images
          org-limit-image-size
          (null type)
          ;;(image-type-available-p 'imagemagick) ;;Emacs27 support scaling by default?
          (null (plist-get props :width)))
      ;; limit to maximum size
      (apply
        old-func
        file-or-data
        (if (image-type-available-p 'imagemagick) 'imagemagick)
        data-p
        (plist-put
          (plist-put
            (org-plist-delete props :width) ;;remove (:width nil)
            :max-width (org-limit-image-size--get-limit-size t))
          :max-height (org-limit-image-size--get-limit-size nil)))

      ;; default
      (apply old-func file-or-data type data-p props)))

  (defun org-limit-image-size--org-display-inline-images (old-func &rest args)
    (let ((org-limit-image-size--in-org-display-inline-images t))
      (apply old-func args)))

  (defun org-limit-image-size-activate ()
    (interactive)
    (advice-add #'create-image :around #'org-limit-image-size--create-image)
    (advice-add #'org-display-inline-images :around #'org-limit-image-size--org-display-inline-images))

  (defun org-limit-image-size-deactivate ()
    (interactive)
    (advice-remove #'create-image #'org-limit-image-size--create-image)
    (advice-remove #'org-display-inline-images #'org-limit-image-size--org-display-inline-images))

  (add-hook 'org-mode-hook 'org-limit-image-size-activate)
  )

(leaf org-ql
  :doc "Org Query Language, search command, and agenda-like view"
  :req "emacs-26.1" "dash-2.13" "dash-functional-1.2.0" "f-0.17.2" "map-2.1" "org-9.0" "org-super-agenda-1.2" "ov-1.0.6" "peg-1.0" "s-1.12.0" "transient-0.1" "ts-0.2.-1"
  :tag "agenda" "org" "outlines" "hypermedia" "emacs>=26.1"
  :url "https://github.com/alphapapa/org-ql"
  :emacs>= 26.1
  :ensure t
  :after map org org-super-agenda peg ts)
(leaf org-super-agenda
  :doc "Supercharge your agenda"
  :req "emacs-26.1" "s-1.10.0" "dash-2.13" "org-9.0" "ht-2.2" "ts-0.2"
  :tag "agenda" "org" "outlines" "hypermedia" "emacs>=26.1"
  :url "http://github.com/alphapapa/org-super-agenda"
  :emacs>= 26.1
  :ensure t
  :after org ts)
(leaf org-analyzer
  :doc "org-analyzer is a tool that extracts time tracking data from org files."
  :tag "calendar"
  :url "https://github.com/rksm/clj-org-analyzer"
  :ensure t)
(leaf org-edna
  :doc "Extensible Dependencies 'N' Actions"
  :req "emacs-25.1" "seq-2.19" "org-9.0.5"
  :tag "org" "text" "convenience" "emacs>=25.1"
  :url "https://savannah.nongnu.org/projects/org-edna-el/"
  :emacs>= 25.1
  :ensure t
  :after org)

(leaf org-ref
  :doc "citations, cross-references and bibliographies in org-mode"
  :req "dash-2.11.0" "htmlize-1.51" "helm-1.5.5" "helm-bibtex-2.0.0" "ivy-0.8.0" "hydra-0.13.2" "key-chord-0" "s-1.10.0" "f-0.18.0" "pdf-tools-0.7"
  :tag "label" "ref" "cite" "org-mode"
  :url "https://github.com/jkitchin/org-ref"
  :ensure t
  :after helm helm-bibtex ivy hydra key-chord pdf-tools)

(leaf org-bullets
  :doc "Show bullets in org-mode as UTF-8 characters"
  :url "https://github.com/integral-dw/org-bullets"
  :ensure t
  :custom ((org-bullets-bullet-list . '("" "" "" "" "" "" "" "" "" "")))
  :hook (org-mode-hook . org-bullets-mode)
  )

(leaf org-contrib
  :doc "Outline-based notes management and organizer"
  :tag "builtin"
  :config
  (leaf ox
    :doc "Export Framework for Org Mode"
    :tag "out-of-MELPA" "wp" "calendar" "hypermedia" "outlines"
    :custom ((org-export-backends quote
                                  (ascii html latex beamer odt org extra)))
    :config
    (leaf ox-extra
      :doc "Convenience functions for org export"
      :tag "out-of-MELPA"
      :added "2020-03-26"
      :commands (ox-extras-activate)
      :config
      (ox-extras-activate
       '(latex-header-blocks ignore-headlines)))

    (leaf ox-hugo
      :doc "Hugo Markdown Back-End for Org Export Engine"
      :req "emacs-24.4" "org-9.0"
      :tag "docs" "markdown" "org" "emacs>=24.4"
      :url "https://ox-hugo.scripter.co"
      :emacs>= 24.4
      :ensure t
      :after org org-roam
      :config
      (defun jethro/conditional-hugo-enable ()
        (save-excursion
          (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
              (org-hugo-auto-export-mode +1)
            (org-hugo-auto-export-mode -1))))
      (add-hook 'org-mode-hook #'jethro/conditional-hugo-enable))

    (leaf ox-latex
      :disabled nil
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

        (setq org-latex-pdf-process (list (my-latexmk-command TeX-engine "-pvc" "%f" "%o")))
        )))

  (leaf *org-patch
    :disabled t
    :config
    (leaf *org-patch-src
      :disabled t
      :defvar (org-eldoc-local-functions-cache)
      :defun ((org-src--get-lang-mode . org))
      :preface
      (defun c/org-eldoc-get-mode-local-documentation-function (f &rest args)
        "Check if LANG-mode sets eldoc-documentation-function and return its value."
        (let ((lang (nth 0 args)))
          (let ((cached-func (gethash lang org-eldoc-local-functions-cache 'empty))
                (mode-func (org-src--get-lang-mode lang))
                doc-func)
            (if (eq 'empty cached-func)
                (when (fboundp mode-func)
                  (with-temp-buffer
                    (funcall mode-func)
                    (setq doc-func (and eldoc-documentation-function
                                        (symbol-value 'eldoc-documentation-function)))
                    (puthash lang doc-func org-eldoc-local-functions-cache))
                  doc-func)
              cached-func))))

      :advice (:around org-eldoc-get-mode-local-documentation-function c/org-eldoc-get-mode-local-documentation-function))

    (leaf *org-patch-split-horizontal
      :disabled t
      :preface
      (defun c/org-export-dispatch-done nil
        (when (eq this-command 'org-export-dispatch)
          (delete-window))
        (remove-hook 'post-command-hook #'c/org-export-dispatch-done))

      (defun c/moom-org-export-dispatch-done nil
        (when (eq this-command 'org-export-dispatch)
          (moom-delete-windows))
        (remove-hook 'post-command-hook #'c/moom-org-export-dispatch-done))

      (defun c/org-export-dispatch (f ARG)
        (interactive "P")
        (cond
         (org-export-dispatch-use-expert-ui
          (apply f ARG))
         ((eq
           (frame-width)
           80)
          (if (require 'moom nil t)
              (let ((moom-verbose nil))
                (add-hook 'post-command-hook #'c/moom-org-export-dispatch-done)
                (moom-split-window)
                (apply f ARG))
            (apply f ARG)))
         ((>
           (frame-width)
           160)
          (add-hook 'post-command-hook #'c/org-export-dispatch-done)
          (split-window-right)
          (apply f ARG))
         (t
          (apply f ARG))))

      :advice (:around org-export-dispatch c/org-export-dispatch))))

(leaf org-roam
  :doc "Roam Research replica with Org-mode"
  :req "emacs-26.1" "dash-2.13" "f-0.17.2" "s-1.12.0" "org-9.3" "emacsql-3.0.0" "emacsql-sqlite3-1.0.2"
  :tag "convenience" "roam" "org-mode" "emacs>=26.1"
  :url "https://github.com/org-roam/org-roam"
  :emacs>= 26.1
  :ensure t
  :require t company
  :global-minor-mode org-roam-mode
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :bind ( ("C-c n l" . org-roam)
         ("C-c n i" . org-roam-insert)
         ("C-c n b" . org-roam-switch-to-buffer)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-show-graph)
         ("C-c n c" . org-roam-capture))
  :init
  (setq org-roam-directory (file-truename "~/org/braindump/org/")
        ;; org-roam-db-location "~/.doom.d/org-roam.db"
        org-roam-db-location (concat org-roam-directory "org-roam.db")
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher "private"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
      '(("l" "lit" plain (function org-roam--capture-get-point)
         "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:../preferences/hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :head "#+setupfile:../preferences/hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :head "#+setupfile:../preferences/hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:../preferences/hugo_setup.org
#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}
- source :: ${ref}"
           :unnarrowed t)))
  ;; (add-to-list 'company-backends 'org-mode)
  (add-to-list 'company-backends '(company-capf))
  ;; (set-company-backend! 'org-mode '(company-capf))

  (org-roam-mode)

  (leaf org-roam-bibtex
    :doc "Org Roam meets BibTeX"
    :req "emacs-27.1" "org-roam-1.2.2" "bibtex-completion-2.0.0"
    :tag "wp" "outlines" "hypermedia" "bib" "emacs>=27.1"
    :url "https://github.com/org-roam/org-roam-bibtex"
    :emacs>= 27.1
    :ensure t
    :after org-roam bibtex-completion))

(leaf paren
  :hook
  (after-init-hook . show-paren-mode)
  :custom-face
  ((show-paren-match . '((nil (:background "#44475a" :foreground "#f1fa8c")))))
  :custom
  ((show-paren-style . 'mixed)
   (show-paren-when-point-inside-paren . t)
   (show-paren-when-point-in-periphery . t)))

(leaf posframe
  :when window-system
  :doc "Pop a posframe (just a frame) at point"
  :req "emacs-26"
  :tag "tooltip" "convenience" "emacs>=26"
  :url "https://github.com/tumashu/posframe"
  :emacs>= 26
  :ensure t)

(leaf python-mode
  :doc "Python major mode"
  :tag "oop" "python" "processes" "languages"
  :url "https://gitlab.com/groups/python-mode-devs"
  :ensure t
  :custom
  ((python-indent-guess-indent-offset . t)
    (python-indent-guess-indent-offset-verbose . nil))
  :config
  (leaf pythonic
    :doc "Utility functions for writing pythonic emacs package."
    :req "emacs-25.1" "s-1.9" "f-0.17.2"
    :tag "emacs>=25.1"
    :url "https://github.com/proofit404/pythonic"
    :emacs>= 25.1
    :ensure t
    :config
    (leaf conda
      :doc "Work with your conda environments"
      :req "emacs-24.4" "pythonic-0.1.0" "dash-2.13.0" "s-1.11.0" "f-0.18.2"
      :tag "conda" "environment" "python" "emacs>=24.4"
      :added "2021-04-10"
      :url "http://github.com/necaris/conda.el"
      :after lsp-pyright
      :emacs>= 24.4
      :ensure t
      :require t
      :config
      (cond
        (window-system
          (custom-set-variables
            '(conda-anaconda-home (expand-file-name "~/miniconda/"))
            '(conda-env-home-directory (expand-file-name "~/miniconda/"))))
        (t
          (custom-set-variables
            '(conda-anaconda-home (expand-file-name "~/miniconda3/"))
            '(conda-env-home-directory (expand-file-name "~/miniconda3/"))))
        )
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)
      ;; these hooks can't go in the :hook section since lsp-restart-workspace
      ;; is not available if lsp isn't active
      ;; (add-hook 'conda-postactivate-hook (lambda () (lsp-restart-workspace)))
      ;; (add-hook 'conda-postdeactivate-hook (lambda () (lsp-restart-workspace)))
      (add-hook 'conda-postactivate-hook (lambda () (lsp-pyright-setup-when-conda)))
      (add-hook 'conda-postdeactivate-hook (lambda () (lsp-pyright-setup-when-conda)))
      )
    )

  (leaf lsp-pyright
    :doc "Python LSP client using Pyright"
    :req "emacs-26.1" "lsp-mode-7.0" "dash-2.18.0" "ht-2.0"
    :tag "lsp" "tools" "languages" "emacs>=26.1"
    :url "https://github.com/emacs-lsp/lsp-pyright"
    :emacs>= 26.1
    :ensure t
    :require t
    :defvar lsp-pyright-venv-path
    :init
    (defun lsp-pyright-setup-when-conda ()
      (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
      (lsp-restart-workspace))
    :hook (python-mode-hook . (lambda ()
                                (setq
                                  indent-tabs-mode nil
                                  python-indent 4
                                  tab-width 4)
                                (require 'lsp-pyright)
                                (lsp)
                                ))
    :custom
    (lsp-pyright-disable-organize-imports . t)
    (lsp-pyright-disable-language-services . t)
    )

  (defadvice python-shell-completion-at-point (around fix-company-bug activate)
    "python-shell-completion-at-point breaks when point is before the prompt"
    (when (or (not comint-last-prompt)
              (>= (point) (cdr comint-last-prompt)))
      ad-do-it))
  )

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :tag "tools" "lisp" "convenience" "faces"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :ensure t
  :hook ((prog-mode-hook . rainbow-delimiters-mode)))

(leaf rainbow-mode
  :doc "Colorize color names in buffers"
  :tag "faces"
  :url "http://elpa.gnu.org/packages/rainbow-mode.html"
  :ensure t
  :blackout t
  :custom ((rainbow-html-colors-major-mode-list quote
                                                (css-mode html-mode php-mode nxml-mode xml-mode))
           (rainbow-x-colors-major-mode-list quote
                                             (emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode))
           (rainbow-latex-colors-major-mode-list quote
                                                 (latex-mode))
           (rainbow-ansi-colors-major-mode-list quote
                                                (sh-mode c-mode c++-mode))
           (rainbow-r-colors-major-mode-list quote
                                             (ess-mode)))
  :hook (lisp-interaction-mode-hook emacs-lisp-mode-hook web-mode-hook))

(leaf rotate
  :doc "Rotate the layout of emacs"
  :tag "layout" "window"
  :url "https://github.com/daichirata/emacs-rotate"
  :ensure t)

(leaf server
  :doc "Lisp code for GNU Emacs running as server process"
  :tag "builtin"
  :require server
  :commands (server-running-p server-start)
  :bind ("C-x C-c" . server-edit)
  :config
  (unless (server-running-p)
    (server-start)
    (defun iconify-emacs-when-server-is-done ()
      (unless server-clients (iconify-frame)))
    )
  )

(leaf super-save
  :diminish
  :doc "Auto-save buffers, based on your activity."
  :req "emacs-24.4"
  :tag "convenience" "emacs>=24.4"
  :url "https://github.com/bbatsov/super-save"
  :emacs>= 24.4
  :ensure t
  :require t
  :custom ((super-save-auto-save-when-idle . t)
           (super-save-idle-duration . 10))
  :defvar (super-save-triggers super-save-hook-triggers)
  :hook (after-init-hook . super-save-mode)
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(leaf transient
  :doc "Transient commands"
  :req "emacs-25.1"
  :tag "bindings" "emacs>=25.1"
  :url "https://github.com/magit/transient"
  :emacs>= 25.1
  :ensure t
  :custom ((transient-detect-key-conflicts . t)))

(leaf transient-dwim
  :doc "Useful preset transient commands"
  :req "emacs-26.1" "transient-0.1.0"
  :tag "conao3" "conao3-dev" "out-of-MELPA"
  :tag "tools" "emacs>=26.1"
  :url "https://github.com/conao3/transient-dwim.el"
  :emacs>= 26.1
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

(leaf undo-tree
  :doc "Treat undo history as a tree"
  :tag "tree" "history" "redo" "undo" "files" "convenience"
  :url "http://www.dr-qubit.org/emacs.php"
  :ensure t
  :bind ("M-/" . undo-tree-redo)
  :custom
  ((undo-limit . 80000)
   (undo-strong-limit . 120000)
   (undo-outer-limit . 360000)
   (undo-tree-limit . 80000)
   (undo-tree-strong-limit . 120000)
   (undo-tree-outer-limit . 360000))
  :config
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".zip")))
  )

(leaf volatile-highlights
  :diminish
  :doc "Minor mode for visual feedback on some operations."
  :tag "wp" "convenience" "emulations"
  :url "http://www.emacswiki.org/emacs/download/volatile-highlights.el"
  :ensure t
  :hook
  ((after-init-hook . volatile-highlights-mode))
  :custom-face
  ((vhl/default-face . '((nil (:foreground "#FF3333" :background "#FFCDCD"))))))

;; (leaf ob-ipython
;;   :doc "org-babel functions for IPython evaluation"
;;   :req "s-1.9.0" "dash-2.10.0" "dash-functional-1.2.0" "f-0.17.2" "emacs-24"
;;   :tag "reproducible research" "literate programming" "emacs>=24"
;;   :added "2021-01-05"
;;   :url "http://www.gregsexton.org"
;;   :emacs>= 24
;;   :ensure t)

(leaf wgrep
  :doc "Writable grep buffer and apply the changes to files"
  :tag "extensions" "edit" "grep"
  :added "2021-01-14"
  :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el"
  :ensure t
  :custom ((wgrep-enable-key . "e")
           (wgrep-auto-save-buffer . t)
           (wgrep-change-readonly-file . t)))

(leaf winner
  :doc "Restore old window configurations"
  :tag "builtin"
  :bind (("C-x <right>" . winner-redo)
         ("C-x <left>" . winner-undo))
  :config
  (winner-mode))

(leaf xref
  :doc "Cross-referencing commands"
  :req "emacs-26.3"
  :tag "emacs>=26.3"
  :url "http://elpa.gnu.org/packages/xref.html"
  :emacs>= 26.3
  :ensure t)

(leaf yasnippet
  :ensure t
  :blackout yas-minor-mode
  :custom ((yas-indent-line . 'fixed)
           (yas-global-mode . t))
  :bind ((yas-keymap
          ("<tab>" . nil))            ; conflict with company
         (yas-minor-mode-map
          ("C-c y i" . yas-insert-snippet)
          ("C-c y n" . yas-new-snippet)
          ("C-c y v" . yas-visit-snippet-file)
          ("C-c y l" . yas-describe-tables)
          ("C-c y g" . yas-reload-all)))
  :defun (company-mode/backend-with-yas)
  :config
  (leaf yasnippet-snippets :ensure t)
  (leaf yatemplate
    :ensure t
    :config
    (yatemplate-fill-alist))

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
      (append (if (consp backend) backend (list backend))
        '(:with company-yasnippet))))
  (defun set-yas-as-company-backend ()
    (setq company-backends (mapc #'company-mode/backend-with-yas company-backends))
    )
  :hook
  ((company-mode-hook . set-yas-as-company-backend))
  )

(leaf *font
  :config
  (leaf *font-setting
    :when window-system
    :config
    ;; フォントセットを作る
    ;; (leaf font-setting
    ;;   :disabled nil
    ;;   :when window-system
    ;;   :config
    ;;                                 ;(set-face-attribute 'default nil :family "Source Code Pro" :height 100)
    ;;   (set-face-attribute 'default nil :family "Consolas" :height 100)
    ;;                                 ;(set-fontset-font nil '(#x0000 . #x0080) (font-spec :family "Consolas" :size 14))
    ;;                                 ;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished Discord" :size 11))
    ;;   (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Yu Gothic UI" :size 12))
    ;;   (add-to-list 'face-font-rescale-alist '(".*Yu Gothic UI*" . 0.95)))

    (let* ((fontset-name "myfonts") ; フォントセットの名前
            (size 14) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
            (asciifont "JetBrains Mono") ; ASCIIフォント
            (jpfont "Noto Serif CJK JP") ; 日本語フォント
            (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
            (fontspec (font-spec :family asciifont))
            (jp-fontspec (font-spec :family jpfont))
            (fsn (create-fontset-from-ascii-font font nil fontset-name)))
      (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
      (set-fontset-font fsn '(#x0080 . #x024F) fontspec)    ; 分音符付きラテン
      (set-fontset-font fsn '(#x0370 . #x03FF) fontspec)    ; ギリシャ文字
      ;; )  ;; commented 2021/05/29

      ;; デフォルトのフレームパラメータでフォントセットを指定
      (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))
      )  ;; add 2021/05/29

    ;; デフォルトフェイスにフォントセットを設定
    ;; # これは起動時に default-frame-alist に従ったフレームが作成されない現象への対処
    (set-face-font 'default "fontset-myfonts"))

  ;; Ligatureの設定 (対応フォント限定: Fira Code や JetBrains Mono)
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                  (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                  (36 . ".\\(?:>\\)")
                  (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                  (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                  (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                  (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                  (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                  (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                  (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                  (48 . ".\\(?:x[a-zA-Z]\\)")
                  (58 . ".\\(?:::\\|[:=]\\)")
                  (59 . ".\\(?:;;\\|;\\)")
                  (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                  (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                  (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                  (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                  (91 . ".\\(?:]\\)")
                  (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                  (94 . ".\\(?:=\\)")
                  (119 . ".\\(?:ww\\)")
                  (123 . ".\\(?:-\\)")
                  (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                  (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                  )
          ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
        `([,(cdr char-regexp) 0 font-shape-gstring]))))
  )


(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here

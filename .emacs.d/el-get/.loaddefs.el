;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "flymake-posframe/flymake-posframe" "flymake-posframe/flymake-posframe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flymake-posframe/flymake-posframe.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flymake-posframe/flymake-posframe" '("flymake-posframe-")))

;;;***

;;;### (autoloads nil "pyenv.el/pyenv" "pyenv.el/pyenv.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from pyenv.el/pyenv.el

(autoload 'pyenv-use-global "pyenv.el/pyenv" "\
Activate pyenv global python." t nil)

(autoload 'pyenv-use-corresponding "pyenv.el/pyenv" "\
Search for .python-version and activate the corresponding python." t nil)

(autoload 'pyenv-use "pyenv.el/pyenv" "\
Choose what PYTHON-VERSION you want to activate, using an optional ALIAS for display.

\(fn PYTHON-VERSION &optional ALIAS)" t nil)

(defvar global-pyenv-mode nil "\
Non-nil if Global Pyenv mode is enabled.
See the `global-pyenv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pyenv-mode'.")

(custom-autoload 'global-pyenv-mode "pyenv.el/pyenv" nil)

(autoload 'global-pyenv-mode "pyenv.el/pyenv" "\
use pyenv to configure the python version used by your Emacs.

If called interactively, enable Global Pyenv mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pyenv.el/pyenv" '("pyenv-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here

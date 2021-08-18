((el-get status "required")
 (emacs-mirror/emacs/blob/master/lisp/org/org-macs status "required" recipe nil)
 (flymake-posframe status "installed" recipe
                   (:name flymake-posframe :type github :pkgname "Ladicle/flymake-posframe" :after nil))
 (ligature status "required" recipe nil)
 (ligature\.el status "required" recipe nil)
 (org-macs status "required" recipe nil)
 (pyenv\.el status "installed" recipe
            (:name pyenv\.el :type github :pkgname "aiguofer/pyenv.el" :after nil)))

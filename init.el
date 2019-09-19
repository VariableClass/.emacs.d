;;; init.el --- Emacs initialisation
;;; Commentary:

;;; Code:
(defun init()
  "Initialise Emacs."

  ;; Add .emacs.d/lisp to load path and import local modules
  (add-to-list 'load-path "~/.emacs.d/elisp/")

  ;; Perform any setup that can be achieved without external packages
  (require 'core-setup)
  (declare-function customise-frame 'core-setup ())
  (declare-function set-theme 'core-setup ())
  (declare-function configure-editor 'core-setup ())
  (declare-function define-mouse-behaviour 'core-setup ())
  (declare-function divert-customize 'core-setup (custom-file-path))
  (customise-frame)
  (set-theme)
  (configure-editor)
  (define-mouse-behaviour)
  (divert-customize "~/.emacs.d/elisp/custom.el")

  ;; Attempt package installation
  (require 'install-packages)
  (declare-function install-packages "install-packages" (packages))
  (install-packages
   '(auto-complete csharp-mode dash editorconfig epl f flycheck handoff multi-term omnisharp pkg-info popup projectile s sx visual-fill-column))

  ;; Configure the various downloaded packages
  (require 'package-config)
  (declare-function setup-multi-term "package-config"())
  (declare-function setup-visual-fill-column "package-config" (line-wrap-index))
  (declare-function setup-auto-complete "package-config" ())
  (declare-function setup-handoff "package-config" ())
  (declare-function setup-projectile "package-config" ())
  (declare-function setup-editorconfig "package-config" ())
  (declare-function setup-flycheck "package-config" ())
  (declare-function setup-omnisharp "package-config" ())
  (setup-multi-term)
  (setup-visual-fill-column 120)
  (setup-auto-complete)
  (setup-handoff)
  (setup-projectile)
  (setup-editorconfig)
  (setup-flycheck)
  (setup-omnisharp))

(init)

(provide 'init)
;;; init.el ends here

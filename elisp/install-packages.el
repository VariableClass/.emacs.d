;;; install-packages.el --- Install a provided list of packages
;;; Commentary:

;;; Code:
(defun install-packages(packages)
  "Install packages from the provided PACKAGES list."

  ;; List the repositories containing them
  (setq package-archives
    '(("elpa" . "http://tromey.com/elpa/")
      ("gnu" . "http://elpa.gnu.org/packages/")
      ("mepla" . "http://melpa.org/packages/")))
  
  ;; Activate all the packages (in particular autoloads)
  (package-initialize)
  
  ;; Import package module
  (require 'package)

  ;; Fetch the list of packages available
  (defvar package-refresh-contents)
  (unless package-archive-contents
    (package-refresh-contents))
   
  ;; Install the missing packages
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(provide 'install-packages)
;;; install-packages.el ends here

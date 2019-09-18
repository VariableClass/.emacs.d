;;; 
(defun install-packages(packages)

  ;; Assign passed packages to package-list
  (setq package-list packages)

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
  (unless package-archive-contents
    (package-refresh-contents))
   
  ;; Install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(provide 'install-packages)

;; Add .emacs.d/lisp to load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Add additional package source (MELPA)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages (quote (multi-term visual-fill-column))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2d3743" :foreground "#e1e1e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "DAMA" :family "Ubuntu Mono")))))

;; Disable menu bar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Disable scroll-bar
(toggle-scroll-bar -1)

;; Set frame to fullscreen
(toggle-frame-fullscreen)

;; Disable splash screen
(setq-default inhibit-splash-screen t)

;; Configure multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; Open ansi-term in main buffer on startup
(setq-default initial-buffer-choice 'multi-term)

;; Open window to left of terminal
(split-window-horizontally)

;; Enable soft wrap at 120 characters
(setq-default visual-fill-column-width 120)
(global-visual-fill-column-mode 1)
(global-visual-line-mode 1)

;; Make mouse pointer visible
(setq-default make-pointer-invisible nil)

;; Enable cat and mouse
(mouse-avoidance-mode 'cat-and-mouse)

;; Enable column index indicator
(setq-default column-number-mode t)

;; Set tab width to 2 spaces
(setq-default tab-width 2)

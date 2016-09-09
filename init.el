;;; init.el --- Brad's init script
;; v20160208
;; https://github.com/klocksib/configs

;;; Code:
;; Empty

;;; Commentary:
;; Empty

;; PACKAGES
;; -----------------------------------------------------------------------------

(cond ((eq system-type 'darwin)
       (message "%s" "Loading OSX specific settings.")
       (setq ns-use-srgb-colorspace t)
       (set-frame-font "Fira Code 11"))
      ((or
        (eq system-type 'gnu/linux)
        (eq system-type 'berkeley-unix))
       (message "%s" "Loading Linux and BSD specific settings.")
       (setq select-enable-clipboard t)
       (set-frame-font "Ubuntu Mono 10"))
      ((or
        (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
       (message "%s" "Loading Windows specific settings.")
       (set-frame-font "Consolas 10")))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar klocksib/packages
  '(clojure-mode
    coffee-mode
    csharp-mode
    deft
    elpy
    feature-mode
    flycheck
    flycheck-pyflakes
    flycheck-perl6
    flycheck-clangcheck
    flycheck-ocaml
    geiser
    gist
    go-mode
    graphviz-dot-mode
    haskell-mode
    htmlize
    json-mode
    keychain-environment
    magit
    markdown-mode
    nodejs-repl
    org
    php-mode
    puppet-mode
    rvm
    web-mode
    writegood-mode
    sly
    yaml-mode
    ample-theme
    gruvbox-theme
    molokai-theme
    monokai-theme
    noctilux-theme
    solarized-theme
    sublime-themes
    zenburn-theme))

;; refresh list of packages and install if they are not
(message "%s" "Refreshing package database...")
(package-refresh-contents)
(message "%s" "Updating or installing newly specified packages...")
(dolist (pkg klocksib/packages)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (package-menu-execute t)
  (message "%s" "Updating packages."))

;; GUI SETTINGS
;; -----------------------------------------------------------------------------
(tool-bar-mode -1)
(display-time)
(show-paren-mode t)
;;(global-flycheck-mode t)
(setq indicate-empty-lines t)

(setq custom-safe-themes t)
(load-theme 'monokai)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; GENERAL SETTINGS
;; -----------------------------------------------------------------------------
(setq gc-cons-threshold 100000000)
(setq initial-scratch-message "")
(setq display-time-day-and-date t)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq tab-width 4
      indent-tabs-mode nil)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Saving the world helpers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Window helpers
(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Line numbers
(global-linum-mode t)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;; TRAMP mode
(require 'keychain-environment)
(keychain-refresh-environment)
(setq tramp-default-method "ssh")


;; FILE BACKUP SETTINGS
;; -----------------------------------------------------------------------------
(setq backup-directory-alist `(("." . "~/.emacs.s")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; LANGUAGE SPECIFIC SETTINGS
;; -----------------------------------------------------------------------------

;; Bind extensions to specific modes
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("sudoers.*$" . conf-mode))

;; PERL
(defalias 'perl-mode 'cperl-mode)

;; COMMON LISP
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq scheme-program-name "racket")

;; PYTHON
;(elpy-enable)
;(setq elpy-rpc-python-command "/usr/local/Cellar/python3/3.5.2_1/bin/python3")
;(when (require 'flycheck nil t)
;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Exit
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "80aca613e13b40f3ffe6a8ce471c6388b93e5f026fe35ac9925c5fb84405a47a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

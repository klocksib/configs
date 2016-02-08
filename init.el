;;; init.el --- Brad's init script
;; v20160208
;; https://github.com/klocksib/configs

;;; Code:
;; Empty

;;; Commentary:
;; Empty

;; PACKAGES
;; -----------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar klocksib/packages
  '(clojure-mode
    coffee-mode
    csharp-mode
    deft
    elpy
    erlang
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

(message "%s" "Refreshing package database...")
(package-refresh-contents)
(message "%s" "Updating or installing newly specified packages...")
(dolist (pkg klocksib/packages)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" default))))
(custom-set-faces)


;; GUI SETTINGS
;; -----------------------------------------------------------------------------
(tool-bar-mode -1)
(display-time)
(show-paren-mode t)
(global-flycheck-mode t)
(setq indicate-empty-lines t)
(load-theme 'gruvbox)

(cond ((eq system-type 'darwin)
       (message "%s" "Loading OSX specific settings.")
       (setq ns-use-srgb-colorspace t)
       (set-frame-font "Menlo 11"))
      ((or
        (eq system-type 'gnu/linux)
        (eq system-type 'berkeley-unix))
       (message "%s" "Loading Linux and BSD specific settings.")
       (setq select-enable-clipboard t)
       (set-frame-font "Ubuntu Mono 11"))
      ((or
        (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
       (message "%s" "Loading Windows specific settings.")
       (set-frame-font "Consolas 10")))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))


;; GENERAL SETTINGS
;; -----------------------------------------------------------------------------
(setq gc-cons-threshold 100000000)
(setq initial-scratch-message "")
(setq display-time-day-and-date t)ex
(setq inhibit-startup-message t)
(setq column-number-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq tab-width 4
      indent-tabs-mode nil)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

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
(add-to-list 'auto-mode-alist '("^sudoers" . conf-mode))

;; PERL
(defalias 'perl-mode 'cperl-mode)

;; COMMON LISP
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; SCHEME
(setq scheme-program-name "racket")

;; PYTHON
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Exit
(provide 'init)
;;; init.el ends here

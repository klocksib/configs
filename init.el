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
    zerodark-theme
    zenburn-theme))

(message "%s" "Refreshing package database...")
(package-refresh-contents)
(message "%s" "Updating or installing newly specified packages...")
(dolist (pkg klocksib/packages)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "bc40f613df8e0d8f31c5eb3380b61f587e1b5bc439212e03d4ea44b26b4f408a" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" default)))
 '(package-selected-packages
   (quote
    (zerodark-theme soothe-theme ample-zen-theme smyx-theme zenburn-theme yaml-mode writegood-mode web-mode sublime-themes solarized-theme sly rvm puppet-mode php-mode nodejs-repl noctilux-theme monokai-theme molokai-theme markdown-mode magit json-mode htmlize haskell-mode gruvbox-theme graphviz-dot-mode go-mode gist geiser flymake-python-pyflakes flycheck-pyflakes flycheck-perl6 flycheck-ocaml flycheck-clangcheck feature-mode erlang elpy deft csharp-mode coffee-mode clojure-mode ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; GUI SETTINGS
;; -----------------------------------------------------------------------------
(tool-bar-mode -1)
(display-time)
(show-paren-mode t)
(global-flycheck-mode t)
(setq indicate-empty-lines t)
(load-theme 'monokai)

(cond ((eq system-type 'darwin)
       (message "%s" "Loading OSX specific settings.")
       (setq ns-use-srgb-colorspace t)
       (set-frame-font "Fira Code 11")
       (let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
		      (35 . ".\\(?:[(?[_{]\\)")
		      (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
		      (42 . ".\\(?:\\(?:\\*\\*\\)\\|[*/]\\)")
		      (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
		      (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
		      (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=]\\)")
		      (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
		      (58 . ".\\(?:[:=]\\)")
		      (59 . ".\\(?:;\\)")
		      (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
		      (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
		      (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
		      (63 . ".\\(?:[:=?]\\)")
		      (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
		      (94 . ".\\(?:=\\)")
		      (123 . ".\\(?:-\\)")
		      (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
		      (126 . ".\\(?:[=@~-]\\)")
		      )))
	 (dolist (char-regexp alist)
	   (set-char-table-range composition-function-table (car char-regexp)
				 `([,(cdr char-regexp) 0 font-shape-gstring])))))
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
(setq user-full-name "Brad Klocksiem")
(setq user-mail-address "brad.klocksiem@bigfishgames.com")
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

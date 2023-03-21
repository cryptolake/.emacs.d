;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(setq ring-bell-function 'ignore)
(defun crypt/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		     (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'crypt/display-startup-time)

;; Silence native compiler warnings
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(set-language-environment "UTF-8")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(global-auto-revert-mode t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; backup and auto-save files in one single room

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup"))))

(use-package no-littering)

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;Disable scrollb
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some room

(menu-bar-mode -1) ; Disable menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		 term-mode-hook
		 shell-mode-hook
		 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Hack" :height 120)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
    '("j" . meow-next)
    '("k" . meow-prev)
    '("<escape>" . ignore))
  (meow-leader-define-key
    ;; SPC j/k will run the original command in MOTION state.
    '("j" . "H-j")
    '("k" . "H-k")
    ;; Use SPC (0-9) for digit arguments.
    '("1" . meow-digit-argument)
    '("2" . meow-digit-argument)
    '("3" . meow-digit-argument)
    '("4" . meow-digit-argument)
    '("5" . meow-digit-argument)
    '("6" . meow-digit-argument)
    '("7" . meow-digit-argument)
    '("8" . meow-digit-argument)
    '("9" . meow-digit-argument)
    '("0" . meow-digit-argument)
    '("/" . meow-keypad-describe-key)
    '("?" . meow-cheatsheet))
  (meow-normal-define-key
    '("0" . meow-expand-0)
    '("9" . meow-expand-9)
    '("8" . meow-expand-8)
    '("7" . meow-expand-7)
    '("6" . meow-expand-6)
    '("5" . meow-expand-5)
    '("4" . meow-expand-4)
    '("3" . meow-expand-3)
    '("2" . meow-expand-2)
    '("1" . meow-expand-1)
    '("-" . negative-argument)
    '(";" . meow-reverse)
    '("," . meow-inner-of-thing)
    '("." . meow-bounds-of-thing)
    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)
    '("a" . meow-append)
    '("A" . meow-open-below)
    '("b" . meow-back-word)
    '("B" . meow-back-symbol)
    '("c" . meow-change)
    '("d" . meow-delete)
    '("D" . meow-backward-delete)
    '("e" . meow-next-word)
    '("E" . meow-next-symbol)
    '("f" . meow-find)
    '("g" . meow-cancel-selection)
    '("G" . meow-grab)
    '("h" . meow-left)
    '("H" . meow-left-expand)
    '("i" . meow-insert)
    '("I" . meow-open-above)
    '("j" . meow-next)
    '("J" . meow-next-expand)
    '("k" . meow-prev)
    '("K" . meow-prev-expand)
    '("l" . meow-right)
    '("L" . meow-right-expand)
    '("m" . meow-join)
    '("n" . meow-search)
    '("o" . meow-block)
    '("O" . meow-to-block)
    '("p" . meow-yank)
    '("q" . meow-quit)
    '("Q" . meow-goto-line)
    '("r" . meow-replace)
    '("R" . meow-swap-grab)
    '("s" . meow-kill)
    '("t" . meow-till)
    '("u" . meow-undo)
    '("U" . meow-undo-in-selection)
    '("v" . meow-visit)
    '("w" . meow-mark-word)
    '("W" . meow-mark-symbol)
    '("x" . meow-line)
    '("X" . meow-goto-line)
    '("y" . meow-save)
    '("Y" . meow-sync-grab)
    '("z" . meow-pop-selection)
    '("'" . repeat)
    '("<escape>" . ignore)))

(use-package meow)
(meow-setup)
(meow-global-mode 1)

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package company
	     :init (global-company-mode)
	     (setq company-global-modes '(not org-mode))
	     :bind (:map company-active-map
			 ("<tab>" . company-complete-selection))
	     :custom
	     (company-minimum-prefix-length 1)
	     (company-idle-delay 0.0))

(defun text-mode-hook-setup ()
  ;; make `company-backends' local is critcal
  ;; or else, you will have completion in every major mode, that's very annoying!
  (make-local-variable 'company-backends)

  ;; company-ispell is the plugin to complete words
  (add-to-list 'company-backends 'company-ispell))

(add-hook 'text-mode-hook 'text-mode-hook-setup)

(use-package magit)

;; Add git  to the side
(use-package git-gutter-fringe
	     :init
	     (global-git-gutter-mode t))

(use-package rainbow-delimiters
	     :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter-langs)

(defun crypt/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(use-package eshell
	     :hook (eshell-first-time-mode . crypt/configure-eshell)
	     :config

	     (with-eval-after-load 'esh-opt
				   (setq eshell-destroy-buffer-when-process-dies t)))

(use-package which-key
	     :init (which-key-mode)
	     :diminish which-key-mode
	     :config
	     (setq which-key-idle-delay 0.3))

(use-package vertico
	     :init (vertico-mode)
	     :demand t
	     :bind (
		    :map vertico-map
		    ("C-j" . vertico-next)
		    ("C-k" . vertico-previous))
	     :config
	     (setq vertico-sycle t))

(use-package savehist
	     :init
	     (savehist-mode))

(use-package orderless
	     :init
	     (setq completion-styles '(orderless)
		   completion-category-defaults nil
		   completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
	     :bind (
		    :map minibuffer-local-map
		    ("M-A" . marginalia-cycle))
	     :init
	     (marginalia-mode))

(defun crypt/org-mode-setup ()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
	     :hook (org-mode . crypt/org-mode-setup)
	     :config
	     (setq org-agenda-files
		   '("~/Documents/org/todo.org"))
	     (setq org-agenda-start-with-log-mode t)
	     (setq org-log-done 'time)
	     (setq org-ellipsis " ▾")

	     (setq org-capture-templates
		   `(("t" "Tasks / Projects")
		     ("tt" "Task" entry (file+olp "~/Documents/org/todo.org" "Inbox")
		      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

		     ("j" "Journal Entries")
		     ("jj" "Journal" entry
		      (file+olp+datetree "~/Documents/org/journal.org")
		      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
		      :clock-in :clock-resume
		      :empty-lines 1)
		     ("jm" "Meeting" entry
		      (file+olp+datetree "~/Documents/org/journal.org")
		      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
		      :clock-in :clock-resume
		      :empty-lines 1))))

(use-package org-bullets
	     :after org
	     :hook (org-mode . org-bullets-mode)
	     :custom
	     (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun crypt/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
	     :hook (org-mode . crypt/org-mode-visual-fill))

(use-package dired
	     :ensure nil
	     :commands (dired dired-jump)
	     :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single)
(use-package all-the-icons
	     :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package docker
  :ensure t
  :defer t)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(setq lsp-pyright-use-library-code-for-types nil) ;; set this to nil if getting too many false positive type errors
(setq lsp-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs")) ;; example
(setq lsp-pyright-auto-import-completions nil)
(setq lsp-pyright-typechecking-mode "basic")

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (add-hook 'python-mode-hook #'pyvenv-mode))
(use-package ein)
(use-package treemacs
  :ensure t
  :defer t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-gruvbox") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(add-to-list 'default-frame-alist '(alpha-background . 95)) ; For all new frames henceforth
;; Debugging support
(setq dap-auto-configure-features '(sessions locals controls tooltip))

;; Tree sitter support
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


;; smart parens
(use-package smartparens
  :init (show-smartparens-global-mode)
  :config (require 'smartparens-config))

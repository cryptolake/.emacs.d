;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(setq ring-bell-function 'ignore)
(cua-mode t)
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
(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

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

(use-package undo-tree
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/backup/"))))

(setq evil-want-keybinding 'nil)

(use-package evil
  :init (evil-mode 1))

(use-package evil-commentary
  :init (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))


(use-package evil-collection
  :init (evil-collection-init))

;; THE BEST ELISP EVER

(defun enable-translation ()
  "Enable key translation."
  (define-key key-translation-map (kbd "SPC") 'event-apply-control-modifier))

(defun disable-translation ()
  "Disable key translation."
  (define-key key-translation-map (kbd "SPC") nil))

(add-hook 'evil-normal-state-entry-hook #'enable-translation)
(add-hook 'evil-normal-state-exit-hook #'disable-translation)

(add-hook 'minibuffer-setup-hook #'disable-translation)
(add-hook 'minibuffer-exit-hook #'enable-translation)


(evil-set-undo-system 'undo-tree)

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
  :straight (:type built-in)
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
  :init (global-flycheck-mode))

(use-package docker
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
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(setq lsp-pyright-use-library-code-for-types nil) ;; set this to nil if getting too many false positive type errors
(setq lsp-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs")) ;; example
(setq lsp-pyright-auto-import-completions nil)

(use-package conda
  :init
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell))
(use-package ein)
(use-package treemacs
  
  :defer t)

(use-package doom-themes
  
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

(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))

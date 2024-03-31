;;; .emacs --- Summary ;;; Commentary: Personal emacs config of Hunter, Christerpher -*- lexical-binding: t; -*-

;;; Code:
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold 100000000)  ;; 100mb

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Use plists instead of hashlist
(setq lsp-use-plists t)

;; Prevent package.el loading packages prior to init-file loading.
(setq package-enable-at-startup nil)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))
(require 'package)
(setq package-archives
             '(("melpa" . "https://melpa.org/packages/")
               ("org"  .  "https://orgmode.org/elpa/")
               ("elpa" . "https://elpa.gnu.org/packages/")))

;; list the packages you want
(setq package-list
      '(
	wakatime-mode
	lsp-mode
	use-package
	multiple-cursors
	counsel
	yasnippet
	ivy-rich
	ivy-yasnippet
	ivy-prescient
	company
	company-box
	ivy-posframe
	lsp-origami
	nyan-mode
	dired-single
	dired-open
	auto-package-update
	rainbow-delimiters
	all-the-icons-dired
	which-key
	projectile
	all-the-icons
	all-the-icons-dired
	doom-modeline
	doom-themes
	)
      )

;; install the missing packages
(dolist (package package-list)
 (unless (package-installed-p package)
   (package-install package)))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(require 'use-package)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Append to emacs PATH
(setq user-emacs-directory "~/.emacs.d")

;; Personal Setup
;;====================================



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)        ;; Disable visible scrollbar
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(set-fringe-mode 10)        ;; Give some breathing room
(menu-bar-mode -1)          ;; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; y == yes && n == no
(fset 'yes-or-no-p 'y-or-n-p)

;; Line numbers
(setq column-number-mode t)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Make frame transparency overridable
(defvar cvh/frame-transparency '(90 . 90))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha cvh/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,cvh/frame-transparency))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Enable wakatime
(global-wakatime-mode)


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/work_worK_woRk_wOrk_Work_WORK")
    (setq projectile-project-search-path '("~/Documents/work_worK_woRk_wOrk_Work_WORK")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;;Company mode 
(use-package company
  :ensure
  :custom
  (company-idle-delay 0.2) ;; how long to wait until popup, was 0.5
  (company-minimum-prefix-length 1) ;; how many chars before autocomplete
  ;; (company-begin-commands nif) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next-or-abort)
	      ("C-p". company-select-previous)
	      ("<tab>". tab-indent-or-complete)
	      ("TAB". tab-indent-or-complete)
	      ;; ("M-<". company-select-first)
	      ;; ("M->". company-select-last)))
	      ))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-pylint-executable "python3")
  (setq flycheck-python-mypy-executable "mypy")
  (setq flycheck-python-pyright-executable "pyright")
  (setq flycheck-python-pyright-venv "venv")
  (setq flycheck-python-pyright-typechecking-mode "strict")
  ;; ruff-lsp
  (setq flycheck-ruff-executable "ruff")
  (setq flycheck-rust-executable "cargo")
  (setq flycheck-rust-clippy-executable "cargo-clippy")
  :bind-keymap
  ("M-g M-n" . flycheck-next-error)
  ("M-g M-p" . flycheck-previous-error)
)

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


;; You will most likely need to adjust this font size for your system!
(defvar cvh/default-font-size 90)
(defvar cvh/default-variable-font-size 90)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package fira-code-mode								  ;;
  ;; :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want ;;
  ;; :hook prog-mode)  								  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cvh/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "Fira Code Retina" :height cvh/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height cvh/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height cvh/default-variable-font-size :weight 'regular)
  )



(defun cvh/tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(customize-set-variable 'load-prefer-newer t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package nyan-mode
  :config
  (custom-set-variables
   '(nyan-cat-face-number 1)
   '(nyan-wavy-trail t)
   '(nyan-animate-nyancat t)
   '(nyan-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; KEY BINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easily swith between windows in one buffer (split screen)
(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-{") 'previous-window-any-frame)
(global-set-key (kbd "C-}") 'next-window-any-frame)

;; Switch buffers fast
(global-set-key (kbd "C-<prior>") 'switch-to-next-buffer)
(global-set-key (kbd "C-<next>") 'switch-to-prev-buffer)

;; unset C-S-i so that it can be used to format buffer globally
(global-unset-key (kbd "C-S-i"))

;; Set C+; to toggle comment entire line
(global-set-key (kbd "C-;") 'comment-line)

;; Set C-<tab> to company-complete for only python-mode
(global-set-key (kbd "C-<tab>") 'company-complete)

;; Set C-' to xref-find-references in python-mode only
;; (global-set-key (kbd "C-'") 'xref-find-references)
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-S-i") 'py-autopep8-buffer)
	    (local-set-key (kbd "C-'") 'lsp-ui-peek-find-references)))

;; unbind M-g M-n to go to flycheck next error
(global-unset-key (kbd "M-g M-n"))
(global-unset-key (kbd "M-g M-p"))
(global-set-key (kbd "M-g M-n") 'flycheck-next-error)
(global-set-key (kbd "M-g M-p") 'flycheck-previous-error)


;; Format Python code with autopep8
;; Autopep8 execute
(setq py-autopep8-options '("--max-line-length=79"))

;; Set C-S-i to indent-for-tab in html-mode
(defun cvh/indent-buffer ()
  "Indent the entire buffer using `indent-for-tab-command'."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-for-tab-command)))

(defvar cvh/modes-to-indent
  '(html-mode
    css-mode
    scss-mode
    elisp-mode
    yaml-mode
    emacs-lisp-mode
    ))

(dolist (mode cvh/modes-to-indent)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda ()
	      (local-set-key (kbd "C-S-i") 'cvh/indent-buffer))
	    t))

(require 'multiple-cursors)

;; Multiple Cursors
(global-set-key (kbd "C-S-z ") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)
(global-unset-key (kbd "C-<down-mouse-1>"))  ;; Just in case
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
(define-key mc/keymap (kbd "<return>") nil)  ;; Disable non-newline enter

;; Delete line from cursor to beginning
(global-set-key (kbd "S-<delete>") 'kill-whole-line)

;; Immediately kill the focused buffer
(global-unset-key (kbd "C-x k"))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

(global-unset-key (kbd "C-r"))  ;; Just in case

;; Set swiper-thing-at-point to C-r
(global-set-key (kbd "C-r") 'swiper-thing-at-point)

;; Select the entire word at point using mc--mark-symbol-at-point
(global-set-key (kbd "C-<return>") 'mc--mark-symbol-at-point)

;;;;;;;;;;;;;;;;;;;;
;; HTML variables ;;
;;;;;;;;;;;;;;;;;;;;

;; Enable LSP for HTML for HTML files
(add-hook 'html-mode-hook #'lsp)

;; A list of JSON file paths that define custom tags, properties and other HTML syntax constructs.
(setq lsp-html-experimental-custom-data (list (expand-file-name "html-languageserver.json" user-emacs-directory)))

;; Format unformatted content
(setq lsp-html-format-content-unformatted t)

;; Format end with newline
(setq lsp-html-format-end-with-newline t)

;; Format indent handlebars
(setq lsp-html-format-indent-handlebars t)

;; Format indent inner html
(setq lsp-html-format-indent-inner-html t)

;; Create a function that runs indent-for-tab-command for every line in the buffer
(defun cvh/my-indent-whole-buffer ()
  "Indent the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; In css and html mode keybind my-format-python-text to C-S-i
(add-hook 'css-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-S-i") 'cvh/my-indent-whole-buffer)
	    )
	  )

(add-hook 'html-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-S-i") 'cvh/my-indent-whole-buffer)
	    )
	  )

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Allows some expected functionality
(require 'dired-x)

;; Keep dired to one buffer
(use-package dired-single)

;; Configure dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (define-key dired-mode-map (kbd "C-<") 'dired-single-up-directory)
  (define-key dired-mode-map (kbd "C-m") 'dired-single-buffer)
	   )  

;; Setup dired find file to open using custom program
(use-package dired-open
  :config
  (setq dired-open-extensions '(("pdf" . "zathura")
				("mkv" . "vlc")
				("mp4" . "vlc")
				("avi" . "vlc")
				("mp3" . "vlc")
				("jpg" . "feh")
				("png" . "feh")
				("jpeg" . "feh")
				("gif" . "feh")
				("bmp" . "feh")
				("tiff" . "feh")
				("tif" . "feh")
				("svg" . "feh")
				("html" . "firefox")
				("htm" . "firefox")
				("tex" . "zathura")
				("texi" . "zathura"))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("TAB" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line))
         ;; ("C-r" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;  A doom theme
(use-package doom-themes
 :init (load-theme 'doom-dark+ t))
;; (load-theme 'modus-vivendi t)

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package counsel
  :bind (("M-x" . 'counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  (counsel-mode 1))

;; Make parenthesie life easier
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun cvh/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-ui
  :after lsp
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 0.5
        lsp-ui-doc-delay 2
        lsp-ui-sideline-ignore-duplicates t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t)
  :custom
  ('cvh/lsp-mode-setup)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show))

(use-package lsp-ivy
  :commands (lsp-ivy-workspace-symbol)
  :after lsp)

(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
(ivy-posframe-mode 1) 

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package bash-completion
  :mode "\\.sh\\'"
  :hook (shell-mode . lsp-deferred)
  :config
  (setq lsp-bash-highlight-parsing-errors t))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Setup eglot for rust
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'flymake-mode)
(add-hook 'rust-mode-hook 'company-mode)
(add-hook 'rust-mode-hook 'origami-mode)
(add-hook 'rust-mode-hook 'yas-minor-mode)

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("C-'" . lsp-ui-peek-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-a" . lsp-execute-code-action)
	      ("C-S-i" . lsp-format-buffer)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq lsp-ui-sideline-enable nil)
  (setq rustic-rustfmt-config-alist '((hard_tabs . t) (skip_children . nil)))
  
  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'cvh/rustic-mode-hook))

(defun cvh/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c a")
  :hook
  (prog-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :ensure t
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; Zig Mode
(unless (version< emacs-version "24")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Copilot  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eval-when-compile
  (require 'use-package))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;; you can utilize :map :hook and :config to customize copilot

;; Enable copilot globally
;;(use-package copilot
  ;;:load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory))
  ;; don't show in mode line
  ;;:diminish)

;; Github Copilot
(defun cvh/no-copilot-mode ()
  "Helper for `cvh/no-copilot-modes'."
  (copilot-mode -1))

(defvar cvh/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defun cvh/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or cvh/copilot-manual-mode
      (member major-mode cvh/no-copilot-modes)
      (company--active-p)))

(add-to-list 'copilot-disable-predicates #'cvh/copilot-disable-predicate)

(defvar cvh/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun cvh/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode cvh/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq cvh/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq cvh/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(define-key global-map (kbd "C-.") #'cvh/copilot-change-activation)

(defun cvh/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion) 
(define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "M-C-=") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "M-C-,") #'cvh/copilot-complete-or-accept)

(defun cvh/copilot-tab ()
  "Tab command that will complete with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or   
   (company-complete)
   (indent-for-tab-command)))

;; (define-key global-map (kbd "<tab>") #'cvh/copilot-tab)

(defun cvh/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'cvh/copilot-quit)


(setq custom-file (locate-user-emacs-file "~/.emacs.d/custom-vars.el"))
(load custom-file 'noerror 'nomessage)


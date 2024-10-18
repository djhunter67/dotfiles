;;; Commentary: Personal emacs config of Hunter, Christerpher

;; -*- lexical-binding: t; -*-

;;; Code:
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold 1000)  ;; 1Mb

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Increase maximum number of lisp evaluations
(setq max-lisp-eval-depth 10000)

;; Use plists instead of hashlist
(setq lsp-use-plists t)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"  .  "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))


;; Prevent package.el loading packages prior to init-file loading.
(setq package-enable-at-startup nil)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Append to emacs PATH
(setq user-emacs-directory "~/.emacs.d")

;; Set the warning level
(setq warning-minimum-level :error)

;; Make warning appear in smaller buffer
(setq display-buffer-alist
      '(("[*]Warnings[*]" .
	 (display-buffer-in-side-window . '((side . bottom))))))

;; list the packages you want
(setq package-list
      '(
	eglot
	better-defaults
	elpy
	flycheck
	py-autopep8
	blacken
	material-theme
	ob-rust
	editorconfig
	wakatime-mode
	lsp-mode
	use-package
	multiple-cursors
	ivy-yasnippet
	company
	prettier-js
	flymake-ruff
	ivy-posframe
	origami
	lsp-origami
	tree-sitter-langs
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
	dired-open
	auto-package-update
	rainbow-delimiters
	all-the-icons-dired
	which-key
	projectile
	projectile-ripgrep
	all-the-icons
	all-the-icons-dired
	doom-modeline
	doom-themes
	rust-mode
	syntactic-close
	drag-stuff
	rjsx-mode
	)
      )

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


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
        (copilot-mode -1)
        (setq cvh/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq cvh/copilot-manual-mode t))
      (message "activating copilot mode")
      (copilot-mode))))

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

;; Silence compiler warnings as they are disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Don't pop up UI dialogs
(setq use-dialog-box nil)

;; Disable init popup
(setq inhibit-startup-message t)

;; Revert buffers when underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired buffer to live reload
(setq global-auto-revert-non-file-buffers t)

;; You will most likely need to adjust this font size for your system!
(defvar cvh/default-font-size 90)
(defvar cvh/default-variable-font-size 90)

(require 'package)

;; Setup
;;====================================

;; Tree-sitter for syntax highlighting
;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; Solidity support
;;(setq solidity-solc-path "/home/djhunter67/.BUILDS/solidity-0.8.23/build/solc/solc")
;;TODO
;; install solium or ethlint

;;(require 'solidity-mode)
;;(setq solidity-comment-style 'slash) ;; // comments
;;(define-key solidity-mode-map (kbd "C-c C-p") 'solidity-estimate-gas-at-point)

;; Solidity flycheck
;;(require 'solidity-flycheck)
;;(setq solidity-flycheck-solc-checker-active t)
;; TODO
;; install solium or ethlint for flycheck

;; Solidity std contracts checker
;;(setq flycheck-solidity-solc-addstd-contracts t)
;; TODO
;; Solium or Ethlint RC file

;; Solidity company
;;(require 'company-solidity)
;; Company can use local variables
;;(add-hook 'solidity-mode-hook
;;	(lambda ()
;;	(set (make-local-variable 'company-backends)
;;		(append '((company-solidity company-capf company-dabbrev-code))
;;			company-backends))))


;; Install origami for code folding
(use-package origami
  :ensure t
  :config
  (global-origami-mode)
  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))

(require 'flymake-ruff)
(add-hook 'python-mode-hook #'flymake-ruff-load)
;; Enable flycheck
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-python-flake8-executable "python3")
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-pylint-executable "python3")
  (setq flycheck-python-mypy-executable "python3")
  (setq flycheck-python-pyright-executable "python3")
  (setq flycheck-python-pyright-venv "venv")
  (setq flycheck-python-pyright-typechecking-mode "strict")
  ;; ruff-lsp
  (setq flycheck-ruff-executable "ruff-lsp")
  )

(custom-set-variables
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3"))

;; Enable wakatime
(global-wakatime-mode)

;; Enable autopep8
;; (require 'py-autopep8)


;; Rust IDE feature
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c a")
  :hook
  (prog-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :ensure t
  :commands (lsp)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.1)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Rust format on save
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))


;; Zig Mode
(unless (version< emacs-version "24")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

;; Setup eglot for rust
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'flymake-mode)
(add-hook 'rust-mode-hook 'company-mode)
(add-hook 'rust-mode-hook 'origami-mode)
(add-hook 'rust-mode-hook 'yas-minor-mode)

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
	;; ("M-<". company-select-first)
	;; ("M->". company-select-last)))
	))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

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

(defun cvh/tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; inline inferred types
;; (setq lsp-rust-analyzer-server-display-inlay-hints t)

(require 'multiple-cursors)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(customize-set-variable 'load-prefer-newer t)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package fira-code-mode								  ;;
;; :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want ;;
;; :hook prog-mode)  								  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cvh/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "Fira Code" :height cvh/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height cvh/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Fira Code" :height cvh/default-variable-font-size :weight 'regular)
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (cvh/set-font-faces))))
  (cvh/set-font-faces))

;;=======================================
;; CUSTOM BINDINGS
;;=======================================

;; Imenu-list bindings
(global-set-key (kbd "C-,") #'imenu-list-smart-toggle)
(setq imenu-list-focus-after-activation t)

;; Find references
(global-set-key (kbd "C-'") 'lsp-ui-peek-find-references)

;; Comment the line
(global-set-key (kbd "C-;") 'comment-line)

;; Switch buffers fast
(global-set-key (kbd "C-<prior>") 'switch-to-next-buffer)
(global-set-key (kbd "C-<next>") 'switch-to-prev-buffer)

;; Multiple Cursors
(global-set-key (kbd "C-S-l C-S-l") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)

;; Delete line from cursor to beginning
(global-set-key (kbd "S-<delete>") 'kill-whole-line)

;; Immediately kill the focused buffer
(global-unset-key (kbd "C-x k"))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

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

;; Set C-<tab> to company-complete for lsp complete
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
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Eldoc at point in rust-mode only; set C-i to eldoc-buffer-at-point
(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-i") 'lsp-ui-doc-show)))

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

(global-unset-key (kbd "C-r"))  ;; Just in case

;; Set swiper-thing-at-point to C-r
(global-set-key (kbd "C-r") 'swiper-thing-at-point)

;; Select the entire word at point using mc--mark-symbol-at-point
(global-set-key (kbd "C-<return>") 'mc--mark-symbol-at-point)



;; Setup Minted for pretty LaTeX code blocks in PDF's from org-mode
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Enable Nyan Cat scrolling
(use-package nyan-mode
  :config
  (custom-set-variables
   '(nyan-cat-face-number 1)
   '(nyan-wavy-trail t)
   '(nyan-animate-nyancat t)
   '(nyan-mode t)))

;; Get autopep8p
(use-package py-autopep8
  :ensure t
  )

;; Disable the busted ass python-mypy checker
(setq-default flycheck-disabled-checkers '(python-mypy))

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

;; if rust-mode is enabled, set C-S-i to rustfmt-buffer
(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-S-i") 'lsp-format-buffer)))

(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-a") 'lsp-execute-code-action)))

(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<tab>") 'indent-for-tab-command)))

(require 'multiple-cursors)

;; Enable hunspell
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "de_DE")
(setq ispell-local-dictionary-alist
      '(("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

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



;; Python mode --> autoformat tabs and comments
(defun cvh/my-format-python-text ()
  "untabify and wrap python comments"
  (interactive)
  (untabify (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward comment-start nil t)
    (call-interactively 'fill-paragraph)
    (forward-line 1)))

;; Create a function that runs indent-for-tab-command for every line in the buffer
(defun cvh/my-indent-whole-buffer ()
  "Indent the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "RET") 'newline-and-indent)))
;; (define-key python-mode-map (kbd "<f4>") 'cvh/my-format-python-text)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(scroll-bar-mode -1)        ;; Disable visible scrollbar
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(set-fringe-mode 10)        ;; Give some breathing room
(menu-bar-mode -1)          ;; Disable the menu bar

;; Go directly to emacs config
;; Open .emacs with C-x c
(defun dotemacs () (interactive) (switch-to-buffer (find-file-noselect "~/dotfiles/.emacs")))
(global-set-key (kbd "C-x c") 'dotemacs)

;; Visual line mode everywhere, please
(global-visual-line-mode t)

;; Show the matching parenthesis
(show-paren-mode 1)

(global-unset-key (kbd "C-]"))

;; Syntactic close
;; https://github.com/emacs-berlin/syntactic-close
(global-set-key (kbd "C-]") 'syntactic-close)

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

(global-unset-key (kbd "C-<delete>"))

;; https://stackoverflow.com/questions/17958397/emacs-delete-whitespaces-or-a-word
(defun kill-whitespace-or-word ()
  (interactive)
  (if (looking-at "[ \t\n]")
      (let ((p (point)))
        (re-search-forward "[^ \t\n]" nil :no-error)
        (backward-char)
        (kill-region p (point)))
    (kill-word 1)))
(global-set-key (kbd "C-<delete>") 'kill-whitespace-or-word)

;; Drag-stuff - Drag lines and regions
(drag-stuff-global-mode 1)
;; Use C-S-up/down
(setq drag-stuff-modifier '(control shift))
(define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
(define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)

;; Duplicate line with C-S-d
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-S-d") 'duplicate-current-line-or-region)

;; Skip system buffers when cycling
(set-frame-parameter (selected-frame) 'buffer-predicate
		     (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))

;; Spell check
(global-set-key (kbd "<f6>") 'flyspell-mode)

;; Spell check with hunspell
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;;  A doom theme
(use-package doom-themes
  :init (load-theme 'doom-dark+ t))
;; (load-theme 'modus-vivendi t)

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

;; Disable permanent deletion
(setq delete-by-moving-to-trash t)

;; Keep dired to one buffer
;; (use-package dired-single)

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

;; Toggle visible dotfiles
;; (use-package dired-hide-dotfiles
;; :hook (dired-mode . dired-hide-dotfiles-mode)
;; :config
;; (define-key dired-mode-map (kbd "C-H") 'dired-hide-dotfiles-mode))

(setq org-plantuml-jar-path (expand-file-name "/home/$USER/.BUILDS/plantuml-1.2023.5.jar"))
;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (java . t)
     (sass . t)
     (plantuml . t)
     (matlab . t)
     (C . t)
     (js . t)
     (latex . t)
     (shell . t)
     (python . t)
     (rust . t)))

  (setq org-confirm-babel-evaluate t)
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo))

;;(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
;;(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;;(add-to-list 'org-structure-template-alist '("py" . "src python")))
;;(add-to-list 'org-structure-template-alist '("jv" . "src java"))
;;(add-to-list 'org-structure-template-alist '("ss" . "src sass"))
;;(add-to-list 'org-structure-template-alist '("js" . "src js"))
;;(add-to-list 'org-structure-template-alist '("C" . "src C")))

;; Automatically tangle our Emacs.org config file when we save it
(defun cvh/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cvh/org-babel-tangle-config)))

(use-package org
  ;;:pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . cvh/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Add path to this list to use in org agendas
  (setq org-agenda-files
        '("~/Org_mode/Tasks.org"
          "~/Org_mode/Journal.org"
          "~/Org_mode/Birthdays.org"
          "~/Org_mode/Metrics.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; Mutiple sequences of list to represent present state
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
	'(("Archive.org" :maxlevel . 1)
	  ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/Org_mode/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
           (file+olp+datetree "~/Org_mode/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
	  ("jm" "Meeting" entry
           (file+olp+datetree "~/Org_mode/Meeting.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

	  ("w" "Workflows")
	  ("we" "Checking Email" entry (file+olp+datetree "~/Org_mode/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

	  ("m" "Metrics Capture")
	  ("mw" "Weight" table-line (file+headline "~/Org_mode/Metrics.org" "Weight")
	   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
	      (lambda () (interactive) (org-capture nil "jj"))))

(defun cvh/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))
;;(set-face-attribute 'line-number nil :inherit 'fixed-pitch)
;;(set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun cvh/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun cvh/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cvh/org-mode-visual-fill))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package magit
  :commands magit-status)
;;:custom
;;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package term
  :commands term
  :config
  ;; (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  (setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args
  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun cvh/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . cvh/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package helpful
  ;;  :ensure t  ;; Redundant
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

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

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

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

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;; Markdown live rendering

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
	   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
	 (current-buffer)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-Emodeline-height 55)
           (setq doom-modeline-buffer-file-name-style 'auto)
           (setq doom-modeline-icon (display-graphic-p))
           (setq doom-modeline-major-mode-icon t)
           (setq doom-modeline-major-mode-color-icon t)
           (setq doom-modeline-buffer-state-icon t)
           (setq doom-modeline-buffer-modification-icon t)
           (setq doom-modeline-unicode-fallback t)
           (setq doom-modeline-minor-modes t)
           (setq doom-modeline-enable-word-count t)
           (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
           ;;(setq doom-modeline-number-limit 99)
           (setq doom-modeline-workspace-name t)
           (setq doom-modeline-persp-name t)
           (setq doom-modeline-persp-icon t)
           (setq doom-modeline-lsp t)
           (setq doom-modeline-github t)
           (setq doom-modeline-irc t)
           (setq doom-modeline-irc-stylize 'identity)
           (setq doom-modeline-env-version t)
	   ;;           (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
           (setq doom-modeline-env-go-executable "go")
           (setq doom-modeline-env-load-string "...")))

;; (use-package general
;; :config
;; (general-create-definer cvh/leader-keys
;; :keymaps '(normal insert visual emacs)
;; :prefix "m"
;; :global-prefix "M-m")

;; (cvh/leader-keys
;; "t" '(:ignore t :which-key "choose theme")
;; "tt" '(counsel-load-theme :which-key "choose theme")))

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
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show))

(use-package treemacs
  :ensure t
  :bind ("<f5>" . treemacs)
  :custom
  (treemacs-is-never-other-window t)
  :hook
  (treemacs-mode . treemacs-project-follow-mode))

(use-package solaire-mode
  :ensure t
  :hook (after-init . solaire-global-mode)
  :config
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))

(use-package fill-function-arguments
  :ensure t
  :defer
  :bind (:map prog-mode-map
              ("M-q" . fill-function-arguments-dwim)))

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list)
  :after lsp)

(use-package lsp-ivy
  :commands (lsp-ivy-workspace-symbol)
  :after lsp)

(defun cvh/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))


;; Disable warnings on cargo test
(setq rustic-cargo-test-disable-warnings t)

(use-package dap-mode
  :config
  (dap-mode t)
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))
;; (dap-ui-mode t))

(require 'dap-python)
;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
(setq dap-python-debugger 'debugpy)

;; Template examples
;; (dap-register-debug-template "My App"
;;   (list :type "python"
;;         :args "-i"
;;         :cwd nil
;;         :env '(("DEBUG" . "1"))
;;         :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
;;         :request "launch"
;;         :name "My App"))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . lsp)
;;   :custom
;;   ;; NOTE: Set these if Python 3 is called "python3" on your system!
;;   ;; (python-shell-interpreter "python3")
;;   ;; (dap-python-executable "python3")
;;   (dap-python-debugger 'debugpy)
;;   :config
;;   (lsp-register-custom-settings
;;    '(("pyls.plugins.pyls_mypy.enabled" t t)
;;      ("pyls.plugins.pyls_mypy.live_mode" nil t)
;;      ("pyls.plugins.pyls_black.enabled" t t)
;;      ("pyls.plugins.pyls_isort.enabled" t t)))
;;   (require 'dap-python))

;; (use-package lsp-jedi
;; :ensure t)

;; Enable Elpy for Python development
;; https://elpy.readthedocs.io/en/latest/
(setq elpy-rpc-python-command "python3")
(elpy-enable)

;; Run black on save
(add-hook 'elpy-mode-hook (lambda ()
  (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package lsp-java
  :mode "\\.java\\'"
  :hook (java-mode . lsp))

;; Add a hook for javascript mode to enable company
(add-hook 'js-mode-hook 'company-mode)
(add-hook 'js-mode-hook 'flycheck-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-S-i") 'prettier-js)))

(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))


(setq js-indent-level 2)

;; Pretty JS code
;; https://github.com/prettier/prettier-emacs
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)

(add-to-list 'auto-mode-alist
	     '("\\.cpp\\'" . c++-mode))

(use-package bash-completion
  :mode "\\.sh\\'"
  :hook (shell-mode . lsp-deferred)
  :config
  (setq lsp-bash-highlight-parsing-errors t))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Set up scss mode
(setq exec-path (cons (expand-file-name "/usr/bin/sass") exec-path))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
(ivy-posframe-mode 1)

;; Live preview of markdown
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
	   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
	 (current-buffer)))

;; SQL lsp
;; (add-hook 'sql-mode-hook 'lsp)
;; (setq lsp-sqls-workspace-config-path nil)
;; (setq lsp-sqls-connections
;;     '(((driver . "mysql") (dataSourceName . "yyoncho:local@tcp(localhost:3306)/foo"))
;;       ((driver . "mssql") (dataSourceName . "Server=localhost;Database=sammy;User Id=yyoncho;Password=hunter2;"))
;;       ((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=postgres password="" dbname=""  sslmode=disable"))))


;; Save emacs auto configs to a seperate file then load it.
(setq custom-file (locate-user-emacs-file "~/.emacs.d/custom-vars.el"))
(load custom-file 'noerror 'nomessage)



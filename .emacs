;; list the packages you want
(setq package-list
      '(better-defaults
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
	sqlite3
	dired-gitignore
	yaml-mode
	)
      )


(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org"  .  "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

					; activate all the packages (in particular autoloads)
(package-initialize)

					; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

					; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))


;; Silence compiler warnings as they are disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Append to emacs PATH
(setq user-emacs-directory "~/.emacs.d")

;; Don't pop up UI dialogs
(setq use-dialog-box nil)

;; Disable init popup
(setq inhibit-startup-message t)

;; Revert buffers when underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired buffer to live reload
(setq global-auto-revert-non-file-buffers t)

;; You will most likely need to adjust this font size for your system!
(defvar cvh/default-font-size 120)
(defvar cvh/default-variable-font-size 120)

(require 'package)
;;====================================
;; Setup
;;====================================

;; Get and enable Elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; Enable flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable yaml mode for .yaml files and .yml files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; Enable the Melpa repo

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Enable wakatime
(global-wakatime-mode)

;; Enable autopep8
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(defun cvh/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; Rust IDE feature
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.3)
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

;; Company mode for Rust
(use-package flycheck :ensure)
(use-package company
  :ensure
  :custom
  (company-idle-delay 0.3) ;; how long to wait until popup
  (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last)))

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

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
	(if (check-expansion)
	    (company-complete-common)
	  (indent-for-tab-command)))))

;; inline inferred types
(setq lsp-rust-analyzer-server-display-inlay-hints t)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))



(require 'multiple-cursors)

(add-to-list 'load-path "~/dotfiles/copilot.el/")
(require 'copilot)


;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package fira-code-mode								  ;;
;; :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want ;;
;; :hook prog-mode)  								  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :font "Fira Code Retina" :height cvh/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height cvh/default-font-size)


;;=======================================
;; CUSTOM BINDINGS
;;=======================================

;; Comment the line
(global-set-key (kbd "C-;") 'comment-line)

;; Set selected indent left
(global-set-key (kbd "C-{") 'indent-rigidly-left-to-tab-stop)

;; Set selected indent right
(global-set-key (kbd "C-}") 'indent-rigidly-right-to-tab-stop)

;; Switch buffers fast
(global-set-key (kbd "C-<prior>") 'switch-to-next-buffer)
(global-set-key (kbd "C-<next>") 'switch-to-prev-buffer)

;; Multiple Cursors
(global-set-key (kbd "C-S-l C-S-l") 'mc/edit-lines)

;; Delete line from cursor to beginning
(global-set-key (kbd "S-<delete>") 'kill-whole-line)

;; Immediately kill the focused buffer
(global-unset-key (kbd "C-x k"))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Python mode --> autoformat tabs and comments
(defun my-format-python-text ()
  "untabify and wrap python comments"
  (interactive)
  (untabify (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward comment-start nil t)
    (call-interactively 'fill-paragraph)
    (forward-line 1)))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "RET") 'newline-and-indent)
     (define-key python-mode-map (kbd "<f4>") 'my-format-python-text)))

;; Autopep8 execute
(setq py-autopep8-options '("--max-line-length=100"))
(define-key python-mode-map (kbd "C-S-i") 'py-autopep8-buffer)

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
  "Tab command that will complet with copilot if a completion is
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

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

;; Set up the visual bell
(setq visual-bell t)

;;  A doom theme
;; (use-package doom-themes
;;   :init (load-theme 'doom-dracula t))
(load-theme 'modus-vivendi t)

;; (use-package all-the-icons
;;   :if (display-graphic-p)
;;   :commands all-the-icons-install-fonts
;;   :init
;;   (unless (find-font (font-spec :name "all-the-icons"))
;;     (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Respect the gitignore
(use-package dired-gitignore
  :hook (dired-mode . dired-gitignore-mode))

;; Allows some expected functionality
(require 'dired-x)

;; Disable permanent deletion
(setq delete-by-moving-to-trash t)

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

;; Toggle visible dotfiles
(use-package dired-hide-dotfiles
  ;;:hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map (kbd "C-b") 'dired-hide-dotfiles-mode))
(setq org-plantuml-jar-path (expand-file-name "/home/djhunter67/.BUILDS/plantuml-1.2023.5.jar"))
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
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

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
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

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

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

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
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/code")
    (setq projectile-project-search-path '("~/Documents/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

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
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-Emodeline-height 35)
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

(use-package general
  :config
  (general-create-definer cvh/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "m"
    :global-prefix "M-m")

  (cvh/leader-keys
    "t" '(:ignore t :which-key "choose theme")
    "tt" '(counsel-load-theme :which-key "choose theme")))

;; Fast keyswitching keybinginds
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(cvh/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Make parenthesie life easier
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun cvh/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . cvh/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t))
  
(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;;:custom
  ;;(lsp-enable-dap-auto-configure nil)
  ;;:config
  ;;(dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; set up Node debugging
  (require 'dap-node)
  (dap-node-setup)) ;; Automatically installs Node debug adapter if needed



;; Setup the rust LSP
(use-package rustic
  :ensure t
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  )
(defun cvh/rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook #'cvh/rustic-mode-auto-save-hook)

;; Disable warnings on cargo test
(setq rustic-cargo-test-disable-warnings t)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . lsp)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  (require 'dap-python))

(use-package lsp-jedi
  :ensure t)

    
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package lsp-java
:mode "\\.java\\'"
:hook (java-mode . lsp))

(add-to-list 'auto-mode-alist
	     '("\\.cu\\'" . c++-mode))

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

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  ;; :bind (:map company-active-map
  ;;        ("<tab>" . company-complete-selection))
  ;;       (:map lsp-mode-map
  ;;        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


(use-package company-box
  :hook (company-mode . company-box-mode))

;; Save emacs auto configs to a seperate file then load it.
(setq custom-file (locate-user-emacs-file "~/.emacs.d/custom-vars.el"))
(load custom-file 'noerror 'nomessage)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nyan-animate-nyancat t)
 '(nyan-cat-face-number 1)
 '(nyan-mode t)
 '(nyan-wavy-trail t)
 '(package-selected-packages
   '(multiple-cursors wakatime-mode use-package py-autopep8 ob-rust material-theme lsp-ui flycheck elpy editorconfig blacken better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

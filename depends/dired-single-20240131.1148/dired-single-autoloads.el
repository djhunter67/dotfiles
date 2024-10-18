;;; dired-single-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from dired-single.el

(autoload 'dired-single-buffer "dired-single" "\
Visit selected directory in current buffer.

Visits the selected directory in the current buffer, replacing the
   current contents with the contents of the new directory.  This doesn't
   prevent you from having more than one Dired buffer.  The main difference
   is that a given Dired buffer will not spawn off a new buffer every time
   a new directory is visited.

If the variable `dired-single-use-magic-buffer' is non-nil, and the current
   buffer's name is the same as that specified by the variable
`dired-single-magic-buffer-name', then the new directory's buffer will retain
   that same name (i.e. not only will Dired only use a single buffer, but
its name will not change every time a new directory is entered).

Optional argument DEFAULT-DIRNAME specifies the directory to visit; if not
specified, the directory or file on the current line is used (assuming it's
a Dired buffer).  If the current line represents a file, the file is visited
in another window.

(fn &optional DEFAULT-DIRNAME)" t)
(autoload 'dired-single-buffer-mouse "dired-single" "\
Mouse-initiated version of `dired-single-buffer' (which see).

Argument CLICK is the mouse-click event.

(fn CLICK)" t)
(autoload 'dired-single-magic-buffer "dired-single" "\
Switch to buffer whose name is the value of `dired-single-magic-buffer-name'.

If no such buffer exists, launch Dired in a new buffer and rename that buffer
to the value of `dired-single-magic-buffer-name'.  If the current buffer is the
magic buffer, it will prompt for a new directory to visit.

Optional argument DEFAULT-DIRNAME specifies the directory to visit (defaults to
the currently displayed directory).

(fn &optional DEFAULT-DIRNAME)" t)
(autoload 'dired-single-toggle-buffer-name "dired-single" "\
Toggle between the `magic' buffer name and the `real' Dired buffer name.

Will also seek to uniquify the `real' buffer name." t)
(autoload 'dired-single-up-directory "dired-single" "\
Like `dired-up-directory' but with `dired-single-buffer'.

If (as OTHER-WINDOW) is non-nil, open the parent directory in a new window.

(fn &optional OTHER-WINDOW)" t)
(register-definition-prefixes "dired-single" '("dired-single-"))

;;; End of scraped data

(provide 'dired-single-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; dired-single-autoloads.el ends here

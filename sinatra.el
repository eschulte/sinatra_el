;;; sinatra.el --- minor mode for the Sinatra web mini-framework

;; Copyright (C) 2009 Eric Schulte

;; Authors: Eric Schulte
;; URL: 
;; Version: 0.1
;; Created: 2009-02-28
;; Keywords: ruby, sinatra, project, convenience, web, framework
;; EmacsWiki: 
;; Package-Requires: ((ruby-mode "1.1") (inf-ruby "2.1") (ruby-compilation "0.7"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 'ruby-mode)
(require 'inf-ruby)
(require 'ruby-compilation)
(require 'cl)

(defgroup sinatra nil
  "Sinatra customizations."
  :prefix "sinatra-"
  :group 'sinatra)

(defvar sinatra-minor-mode-hook nil
  "*Hook for customising Sinatra.")

(defcustom sinatra-env nil
  "Use this to force a value for RAILS_ENV when running sinatra.
Leave this set to nil to not force any value for RAILS_ENV, and
leave this to the environment variables outside of Emacs.")

(defvar sinatra-minor-mode-prefixes
  (list ";" "'")
  "List of characters, each of which will be bound (with C-c) as a sinatra-minor-mode keymap prefix.")

(defun sinatra-root ()
  "Return the root of the sinatra project"
  (file-name-directory (buffer-file-name)))

;;--------------------------------------------------------------------------------
;; user functions

(defun sinatra-rake (&optional task edit-cmd-args)
  "Tab completion selection of a rake task to execute with the
output dumped to a compilation buffer allowing jumping between
errors and source code.  With optional prefix argument allows
editing of the rake command arguments."
  (interactive "P")
  (ruby-compilation-rake task edit-cmd-args
			 (if sinatra-env (list (cons "RAILS_ENV" sinatra-env)))))

(defun sinatra-console (&optional edit-cmd-args)
  "Run script/console in a compilation buffer, with command
history and links between errors and source code.  With optional
prefix argument allows editing of the console command arguments."
  (interactive "P")
  (let* ((req (buffer-file-name)))
    (run-ruby (format "irb -r %s --simple-prompt" req))
    (save-excursion
      (pop-to-buffer "*ruby*")
      (set (make-local-variable 'inf-ruby-first-prompt-pattern) "^>> ")
      (set (make-local-variable 'inf-ruby-prompt-pattern) "^>> ")
      (sinatra-minor-mode t))))

(defun sinatra-web-server (&optional edit-cmd-args)
  "Run script/server.  Dump output to a compilation buffer
allowing jumping between errors and source code.  With optional
prefix argument allows editing of the server command arguments."
  (interactive "P")
  (let* ((script (buffer-file-name))
	 (command (if edit-cmd-args
		      (read-string "Run Ruby: " (concat script " "))
		    script)))
    (ruby-compilation-run command)) (sinatra-minor-mode t))

(defvar sinatra-rgrep-file-endings
  "*.[^l]*"
  "Ending of files to search for matches using `sinatra-rgrep'")

(defun sinatra-rgrep (&optional arg)
  "Search through the rails project for a string or `regexp'.
With optional prefix argument just run `rgrep'."
  (interactive "P")
  (grep-compute-defaults)
  (if arg (call-interactively 'rgrep)
    (let ((word (thing-at-point 'word)))
      (funcall 'rgrep (read-from-minibuffer "search for: " word)
	       sinatra-rgrep-file-endings (sinatra-root)))))

;;--------------------------------------------------------------------
;; minor mode and keymaps

(defvar sinatra-minor-mode-map (make-sparse-keymap) "Key map for Sinatra minor mode.")

(defun sinatra-bind-key-to-func (key func)
  (dolist (prefix sinatra-minor-mode-prefixes)
    (eval `(define-key sinatra-minor-mode-map 
             ,(format "\C-c%s%s" prefix key) ,func))))

(defvar sinatra-minor-mode-keybindings
  '(("r" . 'sinatra-rake)                ("c" . 'sinatra-console)
    ("w" . 'sinatra-web-server)          ("g" . 'sinatra-rgrep))
  "alist mapping of keys to functions in `sinatra-minor-mode'")

(mapcar (lambda (el) (sinatra-bind-key-to-func (car el) (cdr el))) sinatra-minor-mode-keybindings)

;;;###autoload
(define-minor-mode sinatra-minor-mode
  "Enable Sinatra minor mode providing Emacs support for working
with the Sinatra web mini-framework."
  nil
  " do-be-do-be-doooo"
  sinatra-minor-mode-map)

(provide 'sinatra)
;;; sinatra.el ends here

;;; helm-descbinds.el --- A convenient `describe-bindings' with `helm'   -*- lexical-binding: t -*-

;; Copyright (C) 2008, 2009, 2010  Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Copyright (C) 2012, 2013  Michael Markert <markert.michael@googlemail.com>
;; Copyright (C) 2013 Daniel Hackney <dan@haxney.org>
;; Copyright (C) 2015, 2016 Michael Heerdegen <michael_heerdegen@web.de>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; URL: https://github.com/emacs-helm/helm-descbinds
;; Keywords: helm, help
;; Version: 1.12
;; Package-Requires: ((helm "1.5"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; This package is a replacement of `describe-bindings' for Helm.

;; Usage:
;;
;; You can use this package independently from Helm - in particular,
;; you don't need to turn on `helm-mode' to be able to use this.  Helm
;; just needs to be installed.
;;
;; Add followings on your .emacs.
;;
;;   (require 'helm-descbinds)
;;   (helm-descbinds-mode)
;;
;; or use customize to set `helm-descbinds-mode' to t.
;;
;; Now, `describe-bindings' is replaced with `helm-descbinds'. As
;; usual, type `C-h b', or any incomplete key sequence plus C-h , to
;; run `helm-descbinds'.  The bindings are presented in a similar way
;; as `describe-bindings ' does, but you can use completion to find
;; the command you searched for and execute it, or view it's
;; documentation.
;;
;; In the Helm completions buffer, you match key bindings with the
;; Helm interface:
;;
;;  - When you type RET, the selected candidate command is executed.
;;
;;  - When you hit RET on a prefix key, the candidates are narrowed to
;;    this prefix
;;
;;  - When you type TAB, you can select "Execute", "Describe" or "Find
;;    Function" by the menu (i.e. these are the available "actions"
;;    and are of course also available via their usual shortcuts).
;;
;;  - When you type C-z (aka "persistent action"), the selected
;;    command is described without quitting Helm.



;;; Code:

(eval-when-compile (require 'cl-lib)) ;cl-loop
(require 'helm)

(defvar which-key-mode)
(declare-function which-key-mode "ext:which-key.el")

(defgroup helm-descbinds nil
  "A convenient `describe-bindings' with `helm'."
  :prefix "helm-descbinds-"
  :group 'helm)

(defface helm-descbinds-key '((t :box t))
  "Face for keys in helm-descbinds.")

(defface helm-descbinds-binding '((t :inherit font-lock-warning-face))
  "Face for bindings in helm-descbinds.")

(defcustom helm-descbinds-actions
  '(("Execute" . helm-descbinds-action:execute)
    ("Describe" . helm-descbinds-action:describe)
    ("Find Function" . helm-descbinds-action:find-func))
  "Actions of selected candidate."
  :type '(repeat
	  (cons
	   :tag "Action"
	   (string :tag "Name")
	   (function :tag "Function"))))

(defcustom helm-descbinds-candidate-formatter
  #'helm-descbinds-default-candidate-formatter
  "Candidate formatter function.
This function will be called with two arguments KEY and BINDING."
  :type 'function)

(defcustom helm-descbinds-window-style 'one-window
  "Window splitting style."
  :type '(choice
	  (const :tag "One Window" one-window)
	  (const :tag "Same Window" same-window)
	  (const :tag "Split Window" split-window)))

(defcustom helm-descbinds-section-order
  '("Major Mode Bindings" "Minor Mode Bindings" "Global Bindings")
  "A list of section order by name regexp."
  :type '(repeat (regexp :tag "Regexp")))

(defcustom helm-descbinds-disable-which-key t
  "Prevent using `which-key-mode' and `helm-descbinds-mode' together.
When nil allow using both packages together."
  :type 'boolean)

(defvar helm-descbinds-prefix-help
  "This is a prefix key, hit RET to see all bindings using this prefix.

A “prefix key” is a key sequence whose binding is a keymap.  The keymap
defines what to do with key sequences that extend the prefix key.  For
example, ‘C-x’ is a prefix key, and it uses a keymap that is also stored
in the variable ‘ctl-x-map’.  This keymap defines bindings for key
sequences starting with ‘C-x’.
See (info \"(elisp) Prefix Keys\") for more infos."
  "A brief documentation of what is a prefix key.
This string is extracted from Elisp manual,
see (info \"(elisp) Prefix Keys\").")

(defvar helm-descbinds-Orig-describe-bindings (symbol-function 'describe-bindings))
(defvar helm-descbind--initial-full-frame helm-full-frame)
(defvar helm-descbinds--Orig-which-key-mode nil)

;; Prevent usage of both which-key and helm-descbinds, which-key is
;; starting a nasty timer which override helm-descbinds if user do not
;; type fast C-h after a prefix command e.g. C-x, ensure which-key is
;; disabled when turning on helm-descbinds-mode and reenabled (if
;; already enabled and available) when disabling helm-descbinds-mode.
(defun helm-descbinds--override-which-key (&rest _args)
  "Used to override `which-key-mode' by advice."
  (error "`which-key-mode' can't be used with `helm-descbinds-mode'"))

;;;###autoload
(define-minor-mode helm-descbinds-mode
    "Use `helm' for `describe-bindings'.

When this mode is enabled, pressing `C-h' after a prefix key
e.g. `C-x' will pop up a helm buffer showing all the keys starting
with this prefix, one can then execute the command bound to this key,
look at the docstring or find the definition of this command."
  :group 'helm-descbinds
  :global t
  (if helm-descbinds-mode
      (progn
        (advice-add 'describe-bindings :override #'helm-descbinds)
        (global-unset-key (kbd "<help> C-h"))
        ;; Which-key mode has been started before enabling helm-descbinds-mode
        (when (and (fboundp 'which-key-mode) which-key-mode
                   helm-descbinds-disable-which-key)
          (setq helm-descbinds--Orig-which-key-mode which-key-mode)
          (which-key-mode -1)
          (message "Disabling `which-key-mode' which would defeat helm-descbinds"))
        ;; Which-key mode is not started yet, prevent starting it
        ;; We don't check for (fboundp 'which-key-mode) in case
        ;; which-key is not already installed.
        (when helm-descbinds-disable-which-key
          (advice-add 'which-key-mode :override #'helm-descbinds--override-which-key)))
    (advice-remove 'describe-bindings #'helm-descbinds)
    (global-set-key (kbd "<help> C-h") 'help-for-help)
    (when (and (fboundp 'which-key-mode) helm-descbinds-disable-which-key)
      (advice-remove 'which-key-mode #'helm-descbinds--override-which-key)
      (which-key-mode helm-descbinds--Orig-which-key-mode))))

;;;###autoload
(defun helm-descbinds-install ()
  "Use `helm-descbinds' as a replacement of `describe-bindings'."
  (interactive)
  (helm-descbinds-mode 1))
(make-obsolete 'helm-descbinds-install 'helm-descbinds-mode "1.08")

;;;###autoload
(defun helm-descbinds-uninstall ()
  "Restore original `describe-bindings'."
  (interactive)
  (helm-descbinds-mode -1))
(make-obsolete 'helm-descbinds-uninstall 'helm-descbinds-mode "1.08")

(defun helm-descbinds-all-sections (buffer &optional prefix menus)
  "Collect data from `describe-buffer-bindings' output.

Return a list of sections, each section beeing an alist composed of
\(KEY . COMMAND)."
  (with-temp-buffer
    (let ((indent-tabs-mode t))
      (describe-buffer-bindings buffer prefix menus))
    (goto-char (point-min))
    (let ((header-p (not (= (char-after) ?\f))) ;; ?\f == ^L
	  sections header section)
      (while (not (eobp))
	(cond
	 (header-p
	  (setq header (buffer-substring-no-properties
			(point) (line-end-position)))
	  (setq header-p nil)
	  (forward-line 3))
	 ((= (char-after) ?\f) ;; ?\f == ^L
	  (push (cons header (nreverse section)) sections)
	  (setq section nil)
	  (setq header-p t))
	 ((looking-at "^[ \t]*$")) ;; ignore
	 (t
	  (let ((binding-start (save-excursion
				 (and (re-search-forward "\t+" nil t)
				      (match-end 0))))
		key binding)
	    (when binding-start
              ;; For some reasons on Emacs-29 key description is
              ;; sometimes 2 lines long, it seems it happens with menus
              ;; but `describe-buffer-bindings' is always called with
              ;; MENUS == nil...?
	      (setq key (car (split-string
                              (buffer-substring-no-properties
                               (point) binding-start)
                              "\n" t))
		    key (replace-regexp-in-string "^[ \t\n]+" "" key)
		    key (replace-regexp-in-string "[ \t\n]+$" "" key))
	      (goto-char binding-start)
	      (setq binding (buffer-substring-no-properties
			     binding-start
			     (line-end-position)))
	      (unless (member binding '("self-insert-command"))
		(push (cons key binding) section))))))
	(forward-line))
      (push (cons header (nreverse section)) sections)
      (nreverse sections))))

(defun helm-descbinds-action:execute (candidate)
  "An action that execute selected CANDIDATE command."
  (let ((x (cdr candidate))
        (helm-full-frame helm-descbind--initial-full-frame))
    (cond
     ((equal x "Keyboard Macro")
      (command-execute (kbd (car candidate))))
     ((stringp x)
      (insert x))
     ((commandp x)
      ;; Using a timer here trigger a timer error with help-for-help
      ;; (and perhaps others that use a timer themselves), so use
      ;; directly `call-interactively'.
      (call-interactively x)))))

(defun helm-descbinds-display-string-in-help (str)
  "Display string STR in an help buffer."
  (with-current-buffer (help-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert str))
    (help-mode)
    (display-buffer (current-buffer))))

(defun helm-descbinds-action:describe (candidate)
  "An action that describe selected CANDIDATE function."
  (let ((name (cdr candidate)))
    (when (member name '("ignore" "ignore-event"))
      (setq name 'ignore))
    (pcase name
      ((pred (string= "Keyboard Macro"))
       (describe-key (kbd (car candidate))))
      ((pred (string= "Prefix Command"))
       (helm-descbinds-display-string-in-help
        helm-descbinds-prefix-help))
      ((guard (and (symbolp name) (fboundp name)))
       (describe-function name)))))

(defun helm-descbinds-action:find-func (candidate)
  "An action that find selected CANDIDATE function."
  (find-function (cdr candidate)))

(defun helm-descbinds-default-candidate-formatter (key binding)
  "Default candidate formatter."
  (format "%-10s\t%s"
          (propertize key 'face 'helm-descbinds-key)
          (propertize binding 'face 'helm-descbinds-binding)))

(defun helm-descbinds-order-section (section)
  "Return the number in which SECTION should appear.

This is used to reorder all sections as sources."
  (cl-loop for n = 0 then (1+ n)
           for regexp in helm-descbinds-section-order
           if (and (car section) (string-match regexp (car section)))
           return n
           finally
           return n))

(defun helm-descbinds-transform-candidates (candidates)
  "Transform CANDIDATES for display."
  (cl-loop for (key . command) in candidates
           for sym = (intern-soft command)
           collect
           (cons (funcall helm-descbinds-candidate-formatter key command)
                 (cons key (if (commandp sym) sym command)))))

(defun helm-descbinds-action-transformer (actions cand)
  "Default action transformer for `helm-descbinds'.
Provide a useful behavior for prefix commands."
  (if (stringp (cdr cand))
      (helm-make-actions
       "helm-descbinds this prefix"
       (lambda (cand)
         (let ((binding (car cand)))
           (if (member binding '("<make-frame-visible>" "<iconify-frame>"))
               (message "Key is bound to `ignore' because there is nothing to do")
             (describe-bindings (kbd binding))))))
      actions))

(defun helm-descbinds-sources (buffer &optional prefix menus)
  "Build helm-descbinds sources for BUFFER.
If PREFIX is specified only sources for bindings starting with PREFIX
are shown.  Optionally if MENUS is specified show commands that have a
starting point in menus."
  (mapcar
   (lambda (section)
     (helm-descbinds-source (car section) (cdr section)))
   (sort
    (helm-descbinds-all-sections buffer prefix menus)
    (lambda (a b)
      (< (helm-descbinds-order-section a)
         (helm-descbinds-order-section b))))))

(defclass helm-descbinds-source-class (helm-source-sync) ())

(defun helm-descbinds-source (name candidates)
  "Return a helm source named NAME for displaying CANDIDATES."
  (when (and name candidates)
    (helm-make-source name 'helm-descbinds-source-class
      :candidates candidates
      :candidate-transformer #'helm-descbinds-transform-candidates
      :persistent-action #'helm-descbinds-action:describe
      :action-transformer #'helm-descbinds-action-transformer
      :action 'helm-descbinds-actions)))

;;;###autoload
(defun helm-descbinds (&optional prefix buffer)
  "A convenient helm version of `describe-bindings'.

Turning on `helm-descbinds-mode' is the recommended way to
install this command to replace `describe-bindings'.

You complete against a list of keys + command pairs presented in
a similar way as `describe-bindings' does, split into sections
defined by the types of the key bindings (minor and major modes,
global bindings, etc).

The default action executes a command as if the binding had been
entered, or narrows the commands according to a prefix key,
respectively.

The persistent action pops up a help buffer for the selected
command without quitting.

For key translation maps, the default actions are not very
useful, yet they are listed for completeness."
  (interactive)
  (let ((old-helm-full-frame helm-full-frame)
        (helm-full-frame (and (not (minibufferp))
                              (memq helm-descbinds-window-style
                                    '(same-window one-window))))
        (helm-before-initialize-hook (if (and (not (minibufferp))
                                              (eq helm-descbinds-window-style
                                                  'one-window))
                                         (cons 'delete-other-windows
                                               helm-before-initialize-hook)
                                       helm-before-initialize-hook))
        (enable-recursive-minibuffers t))
    (setq helm-descbind--initial-full-frame old-helm-full-frame)
    (helm :sources (helm-descbinds-sources
                    (or buffer (current-buffer)) prefix)
          :buffer "*helm-descbinds*"
          :resume 'noresume
          :allow-nest t)))

(provide 'helm-descbinds)

;;; helm-descbinds.el ends here

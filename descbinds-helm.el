;;; descbinds-helm.el --- Yet Another `describe-bindings' with `helm'.

;; Copyright (C) 2008, 2009, 2010  Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Copyright (C) 2012  Michael Markert <markert.michael@googlemail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: helm, help
;; Version: 1.06
;; Time-stamp: <2012-03-18 01:35:51 cofi>

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
;; This package is a replacement of `describe-bindings'.

;;; Usage:
;; Add followings on your .emacs.
;;
;;   (require 'descbinds-helm)
;;   (descbinds-helm-install)
;;
;; Now, `describe-bindings' is replaced to `descbinds-helm'. Type
;; `C-h b', `C-x C-h' these run `descbinds-helm'.
;;
;; In the Helm buffer, you can select key-binds with helm interface.
;;
;;  - When type RET, selected candidate command is executed.
;;
;;  - When type ESC, You can "Execute", "Describe Function" or "Find
;;    Function" by the menu.
;;
;;  - When type C-z, selected command is described without quiting.

;;; History:
;; 2012-03-18 Michael Markert <markert.michael@googlemail.com>
;;   * descbinds-helm.el: Version 1.06
;;   port to helm
;;
;; 2010-02-05   Taiki SUGAWARA  <sugawara_t@ariel-networks.com>
;;
;;   * descbinds-anything.el: Version 1.05
;;   bug fix.
;;
;; 2010-02-02 UTC  Taiki SUGAWARA  <buzz.taiki@gmail.com>
;;
;;   * descbinds-anything.el: Version 1.04
;;   add sorting feature.
;;   separete sorce creation function.
;;   add persistent action.
;;
;; 2009-03-29 UTC  Taiki SUGAWARA  <buzz.taiki@gmail.com>
;;
;;   * descbinds-anything.el: Version 1.03
;;   fix typo.
;;
;; 2008-11-16 UTC  Taiki SUGAWARA  <buzz.taiki@gmail.com>
;;
;;   * descbinds-anything.el: Version 1.02
;;   bound `indent-tabs-mode` to t for nil environment.
;;
;; 2008-11-16 UTC  Taiki SUGAWARA  <buzz.taiki@gmail.com>
;;
;;   * descbinds-anything.el: fix infinitive-loop when binding-line
;;   has not tab.

;;; Code:

(require 'helm)

(defgroup descbinds-helm nil
  "Yet Another `describe-bindings' with `helm'."
  :prefix "descbinds-helm-"
  :group 'helm)

(defcustom descbinds-helm-actions
  '(("Execute" . descbinds-helm-action:execute)
    ("Describe Function" . descbinds-helm-action:describe)
    ("Find Function" . descbinds-helm-action:find-func))
  "Actions of selected candidate."
  :type '(repeat
	  (cons
	   :tag "Action"
	   (string :tag "Name")
	   (function :tag "Function")))
  :group 'descbinds-helm)

(defcustom descbinds-helm-candidate-formatter
  'descbinds-helm-default-candidate-formatter
  "Candidate formatter function.
This function called two argument KEY and BINDING."
  :type 'function
  :group 'descbinds-helm)


(defcustom descbinds-helm-window-style 'one-window
  "Window splitting style."
  :type '(choice
	  (const :tag "One Window" one-window)
	  (const :tag "Same Window" same-window)
	  (const :tag "Split Window" split-window))
  :group 'descbinds-helm)


(defcustom descbinds-helm-section-order
  '("Major Mode Bindings" "Minor Mode Bindings" "Global Bindings")
  "A list of section order by name regexp."
  :type '(repeat (regexp :tag "Regexp"))
  :group 'descbinds-helm)

(defcustom descbinds-helm-source-template
  '((candidate-transformer . descbinds-helm-transform-candidates)
    (persistent-action . descbinds-helm-action:describe)
    (action-transformer . descbinds-helm-transform-actions))
  "A template of `descbinds-helm' source."
  :type 'sexp
  :group 'descbinds-helm)


(defun descbinds-helm-all-sections (buffer &optional prefix menus)
  (with-temp-buffer
    (let ((indent-tabs-mode t))
      (describe-buffer-bindings buffer prefix menus))
    (goto-char (point-min))
    (let ((header-p (not (= (char-after) ?\f)))
	  sections header section)
      (while (not (eobp))
	(cond
	 (header-p
	  (setq header (buffer-substring-no-properties
			(point)
			(line-end-position)))
	  (setq header-p nil)
	  (forward-line 3))
	 ((= (char-after) ?\f)
	  (push (cons header (nreverse section)) sections)
	  (setq section nil)
	  (setq header-p t))
	 ((looking-at "^[ \t]*$")
	  ;; ignore
	  )
	 (t
	  (let ((binding-start (save-excursion
				 (and (re-search-forward "\t+" nil t)
				      (match-end 0))))
		key binding)
	    (when binding-start
	      (setq key (buffer-substring-no-properties (point) binding-start)
		    key (replace-regexp-in-string"^[ \t\n]+" "" key)
		    key (replace-regexp-in-string"[ \t\n]+$" "" key))
	      (goto-char binding-start)
	      (setq binding (buffer-substring-no-properties
			     binding-start
			     (line-end-position)))
	      (unless (member binding '("self-insert-command"))
		(push (cons key binding) section))))))
	(forward-line))
      (push (cons header (nreverse section)) sections)
      (nreverse sections))))

(defun descbinds-helm-action:execute (candidate)
  "An action that execute selected CANDIDATE command."
  (call-interactively (cdr candidate)))

(defun descbinds-helm-action:describe (candidate)
  "An action that describe selected CANDIDATE function."
  (describe-function (cdr candidate)))

(defun descbinds-helm-action:find-func (candidate)
  "An action that find selected CANDIDATE function."
  (find-function (cdr candidate)))

(defun descbinds-helm-default-candidate-formatter (key binding)
  "Default candidate formatter."
  (format "%-10s\t%s" key binding))

(defun descbinds-helm-sort-sections (sections)
  (flet ((order (x)
		(loop for n = 0 then (1+ n)
		      for regexp in descbinds-helm-section-order
		      if (and (car x) (string-match regexp (car x))) return n
		      finally return n)))
    (sort sections (lambda (a b)
		     (< (order a) (order b))))))

(defun descbinds-helm-transform-candidates (candidates)
  (mapcar
   (lambda (pair)
     (cons (funcall descbinds-helm-candidate-formatter
		    (car pair) (cdr pair))
	   (cons (car pair) (intern-soft (cdr pair)))))
   candidates))

(defun descbinds-helm-transform-actions (actions candidate)
  (and (commandp (cdr candidate)) (or actions descbinds-helm-actions)))

(defun descbinds-helm-sources (buffer &optional prefix menus)
  (mapcar
   (lambda (section)
     (descbinds-helm-source (car section) (cdr section)))
   (descbinds-helm-sort-sections
    (descbinds-helm-all-sections buffer prefix menus))))

(defun descbinds-helm-source (name candidates)
  `((name . ,name)
    (candidates . ,candidates)
    ,@descbinds-helm-source-template))

(defun descbinds-helm (&optional prefix buffer)
  "Yet Another `describe-bindings' with `helm'."
  (interactive)
  (let ((helm-samewindow (and (not (minibufferp))
                            (memq descbinds-helm-window-style
                                  '(same-window one-window))))
        (helm-before-initialize-hook (if (and (not (minibufferp))
                                            (eq descbinds-helm-window-style 'one-window))
                                         (cons 'delete-other-windows helm-before-initialize-hook)
                                       helm-before-initialize-hook)))
    (helm :sources (descbinds-helm-sources (or buffer (current-buffer)) prefix))))

(defvar descbinds-helm-Orig-describe-bindings
  (symbol-function 'describe-bindings))

(defun descbinds-helm-install ()
  "Use `descbinds-helm' as a replacement of `describe-bindings'."
  (interactive)
  (fset 'describe-bindings 'descbinds-helm))

(defun descbinds-helm-uninstall ()
  "Restore original `describe-bindings'."
  (interactive)
  (fset 'describe-bindings descbinds-helm-Orig-describe-bindings))

(provide 'descbinds-helm)

;;; descbinds-helm.el ends here

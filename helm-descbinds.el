;;; helm-descbinds.el --- Yet Another `describe-bindings' with `helm'.

;; Copyright (C) 2008, 2009, 2010  Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Copyright (C) 2012  Michael Markert <markert.michael@googlemail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: helm, help
;; Version: 1.07

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
;;   (require 'helm-descbinds)
;;   (helm-descbinds-install)
;;
;; Now, `describe-bindings' is replaced to `helm-descbinds'. Type
;; `C-h b', `C-x C-h' these run `helm-descbinds'.
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
;;   * helm-descbinds.el: Version 1.07
;;   make strings bound to keys insertable
;;
;; 2012-03-18 Michael Markert <markert.michael@googlemail.com>
;;   * helm-descbinds.el: Version 1.06
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

(defgroup helm-descbinds nil
  "Yet Another `describe-bindings' with `helm'."
  :prefix "helm-descbinds-"
  :group 'helm)

(defcustom helm-descbinds-actions
  '(("Execute" . helm-descbinds-action:execute)
    ("Describe Function" . helm-descbinds-action:describe)
    ("Find Function" . helm-descbinds-action:find-func))
  "Actions of selected candidate."
  :type '(repeat
	  (cons
	   :tag "Action"
	   (string :tag "Name")
	   (function :tag "Function")))
  :group 'helm-descbinds)

(defcustom helm-descbinds-string-actions
  '(("Insert" . helm-descbinds-action:insert-string))
  "Actions of selected string candidate."
  :type '(repeat
          (cons
           :tag "Action"
           (string :tag "Name")
           (function :tag "Function")))
  :group 'helm-descbinds)

(defcustom helm-descbinds-strings-to-ignore
  '("Keyboard Macro" "Prefix Command")
  "Strings to ignore as a possible string candidate."
  :type '(repeat string))

(defcustom helm-descbinds-candidate-formatter
  'helm-descbinds-default-candidate-formatter
  "Candidate formatter function.
This function called two argument KEY and BINDING."
  :type 'function
  :group 'helm-descbinds)


(defcustom helm-descbinds-window-style 'one-window
  "Window splitting style."
  :type '(choice
	  (const :tag "One Window" one-window)
	  (const :tag "Same Window" same-window)
	  (const :tag "Split Window" split-window))
  :group 'helm-descbinds)


(defcustom helm-descbinds-section-order
  '("Major Mode Bindings" "Minor Mode Bindings" "Global Bindings")
  "A list of section order by name regexp."
  :type '(repeat (regexp :tag "Regexp"))
  :group 'helm-descbinds)

(defcustom helm-descbinds-source-template
  '((candidate-transformer . helm-descbinds-transform-candidates)
    (persistent-action . helm-descbinds-action:describe)
    (action-transformer . helm-descbinds-transform-actions))
  "A template of `helm-descbinds' source."
  :type 'sexp
  :group 'helm-descbinds)


(defun helm-descbinds-all-sections (buffer &optional prefix menus)
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

(defun helm-descbinds-action:execute (candidate)
  "An action that execute selected CANDIDATE command."
  (call-interactively (cdr candidate)))

(defun helm-descbinds-action:describe (candidate)
  "An action that describe selected CANDIDATE function."
  (describe-function (cdr candidate)))

(defun helm-descbinds-action:find-func (candidate)
  "An action that find selected CANDIDATE function."
  (find-function (cdr candidate)))

(defun helm-descbinds-action:insert-string (candidate)
  "An action that inserts the string CANDIDATE."
  (insert (cdr candidate)))

(defun helm-descbinds-default-candidate-formatter (key binding)
  "Default candidate formatter."
  (format "%-10s\t%s" key binding))

(defun helm-descbinds-sort-sections (sections)
  (flet ((order (x)
		(loop for n = 0 then (1+ n)
		      for regexp in helm-descbinds-section-order
		      if (and (car x) (string-match regexp (car x))) return n
		      finally return n)))
    (sort sections (lambda (a b)
		     (< (order a) (order b))))))

(defun helm-descbinds-transform-candidates (candidates)
  (mapcar
   (lambda (pair)
     (let ((key (car pair))
           (command (cdr pair)))
       (cons (funcall helm-descbinds-candidate-formatter key command)
             (cons key (or (intern-soft command)
                           (unless (member command helm-descbinds-strings-to-ignore)
                             command))))))
   candidates))

(defun helm-descbinds-transform-actions (actions candidate)
  (or (and (commandp (cdr candidate) 'interactive) (or actions helm-descbinds-actions))
      (and (stringp (cdr candidate)) helm-descbinds-string-actions)))

(defun helm-descbinds-sources (buffer &optional prefix menus)
  (mapcar
   (lambda (section)
     (helm-descbinds-source (car section) (cdr section)))
   (helm-descbinds-sort-sections
    (helm-descbinds-all-sections buffer prefix menus))))

(defun helm-descbinds-source (name candidates)
  `((name . ,name)
    (candidates . ,candidates)
    ,@helm-descbinds-source-template))

;;;###autoload
(defun helm-descbinds (&optional prefix buffer)
  "Yet Another `describe-bindings' with `helm'."
  (interactive)
  (let ((helm-samewindow (and (not (minibufferp))
                            (memq helm-descbinds-window-style
                                  '(same-window one-window))))
        (helm-before-initialize-hook (if (and (not (minibufferp))
                                            (eq helm-descbinds-window-style 'one-window))
                                         (cons 'delete-other-windows helm-before-initialize-hook)
                                       helm-before-initialize-hook)))
    (helm :sources (helm-descbinds-sources (or buffer (current-buffer)) prefix))))

(defvar helm-descbinds-Orig-describe-bindings
  (symbol-function 'describe-bindings))

;;;###autoload
(defun helm-descbinds-install ()
  "Use `helm-descbinds' as a replacement of `describe-bindings'."
  (interactive)
  (fset 'describe-bindings 'helm-descbinds))

;;;###autoload
(defun helm-descbinds-uninstall ()
  "Restore original `describe-bindings'."
  (interactive)
  (fset 'describe-bindings helm-descbinds-Orig-describe-bindings))

(provide 'helm-descbinds)

;;; helm-descbinds.el ends here

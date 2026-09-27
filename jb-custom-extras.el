;;; -*- lexical-binding: t -*-
;;; jb-custom-extras.el --- Extra functions for working with customizable user options

;; Filename: jb-custom-extras.el
;; Description: Extra functions for working with customizable user options
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Assisted-by: gptel:openrouter/auto-beta
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2026, Joe Bloggs, all rites reversed.
;; Created: 2026-09-13 15:33:23
;; Version: 0.1
;; Last-Updated: 2026-09-13 15:33:23
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/jb-custom-extras
;; Keywords: internal
;; Compatibility: GNU Emacs 30.2
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; cl-lib cus-edit wid-edit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; Extra functions for working with customizable user options
;; 
;;;;;;;;

;;; Commands:
;;

;;
;; All of the above can be customized by:
;;      M-x customize-group RET jb-custom-extras RET
;;

;;; Installation:
;;
;; Put jb-custom-extras.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'jb-custom-extras)

;;; History:

;;; Require
(require 'cl-lib)
(require 'cus-edit)
(require 'wid-edit)

;;; Code:

;; REMEMBER TODO ;;;###autoload's 
;; Extra prompting functions for customization types
(defsubst custom-extras-get-tag (w)
  (and (widgetp w) (consp w) (memq :tag w)
       (widget-get w :tag)))

;;;###autoload
(defun custom-extras-list-prompt-value (widget prompt value unbound)
  (let ((args (widget-get widget :args))
	(listprompt (or (custom-extras-get-tag widget) prompt)))
    (cl-flet ((promptusr (c j)
		(widget-prompt-value c (format "%s [%d] %s: "
					       listprompt j
					       (or (custom-extras-get-tag c) ""))
				     (nth (1- j) value) unbound)))
      (cl-loop for child in args
               for i from 1
	       for pos = (and (listp child)
			      (cl-position :inline child))
	       if (and pos (nth (1+ pos) child))
	       append (promptusr child i)
	       else collect (promptusr child i)))))

;;;###autoload
(defun custom-extras-vector-prompt-value (widget prompt value unbound)
  (vconcat (custom-extras-list-prompt-value widget prompt (append value nil) unbound)))

;;;###autoload
(defun custom-extras-repeat-prompt-value (widget prompt value unbound)
  (let* ((child (or (car (widget-get widget :args)) 'sexp))
	 (parentprompt (or (custom-extras-get-tag widget) prompt))
	 (childprompt (or (custom-extras-get-tag child) parentprompt))
         (n (read-number (format "No. of elements for %s: " parentprompt)
                         (and (sequencep value) (length value)))))
    (cl-loop for i from 1 upto n
             collect (widget-prompt-value child (format "%s [%d]: " parentprompt i)
					  (nth (1- i) value) unbound))))

;;;###autoload
(defun custom-extras-cons-prompt-value (widget prompt value unbound)
  (let ((args (widget-get widget :args))
	(parentprompt (or (custom-extras-get-tag widget) prompt)))
    (cons (let ((w (or (car args) 'sexp)))
	    (widget-prompt-value w (format "%s: %s " parentprompt
					   (or (custom-extras-get-tag w) "car"))
				 (car value) unbound))
          (let ((w (or (cadr args) 'sexp)))
	    (widget-prompt-value w (format "%s: %s " parentprompt
					   (or (custom-extras-get-tag w) "cdr"))
				 (cdr value) unbound)))))

;;;###autoload
(defun custom-extras-set-prompt-value (widget prompt _value _unbound)
  (cl-loop for child in (widget-get widget :args)
	   for pos = (and (listp child)
			  (cl-position :inline child))
	   if (and pos (nth (1+ pos) child))
	   append (widget-prompt-value child prompt nil t)
	   else when (y-or-n-p (format "Include %s? "
				       (or (widget-get child :tag)
					   (prin1-to-string child))))
           collect (widget-prompt-value child prompt nil t)))

;;;###autoload
(defun custom-extras-alist-prompt-value (widget prompt value unbound)
  (let* ((kt (or (widget-get widget :key-type) 'sexp))
	 (ktag (custom-extras-get-tag kt))
         (vt (or (widget-get widget :value-type) 'sexp))
	 (vtag (custom-extras-get-tag vt))
         (n (read-number (format "No. of entries for %s: "
				 (or (custom-extras-get-tag widget) prompt))
                         (and (listp value) (length value)))))
    (cl-loop for i from 1 upto n
             collect (cons (widget-prompt-value kt (format "%s [%d]: " (or ktag "Key") i)
						(car (nth (1- i) value)) unbound)
                           (widget-prompt-value vt (format "%s [%d]: " (or vtag "Value") i)
						(cdr (nth (1- i) value)) unbound)))))

;; Note: don't be tempted to try and account for :inline items in choice widgets
;;  the final `widget-prompt-value' call will give an error about mismatching types.
;;;###autoload
(defun custom-extras-choice-prompt-value (widget prompt value unbound)
  (let* ((args (widget-get widget :args))
	 (tag (custom-extras-get-tag widget))
         (items
          (let ((seen (make-hash-table :test 'equal)))
            (mapcar (lambda (child)
		      (let* ((tag  (custom-extras-get-tag child))
			     (type (widget-type child))
			     (base (cond (tag)
					 ((memq type '(const function-item variable-item))
					  (format "%s" (widget-get child :value)))
					 (t (format "%s" type))))
			     (display base)
			     (n 2))
			;; Ensure unique display strings (two consts with no tag)
			(while (gethash display seen)
			  (setq display (format "%s [%d]" base n))
			  (cl-incf n))
			(puthash display t seen)
			(cons display child)))
		    args)))
         (display-strings (mapcar #'car items))
         (default (when (and value (not unbound))
                    (car (cl-find-if (lambda (item) (widget-apply (cdr item) :match value))
				     items))))
         (chosen (ido-completing-read (format "%s: " (or tag prompt))
				      display-strings nil t nil
				      (widget-get widget :history)
				      default))
	 (chosedef (string= chosen default)))
    ;; Recursively prompt for the chosen alternative's value
    (widget-prompt-value (cdr (assoc chosen items)) prompt
			 (when chosedef value)
			 (or unbound (not chosedef)))))

;;;###autoload
(defun custom-extras-plist-prompt-value (widget prompt value unbound)
  (let* ((key-type   (or (widget-get widget :key-type)  'symbol))
	 (ktag       (custom-extras-get-tag key-type))
         (value-type (or (widget-get widget :value-type) 'sexp))
	 (vtag       (custom-extras-get-tag value-type))
         (cur-len    (and (plistp value) (/ (length value) 2)))
	 (tag        (custom-extras-get-tag widget))
         (count      (read-number (format "No. of key-value pairs for %s: "
					  (or tag prompt))
				  (or cur-len 0)))
         result)
    (dotimes (i count)
      (let ((k (widget-prompt-value key-type (format "%s [%d]: " (or ktag "Key") (1+ i))
				    (nth (* i 2) value) unbound))
            (v (widget-prompt-value value-type (format "%s [%d]: " (or vtag "Value") (1+ i))
				    (nth (1+ (* i 2)) value) unbound)))
        (setq result (plist-put result k v))))
    result))

;; Install previously defined functions into associated customization type symbols
(dolist (type '((list     . custom-extras-list-prompt-value)
                (group    . custom-extras-list-prompt-value)
		(vector   . custom-extras-vector-prompt-value)
                (repeat   . custom-extras-repeat-prompt-value)
                (cons     . custom-extras-cons-prompt-value)
                (set      . custom-extras-set-prompt-value)
                (alist    . custom-extras-alist-prompt-value)
		(choice   . custom-extras-choice-prompt-value)
		(radio    . custom-extras-choice-prompt-value)
		(plist    . custom-extras-plist-prompt-value)))
  (let ((def (get (car type) 'widget-type)))
    (when def
      (plist-put (cdr def) :prompt-value (cdr type)))))

(defun custom-type--norm (type)
  "Normalise TYPE: drop keyword args, rewrite `alist' as `repeat' of `cons'."
  (pcase type
    ((pred symbolp) (list type))
    (`(alist . ,pl) `(repeat (cons ,(or (plist-get pl :key-type) 'sexp)
                                   ,(or (plist-get pl :value-type) 'sexp))))
    (`(,head . ,args)
     (while (keywordp (car args)) (setq args (cddr args)))
     (cons head args))))

(defun custom-type--resolve-choice (alts value path)
  "Pick the first alternative in ALTS matching VALUE that admits PATH."
  (cl-flet ((matches-p (a) (widget-apply (widget-convert a) :match value))
            (admits-p  (a) (condition-case nil
                               (progn (custom-type-focus a value path) t)
                             (error nil))))
    (or (seq-find #'admits-p (seq-filter #'matches-p alts))
        (error "No alternative of %S matches %S and admits path %S" alts value path))))

(defun custom-type-focus (type value path)
  "Follow PATH into VALUE, an instance of the custom TYPE.

Return a list (SUBTYPE SUBVALUE PUT) where SUBTYPE is the custom type
found at the end of PATH, SUBVALUE is the corresponding part of VALUE,
and PUT is a function of one argument that returns a copy of VALUE with
that part replaced by its argument.  VALUE itself is never modified.
With an empty PATH the result is (TYPE VALUE #'identity).

TYPE may be any customization type as stored in the `custom-type'
property of a user option, e.g. (get \\='my-var \\='custom-type).
Keyword arguments such as :tag are ignored, and (alist :key-type K
:value-type V) is treated as (repeat (cons K V)).

PATH is a list of steps, each applied to the result of the previous one.
The steps allowed depend on the type reached so far:

  TYPE REACHED                        NEXT STEP           DESCRIPTION 
  -------------------------------------------------------------------
  (cons K V)                          0 / `key' / `car'   K, the car of the value
  (alist :key-type K :value-type V)   0 / `key' / `car'   K, the car of the value
  (cons K V)                          `cdr' / `value'     V, cdr of the value
  (alist :key-type K :value-type V)   `cdr' / `value'     V, cdr of the value
  (repeat E) / alist                  N (integer)         E, the Nth element (0-based)
  (list/group T0 T1 ...)              N (integer)         Tn, the Nth component
  (repeat E) / alist                  `cdr' / `value'     all but first component
  (list/group T0 T1 ...)              `cdr' / `value'     all but first component,
                                                          (list T1..)
  (choice/radio A0 A1 ...)            (alt N)             AN, the Nth alternative

The symbols `car', `cadr' `caddr', and `cadddr' are synonyms for 0, 1, 2 & 3,
and `first', `second', `third', `fourth', `fifth', `sixth', `seventh', `eighth',
`ninth' & `tenth' are synonyms for 0, 1, 2, 3, 4, 5, 6, 7, 8, & 9 respectively,
and may be used wherever an integer step is allowed.
`key' and `value' are synonyms for 0 and `cdr', intended for alist entries.
Note that 1 (or `cadr') is not allowed on a cons type; use `cdr'/`value' instead.

If a `choice' or `radio' type is reached and the next step is not (alt N),
the alternative is selected automatically by matching VALUE against each alternative
in turn, without consuming a step.

Integer steps must index an existing element of VALUE, or one element past
the end (in which case a value will be appended). An error is signalled if
a step cannot be applied to the type reached, or if no alternative of a choice
matches VALUE.

Example: for a variable of type (alist :key-type string :value-type integer)
whose value is ((\"a\" . 1) (\"b\" . 2)), the path (1 value) yields (integer 2 PUT),
and (funcall PUT 5) returns \((\"a\" . 1) (\"b\" . 5)).
For type (repeat (list string function)) with an analogous value, the path (1 1)
focuses on the function of the second entry."
  ;; Note: this won't work properly without `lexical-binding' enabled
  (if (null path)
      (list type value #'identity)
    (let* ((step (pcase (car path) ('key 0) ('value 'cdr) ('car 0) ('cadr 1) ('caddr 2)
			('cadddr 3) ('first 0) ('second 1) ('third 2) ('fourth 3) ('fifth 4)
			('sixth 5) ('seventh 5) ('eighth 7) ('ninth 8) ('tenth 9) (s s)))
           (nt (custom-type--norm type))
           (put-nth (lambda (x) (let ((l (length value)))
				  (if (= n l) (append value (list x))
				    (if (or (> n l) (< n 0))
					(error "Invalid index: %S (should be between 0 & %S)" n l)
				      (let ((c (copy-sequence value)))
					(setf (nth n c) x)
					c)))))))
      ;; Resolve an implicit choice by matching VALUE and PATH, without consuming a step.
      (while (and (memq (car nt) '(choice radio)) (not (eq (car-safe step) 'alt)))
        (setq nt (custom-type--norm
                  (or (custom-type--resolve-choice (cdr nt) value path)
		      (error "Value %S matches no branch of %S" value type)))))
      (pcase-let ((`(,subtype ,subval ,put)
                   (pcase (cons nt step)
                     (`((cons ,k ,_) . 0)
                      (list k (car value) (lambda (x) (cons x (cdr value)))))
                     (`((cons ,_ ,v) . cdr)
                      (list v (cdr value) (lambda (x) (cons (car value) x))))
                     (`((repeat ,e) . ,(and (pred natnump) n))
                      (list e (nth n value) (funcall put-nth n)))
                     (`((,(or 'list 'group) . ,es) . ,(and (pred natnump) n))
                      (list (nth n es) (nth n value) (funcall put-nth n)))
                     (`((,(or 'list 'group) ,_ . ,es) . cdr)
                      (list (cons 'list es) (cdr value) (lambda (x) (cons (car value) x))))
                     (`((,(or 'choice 'radio) . ,alts) . (alt ,n))
                      (list (nth n alts) value #'identity))
                     (_ (error "Cannot take step %S into type %S" (car path) type)))))
        (pcase-let ((`(,subtype2 ,subval2 ,put2) (custom-type-focus subtype subval (cdr path))))
          (list subtype2 subval2 (lambda (x) (funcall put (funcall put2 x)))))))))

;; TODO: function for converting customization type with X in it to path to X (for use with `custom-type-focus')
;;       function for searching customization types by type/subtype/path/subpath
;;       function for searching elisp functions (and their locations) by code path/snippet? (put in different library)

(provide 'jb-custom-extras)

;; (org-readme-sync)
;; (magit-push)

;;; jb-custom-extras.el ends here


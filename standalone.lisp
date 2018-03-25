;;; -*- Mode:lisp; coding:utf-8; -*-


;;; file standalone.lisp taken as-is from
;;; http://www.csc.ncsu.edu/faculty/stamant/simple-planners.zip
;;;


;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig


;;; To download the code for the simple planners, visit
;;; <http://www.csc.ncsu.edu/faculty/stamant/simple-planners/simple-planners.html>.

;;; About standalone.lisp:

;;; This file is mostly code written by Peter Norvig, taken from the
;;; files named in the comments for each section.

;;; Some small modifications for the JSCL environment
;;; july, 2017, March, 2018. MVK

;;; ==============================
;;; From file auxfns.lisp:

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun pat-match (pattern input &optional (bindings no-bindings))
    "Match pattern against input in the context of the bindings"
    (cond ((eq bindings fail) fail)
          ((variable-p pattern) (match-variable pattern input bindings))
          ((eql pattern input) bindings)
          ((and (consp pattern) (consp input))
           (pat-match (rest pattern) (rest input)
                      (pat-match (first pattern) (first input) bindings)))
          (t fail)))

(defun match-variable (var input bindings)
    "Does VAR match input?  Uses (or updates) and returns bindings."
    (let ((binding (get-binding var bindings)))
        (cond ((not binding) (extend-bindings var input bindings))
              ((equal input (binding-val binding)) bindings)
              (t fail))))

(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
    "Get the variable part of a single binding."
    (car binding))

(defun binding-val (binding)
    "Get the value part of a single binding."
    (cdr binding))

(defun get-binding (var bindings)
    "Find a (variable . value) pair in a binding list."
    (assoc var bindings))

(defun lookup (var bindings)
    "Get the value part (for var) from a binding list."
    (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
    "Add a (var . value) pair to a binding list."
    (cons (cons var val)
          ;; Once we add a "real" binding,
          ;; we can get rid of the dummy no-bindings
          (if (eq bindings no-bindings)
              nil
              bindings)))

;;; ==============================

(defun reuse-cons (x y x-y)
    "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
    (if (and (eql x (car x-y)) (eql y (cdr x-y)))
        x-y
        (cons x y)))

;;; mvk
;;; (setf (symbol-function 'find-all-if) #'remove-if-not)
(fset 'find-all-if #'remove-if-not)


(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
    "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
    (if test-not
        (apply #'remove item sequence
               :test-not (complement test-not) keyword-args)
        (apply #'remove item sequence
               :test (complement test) keyword-args)))

(defun mappend (fn list)
    "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
    (apply #'append (mapcar fn list)))

;;; ==============================
;;;; From file unify.lisp:

(defparameter *occurs-check* t "Should we do the occurs check?")

(defun unify (x y &optional (bindings no-bindings))
    "See if x and y match with given bindings."
    (cond ((eq bindings fail) fail)
          ((eql x y) bindings)
          ((variable-p x) (unify-variable x y bindings))
          ((variable-p y) (unify-variable y x bindings))
          ((and (consp x) (consp y))
           (unify (rest x) (rest y)
                  (unify (first x) (first y) bindings)))
          (t fail)))

(defun unify-variable (var x bindings)
    "Unify var with x, using (and maybe extending) bindings."
    (cond ((get-binding var bindings)
           (unify (lookup var bindings) x bindings))
          ((and (variable-p x) (get-binding x bindings))
           (unify var (lookup x bindings) bindings))
          ((and *occurs-check* (occurs-check var x bindings))
           fail)
          (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
    "Does var occur anywhere inside x?"
    (cond ((eq var x) t)
          ((and (variable-p x) (get-binding x bindings))
           (occurs-check var (lookup x bindings) bindings))
          ((consp x) (or (occurs-check var (first x) bindings)
                         (occurs-check var (rest x) bindings)))
          (t nil)))

;;; ==============================

(defun subst-bindings (bindings x)
    "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
    (cond ((eq bindings fail) fail)
          ((eq bindings no-bindings) x)
          ((and (variable-p x) (get-binding x bindings))
           (subst-bindings bindings (lookup x bindings)))
          ((atom x) x)
          (t (reuse-cons (subst-bindings bindings (car x))
                         (subst-bindings bindings (cdr x))
                         x))))

;;; ==============================

(defun unifier (x y)
    "Return something that unifies with both x and y (or fail)."
    (subst-bindings (unify x y) x))

;;; ==============================
;;; From file search.lisp

(defun tree-search (states goal-p successors combiner)
    "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
    ;; (dbg :search "~&;; Search: ~a" states)
    (cond ((null states) fail)
          ((funcall goal-p (first states)) (first states))
          (t (tree-search
              (funcall combiner
                       (funcall successors (first states))
                       (rest states))
              goal-p successors combiner))))


;;;
;;; mvk
#|
(defun sorter (cost-fn)
    "Return a combiner function that sorts according to cost-fn."
    #'(lambda (new old)
          (sort (append new old) #'< :key cost-fn)))
|#




;;; ==============================
;;; Fixups

(defconstant +no-bindings+ no-bindings)

(defconstant +variable-prefix-char+ #\?)

#|
(defun variable-p (x)
    "Is x a variable (a symbol beginning with `?')?"
    (and (symbolp x) (equal (elt (symbol-name x) 0) +variable-prefix-char+)))
|#


(defun variable-p (x)
    "Is x a variable (a symbol beginning with `?')?"
    (and (symbolp x) (eql (char (symbol-name x) 0) +variable-prefix-char+)))



(defun simple-search (start goal-p successors cost-fn)
    "Search lowest cost states first until goal is reached."
    (tree-search (list start) goal-p successors (sorter cost-fn)))

;;; ==============================

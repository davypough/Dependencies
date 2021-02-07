;;; Filename:  dependencies.lisp

;;; Finds the dependencies among files (ie, inter-file references) in a project directory.
;;; Assumes the project files have already been loaded, and
;;; that *default-pathname-defaults* points to the project directory.


(in-package :cl-user)


#-:alexandria (progn (ql:quickload :alexandria) (push :alexandria *features*))
#-:cl-ppcre (ql:quickload :cl-ppcre)


(use-package :alexandria)
(use-package :cl-ppcre)


(defun purify-file (file)
  "Transform problematic symbols to benign nil in file, before reading.
   Returns a string of altered file content."
  (let ((file-string (alexandria:read-file-into-string file)))
    (ppcre:regex-replace-all
      "[ \t\r\n]'[A-Za-z0-9!@$%&*_+:=<.>/?-]+"
      file-string " nil")
    (ppcre:regex-replace-all
      "[(][qQ][uU][oO][tT][eE][ \t\r\n]+[A-Za-z0-9!@$%&*_+:=<.>/?-]+[)]"
      file-string "nil")))
    

(defun collect-symbols (tree)
  "Collects all of the symbols in a form."
  (let ((all-items (alexandria:flatten tree)))
    (delete-if (lambda (item)
                 (or (not (symbolp item))
                     (find-symbol (symbol-name item) :cl)))
               (delete-duplicates all-items))))


(defun collect-defs (forms)
  "Collects all of the defined names in forms, excluding defstructs."
  (loop for form in forms
        when (member (first form)
               '(defun defmacro defparameter defvar defmethod))
        collect (second form)))


(defun collect-defstructs (forms)
  "Collects all of the defined defstruct names in forms."
  (loop for form in forms
        when (member (first form) 
               '(defstruct))
         if (symbolp (second form))
           collect (second form)
           else if (listp (second form))
                  collect (first (second form))))


(defun pseudo-load (file)
  "Attempt to read a file doing what LOAD would do. May not always do
   the right thing. Returns list of all forms, including package prefixes.
   Based on a function provided by tfb on Stack Overflow."
  (let ((file-string (purify-file file))
        (*package* *package*))
    (with-input-from-string (in-stream file-string)
      (loop for form = (read in-stream nil in-stream)
            while (not (eql form in-stream))
            when (and (consp form)
                      (eq (first form) 'in-package))
             do (setf *package* (find-package (second form)))
            collect form))))


(defun file1-depends-on-file2 (file1 file2 &key verbose)
  "Determines if file1 depends on file2."
  (let* ((1forms (pseudo-load file1))
         (all1-syms (collect-symbols 1forms))
         (defstruct1-syms (collect-defstructs 1forms))
         (1syms (set-difference all1-syms defstruct1-syms :test #'equal))
         (forms2 (pseudo-load file2))
         (def2-syms (collect-defs forms2))
         (defstruct2-syms (collect-defstructs forms2))
         (11syms (loop for defstruct2-sym in defstruct2-syms
                   append (loop for 1sym in 1syms
                            when (and (not (eql defstruct2-sym 1sym))
                                      (search (format nil "~S" defstruct2-sym)
                                              (format nil "~S" 1sym)))
                             collect 1sym))))
    (when verbose
      (format t "~%~A symbols:~%~S~%" file1 1syms)
      (format t "~%~A symbols:~%~S~%" file2 def2-syms)
      (format t "~%~A structures:~%~S~%" file2 defstruct2-syms)
      (format t "~%~A symbols dependent on ~A:~%" file1 file2))
    (append (intersection 1syms def2-syms)
            11syms)))


(defun file-depends-on-what (file1 &key (pathspec "*.lisp") verbose)
  "Prints out all dependencies of a file in directory = *default-pathname-defaults*."
  (let ((files (mapcar #'file-namestring (directory pathspec))))
    (loop for file2 in files
          unless (equal file1 file2)
           do (let ((deps (file1-depends-on-file2 file1 file2)))
                (when deps
                  (if verbose
                    (format t "~%~A depends on ~A~%~S~%"
                            (file-namestring file1) (file-namestring file2) deps)
                    (format t "~%~A depends on ~A~%"
                            (file-namestring file1) (file-namestring file2))))))))


(defun all-path-cycles (node current-path adjacency-table)
  "Recursively follow all paths in a dependency network."
  (if (member node current-path :test #'equal)
    (list (subseq current-path 0 (1+ (position node current-path :test #'equal))))
    (loop for child in (gethash node adjacency-table)
          append (all-path-cycles child (cons node current-path) adjacency-table))))


(defun codependents (node-list dependencies)
  "Returns all dependent cycles for all nodes."
  (let ((adjacency-table (make-hash-table :test #'equal)))
    (loop for dep in dependencies
          do (push (second dep) (gethash (first dep) adjacency-table)))
    (loop for node in node-list
          append (all-path-cycles node nil adjacency-table))))


(defun display-codependencies (dependencies)
  "Prints out all codependencies among a group of files."
  (format t "~%Codependent files (with circular reference):~%")
  (let* ((node-list (remove-duplicates (alexandria:flatten dependencies)
                                       :test #'equal))
         (codependents (remove-duplicates (codependents node-list dependencies)
                                          :test (lambda (set1 set2)
                                                  (alexandria:set-equal set1 set2
                                                                        :test #'equal)))))
    (loop for co-set in codependents
          do (format t "~S~%" co-set))))
    
  
(defun display-all-dependencies (&key (pathspec "*.lisp") verbose)
  "Prints out all dependencies of every pathspec file in 
   directory = *default-pathname-defaults*."
  (let ((files (mapcar #'file-namestring (directory pathspec))))
    (loop with dependencies
          for file1 in files
          do (loop for file2 in files
                   unless (equal file1 file2)
                     do (let ((deps (file1-depends-on-file2 file1 file2)))
                          (when deps
                            (push (list file1 file2) dependencies)
                            (if verbose
                              (format t "~%~A depends on ~A~%~S~%"
                                      (file-namestring file1) (file-namestring file2) deps)
                              (format t "~%~A depends on ~A~%"
                                      (file-namestring file1) (file-namestring file2))))))
          finally (display-codependencies dependencies))))

;;; Filename:  dependencies.lisp

; Finds the dependencies among files (ie, inter-file references) in a Common
; Lisp project directory. Assumes the project files have already been loaded,
; and that *default-pathname-defaults* points to the project directory.


(defpackage :dependencies
  (:use :cl :alexandria :cl-ppcre)
  (:nicknames :dep)
  (:export #:file1-depends-on-file2 #:file-depends-on-what
           #:get-all-dependencies))


(in-package :dep)


; Function Specs
(declaim
  (ftype (function (string) (values t t))  ;ideal: (values string boolean)
         purify-file)
  (ftype (function (list) list)
         collect-symbols)
  (ftype (function (t) list)
         collect-defs)
  (ftype (function (t) list)
         collect-defstructs)
  (ftype (function (list) list)
         delete-duplicate-cycles)
  (ftype (function (string) list)
         pseudo-load)
  (ftype (function (string string &key (:stream stream) (:print t))
                   (or null dependency))
         file1-depends-on-file2)
  (ftype (function (string &key (:pathspec pathname) (:stream stream)
                                (:print t))
                   list)
         file-depends-on-what)
  (ftype (function (list hash-table hash-table) list)
         backtrack-search)
  (ftype (function (list list) list)
         search-for-codependents)
  (ftype (function (list) list)
         get-codependencies)
  (ftype (function (&key (:pathspec pathname) (:stream stream) (:print t))
                   (values list list))
         get-all-dependencies))


(defmacro prt (&rest vars)
  "For debugging: Print the names & values of given variables or accessors.
   Can also wrap around an expression, returning its value."
  `(progn ,@(loop for var in vars
              collect `(format t "~&  ~S => ~S~%" ',var ,var))
          ,@(last `,vars)))


(defstruct (dependency (:conc-name dependency.))
  (file1 "" :type string)  ;the dependent file
  (file2 "" :type string)  ;the independent file containing definitions
  (symbols nil :type list))  ;the symbols in file1 that depend on file2


(defun purify-file (file)
  "Transforms problematic symbols to benign NIL in file, before reading.
   Returns a string of altered file content."
  (let ((file-string (alexandria:read-file-into-string file))
        (modified-file-string ""))
    (setf modified-file-string 
      (ppcre:regex-replace-all
        "[ \t\r\n]'[A-Za-z0-9!@$%&*_+:=<.>/?-]+"
        file-string " NIL"))
    (ppcre:regex-replace-all
      "[(][qQ][uU][oO][tT][eE][ \t\r\n]+[A-Za-z0-9!@$%&*_+:=<.>/?-]+[)]"
      modified-file-string "NIL")))


(defun collect-symbols (form)
  "Collects all of the unique symbols in a form."
  (let ((all-items (alexandria:flatten form)))
    (delete-if (lambda (item)
                 (or (not (symbolp item))
                     (find-symbol (symbol-name item) :cl)))
               (delete-duplicates all-items))))


(defun collect-defs (forms)
  "Collects all of the defined names in forms, excluding defstructs."
  (loop for form in forms
    when (and (consp form)
              (member (first form)
                      '(defun defmacro defparameter defvar defmethod)))
      collect (second form)))


(defun collect-defstructs (forms)
  "Collects all of the defined defstruct names in forms."
  (loop for form in forms
    when (and (consp form)
              (member (first form) 
                      '(defstruct)))
      if (symbolp (second form))
        collect (second form)
        else if (listp (second form))
               collect (first (second form))))


(defun collect-struct-fns (defstruct-syms all-syms)
  "Collects all of the structure access function names in all-syms
   that are associated with the structure names in defstruct-syms."
  (delete-if #'null
    (alexandria:map-product
      (lambda (defstruct-sym sym)
        (when (and (not (eql defstruct-sym sym))
                   ;only std :conc-name structure prefix
                   ;with hyphen recognized
                   (or (search (format NIL "~S-" defstruct-sym)
                               (princ-to-string sym))
                       (equal (format NIL "make-~S" defstruct-sym)
                              (princ-to-string sym))))
          sym))
      defstruct-syms all-syms)))


(defun delete-duplicate-cycles (cycles)
  "Deletes any duplicate dependency cycles found during search."
  (delete-duplicates cycles
    :test (lambda (cyc1 cyc2)
            (alexandria:set-equal cyc1 cyc2 :test #'equal))))


(defun pseudo-load (file)
  "Attempt to read a file doing what LOAD would do. May not always do
   the right thing. Returns list of all forms, including package prefixes.
   Based on a function provided by tfb on Stack Overflow."
  (let ((file-string (purify-file file))
        (*package* *package*))
    (with-input-from-string (in-stream file-string)
      (loop for form = (read in-stream NIL in-stream)
        while (not (eql form in-stream))
        when (and (consp form)
                  (eql (first form) 'in-package))
          do (let ((file-pkg (find-package (second form))))
               (if file-pkg
                 (setf *package* file-pkg)
                 (error "~%Unknown package name: ~A in file: ~A
                         ~&Make sure project files are loaded.~%"
                        (second form) file)))
          collect form))))


(defun file1-depends-on-file2 (file1 file2 &key (stream *standard-output*)
                                                (print nil))
  "Returns or prints those symbols in file1 that are defined in file2."
  (let* ((forms1 (pseudo-load file1))
         (all-syms1 (collect-symbols forms1))
         (def-syms1 (collect-defs forms1))  ;eg, (defun sym ...
         (defstruct-syms1 (collect-defstructs forms1))  ;eg, (defstruct sym ...
         (struct-fns1 (collect-struct-fns defstruct-syms1  ;eg, (sym-slot ...
                                          all-syms1))
         (active-syms1 (set-difference 
                         (set-difference
                           (set-difference all-syms1 def-syms1)
                           defstruct-syms1)
                         struct-fns1))
         (forms2 (pseudo-load file2))
         (def-syms2 (collect-defs forms2))
         (defstruct-syms2 (collect-defstructs forms2))
         (all-defs2 (append def-syms2 defstruct-syms2))
         ;collect dependent structure accessor fn names in active-sims1
         ;for structures defined in forms2
         (dep-struct-fns1 (collect-struct-fns defstruct-syms2 active-syms1)))
    (let ((dependent-symbols 
            (append (set-difference (intersection active-syms1 all-defs2)
                                    defstruct-syms2)
                    dep-struct-fns1)))
      (if print
        (format stream "~%~S symbols dependent on ~S definitions:~%~S~2%"
                       file1 file2 dependent-symbols)
        (when dependent-symbols
          (make-dependency :file1 file1 :file2 file2
                           :symbols dependent-symbols))))))


(defun file-depends-on-what (file1 &key (pathspec #P"*.lisp")
                                        (stream *standard-output*) (print nil))
  "Returns a list of, or prints, all dependencies of a file
   in directory = *default-pathname-defaults*."
  (loop with files = (mapcar #'file-namestring (directory pathspec))
    for file2 in files
    unless (equal file1 file2)
      collect (file1-depends-on-file2 file1 file2 :stream stream)
              into dependencies
    finally (let ((actual-dependencies (delete-if #'null dependencies)))
              (if print
                (dolist (dep actual-dependencies)
                  (format stream 
                          "~%~S symbols dependent on ~S definitions:~%~S~2%"
                          (dependency.file1 dep) (dependency.file2 dep)
                          (dependency.symbols dep)))
                (return actual-dependencies)))))


(defun backtrack-search (current-path adjacency-table visited)
  "Recursively follow dependency paths from all file nodes, detecting cycles."
  (let* ((node (first current-path))
         (cycle (member node (reverse (cdr current-path)) :test #'equal)))
    (if cycle
      (progn (setf (gethash node visited) T)
             (list cycle))
      (let ((children (gethash node adjacency-table)))
        (if children
          (loop for child in children
            unless (gethash child visited)
            nconc (backtrack-search (cons child current-path)
                                    adjacency-table visited)
            finally (setf (gethash node visited) T))
          (not (setf (gethash node visited) T)))))))


(defun search-for-codependents (node-list dependencies)
  "Detects & returns all dependent cycles for all nodes
   in a dependency network."
  (let* ((node-count (length node-list))
         (adjacency-table (make-hash-table :test #'equal :size node-count))
         (visited (make-hash-table :test #'equal :size node-count)))
    (dolist (dep dependencies)
      (push (dependency.file2 dep)
            (gethash (dependency.file1 dep) adjacency-table)))
    (loop for node in node-list
      do (clrhash visited)
      nconc (backtrack-search (list node) adjacency-table visited)
            into all-cycles
      finally (return (delete-duplicate-cycles all-cycles)))))


(defun get-codependencies (dependencies)
  "Returns all codependencies among a group of inter-dependent files."
  (let ((file-list (delete-duplicates 
                     (loop for dep in dependencies
                       collect (dependency.file1 dep)
                       collect (dependency.file2 dep))
                     :test #'equal)))
    (search-for-codependents file-list dependencies)))


(defun get-all-dependencies (&key (pathspec #P"*.lisp")
                                  (stream *standard-output*) (print nil))
  "Returns or prints dependencies and codependencies of every pathspec file in 
   directory = *default-pathname-defaults*."
  (let* ((files (mapcar #'file-namestring (directory pathspec)))
         (dependencies (delete-if #'null
                         (alexandria:map-product
                           (lambda (file1 file2)
                             (unless (equal file1 file2)
                               (file1-depends-on-file2 file1 file2)))
                           files files)))
         (codependencies (get-codependencies dependencies)))
    (cond (print
            (dolist (dep dependencies)
              (format stream "~%~S symbols dependent on ~S definitions:~%~S~%"
                             (dependency.file1 dep) (dependency.file2 dep)
                             (dependency.symbols dep)))
            (format stream "~2%Codependent files (with circular references):~%")
            (dolist (codep codependencies)
             (format stream "~S~%" codep)))
          (t (values dependencies codependencies)))))

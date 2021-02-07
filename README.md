# Dependencies
Detects the inter-file dependencies in a project directory

The following short program is a collection of routines for detecting the dependencies among a project’s component files.  Dependencies serve as a rough measure of the complexity of a program.  Minimizing inter-file dependencies is a worthy goal, since understanding and refactoring a complex program is often challenging.  However, Common Lisp can support a wide latitude of inter-file dependencies within a compilation unit, so it is generally not advisable to sacrifice program modularity in order to remove all dependencies.
To find the dependencies in a project’s files, first load the project files.  The dependency analysis is applied to specified files in the project’s directory.  The pathname for this directory is assumed to be the value of *default-pathname-defaults*, which must be assigned before detecting dependencies.  Which files are included is determined by supplying a standard Common Lisp filespec, which defaults to “*.lisp”.
A simple example of a dependency is the situation where a function is defined in one file, but used in a different file.  In this case the using file depends on the defining file.  But there are a number of other kinds of definitional dependencies checked besides defun, including defmacro, defparameter, defvar, defmethod, and defstruct.
More complex examples of dependencies include codependencies, where several files depend on each other.  If two files each contain definitions used by the other, then they are codependent.  Multiple files can be circularly dependent on each other, also making them codependent.
The amount of information printed to the terminal can be controlled by the key parameter verbose.  The verbose option indicates why the files are dependent.  For example, entering (display-all-dependencies) at the REPL simply prints out all detected file dependencies.  But entering (display-all-dependencies :verbose t) will additionally show which symbols in the dependent file have definitions in another file.  
Interface
function:  `display-all-dependencies (&key (pathspec "*.lisp") verbose)`
	Prints out all the dependencies and codependencies among all the files matching the directory pathspec in *default-pathname-defaults*
function:  `file-depends-on-what (file1 &key (pathspec "*.lisp") verbose)`
	Prints out all the files which a given file depends on.
function:  `file1-depends-on-file2 (file1 file2 &key verbose)`
	Determines if file1 depends on file2.

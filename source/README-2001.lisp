
    INSTRUCTIONS FOR BRINGING UP THE *LISP SIMULATOR


    ***** It's a good idea to read over all the instructions
    ***** before you do anything.


1.  Requirements:  You must have an ANSI Common Lisp running under  
    Windows to use these instructions.   If you have an ANSI Common
    Lisp running under Unix, Linux, a Macintosh or some other OS
    you will have to improvise slightly, but the basic idea of what
    to do should be pretty similar.

    This version of the *Lisp simulator has run under Corman Lisp,
    Allegro version 5.1, and Lispworks 4.1 on a PC under Windows 98.
    This port, from the CLtL I version of the *Lisp simulator, to
    ANSI Common Lisp, was done in September 2001 by
    JP Massar, massar@alum.mit.edu.

    The other README files are circa 1992.
     
    If you have a different version of Common Lisp then some
    conditionalization of the code may well have to be done.

2.  First, decide on a source directory wherein you will store the
    files you are about to extract, and create the directory.

    suggestion: C:\Lispcode\Starsim\f20\

    This path will be assumed in the text below; substitute 
    appropriately if you use another path.

    You should create a top level directory called
    'starsim' and then a subdirectory called 'f20', as above.
    If a future version of the simulator became available you
    could then easily install it without affecting this version.    

    (If you've already unzipped the files into some
    random directory, you can of course create the above directories
    and copy all the files into F20)

3.  Download and unzip, or unzip, or just copy, as appropriate,
    the files that compose the archive into this newly created directory.

4.  The *Lisp simulator comes with a primitive DEFSYSTEM (i.e.,
    a 'make' facility for Lisp programs).  It assumes that a library 
    directory exists with all the DEFSYSTEM files.  (These files end in
    .sys) Create this directory (suggestion: C:\Lispcode\Makefiles\).

    (Remember, to get a '\' into a Lisp string one needs to type it twice.
     So "\\" is a one-character string, composed of a single '\'.
     Most Lisp systems allow '/' in place of '\' these days, so you
     can see if that works for you if you wish)

    One of the files you extracted is called make.lisp.  Edit this
    file and change the second element of the
    *defsystem-search-path* initial value list to reflect your
    choice of library directory.  Make sure you leave that last '\'
    or '/' in there!  You may also have to add to or
    change the variable *all-possible-lisp-extensions* if your Lisp uses
    some extension for lisp files not included on that list.   
    You may also have to change or add to the
    set of #+ forms wherein *binary-lisp-extension* is defined.
    You should start up a Common Lisp and see what symbols are
    included in the list *features* and choose appropriate ones
    to uniquely identify your version of Common Lisp.

    Another of the files you extracted ends in the extension .sys.
    (It should be called f20.sys; I will henceforth
    assume its name is f20.sys).  Edit the file, and change
    the pathname strings after the strings "STARLISP" and "VERSION"
    to reflect your choice of source directory.  For instance, it
    might look like below after you've edited it:

    (define-alias "STARLISP" "C:\\Lispcode\\Starsim\\")
    (define-alias "VERSION" "f20\\")
 
    Save the edited file out.

    Finally, copy f20.sys into the library directory you just
    created.

5.  If your Common Lisp is not a version of Common Lisp under which
    this code is known to run (see above), then you may have to
    edit the code and make changes.  Use an editor and search
    for all occurrances of "#+" and "#-".  This will indicate
    where conditionals might have to be inserted for your version of
    Common Lisp.  You can ignore #+*LISP-HARDWARE and #+*LISP-SIMULATOR
    conditionals.  Scan all of the file port.lisp, and in the
    file simulator-specification.lisp, the function
    proper-symbol-for-*features* needs to be correct for the version
    of Common Lisp you are using, or things will mysteriously break.
        
    In the file patches.lisp is a function called load-patches which
    loads the patches.lisp file.  You should edit this function to
    use the correct pathname.
    In the future, if patches are sent
    for the *Lisp Simulator,  you can put them in this file.  Then you
    can call the load-patches function and save out a new *Lisp Simulator
    image (see below) if you desire, or just load the patch file each
    time you run the *Lisp simulator.

6.     

    A.  If you are using Corman Lisp go to B.

    Load up your Common Lisp.  Compile and load in the make.lisp file.

    (compile-file "C:\\Starsim\\F20\\make.lisp")
    (load "C:\\Starsim\\F20\\make")

    If you are using Allegro, and the current package is CG-USER,
    then switch to the USER package:

    (in-package :user)

    Then evaluate the form

    (ms :f20) 

    (using whatever the name of the .sys file you got was as the keyword name)
    This should compile and load the entire *Lisp simulator.
    You may get some warnings about functions not defined or
    redefined.  You can ignore them.

    Go to C.

    B.  If you are using Corman Lisp:  

    Start Corman Lisp.  Corman Lisp has the property that LOAD
    both compiles and loads a file, and, in fact, COMPILE-FILE
    does not quite work correctly.  Therefore you must avoid
    having the *Lisp loader DEFSYSTEM use COMPILE-FILE.

    (load "C:\\Starsim\\F20\\make.lisp")

    Then evaluate the form

    (mss-all :f20)

    This should load all the *Lisp simulator files.
    You may get some warnings about functions not defined or
    redefined.  You can ignore them.


    C.

    If you run into a problem in some file, you will have to
    edit the file and correct the problem, if possible.  Once you
    have corrected the problem you can start up new Lisp, load
    in make.lisp again, and rerun the 'ms' command.  Files which have
    already been compiled will not be recompiled.  If you wish
    to do a full compile use

    (msc :f20)

    If you are using Corman Lisp, always use 

    (mss-all :f20)

    If you are successful, it will print the message

    Thinking Machines *Lisp Simulator.  Version 20.0

    This means that the simulator has successfully *cold-booted.

7.  Evaluate the form

    (in-package :*LISP)

    By default, the *Lisp simulator comes up cold-booted with
    a configuration of 32 processors arranged in an 8 by 4 grid.


8.  If you want to run a test, one of the sample files has been
    compiled and loaded for your convienence.
    
    Type:

    (do-text-processing "This is some text to process")

    You should get output similar to what is shown below.

Processor 0. Length: 4.  Word: This                            
Processor 1. Length: 2.  Word: is                              
Processor 2. Length: 4.  Word: some                            
Processor 3. Length: 4.  Word: text                            
Processor 4. Length: 2.  Word: to                              
Processor 5. Length: 7.  Word: process                         
NIL


9.  If you have succeeded so far, congratulations!  If possible,
    you may wish to 'create an executable' or 'dump a core
    image' or whatever your version of Common Lisp calls the
    ability to create an executable image with pre-loaded lisp code.

    To do this, kill off the simulator and reload it again, without
    loading in the example program and without doing any compilations,
    by following the above directions. Then create the disk  
    image after it has finished loading.

    If no such feature exists, you will have to load the simulator
    each time by loading in the make.lisp file and executing

    (ms "f20")

    as above.


9a. SAVING AN IMAGE USING CORMAN LISP

    If you are using Corman Lisp, there is no one command that will
    produce a saved image that you can just start up but the
    following sequence (for Corman Lisp version 1.5) will get you there:

    1.  In the directory where Corman Lisp is installed, edit the
    file init.lisp and put an (in-package :ccl) as the very first
    line of the file.

    2.  In the same directory, make a copy of the file CormanLisp.exe
    and call it Starsim-f20.exe.

    3.  Start up Corman Lisp and load in the simulator, as above.
    After it is loaded do an (in-package :*lisp).

    4.  Do (cll::save-image "<path>\\Starsim-f20.img")

    where <path> represents the path of the directory where Corman
    Lisp is installed.
     
    5.  Now if you double-click on the file <path>\Starsim-f20.exe

    a version of the Corman Lisp IDE with the *Lisp Simulator preloaded and
    in the *Lisp package should come up.


11. Problems with the *Lisp Simulator can be reported to

    massar@alum.mit.edu

but there is no promise of help or support.

12.  The 'Getting Started in *Lisp' Guide is available in the file
getting-started-guide.text that you should have extracted.  This
is an ASCII-ized version of an InterLeaf document.  It should
be required reading for anyone not already familiar with *Lisp.

There is another tutorial on *Lisp, written by some people at an
Australian University that acquired a Connection Machine.  It's
called 'tutorial' and has a .pdf version and an .ez version.

There are some other example *Lisp programs included, usually with
the word 'example' as part of the filename.
 




    INSTRUCTIONS FOR BRINGING UP THE *LISP SIMULATOR



    ***** It's a good idea to read over all the instructions
    ***** before you do anything!!



1.  Requirements:  You must have a Common Lisp running under some 
    version of Unix to use these instructions.  

    The CM Software 5.x and 6.x series of *Lisp simulator versions
    have been known to run under Lucid Lisp (3.0 and 4.0) on Sun4's, Sun3's
    and Vaxen, under Coral Common Lisp on a Mac II, under Kyoto and Ibuki
    Common Lisps, and under Symbolics Common Lisp.  It has probably
    been ported to other Common Lisps as well.

    It is possible that
    the version you are receiving has not been tested against every
    Common Lisp in the above list, and that some minor thing might go
    wrong when compiling the system using this version.  Don't panic!
    The problem is probably trivial.  If you can't figure it out, give
    Thinking Machines a call (617-876-1111), and we will try to help you.  

    If you have a different version of Common Lisp then some
    conditionalization of the code may will have to be done.

2.  First, decide on a source directory wherein you will store the
    files you are about to extract, and create the directory.

    suggestion: /usr/src/local/starlisp-simulator/f19/

    You should definitely create a top level directory called
    'starlisp-simulator' and then a subdirectory called 'f19'.
    As future versions of the simulator become available you
    can then easily install them without affecting this version.    

    Now change to this directory.  The instructions assume you will
    stay in this directory for the duration.

    (If you've already untarred or unsharred the files into some
     random directory, you can of course create the above directory
     and copy all the files into it)


3.  If you have a tar tape, extract the files using

    tar xv

    If you have a shar archive (called, e.g., foo), extract 
    the files following the directions at the beginning of the
    shar file, or try

    unshar foo

    If the unshar program is available to you.

    If you are using ftp, just copy all the files in the public
    directory whose name you were provided with over to your
    machine into the directory you just created.

4.  The *Lisp simulator comes with a primitive DEFSYSTEM (i.e.,
    a 'make' facility for Lisp programs).  It assumes that a library 
    directory exists with all the DEFSYSTEM files.  Create this
    directory (suggestion: /usr/local/lib/commonlisp/defsystem).

    One of the files you extracted is called make.lisp.  Edit this
    file and change the second element of the
    *defsystem-search-path* initial value list to reflect your
    choice of library directory.  Make sure you leave that last '/'
    in there!  You may also have to add to or
    change the variable *all-possible-lisp-extensions* if you use
    some extension for lisp files not included on that list.  (If you
    are still using old Sun's or old Sun software you may have to
    change .2bin to .lbin).  You may also have to change or add to
    to set of #+ forms wherein *binary-lisp-extension* is defined.
    You should start up a Common Lisp and see what symbols are
    included in the list *features* and choose appropriate ones
    to uniquely identify your version of Common Lisp.

    Another of the files you extracted ends in the extension .sys.
    (It is probably called release.sys or f19.sys, I will henceforth
    assume its name is f19.sys).  Edit the file, and change
    the Unix pathnames after the strings "STARLISP" and "VERSION"
    to reflect your choice of source directory.  For instance, it
    might look like

    (define-alias "STARLISP" "/usr/src/local/starlisp-simulator/")
    (define-alias "VERSION" "f19/")

    Save the edited file out.

    Finally, copy f19.sys into the library directory you just
    created.

5.  If your Common Lisp is not a version of Common Lisp under which
    this code is known to run (see above), then you may have to
    edit the code and make changes.  Use grep and search
    for all occurrances of "#+" and "#-".  This will indicate
    where conditionals might have to be inserted for your version of
    Common Lisp.  You can ignore #+*LISP-HARDWARE and #+*LISP-SIMULATOR
    conditionals, however.  Also, scan all of the file port.lisp, which has
    most of the implementation-dependent code in it.  Also the
    file simulator-specification.lisp, the function
    proper-symbol-for-*features* needs to be correct for the version
    of Common Lisp you are using, or things will mysteriously break.
        
    One of the biggest problems is providing an error handler to use in
    rewriting the 'with-all-errors-trapped' macro, since CLtL I
    provides no standard way of catching errors.

    In the file patches.lisp is a function called load-patches which
    loads the patches.lisp file.  You should edit this function to
    use the correct pathname after the line that reads #-TMC.
    In the future, if patches are sent
    for the *Lisp Simulator, they will be sent in the form of a single
    patch file, which you can append to patches.lisp.  Then you
    can call the load-patches function and save out a new *Lisp Simulator
    image (see below) if you desire, or just tell people to load
    the patch file.


6.     

    A.

    If you are using Lucid Common Lisp into which has been built
    the TMC Symbolics Compatibility Package then do the following,
    otherwise go to B below:

    Type

    *features*

    and see if the symbol :TMC or TMC is on the *features* list.
    If so, remove it, using 

    (setq *features* (delete 'tmc *features*))  

    (or use :tmc if appropriate)

    Compile and load in the make.lisp file.
    
    (compile-file "make.lisp")
    (load "make")

    Compile and load in the f19.sys file by doing

    (compile-file "f19.sys")
    (load "f19.sbin")

    This should compile and load in all the *Lisp Simulator files.
    You may get numerous warnings about functions not defined or
    redefined.  You can ignore them.

    Goto C.

    B. 

    Load up your Common Lisp.  Compile and load in the make.lisp file.

    (compile-file "make.lisp")
    (load "make")

    Then evaluate the form

    (ms "f19") 

    (or whatever the name of the .sys file you got was).
    This should compile and load the entire *Lisp simulator.
    You may get numerous warnings about functions not defined or
    redefined.  You can ignore them.

    C.

    If you run into a problem in some file, you will have to
    edit the file and correct the problem, if possible.  Once you
    have corrected the problem you can start up new Lisp, load
    in make.lisp again, and rerun the command.  Files which have
    already been compiled will not be recompiled.

    If you are successful, it will print the message

    Thinking Machines *Lisp Simulator.  Version 18.0

    This means that the simulator has successfully *cold-booted.

7.  Evaluate the form

    (in-package '*LISP)

    By default, the *Lisp simulator comes up cold-booted with
    a configuration of 32 processors arranged in an 8 by 4 grid.


8.  If you wish, there is a sample program called 
    text-processing-example.lisp.  Compile and load this file.

    (compile-file "text-processing-example.lisp")
    (load "text-processing-example")

    Then type:

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
    you may wish to 'create a suspended system' or 'dump a core
    image' or whatever your version of Common Lisp calls the
    ability to create an executable image with pre-loaded lisp code.

    To do this, kill off the simulator and reload it again, without
    loading in the example program and without doing any compilations,
    by following the above directions. Then create the disk core
    image after it has finished loading.

    For instance, with KCL, the feature is the SAVE function:
    (SAVE "starsim"), while in Lucid 3.0 (LCL:DISKSAVE "starsim") is the
    proper incantation).  You could then install this executable in
    one of your bin directories (perhaps /usr/local).

    Using Lucid 3.0 or Lucid 4.0, we recommend the following specific
    disksave command:

    (lcl:disksave "starsim-f19" :full-gc t :verbose t)

    Using Lucid, the disksaved image will be about 2.5 Megabytes larger
    than the Lisp you began with.  The size of the disksaved image using
    other Common Lisps could vary widely.

    If no such feature exists, you will probably want to put the
    make.lisp file and/or its binary in an accessible place so that when
    others want to run the *Lisp Simulator they can be told simply
    to start Common Lisp, load in this file, and then type

    (ms "f19")

10. There is a small manual page included, starsim.1.  You may want
    to edit it appropriately to make the pathnames conform to your
    installation, and then install it in your man pages.


11. Problems with the *Lisp Simulator should be reported to

 csg@think.com 
 bug-starlisp@think.com


12.  The 'Getting Started in *Lisp' Guide is available in the file
getting-started-guide.text that you should have extracted.  This
is an ASCII-ized version of an InterLeaf document.  It should
be required reading for anyone not already familiar with *Lisp.

A hardcopy version of this document--which includes indexes and     
many helpful illustrations--can be obtained without charge by writing to:          

               Thinking Machines Corporation                               
               245 First Street                                            
               Cambridge, MA 02142 - 1264                                  
               Attention: Katy Smith                                       
                                                                           
       Or by sending e-mail to:                                            
		Internet:	documentation-order@think.com               
		uucp:		harvard!think!documentation-order           

 Other documentation for the *Lisp language can be ordered by contacting
the above addresses, or by contacting

 by email: customer-support@think.com

 by phone: Customer Support at Thinking Machines, 617-876-1111.

At the time this was written the price was $100 for the (huge)
two volume set, the *Lisp Reference Manual, and the *Lisp Dictionary.


13.  Three 'sample-session' tutorial files are included with the distribution.
The first, lisp-session.lisp, is a simple introduction to basic features
of Common Lisp useful when programming in *Lisp.

starlisp-session.lisp and starlisp-session-2.lisp are introductions to
*Lisp itself, illustrating many *Lisp functions and features.


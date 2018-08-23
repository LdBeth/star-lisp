

    INSTRUCTIONS FOR BRINGING UP THE *LISP SIMULATOR ON THE MACINTOSH


1.  Requirements:  You must have Coral Common Lisp.

    The 4.3 *Lisp Simulator has been known to run under Vaxlisp on Ultrix,
    KCL under 4.2 and 4.3 BSD, Sun Common Lisp under Sun 3.0 and 3.2 Unix,
    Franz Extended Common Lisp under 4.3 BSD and Coral Common Lisp on
    a Macintosh.

    The 5.0 *Lisp Simulator has been known to run under Lucid Lisp on
    Sun4, Sun3 and Vax, under Coral Common Lisp on a Mac II
    and under Kyoto Common Lisp, as well as under Symbolics Common Lisp.

    It is possible that
    the version you are receiving has not been tested against every
    Common Lisp in the above list, and that some minor thing might go
    wrong when compiling the system using some version.  Don't panic!
    The problem is probably trivial.  If you can't figure it out, give
    Thinking Machines a call (617-876-1111), and we will try to help you.  

    If you have a different version of Common Lisp then some
    conditionalization of the code may will have to be done.

2.  First, create a folder called Starlisp Simulator and a subfolder 
    for the version you are building, wherein you will store the
    files you are about to extract. 
    suggestion:   Disk Name:Starlisp Simulator:f19:

3.  If you are using ftp, just copy all the files in the public
    directory whose name you were provided with over to your
    machine into the directory you just created.  Otherwise, copy 
    them from a floppy disk.

4.  The *Lisp simulator comes with a primitive DEFSYSTEM (i.e.,
    a 'make' facility for Lisp programs).  It assumes that a library 
    directory exists with all the DEFSYSTEM files.  Create this
    directory (suggestion: Disk Name:Starlisp Simulator:lib).

    One of the files you extracted is called make.lisp.  Edit this
    file and change the second element of the
    *defsystem-search-path* initial value list to reflect your
    choice of library directory.  You may also have to add to or
    change the variable *all-possible-lisp-extensions* if you use
    some extension for lisp files not included on that list.  (If you
    are still using old Sun's or old Sun software you may have to
    change .2bin to .lbin).

    Another of the files you extracted ends in the extension .sys.
    (It is probably called release.sys or f19.sys, I will henceforth
    assume its name is f19.sys).  Edit the file, and change
    the Unix pathnames after the strings "STARLISP" and "VERSION"
    to reflect your choice of source directory.  For instance, it
    might look like

    (define-alias "STARLISP" "Disk Name:Starlisp Simulator:")
    (define-alias "VERSION" "f19:")

    Finally, move f19.sys into the library directory you just
    created.

5.  If your Common Lisp is not a version of Common Lisp under which
    this code is known to run (see above), then you may have to
    edit the code and make changes.  Use grep and search
    for all occurrances of "#+" and "#-".  This will indicate
    where conditionals might have to be inserted for your version of
    Common Lisp.  Also, scan all of the file port.lisp, which has
    most of the implementation-dependent code in it.
        
    One of the biggest problems is providing an error handler to use in
    rewriting the 'with-all-errors-trapped' macro, since Common Lisp
    provides no standard way of catching errors.

    In the file patches.lisp is a function called load-patches which
    loads the patches.lisp file.  You should edit this function to
    use the correct pathname.  In the future, if patches are sent
    for the *Lisp Simulator, they will be sent in the form of a single
    patch file, which you can append to patches.lisp.  Then you
    can call the load-patches function and save out a new *Lisp Simulator
    image (see below) if you desire, or just tell people to load
    the patch file.


6.  Load up your Common Lisp.  Compile and load in the make.lisp file.
    Go to the lisp listener window and evaluate the form

    (ms "f19") 

    (or whatever the name of the .sys file you got was).
    This should compile and load the entire *Lisp simulator.
    One known problem when using Coral Common Lisp is that
    the *Lisp Simulator redefines PROCLAIM and DEFTYPE.  Coral
    breaks when this happens and asks if you wish to continue
    or abort.  You may continue on through.

    If you run into a problem in some file, you will have to
    edit the file and correct the problem, if possible.  Once you
    have corrected the problem you can start up new Lisp, load
    in make.lisp again, and rerun the command.  Files which have
    already been compiled will not be recompiled.

    If you are successful, it will print the message

    Thinking Machines *Lisp Simulator.  Version 19.0

    This means that the simulator has successfully *cold-booted.

7.  Evaluate the form

    (in-package '*LISP)

    By default, the *Lisp simulator comes up cold-booted with
    a configuration of 32 processors arranged in an 8 by 4 grid.


8.  If you wish, there is a sample program called 
    text-processing-example.lisp.  Compile and load this file.
    Then type:

    (do-text-processing "This is some text to process")

    You should get output similar to what is shown below.

TEXT: #\T #\h #\i #\s #\Space #\i #\s #\Space #\s #\o #\m #\e #\Space 
#\t #\e #\x #\t #\Space #\t #\o #\Space #\p #\r #\o #\c #\e 
#\s #\s 
SPACE-P: NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL 
T NIL NIL T NIL NIL NIL NIL NIL NIL NIL 
START-WORD-P: T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL 
NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL 
END-WORD-P: NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL 
T NIL NIL T NIL NIL NIL NIL NIL NIL NIL T 
CHARACTER-POSITION-IN-WORD: 0 1 2 3 -1 0 1 -1 0 1 2 3 -1 0 1 2 3 -1 0 
1 -1 0 1 2 3 4 5 6 
WORD-NUMBER: 0 0 0 0 0 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 5 5 5 5 5 5 5 
(HOW-MANY-WORDS 6) 
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
    For instance, with KCL, you'd use the SAVE function: (SAVE "starsim")
    In Lucid (DISKSAVE "starsim") is the proper incantation.
    In Allegro, the magic formula is (CCL:DUMPLISP "starsim").
    You can then install this executable in
    one of your bin directories (perhaps /usr/local).

    If no such feature exists, you will probably want to put the
    make.lisp file and/or its binary in an accessible place so that when
    others want to run the *Lisp Simulator they can be told simply
    to start Common Lisp, load in this file, and then type

    (ms "f19")

    You can package the commands (load "make.lisp") and evaluate 
    (ms "f19") in a double-clickable file.  Wrap them in the form
    (with-cursor *watch-cursor*...

10. Problems with the *Lisp Simulator should be reported to

 csg@think.com 
 bug-starlisp@think.com

Make sure you include which version of the Simulator you are running
when sending in a bug report.


11.  The 'Getting Started in *Lisp' Guide is available in the file
getting-started-guide.text that you should have extracted.  This
is an ASCII-ized version of an InterLeaf document.  It should
be required reading for anyone not already familiar with *Lisp.

A hardcopy version of this document--which includes indexes and     
many helpful illustrations--can be obtained by writing to:          

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


12.  Three 'sample-session' tutorial files are included with the distribution.
The first, lisp-session.lisp, is a simple introduction to basic features
of Common Lisp useful when programming in *Lisp.

starlisp-session.lisp and starlisp-session-2.lisp are introductions to
*Lisp itself, illustrating many *Lisp functions and features.


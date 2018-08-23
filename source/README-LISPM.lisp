
    INSTRUCTIONS FOR BRINGING UP THE *LISP SIMULATOR


1.  First, decide on a source directory wherein you will store the
    files you are about to extract, and create the directory.

    suggestion: >cm>starlisp>simulator>f19>

2.  Assuming you have a carry tape, create the directory and
    unload the files using the :READ CARRY TAPE command.

    If you have a tar tape, then you will have to unload the
    files using TAR on a Unix box and then copy all the files
    to your Lispm.

3.  In the file patches.lisp in a function called load-patches which
    loads the patches.lisp file.  You should edit this function to
    use the correct pathname.  In the future, if patches are sent
    for the *Lisp Simulator, they will be sent in the form of a single
    patch file, which you can append to patches.lisp.  Then you
    can call the load-patches function and save out a new *Lisp Simulator
    image (see below) if you desire, or just tell people to load
    the patch file.


4.  Look at the file defsys-f19.lisp.  This contains a sample
    Symbolics Defsystem for the Simulator.  Use this to set
    up an appropriate defsystem at your site.

    Once the DEFSYSTEM is set up, type
    :Compile System Starlisp-Simulator-F19

    This will compile and load the *Lisp Simulator.


5.  Evaluate the form

    (in-package '*LISP)

    By default, the *Lisp simulator comes up cold-booted already, with
    a configuration of 32 processors arranged in an 8 by 4 grid.


6.  If you wish, there is a sample program called 
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


7.  If you have succeeded so far, congratulations!  

8.  The 'Getting Started in *Lisp' Guide is available in the file
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


9.  Three 'sample-session' tutorial files are included with the distribution.
The first, lisp-session.lisp, is a simple introduction to basic features
of Common Lisp useful when programming in *Lisp.

starlisp-session.lisp and starlisp-session-2.lisp are introductions to
*Lisp itself, illustrating many *Lisp functions and features.




Here is a sample compilation:

You may safely ignore the :MUSER warnings.  :MUSER is a local
TMC hack for the attribute line.

You may safely proceed through the warnings in pvars.lisp as shown below.

The problem with TRIVIAL-CHAR-FONT!! is a Symbolics compiler problem
which is harmless.

The compiler warnings in address-objects.lisp are an unknown
problem which has yet to be resolved.  No problems have yet
arisen due to them, however.


---> Compile System (a system) starlisp-simulator-f19
 
 
Compiling STARSIM:STARLISP-SIMULATOR;EXTERNAL-SYMBOLS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;EXTERNAL-SYMBOLS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;EXTERNAL-DEFVARS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;EXTERNAL-DEFVARS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;NEW-DESTRUCTURING-BIND.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;NEW-DESTRUCTURING-BIND.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PORTABLE-PROCLAIM.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;PORTABLE-PROCLAIM.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-INTERFACE.LISP.NEWEST
1Retry compiling STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-INTERFACE.LISP.NEWEST
0Loading STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-INTERFACE.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-PARSING.LISP.NEWEST
Loading STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-PARSING.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;GEOMETRIES.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;GEOMETRIES.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PORTABLE-GEOMETRY-OPS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;PORTABLE-GEOMETRY-OPS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;SIMULATOR-SPECIFICATION.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;SIMULATOR-SPECIFICATION.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;UNPORTABLE-PROCLAIM.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;UNPORTABLE-PROCLAIM.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;STARLISP-SETF.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;STARLISP-SETF.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;DEFINITIONS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;DEFINITIONS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PVARS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Warning:  Function (:PROPERTY PVAR DEFTYPE), being redefined by file STARSIM:STARLISP-SIMULATOR;PVARS,
was previously defined by file STARSIM:STARLISP-SIMULATOR;PORTABLE-PROCLAIM. OK? (Y, P, or N) Proceed.
Loading STARSIM:STARLISP-SIMULATOR;PVARS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;SIMULATOR-PROCLAIM.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;SIMULATOR-PROCLAIM.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PORT.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;PORT.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;STARLISP-COMPILER-VARIABLES.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;STARLISP-COMPILER-VARIABLES.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;MACROS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;MACROS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PVAR-ARRAY-POOL.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;PVAR-ARRAY-POOL.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;CONTEXT.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;CONTEXT.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;VP-SETS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;VP-SETS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PVAR-ALLOCATION.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;PVAR-ALLOCATION.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;STARDEFVARS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;STARDEFVARS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;GENFUNCTIONS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;GENFUNCTIONS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PREF.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;PREF.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-OUTPUT.LISP.NEWEST
Loading STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-OUTPUT.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;SIM.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;SIM.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;TRIVFUNCTIONS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
For Function TRIVIAL-CHAR-FONT!!
  The variable PVAR-ARRAY was never used.
Loading STARSIM:STARLISP-SIMULATOR;TRIVFUNCTIONS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;FUNCTIONS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;FUNCTIONS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;INTERNAL-ADDRESSING.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;INTERNAL-ADDRESSING.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;ADDRESSING.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;ADDRESSING.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;GET.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;GET.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;SEND.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
For Function *PSET-INTERNAL-2
  The variable VP-SET was never used.
  The variable ADDRESS-PVAR was never used.
Loading STARSIM:STARLISP-SIMULATOR;SEND.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;ADDRESS-OBJECTS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
For Variable *MAXIMUM-NUMBER-OF-DIMENSIONS*
  *MAX-CUBE-ADDRESS-LENGTH* may not have a constant compile-time value -- '28 will be used.
For Function PVAR-ADDRESS-OBJECT-FROM-RELATIVE-PVARS!!
  While compiling (NIL ABSOLUTE-COORDINATES-ARRAY (!! (THE FIXNUM J))):
    (Invalid form.)
For Function ADDRESS-PLUS-NTH!!
  While compiling (NIL TEMP-GRID-COORDINATES-ARRAY (!! (THE FIXNUM DIMENSION-INDEX))):
    (Invalid form.)
  While compiling (NIL TEMP-GRID-COORDINATES-ARRAY (!! (THE FIXNUM DIMENSION-INDEX))):
    (Invalid form.)
Loading STARSIM:STARLISP-SIMULATOR;ADDRESS-OBJECTS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;ADDRESS-OBJECT-SMASHING.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;ADDRESS-OBJECT-SMASHING.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;ARRAYS.LISP.NEWEST
Loading STARSIM:STARLISP-SIMULATOR;ARRAYS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PPP.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;PPP.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;HYPERGRID.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;HYPERGRID.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;BITBLT.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;BITBLT.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;ADVANCED-FUNCTIONS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;ADVANCED-FUNCTIONS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;VECTOR-FUNCTIONS.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;VECTOR-FUNCTIONS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-TESTS.LISP.NEWEST
Loading STARSIM:STARLISP-SIMULATOR;DEFSTRUCT-TESTS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;ARRAY-TESTS.LISP.NEWEST
Loading STARSIM:STARLISP-SIMULATOR;ARRAY-TESTS.BIN.NEWEST
Compiling STARSIM:STARLISP-SIMULATOR;PATCHES.LISP.NEWEST
Warning: the symbol :MUSER is not one of the defined file attributes.
Loading STARSIM:STARLISP-SIMULATOR;PATCHES.BIN.NEWEST
 
Thinking Machines *Lisp Simulator.  Version 19.0
 
Initializing system Starlisp Simulator F19 version Newest
No system directory for Starlisp Simulator F19, creating one
Starlisp Simulator F19 version 1 created
 
The following functions were referenced but don't seem defined:
 *SIM-I:INDIRECT-SETF-SIDEWAYS-SVREF!! referenced by *SIM-I:TEST-SLICEWISE-INDIRECT-ACCESS-GENERAL,
   *SIM-I:TEST-SLICEWISE-INDIRECT-ACCESS
 *SIM-I:*SET-CAN-BE-COMPILED-P referenced by *SIM:*SET
 *SIM-I:GENERAL-PVAR-ARRAY-LIST referenced by *SIM-I:GENERAL-PVAR-WITHOUT-ARRAYS-P
 *SIM-I:GENERAL-PVAR-STRUCTURE-LIST referenced by *SIM-I:GENERAL-PVAR-WITHOUT-STRUCTURES-P
 *SIM-I:GENERAL-STRUCTURE-FIRST-INDIRECT-ASET!! referenced by *SIM-I:GENERAL-INDIRECT-ARRAY-AND-STRUCTURE-TEST,
   *SIM-I:NESTED-INDIRECT-SLOT-TEST

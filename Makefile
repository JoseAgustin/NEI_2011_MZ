#	Top-level Makefile for convert  conversion program

#	Macros, these should be generic for all machines

.IGNORE:
MAKE    =       make -i -f Makefile
CD      =       cd
LN	=	ln -s
RM	=	/bin/rm -f 
RM_LIST =	*.log
#	Targets for supported architectures

default:
	( $(CD) src   ; $(MAKE)  );
	( $(RM) convert.exe ; $(LN) src/convert.exe . ) ;

code:
	( $(CD) src   ; $(MAKE) code "FC= ifort" );
clean:
	( $(CD) src   ; $(MAKE) clean  );
	$(RM) $(RM_LIST)


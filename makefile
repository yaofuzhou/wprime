# ----------------------------------------------------------------------------
#
# Makefile for wprime MODEL directory
# AUG 07 2010
#
# ----------------------------------------------------------------------------

F77           = gfortran
#Uncomment the following line for MadEvent 5
#F77           = gfortran
FFLAGS        = -O -ffixed-line-length-132
LIBRARY       = ../libmodel.a
LIBDIR        = ../../lib/
MODEL         = couplings.o lha_reading.o printout.o couplings_test.o WPWIDTH.o READMODEL.o CONVERTMODEL.o LOGMODEL.o

.f.o: ; $(F77) $(FFLAGS) -c $*.f

all: $(LIBDIR)libmodel.a

testprog: testprog.o $(MODEL)
	$(F77) $(FFLAGS) -o $@ $^

checkmodel: CHECKMODEL.o $(MODEL)
	$(F77) $(FFLAGS) -o $@ $^

couplings: couplingsvalues.o $(MODEL)
	$(F77) $(FFLAGS) -o $@ $^

$(LIBDIR)libmodel.a: $(MODEL) makefile
	ar cru libmodel.a $(MODEL)
	ranlib libmodel.a
	mv libmodel.a  $(LIBDIR)

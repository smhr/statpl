.KEEP_STATE:
# Compiler flags
# for the g95 compiler -O3 for optimisation
FLAGS= -O3
# which compiler to use
FC95 = gfortran

%.o  : %.f95
	$(FC95) $(FLAGS) -c $<  -o $@

%.mod  : %.f95
	$(FC95) $(FLAGS) -c $<  

SOURCE = statmf.f95
MODSOURCE = utilmod.f95  mathmod.f95  statfunmod.f95 mffunmod.f95 mfestmod.f95 statpl_mod.f95  

MODULES= $(MODSOURCE:.f95=.o)
OBJECTS= $(SOURCE:.f95=.mod)


all: statpl
.PHONY: all

statpl: $(MODULES) statpl.f95
	$(FC95) $(FLAGS) -c statpl.f95
	$(FC95) $(MODULES) statpl.o -o statpl


clean :
	rm *.o
	rm *.mod
	rm statpl

#************************************************************************************************100
#
#  PROGRAM    :
#               IsingModel
#
#  PURPOSE    :
#               Program to explore aspects of Monte Carlo simulation with
#               the Metropolis algorithm using the one- and two-dimensional
#               Ising model.
#
#  PROGRAMMER :
#               Martin Josemaria Vuelta Rojas
#               * martin.vuelta@gmail.com
#               * Faculty of Physical Science
#               * Universidad Nacional Mayor de San Marcos
#               * Lima - Peru
#
#  FILE       :
#               makefile
#
#  DESCRIPTION:
#               This file contains instructions to compile the program.
#
#************************************************************************************************100


# C Compiler options
CC        = gcc
SRCC      = isingcwrapper.c
OBJC      = isingcwrapper.o
FLAGSC    = -g -W -Wall -Werror -Wextra -Wshadow -Wconversion -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings -Wunused -Wunused-function -Wunused-label -Wunused-parameter -Wunused-value -Wunused-variable -Wmissing-braces  -Wswitch -Wswitch-default -Wswitch-enum

# Fortran Compiler options
FORTRAN   = gfortran
SRCF      = datetimemodule.f90 isingmodule.f90 ising.f90
OBJF      = datetimemodule.o isingmodule.o ising.o
MODF      = datetimemodule.mod isingmodule.mod
FLAGSF    = -std=f2008 -g  -O4 -W -Wall -Werror -Wextra -Wshadow -Wconversion -Wcast-align -Wunused -Wunused-function -Wunused-label -Wunused-parameter -Wunused-value -Wunused-variable

# Linker options
LINKER    = gfortran
OBJL      = datetimemodule.o isingcwrapper.o isingmodule.o ising.o
LIBL      = -lgsl -lgslcblas
FLAGSL    = -g -O4

ifeq ($(OS),Windows_NT)
	BINL      = ising.exe
	RM        = del /Q
	FixPath   = $(subst /,\,$1)
	ClrCsl    = CLS
else
	BINL      = ising
	RM        = rm -f
	FixPath   = $1
	ClrCsl    = clear
endif

.PHONY: all all-before all-after clean clean-custom

all: all-before $(OBJL) $(BINL) clean-custom all-after

clean:
	$(RM) $(OBJL) $(BINL) $(MODF)

clean-custom:
	$(RM) $(OBJL) $(MODF)

$(OBJC): $(GLOBALDEPS)
	$(CC) -c $(SRCC) $(FLAGSC)

$(OBJF): $(GLOBALDEPS)
	$(FORTRAN) -c $(SRCF) $(FLAGSF)

$(BINL): $(OBJC) $(OBJF)
	$(LINKER) -o $(BINL) $(OBJL) $(LIBL) $(FLAGSL)

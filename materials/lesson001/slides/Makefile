TEX      = xelatex
SRCTEX   = lesson001.tex
OUTTEX   = lesson001.pdf
OUTDIR   = build
FALGSTEX = --file-line-error --shell-escape --synctex=1

ifeq ($(OS),Windows_NT)
	RM      = del /Q
	MKDIR   = MKDIR
	COPY    = COPY
	FixPath = $(subst /,\,$1)
	ClrCsl  = CLS
else
	RM      = rm -rf
	COPY    = cp
	MKDIR   = mkdir
	FixPath = $1
	ClrCsl  = clear
endif

.PHONY: all all-before all-after clean

all: clean all-before $(OUTTEX) all-after clean-custom

all-before:
ifeq ($(OS),Windows_NT)
	$(MKDIR) $(OUTDIR)
else
	$(MKDIR) $(OUTDIR) 2>/dev/null
endif

all-after:
	$(COPY) $(call FixPath,./$(OUTDIR)/$(OUTTEX)) .

clean:
	$(RM) $(OUTDIR); $(RM) $(OUTTEX)

clean-custom:
	$(RM) $(OUTDIR)

$(OUTTEX): $(GLOBALDEPS)
	for number in 1 2 3 ; do \
	$(TEX) $(FALGSTEX) --output-directory=$(OUTDIR) $(SRCTEX); \
	done
	

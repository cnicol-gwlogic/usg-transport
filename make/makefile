# makefile created by pymake (version 1.2.4) for the 'mfusg' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=..\src

VPATH = \
${SOURCEDIR1} 

.SUFFIXES: .f .for .o

OBJECTS = \
$(OBJDIR)/gmodules.o \
$(OBJDIR)/xmdlib_2.o \
$(OBJDIR)/sparse.o \
$(OBJDIR)/gwf2dpf1u1.o \
$(OBJDIR)/lak_gag_sfr_modules.o \
$(OBJDIR)/gwf2rch8u1.o \
$(OBJDIR)/gwf2evt8u1.o \
$(OBJDIR)/gwf2ets8u1.o \
$(OBJDIR)/gwf2wel7u1.o \
$(OBJDIR)/gwf2riv7u1.o \
$(OBJDIR)/gwf2QRT8u.o \
$(OBJDIR)/gwf2ghb7u1.o \
$(OBJDIR)/gwf2drt8u.o \
$(OBJDIR)/gwf2drn7u1.o \
$(OBJDIR)/gwf2chd7u1.o \
$(OBJDIR)/xmd.o \
$(OBJDIR)/pcgu7.o \
$(OBJDIR)/disu2gncn1.o \
$(OBJDIR)/density.o \
$(OBJDIR)/gwt2mdtu1.o \
$(OBJDIR)/gwf2fhb7u1.o \
$(OBJDIR)/gwf2str7u1.o \
$(OBJDIR)/utl7u1.o \
$(OBJDIR)/utl7u1 RD.o \
$(OBJDIR)/tvmu2.o \
$(OBJDIR)/tvmu1.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/mfusg.o \
$(OBJDIR)/gwt2dptu1.o \
$(OBJDIR)/gwt2bndsu1.o \
$(OBJDIR)/gwt2aw_adsorb.o \
$(OBJDIR)/gwf2sub7u1.o \
$(OBJDIR)/gwf2sfr7u1.o \
$(OBJDIR)/gwf2lak7u1.o \
$(OBJDIR)/gwf2hfb7u1.o \
$(OBJDIR)/gwf2gag7u1.o \
$(OBJDIR)/gwf2bcf-lpf-u1.o \
$(OBJDIR)/gwf2basu1.o \
$(OBJDIR)/glo2sms-u1.o \
$(OBJDIR)/glo2sgbu1.o \
$(OBJDIR)/glo2btnu1.o \
$(OBJDIR)/glo2basu1.o \
$(OBJDIR)/dpt2aw_adsorb.o \
$(OBJDIR)/disu2gncb1.o \
$(OBJDIR)/cln2props1.o \
$(OBJDIR)/cln2basu1.o \
$(OBJDIR)/ReadCBC.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.for
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)


ifneq ("$(wildcard ../../Makefile.include)", "")
include ../../Makefile.include
else
ifneq ("$(wildcard $(ALPINO_HOME)/Makefile.include)", "")
include $(ALPINO_HOME)/Makefile.include
endif
endif

ifeq ($(PLATFORM),windows)
	PRO_LM_LIB=pro_lm.dll
else
	PRO_LM_LIB=pro_lm$(MODULEEXT)
	PRO_LM_STATIC=pro_lm.s.o
endif

CXXFLAGS=-O2 -Wall -pedantic -std=c++11

FADD=$(ALPINO_HOME)/fadd

PLS=maxent_feature_weights.pl maxent_feature_weights_d.pl maxent_feature_weights_p.pl maxent_feature_weights_c.pl maxent_feature_weights_x.pl maxent_feature_weights_w.pl maxent_feature_weights_i.pl
POS=maxent_feature_weights.po

ifeq "$(PROLOG)" "sicstus"
all: $(PRO_LM_LIB) $(PRO_LM_STATIC) $(PLS) $(POS) 
else
all: $(PRO_LM_LIB)                  $(PLS)
endif

all: pro_lm.so

lm.o: lm.cpp lm.h
	g++ -fPIC ${CXXFLAGS} -c $< -o $@

pro_lm.so: pro_lm.cpp pro_lm.pl lm.o lm.h
	$(SPLFR) pro_lm.pl pro_lm.cpp lm.o --cflag="${CXXFLAGS} -I$(FADD)" \
		-LD -Wall -lstdc++ -Wl,-rpath,$(ALPINO_HOME)/fadd \
		-L$(FADD) -lfadd

pro_lm.bundle pro_lm.dylib: pro_lm.cpp pro_lm.pl lm.o lm.h
	$(SPLFR) pro_lm.pl pro_lm.cpp lm.o --cflag="${CXXFLAGS} -I$(FADD)" \
		-LD -Wall -lstdc++ -Wl,-rpath,$(ALPINO_HOME)/fadd \
		-L$(FADD) -lfadd
	install_name_tool -change libfadd.dylib @rpath/libfadd.dylib $@

pro_lm.dll: pro_lm.cpp lm.cpp pro_lm.pl lm.h
	splfr pro_lm.pl pro_lm.cpp lm.cpp $(FADD)/fadd.lib \
		--cflag="/EHsc /I$(FADD)"
	mt -manifest pro_lm.dll.manifest -outputresource:pro_lm.dll\;2

pro_lm.s.o: pro_lm.cpp pro_lm.pl lm.o lm.h
	splfr pro_lm.pl pro_lm.cpp lm.o --cflag="${CXXFLAGS}" \
		--static \
		-LD -Wall -lstdc++ -Wl,-rpath,$(ALPINO_HOME)/fadd \
		-L$(FADD) -lfadd

maxent_feature_weights.pl:maxent_feature_weights.t index.pl estimate2pl
	./estimate2pl < maxent_feature_weights.t |\
          $(HDRUG) -l index > maxent_feature_weights.pl

maxent_feature_weights_c.pl:maxent_feature_weights_c.t index.pl estimate2pl
	./estimate2pl < maxent_feature_weights_c.t |\
          $(HDRUG) -l index > maxent_feature_weights_c.pl

maxent_feature_weights_p.pl:maxent_feature_weights_p.t index.pl estimate2pl
	./estimate2pl < maxent_feature_weights_p.t |\
          $(HDRUG) -l index > maxent_feature_weights_p.pl

maxent_feature_weights_x.pl:maxent_feature_weights_x.t index.pl estimate2pl
	./estimate2pl < maxent_feature_weights_x.t |\
          $(HDRUG) -l index > maxent_feature_weights_x.pl

maxent_feature_weights_d.pl:maxent_feature_weights_d.t index.pl estimate2pl
	./estimate2pl < maxent_feature_weights_d.t |\
          $(HDRUG) -l index > maxent_feature_weights_d.pl

maxent_feature_weights_w.pl:maxent_feature_weights_w.t index.pl estimate2pl
	./estimate2pl < maxent_feature_weights_w.t |\
          $(HDRUG) -l index > maxent_feature_weights_w.pl

maxent_feature_weights_i.pl:maxent_feature_weights_i.t index.pl estimate2pl
	./estimate2pl < maxent_feature_weights_i.t |\
          $(HDRUG) -l index > maxent_feature_weights_i.pl

maxent_feature_weights.po: maxent_feature_weights.pl
	echo "compile(feature_weights:'maxent_feature_weights.pl'), save_files(maxent_feature_weights,maxent_feature_weights)." | $(HDRUG) -notk

clean:
	rm -f *.o

realclean: clean
	rm -f *.so *.dll $(PRO_LM_LIB) $(POS) $(PLS) pro_lm_swi_glue.c

install:

%.po : %.pl
	echo "compile('$*'), save_files('$*','$*')." | $(HDRUG) -notk


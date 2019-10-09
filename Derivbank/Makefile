ifeq "$(shell if [ -r ../Makefile.include ]; then echo yes; fi)" "yes"
include ../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

CXXFLAGS=-I$(LIBCORPUS_PATH) -I$(BOOST_INCLUDE_PATH)

ifeq "$(PROLOG)" "sicstus"
all: derivbank$(MODULEEXT) derivbank.s.o
else
all: derivbank$(MODULEEXT) 
endif

derivbank.so: derivbank.cpp derivbank.pl $(LIBCORPUS_DYLIB)
	$(SPLFR) derivbank.pl derivbank.cpp \
		--cflag="$(CXXFLAGS)" \
		-LD -Wall -Wl,-rpath,$(LIBCORPUS_PATH) -Wl,-rpath,$(BOOST_LIBRARY_PATH) \
		-L$(LIBCORPUS_PATH) -L$(BOOST_LIBRARY_PATH) -lcorpus \
		-lboost_system -lboost_filesystem -lboost_thread -lstdc++

derivbank.bundle derivbank.dylib: derivbank.cpp derivbank.pl $(LIBCORPUS_DYLIB)
	$(SPLFR) derivbank.pl derivbank.cpp \
		--cflag="$(CXXFLAGS)" \
		-LD -Wall -Wl,-rpath,$(LIBCORPUS_PATH) -Wl,-rpath,$(BOOST_LIBRARY_PATH) \
		-L$(LIBCORPUS_PATH) -L$(BOOST_LIBRARY_PATH) -lcorpus \
		-lboost_system-mt -lboost_filesystem-mt -lboost_thread-mt -lstdc++
	install_name_tool -change libcorpus.dylib @rpath/libcorpus.dylib $@

derivbank.s.o: derivbank.cpp derivbank.pl $(LIBCORPUS_DYLIB)
	splfr derivbank.pl derivbank.cpp \
		--cflag="$(CXXFLAGS)" --static -LD -Wall \
		-Wl,-rpath,$(LIBCORPUS_PATH) -Wl,-rpath,$(BOOST_LIBRARY_PATH) \
		-L$(LIBCORPUS_PATH) -L$(BOOST_LIBRARY_PATH) -lcorpus \
		-lboost_system -lboost_filesystem -lboost_thread -lstdc++

clean:

realclean:
	rm -f derivbank$(MODULEEXT) derivbank.s.o

install:

ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

CXXFLAGS=-Wall -pedantic -std=c++11 -Wno-long-long -O3 -I. -I$(LIBCORPUS_PATH)

SOURCES=ProgramOptions/ProgramOptions.cpp miniact.cpp
OBJECTS=$(SOURCES:.cpp=.o)

all: miniact


#BOOST_LIBRARIES=-lboost_system
ifeq ($(PLATFORM),darwin)
	BOOST_LIBRARIES=-lboost_system-mt
endif

install:
	install -m 0755 miniact $(BINDIR)/miniact

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

miniact: $(OBJECTS) $(LIBCORPUS_STATIC)
	$(CXX) -O3 -o $@ $(OBJECTS) $(LIBCORPUS_STATIC) $(BOOST_LIBRARIES) -lz

clean:
	find . -name '*.o' -exec rm -f {} \;

realclean: clean
	rm -f miniact

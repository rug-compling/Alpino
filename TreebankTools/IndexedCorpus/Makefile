ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

DYLIBLINKFLAGS=-fPIC -shared -lz
WARN=-Wno-unused-local-typedefs
CXXFLAGS=-O3 -Wall -I. -fPIC  -I$(BOOST_INCLUDE_PATH) $(WARN)

SOURCES=\
	src/ActCorpusReader/ActCorpusReader.cpp \
	src/DzIstream/DzIstream.cpp \
	src/DzOstream/DzOstream.cpp \
	src/DzOstreamBuf/DzOstreamBuf.cpp \
	src/DzIstreamBuf/DzIstreamBuf.cpp \
	src/IndexNamePair/IndexNamePair.cpp \
	src/IndexedCorpusReader/IndexedCorpusReader.cpp \
	src/IndexedCorpusWriter/IndexedCorpusWriter.cpp \
	src/util/textfile/textfile.cpp
OBJECTS=$(SOURCES:.cpp=.o)


ifeq "$(PROLOG)" "sicstus"
PROLOGLIBS=prolog/corpusreader$(MODULEEXT) prolog/corpusreader.s.o
else
PROLOGLIBS=prolog/corpusreader$(MODULEEXT) 
endif


PYTHONLIBS=python/indexedcorpus$(PYMODULEEXT)

all: libcorpus$(DYLIBEXT) libcorpus.a $(PROLOGLIBS) $(PYTHONLIBS)
	cp python/indexedcorpus$(PYMODULEEXT) ../python-lib

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

libcorpus.so: $(OBJECTS)
	g++ $(DYLIBLINKFLAGS) -Wl,-rpath,$(BOOST_LIBRARY_PATH) \
		-L$(BOOST_LIBRARY_PATH) -o $@ $(OBJECTS)

libcorpus.dylib: $(OBJECTS)
	g++ $(DYLIBLINKFLAGS) -Wl,-rpath,$(BOOST_LIBRARY_PATH) \
		-L$(BOOST_LIBRARY_PATH) -lboost_filesystem-mt -lboost_system-mt \
		-o $@ $(OBJECTS)
	install_name_tool -change libalpino.dylib @rpath/libalpino.dylib \
		$@

libcorpus.a: $(OBJECTS)
	ar cr $@ $(OBJECTS)

prolog/corpusreader.so: prolog/corpusreader.pl prolog/corpusreader.cpp libcorpus$(DYLIBEXT)
	$(SPLFR) prolog/corpusreader.pl prolog/corpusreader.cpp \
		--cflag="$(CXXFLAGS)" -O2 -LD -Wall \
		-Wl,-rpath,$(PWD) -Wl,-rpath,$(BOOST_LIBRARY_PATH) \
		-L. -L$(BOOST_LIBRARY_PATH) -lcorpus -lboost_system \
		-lboost_filesystem -lboost_thread -lstdc++ -lz
ifeq "$(PROLOG)" "sicstus"
	mv corpusreader.so $@
endif

prolog/corpusreader.bundle prolog/corpusreader.dylib: prolog/corpusreader.pl prolog/corpusreader.cpp libcorpus$(DYLIBEXT)
	$(SPLFR) prolog/corpusreader.pl prolog/corpusreader.cpp \
		--cflag="$(CXXFLAGS)" -O2 -LD -Wall \
		-Wl,-rpath,$(PWD) -Wl,-rpath,$(BOOST_LIBRARY_PATH) \
		-L. -L$(BOOST_LIBRARY_PATH) -lcorpus -lboost_system-mt \
		-lboost_filesystem-mt -lboost_thread-mt -lstdc++ -lz
ifeq "$(PROLOG)" "sicstus"
	mv corpusreader.bundle $@
endif
	install_name_tool -change libalpino.dylib @rpath/libalpino.dylib \
		$@
	install_name_tool -change libcorpus.dylib @rpath/libcorpus.dylib \
		$@

prolog/corpusreader.s.o: prolog/corpusreader.pl prolog/corpusreader.cpp libcorpus$(DYLIBEXT)
	splfr prolog/corpusreader.pl prolog/corpusreader.cpp \
		 --static --cflag="$(CXXFLAGS)" -O2 -LD -Wall \
		-Wl,-rpath,$(PWD) -Wl,-rpath,$(BOOST_LIBRARY_PATH) \
		-L. -L$(BOOST_LIBRARY_PATH) -lcorpus -lboost_system \
		-lboost_filesystem -lboost_thread -lstdc++ -lz
	mv corpusreader.s.o $@

python/indexedcorpus$(PYMODULEEXT): python/indexedcorpus.cpp libcorpus.a
	rm -f python/indexedcorpus$(PYMODULEEXT)
	rm -rf python/build
	if [ -x ${PLATFORM} = "darwin" ] ; then \
		( cd python ; ARCHFLAGS="-arch i386 -arch x86_64" BOOST_HOME=$(BOOST_HOME) python setup.py install --install-platlib=. ) \
	else \
		( cd python ; BOOST_HOME=$(BOOST_HOME) python setup.py install --install-platlib=. ) \
	fi

test/test: libcorpus.a test/test.cpp
	g++ $(CXXFLAGS) -Wl,-rpath,$(BOOST_LIBRARY_PATH) -L$(BOOST_LIBRARY_PATH) \
		-Wall -pedantic -I. -lz -o $@ test/test.cpp libcorpus.a

test: test/test
	( cd test ; ./test )

clean:
	find . -name '*.o' -exec rm -f {} \;
	rm -rf python/build dtnext

realclean: clean
	rm -f libcorpus$(DYLIBEXT)
	rm -f prolog/corpusreader$(MODULEEXT)
	rm -f python/indexedcorpus$(DYLIBEXT)
	rm -f libcorpus.a
	rm -f test/test

install: dtnext
	cp dtnext dtprev ../../bin

dtnext: dtnext.cc 
	g++  $(CXXFLAGS) -o dtnext dtnext.cc -L. -lcorpus -Wl,-rpath,$(PWD) -Wl,-rpath,$(BOOST_LIBRARY_PATH) -lboost_system -lboost_filesystem -L$(BOOST_LIBRARY_PATH) -lz

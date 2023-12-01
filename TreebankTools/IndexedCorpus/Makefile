ifeq "$(shell if [ -r ../../Makefile.include ]; then echo yes; fi)" "yes"
include ../../Makefile.include
else
ifeq "$(shell if [ -r $(ALPINO_HOME)/Makefile.include ]; then echo yes; fi)" "yes"
include $(ALPINO_HOME)/Makefile.include
endif
endif

DYLIBLINKFLAGS=-fPIC -shared -lz
WARN=-Wno-unused-local-typedefs
CXXFLAGS=-O3 -Wall -std=c++11 -I. -fPIC   $(WARN)

ifeq "$(PROLOG)" "swipl"
EXTRACXXFLAGS=-cc-options,-std=c++11
else
EXTRACXXFLAGS=
endif

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


PYTHONLIBS=python/indexedcorpus.touch

all: libcorpus$(DYLIBEXT) libcorpus.a $(PROLOGLIBS) $(PYTHONLIBS)
	cp python/indexedcorpus.touch ../python-lib

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

libcorpus.so: $(OBJECTS)
	g++ $(DYLIBLINKFLAGS) \
		-o $@ $(OBJECTS) \
                -lboost_filesystem -lboost_system 

libcorpus.dylib: $(OBJECTS)
	g++ $(DYLIBLINKFLAGS) \
		-lboost_filesystem-mt -lboost_system-mt \
		-o $@ $(OBJECTS)
	install_name_tool -change libalpino.dylib @rpath/libalpino.dylib \
		$@

libcorpus.a: $(OBJECTS)
	ar cr $@ $(OBJECTS)

prolog/corpusreader.so: prolog/corpusreader.pl prolog/corpusreader.cpp libcorpus$(DYLIBEXT)
	$(SPLFR) prolog/corpusreader.pl prolog/corpusreader.cpp \
		--cflag="$(CXXFLAGS)" -O2 -LD -Wall \
                $(EXTRACXXFLAGS) \
		-Wl,-rpath,$(PWD) \
		-L. -lcorpus -lboost_filesystem \
		-lboost_system -lstdc++ -lz
ifeq "$(PROLOG)" "sicstus"
	mv corpusreader.so $@
endif

prolog/corpusreader.bundle prolog/corpusreader.dylib: prolog/corpusreader.pl prolog/corpusreader.cpp libcorpus$(DYLIBEXT)
	$(SPLFR) prolog/corpusreader.pl prolog/corpusreader.cpp \
		--cflag="$(CXXFLAGS)" -O2 -LD -Wall \
                $(EXTRACXXFLAGS) \
		-Wl,-rpath,$(PWD) \
		-L. -lcorpus -lboost_system-mt \
		-lboost_filesystem-mt -lstdc++ -lz
ifeq "$(PROLOG)" "sicstus"
	mv corpusreader.bundle $@
endif
	install_name_tool -change libalpino.dylib @rpath/libalpino.dylib \
		$@
	install_name_tool -change libcorpus.dylib @rpath/libcorpus.dylib \
		$@

prolog/corpusreader.s.o: prolog/corpusreader.pl prolog/corpusreader.cpp libcorpus$(DYLIBEXT)
	splfr prolog/corpusreader.pl prolog/corpusreader.cpp \
		 --static --cflag="$(CXXFLAGS) -fPIE" -O2 -LD -Wall \
		-Wl,-rpath,$(PWD)  \
		-L.  -lcorpus \
		-lboost_filesystem -lboost_system -lstdc++ -lz
	mv corpusreader.s.o $@

python/indexedcorpus.touch: python/indexedcorpus.cpp libcorpus.a
	rm -rf python/IndexedCorpus.egg-info python/build
	if [ -x ${PLATFORM} = "darwin" ] ; then \
		( cd python ; CFLAGS="$(CXXFLAGS)" ARCHFLAGS="-arch i386 -arch x86_64" BOOST_HOME=/usr python3 setup.py install --install-platlib=../../python-lib --single-version-externally-manage --record=/dev/null && touch indexedcorpus.touch ) \
	else \
		( cd python ; CFLAGS="$(CXXFLAGS)" BOOST_HOME=/usr python3 setup.py install --install-platlib=../../python-lib --single-version-externally-manage --record=/dev/null && touch indexedcorpus.touch ) \
	fi

test/test: libcorpus.a test/test.cpp
	g++ $(CXXFLAGS) -Wall -pedantic -I. -lz -o $@ test/test.cpp libcorpus.a

test: test/test
	( cd test ; ./test )

clean:
	find . -name '*.o' -exec rm -f {} \;
	rm -rf python/build dtnext

realclean: clean
	rm -f libcorpus$(DYLIBEXT)
	rm -f prolog/corpusreader$(MODULEEXT) prolog/corpusreader_swi_glue.c
	rm -f python/indexedcorpus.touch
	rm -fr python/IndexedCorpus*.egg-info
	rm -f libcorpus.a
	rm -f test/test
	rm -f ../python-lib/indexedcorpus.*
	rm -fr ../python-lib/IndexedCorpus-1.0.egg-info

install: dtnext
	cp dtnext dtprev ../../bin

dtnext: dtnext.cc 
	g++  $(CXXFLAGS) -o dtnext dtnext.cc -L. -lcorpus -Wl,-rpath,$(PWD) -lboost_system -lboost_filesystem -lz

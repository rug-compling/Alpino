# Makefile for saxon-wrap
# GJK Thu Jan 17 22:12:52 2008

PWD:=${shell pwd}

SAXONJAR  = $(wildcard ~gosse/src/saxon8-8/saxon8.jar)
CLASSPATH = $(SAXONJAR):$(PWD)

JAVA = java
JAVAC = javac

%.class:%.java
	$(JAVAC) -classpath $(CLASSPATH) $<

SRCS = SaxonWrap.java
OBJS = $(patsubst %.java, %.class, $(SRCS))

all: saxon-wrap $(OBJS)

saxon-wrap:
	echo "#!/bin/sh" > $@
	echo '$(JAVA) -cp $(CLASSPATH) SaxonWrap "$$@"' >> $@
	chmod +x $@

clean:
	rm -f saxon-wrap *.class

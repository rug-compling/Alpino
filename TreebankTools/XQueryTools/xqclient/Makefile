# Makefile for xqclient
# GJK Wed Jan 16 22:54:37 2008

JAVA = java
JAVAC = javac


# set XQCLIENT variable according to your Saxon version
# either SAXON8JAR or SAXON9JAR must be set correctly

# original Saxon 8 version
XQCLIENT = XQClient
#XQCLIENT = XQClientSaxon9

SAXON8JAR  = /net/aistaff/gosse/src/saxon8-8/saxon8.jar
SAXON9JAR  = $(wildcard ~/build/saxon9/saxon9.jar)

ifeq ($(XQCLIENT), XQClientSaxon9)
  SAXONJAR = $(SAXON9JAR)
else
  SAXONJAR = $(SAXON8JAR)
endif

CLASSPATH = $(SAXONJAR):$(PWD)/classes

SRCS = $(XQCLIENT).java TreebankClient.java
OBJS = $(addprefix classes/, $(patsubst %.java, %.class, $(SRCS)))

all: xqclient $(OBJS)

xqclient: Makefile
	echo "#!/bin/sh" > $@
	echo '$(JAVA) -cp $(CLASSPATH) $(XQCLIENT) "$$@"' >> $@
	chmod +x $@

classes/%.class:%.java
	mkdir -p classes
	$(JAVAC) -d classes -classpath $(CLASSPATH):. $<

clean:
	rm -f xqclient classes/*.class


% : %.c
	gcc -s -Wall -o $@ $<

% : %.cc
	g++ -std=c++0x -s -Wall -O3 -o $@ $<

% : %.go
	go build $<

all: ev eval

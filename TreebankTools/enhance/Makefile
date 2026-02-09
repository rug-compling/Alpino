all: enhance

install:
	cp enhance ../../bin

enhance: *.go go.mod
	go build .

go.mod: dummy
	go get -u github.com/rug-compling/alud/v2
	go mod tidy

clean:

realclean:
	rm enhance

.PHONY: dummy
dummy:

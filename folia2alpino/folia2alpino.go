package main

import (
	"github.com/pebbe/util"

	"encoding/xml"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
)

var (
	opt_w = flag.Bool("w", false, "inclusief word-IDs")
	x     = util.CheckErr
)

func usage() {
	fmt.Printf(`
Syntax: %s [-w] [folia.xml...]

 -w : inclusief word-IDs

`, os.Args[0])
}

func main() {

	flag.Usage = usage
	flag.Parse()

	if !(flag.NArg() == 0 && !util.IsTerminal(os.Stdin) || flag.NArg() > 0) {
		usage()
		return
	}

	if flag.NArg() == 0 {
		x(folia(os.Stdin, os.Stdout))
	} else {
		for _, arg := range flag.Args() {
			fp, err := os.Open(arg)
			x(err)
			x(folia(fp, os.Stdout))
			fp.Close()
		}
	}
}

func folia(fpin, fpout *os.File) error {

	d := xml.NewDecoder(fpin)
	var inS, inW, inT, inCorrection, inOriginal bool
	var label, wid string
	var teller uint64
	words := make([]string, 0, 500)
	for {
		tt, err := d.Token()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		if t, ok := tt.(xml.StartElement); ok {
			switch t.Name.Local {
			case "whitespace":
				if len(words) > 0 {
					fmt.Fprintf(fpout, "%s:%s\n", label, strings.Join(words, " "))
					words = words[0:0]
					label += ".b"
				}
			case "s":
				teller++
				label = fmt.Sprintf("s.%d", teller)
				for _, e := range t.Attr {
					if e.Name.Local == "id" {
						label = e.Value
						break
					}
				}
				inS = true
				inW = false
				inT = false
				inCorrection = false
				inOriginal = false
			case "w":
				wid = ""
				for _, e := range t.Attr {
					if e.Name.Local == "id" {
						wid = e.Value
						break
					}
				}
				inW = true
				inT = false
			case "t":
				inT = true
			case "correction":
				inCorrection = true
				inOriginal = false
			case "original":
				if inCorrection {
					inOriginal = true
				}
			}
		} else if t, ok := tt.(xml.EndElement); ok {
			switch t.Name.Local {
			case "s":
				if inS {
					if len(words) > 0 {
						fmt.Fprintf(fpout, "%s:%s\n", label, strings.Join(words, " "))
						words = words[0:0]
					}
				}
				inS = false
				inW = false
				inT = false
			case "w":
				inW = false
				inT = false
			case "t":
				inT = false
			case "correction":
				inCorrection = false
				inOriginal = false
			case "original":
				inOriginal = false
			}
		} else if t, ok := tt.(xml.CharData); ok {
			if inS && inW && inT && !inOriginal {
				s := alpinoEscape(string(t))
				if *opt_w && wid != "" {
					s = fmt.Sprintf("[ @id %s ] %s", alpinoEscape(wid), s)
				}
				words = append(words, s)
				wid = ""
				inW = false
				inT = false
			}
		}
	}
	return nil
}

func alpinoEscape(s string) string {
	switch s {
	case `[`:
		return `\[`
	case `]`:
		return `\]`
	case `\[`:
		return `\\[`
	case `\]`:
		return `\\]`
	}
	return s
}

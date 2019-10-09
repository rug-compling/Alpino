package main

import (
	"github.com/pebbe/util"

	"encoding/hex"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

const (
	labelTemplate = "%D.p.%p.s.%l|"
	itemRegexp    = `^\s+(\d+[.)]|\*|-)(\s|$)`
)

var (
	x = util.CheckErr
)

func usage() {
	fmt.Printf(`
Syntax: %s [opties] [<] bestand

Neemt een tekstbestand, verdeelt het onder in paragrafen en in zinnen,
tokeniseert de zinnen, en geeft elke zin een label.

Het tekstbestand kan commentaren en labels hebben.

  - een commentaar bestaat uit een regel die begint met een '%%'
  - een label bestaat uit een regel met tekst (zonder '|') afgesloten door een '|'

De environment variable ALPINO_HOME moet gedefinieerd zijn.

Opties:

  -d string : beginwaarde voor %%D
                default: gelijk aan waarde van %%B
  -i string : reguliere expresie waarmee item in een lijst wordt herkend,
                default: %s
  -t string : template voor label,
                default: %s

Waardes in de template:

  %%P : compleet path van invoerbestand
  %%F : naam van invoerbestand, zonder path
  %%B : naam van invoerbestand, zonder path en laatste extensie
  %%D : label gegeven in invoerbestand
  %%p : paragraafnummer
  %%l : nummer van zin in paragraaf
  %%L : nummer van zin in heel het uitvoerbestand
  %%%% : %%

Waardes met vlaggen, voorbeelden:

  %%3p   : drie tekens breed, met voorloopspaties
  %%04l  : vier tekens breed, met voorloopnullen
  %%-5L  : vijf tekens breed, aangevuld met spaties

Items blijven het begin van een regel. Voorbeelden voor de default:

 1. eerste item
 2) tweede item
 * derde item
 - vierde item

Je kunt dit helemaal uitzetten met optie: -i ""

`, os.Args[0], itemRegexp, labelTemplate)
}

func main() {

	opt_t := flag.String("t", labelTemplate, "label template")
	opt_i := flag.String("i", itemRegexp, "item regexp")
	opt_d := flag.String("d", "", "initial value for %D")
	flag.Usage = usage
	flag.Parse()
	if !(flag.NArg() == 0 && !util.IsTerminal(os.Stdin) || flag.NArg() == 1) {
		usage()
		return
	}

	alpino := os.Getenv("ALPINO_HOME")
	if alpino == "" {
		x(fmt.Errorf("Missing environment variable ALPINO_HOME"))
	}

	tmpl := regexp.MustCompile(`%-?\d*.`).ReplaceAllStringFunc(*opt_t, keys) + "%[8]s\n"

	var fp *os.File
	var pathname, filename, basename, labelname string
	var err error
	if flag.NArg() == 1 {
		fp, err = os.Open(flag.Arg(0))
		x(err)
		pathname = flag.Arg(0)
		filename = filepath.Base(pathname)
		if i := strings.LastIndex(filename, "."); i > 0 {
			basename = filename[:i]
		} else {
			basename = filename
		}
	} else {
		fp = os.Stdin
		pathname = "(stdin)"
		filename = pathname
		basename = pathname
	}
	if *opt_d == "" {
		*opt_d = basename
	}
	labelname = *opt_d

	cmd := exec.Command(filepath.Join(alpino, "Tokenization", "tokenize.sh"))
	cmd.Env = []string{
		"ALPINO_HOME=" + alpino,
		"LANG=en_US.utf8",
		"LANGUAGE=en_US.utf8",
		"LC_ALL=en_US.utf8",
		"PATH=" + os.Getenv("PATH"),
	}
	cmd.Stderr = os.Stderr
	inp, err := cmd.StdinPipe()
	x(err)
	outp, err := cmd.StdoutPipe()
	x(err)

	x(cmd.Start())

	eop := "cwuuUgGyuiKLhYtduiuhUYYfuuihYRkdwiytfOuVytfyFFFFytFtfYfYfyffyytewA"
	cmt := "OIUhdiuiHDodiwjoGUYRDWSxsLKJHGIDUwhgoGuYGGBjfesjgfdUTRehlkgOUyGkjh"
	lbl := "NOIJDnokdnhLKJnwuGIKWJnbLKJndsoiudhgiTEGDhkDkjKEkjhEKHkEhkhHDSHSKk"

	go func() {
		blank := regexp.MustCompile(`^\s*$`)
		label := regexp.MustCompile(`^[^|]+\|\s*$`)
		item, err := regexp.Compile(*opt_i)
		x(err)
		eopln := "\n" + eop + "\n"
		inline := false
		r := util.NewReader(fp)
		for {
			line, err := r.ReadLineString()
			if err == io.EOF {
				break
			}
			x(err)
			if blank.MatchString(line) {
				if inline {
					fmt.Fprint(inp, eopln)
					inline = false
				}
			} else if line[0] == '%' || strings.HasPrefix(strings.ToLower(line), "##meta") {
				if inline {
					fmt.Fprint(inp, eopln)
					inline = false
				}
				fmt.Fprintln(inp, cmt+" "+hex.EncodeToString([]byte(line)))
			} else if label.MatchString(line) {
				if inline {
					fmt.Fprint(inp, eopln)
					inline = false
				}
				fmt.Fprintln(inp, lbl+" "+hex.EncodeToString([]byte(line)))
			} else if *opt_i != "" && item.MatchString(line) {
				fmt.Fprint(inp, "\n", line, " ")
				inline = true
			} else {
				fmt.Fprint(inp, line, " ")
				inline = true
			}
		}
		if inline {
			fmt.Fprint(inp, eopln)
		}
		inp.Close()
	}()

	parno := make(map[string]int)
	parno[labelname] = 1
	parlineno := 0
	lineno := 0
	r := util.NewReader(outp)
	for {
		line, err := r.ReadLineString()
		if err == io.EOF {
			break
		}
		x(err)
		if line == eop {
			parlineno = 0
			parno[labelname]++
		} else if strings.HasPrefix(line, cmt) {
			b, err := hex.DecodeString(strings.Fields(line)[1])
			x(err)
			fmt.Println(string(b))
		} else if strings.HasPrefix(line, lbl) {
			b, err := hex.DecodeString(strings.Fields(line)[1])
			x(err)
			labelname = strings.TrimSpace(string(b))
			labelname = labelname[:len(labelname)-1]
			labelname = strings.TrimSpace(labelname)
			if labelname == "" {
				labelname = *opt_d
			}
			parlineno = 0
			if _, ok := parno[labelname]; !ok {
				parno[labelname] = 1
			}
		} else {
			parlineno++
			lineno++
			_, err := fmt.Printf(tmpl, pathname, filename, basename, parno[labelname], parlineno, lineno, labelname, line)
			x(err)
		}
	}
}

func keys(s string) string {
	var p string
	n := len(s) - 1
	switch s[n] {
	case 'P':
		p = "[1]s"
	case 'F':
		p = "[2]s"
	case 'B':
		p = "[3]s"
	case 'p':
		p = "[4]d"
	case 'l':
		p = "[5]d"
	case 'L':
		p = "[6]d"
	case 'D':
		p = "[7]s"
	case '%':
		p = "%"
	default:
		x(fmt.Errorf("Onbekende code in template: %s" + s))
	}
	return s[:n] + p
}

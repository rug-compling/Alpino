package main

import (
	"github.com/pebbe/util"

	"bytes"
	"encoding/hex"
	"flag"
	"fmt"
	"html"
	"io"
	"io/ioutil"
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
	// foute entities, gedefinieerd in Tokenization/Create/entities.pl
	specials = map[string][]rune{
		"&lsquor;": []rune{'é'},
		"&ldquor;": []rune{'ä'},
		"&circ;":   []rune{'ê'},
		"&caron;":  []rune{},
		"&gcirc;":  []rune{'g', 'e'},
	}

	x = util.CheckErr

	opt_b = flag.Bool("b", false, "raw byte count")
	opt_c = flag.Bool("c", false, "don't recognise comments")
	opt_e = flag.Bool("e", false, "don't process Alpino escapes")
	opt_l = flag.Bool("l", false, "don't recognise labels")
	opt_m = flag.Bool("m", false, "don't recognise metadata")
	opt_v = flag.Bool("v", false, "verbose")
	opt_t = flag.String("t", labelTemplate, "label template")
	opt_i = flag.String("i", itemRegexp, "item regexp")
	opt_d = flag.String("d", "", "initial value for %D")

	reComment = regexp.MustCompile(`(?m:^%.*?$)`)
	reMeta    = regexp.MustCompile(`(?mi:^##meta.*?$)`)
	reLabel   = regexp.MustCompile(`(?m:^[^\n\r|]+\|[ \t]*$)`)

	posData    []rune
	needPos    = false
	bytePos1   int
	bytePos2   int
	linePos1   int
	linePos2   int
	columnPos1 int
	columnPos2 int
)

func usage() {
	fmt.Printf(`
Syntax: %s [opties] [<] bestand

Neemt een tekstbestand, verdeelt het onder in paragrafen en in zinnen,
tokeniseert de zinnen, en geeft elke zin een label.

Het tekstbestand kan commentaren, labels en metadata hebben.

  - een commentaar bestaat uit een regel die begint met een '%%'
  - een label bestaat uit een regel met tekst (zonder '|') afgesloten door een '|'
  - metadata bestaat uit een regel die begint met '##META' of '##meta'

De environment variable ALPINO_HOME moet gedefinieerd zijn.

Opties:

  -b        : lettertelling op basis van bytes in plaats van utf8
  -c        : behandel commentaren als gewone tekst
  -d string : beginwaarde voor %%D
                default: gelijk aan waarde van %%B
  -e        : tekens die voor Alpino speciaal zijn worden niet ge-escaped
  -i string : reguliere expresie waarmee item in een lijst wordt herkend,
                default: %s
  -l        : behandel labels als gewone tekst
  -m        : behandel metadata als gewone tekst
  -t string : template voor label,
                default: %s
  -v        : verbose: print waarschuwingen

Waardes in de template:

  %%%% : %%
  %%P : compleet path van invoerbestand
  %%F : naam van invoerbestand, zonder path
  %%B : naam van invoerbestand, zonder path en laatste extensie
  %%D : label gegeven in invoerbestand
  %%p : paragraafnummer
  %%l : nummer van zin in paragraaf
  %%L : nummer van zin in heel het uitvoerbestand

    De volgende waardes gaan over het invoerbestand.
    Bytepositie en kolomnummer zijn afhankelijk van
    de optie -b. Zonder die optie worden alle tekens 
    uit UTF-8 en de combinaties CR+LF en LF+CR als 
    één teken geteld. Met de optie -b worden rauwe bytes
    geteld.

  %%a : beginpunt: bytepositie
  %%b : beginpunt: regelnummer
  %%c : beginpunt: kolomnummer
  %%x : eindpunt: bytepositie
  %%y : eindpunt: regelnummer
  %%z : eindpunt: kolomnummer

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

	// dit zet de waarde van needPos:
	tmpl := regexp.MustCompile(`%-?\d*.`).ReplaceAllStringFunc(*opt_t, keys) + "%[14]s\n"

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

	var fp1 io.Reader
	if needPos {
		b, err := ioutil.ReadAll(fp)
		x(err)
		var buf bytes.Buffer

		buf.Write(b)
		fp1 = &buf

		// omzetten naar unix line endings
		i1 := bytes.Index(b, []byte("\r\n"))
		i2 := bytes.Index(b, []byte("\n\r"))
		i3 := bytes.Index(b, []byte("\r"))
		i4 := bytes.Index(b, []byte("\n"))
		if i1 < 0 {
			i1 = len(b)
		}
		if i2 < 0 {
			i2 = len(b)
		}
		if i3 < 0 {
			i3 = len(b)
		}
		if i4 < 0 {
			i4 = len(b)
		}
		if i1 < i2 && i1 == i3 && i1 < i4 {
			if *opt_b {
				b = bytes.Replace(b, []byte("\r\n"), []byte(" \n"), -1)
			} else {
				b = bytes.Replace(b, []byte("\r\n"), []byte("\n"), -1)
			}
		} else if i2 < i1 && i2 < i3 && i2 == i4 {
			if *opt_b {
				b = bytes.Replace(b, []byte("\n\r"), []byte(" \n"), -1)
			} else {
				b = bytes.Replace(b, []byte("\n\r"), []byte("\n"), -1)
			}
		} else {
			b = bytes.Replace(b, []byte("\r"), []byte("\n"), -1)
		}

		if !*opt_c {
			b = reComment.ReplaceAllFunc(b, spaces)
		}
		if !*opt_m {
			b = reMeta.ReplaceAllFunc(b, spaces)
		}
		if !*opt_l {
			b = reLabel.ReplaceAllFunc(b, spaces)
		}

		posData = make([]rune, 0, len(b))
		if *opt_b {
			for i := 0; i < len(b); i++ {
				posData = append(posData, rune(uint(b[i])))
			}
		} else {
			for _, c := range string(b) {
				posData = append(posData, c)
			}
		}

		linePos1 = 1
		linePos2 = 1

	} else {
		fp1 = fp
	}

	args := []string{}
	if *opt_e {
		args = []string{"-e"}
	}
	cmd := exec.Command(filepath.Join(alpino, "Tokenization", "tokenize.sh"), args...)
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
		r := util.NewReader(fp1)
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
			} else if line[0] == '%' && !*opt_c || strings.HasPrefix(strings.ToLower(line), "##meta") && !*opt_m {
				if inline {
					fmt.Fprint(inp, eopln)
					inline = false
				}
				fmt.Fprintln(inp, cmt+" "+hex.EncodeToString([]byte(line)))
			} else if !*opt_l && label.MatchString(line) {
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
			var p1, l1, c1, p2, l2, c2 int
			if needPos {
				p1, l1, c1, p2, l2, c2 = getPos(line)
			}
			_, err := fmt.Printf(tmpl, pathname, filename, basename, parno[labelname], parlineno, lineno, labelname, p1, l1, c1, p2, l2, c2, line)
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
	case 'a':
		p = "[8]d"
		needPos = true
	case 'b':
		p = "[9]d"
		needPos = true
	case 'c':
		p = "[10]d"
		needPos = true
	case 'x':
		p = "[11]d"
		needPos = true
	case 'y':
		p = "[12]d"
		needPos = true
	case 'z':
		p = "[13]d"
		needPos = true
	case '%':
		p = "%"
	default:
		x(fmt.Errorf("Onbekende code in template: %s" + s))
	}
	return s[:n] + p
}

func getPos(txt string) (p1, l1, c1, p2, l2, c2 int) {
	text := make([]rune, 0, len(txt))
	if *opt_b {
		for i := 0; i < len(txt); i++ {
			text = append(text, rune(uint(txt[i])))
		}
	} else {
		for _, r := range txt {
			text = append(text, r)
		}
	}

	// begin bij de oude p2
	// skip white space
	for isWhite(bytePos2) {
		bytePos2, linePos2, columnPos2 = nextPos(bytePos2, linePos2, columnPos2)
	}
	// de nieuwe p1 is nu bekend
	bytePos1, linePos1, columnPos1 = bytePos2, linePos2, columnPos2

	t := 0
TOP:
	for t < len(text) {
		if text[t] == posData[bytePos2] {
			t++
			bytePos2, linePos2, columnPos2 = nextPos(bytePos2, linePos2, columnPos2)
			continue
		}

		hasWhite := false
		for isWhite(bytePos2) {
			bytePos2, linePos2, columnPos2 = nextPos(bytePos2, linePos2, columnPos2)
			hasWhite = true
		}
		for text[t] == ' ' {
			t++
			hasWhite = true
		}
		if hasWhite {
			continue
		}

		// ongelijke tekens...

		for posData[bytePos2] == '&' {
			// entity?
			found := -1
			for i := 1; i < 20 && bytePos2+i < len(posData); i++ {
				if posData[bytePos2+i] == ';' {
					found = i
					break
				}
			}
			if found < 0 {
				break
			}
			found++

			b := make([]byte, found)
			for i := 0; i < found; i++ {
				b[i] = byte(posData[bytePos2+i])
			}
			s := string(b)

			s2 := html.UnescapeString(s)
			for i, c := range s2 {
				if i == 0 && c == text[t] {
					t++
					bytePos2 += found
					columnPos2 += found
					continue TOP
				}
				break
			}

			if sp, ok := specials[s]; ok {
				if n := len(sp); t+n < len(text) {
					if runeSliceEqual(text[t:t+n], sp) {
						t += n
						bytePos2 += found
						columnPos2 += found
						continue TOP
					}
				}
			}

			break
		}

		for _, pair := range [][2]int{
			[2]int{0, 1},
			[2]int{1, 0},

			[2]int{1, 1},
			[2]int{0, 2},
			[2]int{2, 0},

			[2]int{1, 2},
			[2]int{2, 1},
			[2]int{0, 3},
			[2]int{3, 0},

			[2]int{2, 2},
			[2]int{1, 3},
			[2]int{3, 1},
			[2]int{0, 4},
			[2]int{4, 0},

			[2]int{2, 3},
			[2]int{3, 2},
			[2]int{1, 4},
			[2]int{4, 1},
			[2]int{0, 5},
			[2]int{5, 0},
		} {
			if t+pair[0] < len(text) &&
				bytePos2+pair[1] < len(posData) &&
				text[t+pair[0]] == posData[bytePos2+pair[1]] {
				if *opt_v {
					n1 := len(posData[bytePos2:])
					n2 := len(text[t:])
					if n1 > 20 {
						n1 = 20
					}
					if n2 > 20 {
						n2 = 20
					}
					fmt.Fprintf(os.Stderr, "Geen vergelijk, skip %d %d\n\t%q\n\t%q\n",
						pair[1], pair[0], posData[bytePos2:bytePos2+n1], text[t:t+n2])
				}
				t += pair[0]
				for i := 0; i < pair[1]; i++ {
					bytePos2, linePos2, columnPos2 = nextPos(bytePos2, linePos2, columnPos2)

				}
				continue TOP
			}
		}

		n1 := len(posData[bytePos2:])
		n2 := len(text[t:])
		if n1 > 20 {
			n1 = 20
		}
		if n2 > 20 {
			n2 = 20
		}
		x(fmt.Errorf("Geen vergelijk\n\t%q\n\t%q", posData[bytePos2:bytePos2+n1], text[t:t+n2]))
	}

	return bytePos1, linePos1, columnPos1, bytePos2, linePos2, columnPos2
}

func spaces(s []byte) []byte {
	return []byte(strings.Repeat(" ", len(s)))
}

func nextPos(b, l, c int) (int, int, int) {
	if b < len(posData) {
		if posData[b] == '\n' {
			return b + 1, l + 1, 0
		}
		if posData[b] == '\t' {
			return b + 1, l, c + 8 - c%8
		}
		return b + 1, l, c + 1
	}
	return b, l, c
}

func isWhite(p int) bool {
	// \r is al omgezet naar \n
	return p < len(posData) &&
		(posData[p] == ' ' ||
			posData[p] == '\t' ||
			posData[p] == '\n' ||
			posData[p] == '\f' ||
			posData[p] == 160)
}

func runeSliceEqual(a, b []rune) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

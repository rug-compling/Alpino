package main

import (
	"github.com/pebbe/util"

	"bufio"
	"bytes"
	"flag"
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
)

type tokenType int

type token struct {
	t tokenType
	s string
	i int64
	f float64
}

const (
	tokenUndefined tokenType = iota // to catch errors, tokenUndefined must be first
	tokenBlockBegin
	tokenBlockEnd
	tokenLineBegin
	tokenLineEnd
	tokenLineError
	tokenFeature
	tokenFileEnd
	tokenString
	tokenInt
	tokenFloat
)

var (
	features = make(map[string]float64)

	opt_d = flag.Bool("d", false, "display continuation messages to standard output")
	opt_r = flag.Bool("r", false, "if set, only the raw number of overlap/correct/system triples per sentence is reported")
	opt_u = flag.Float64("u", 0.0, "weight of unknown features")
	opt_w = flag.String("w", "data.weights", "file containing features and their weights")
	opt_x = flag.Bool("x", false, "don't read weights from file")
)

func main() {

	var filename, inputfile string

	var bCorrect, bN, bOverlap, bPen, bSystem int64
	var cCorrect, cN, cOverlap, cPen, cSystem int64
	var counter, exact int64
	var fCorrect, fN, fOverlap, fPen, fSystem int64
	var iCorrect, iOverlap, iSystem int64
	var iCgn, iCgnTotal int64
	var lbCorrect, lbN, lbOverlap, lbPen, lbSystem int64
	var lcCorrect, lcN, lcOverlap, lcPen, lcSystem int64
	var lcCgn, lcTotalCgn, tCgn, tTotalCgn int64
	var thisN, thisPen int64

	var bAv, bAverage, bFscore, bPrecision, bRecall, bScore float64
	var cAv, cAverage, cFscore, cPrecision, cRecall, cScore float64
	var fAv, fAverage, fFscore, fPrecision, fRecall, fScore float64
	var kappa, kappaAv float64
	var lbScore, lcScore, lcWeight float64
	var thisScore, thisWeight float64

	flag.Parse()
	if flag.NArg() == 0 && !util.IsTerminal(os.Stdin) {
		inputfile = ""
	} else if flag.NArg() == 1 {
		inputfile = flag.Arg(0)
	} else {
		fmt.Fprintf(os.Stderr, "\nUsage: %s [args] [filename]\n\nargs with default values are:\n\n", os.Args[0])
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nif filename is missing, read from stdin\n\n")
		return
	}

	if !*opt_x {
		getWeights(*opt_w)
	}

	ch := make(chan token, 100)
	go lexer(inputfile, ch)

	newSentence := false
MainLoop:
	for {
		it := <-ch
		switch it.t {

		default:
			fmt.Fprintf(os.Stderr, "\nUnexpected token from lexer: %#v\n\n", it)
			os.Exit(1)

		case tokenFileEnd:
			break MainLoop

		case tokenBlockBegin:
			if *opt_d || *opt_r {
				filename = (<-ch).s
			}
			counter++
			newSentence = true

		case tokenBlockEnd:
			tCgn += lcCgn
			tTotalCgn += lcTotalCgn

			cPen += lcPen
			cN += lcN
			cAv += score(lcPen, lcN)

			bPen += lbPen
			bN += lbN
			bAv += score(lbPen, lbN)

			if score(lcPen, lcN) >= score(lbPen, lbN) {
				exact++
			}

			bOverlap += lbOverlap
			bCorrect += lbCorrect
			bSystem += lbSystem

			cOverlap += lcOverlap
			cCorrect += lcCorrect
			cSystem += lcSystem

			if *opt_d {
				cScore = score(cPen, cN)
				fmt.Fprintf(os.Stderr, "\t%s test-score\t%6.2f\t(%6.2f) (exact: %6.2f)\t lemma/pos-score\t%6.2f\t(%6.2f)\n",
					filename, lcScore, cScore, float64(exact)/float64(counter),
					100.0*float64(lcCgn)/float64(lcTotalCgn), 100.0*float64(tCgn)/float64(tTotalCgn))
			}

			if *opt_r {
				fmt.Printf("%v\t%v\t%v\n", filename, lcPen, lcN)
			}

		case tokenLineBegin:
			iOverlap = (<-ch).i
			iCorrect = (<-ch).i
			iSystem = (<-ch).i
			iCgn = (<-ch).i
			iCgnTotal = (<-ch).i

			thisWeight = (<-ch).f

			thisPen = pen(iOverlap, iCorrect, iSystem)
			thisN = max(iCorrect, iSystem)
			thisScore = score(thisPen, thisN)

			if !newSentence {

				if thisWeight > lcWeight {
					lcWeight = thisWeight
					lcPen = thisPen
					lcN = thisN
					lcScore = thisScore
					lcOverlap = iOverlap
					lcCorrect = iCorrect
					lcSystem = iSystem
					lcCgn = iCgn
					lcTotalCgn = iCgnTotal
				}
				if thisScore > lbScore {
					lbScore = thisScore
					lbPen = thisPen
					lbN = thisN
					lbOverlap = iOverlap
					lbCorrect = iCorrect
					lbSystem = iSystem
				}

			} else {

				newSentence = false

				fPen += thisPen
				fN += thisN
				fAv += thisScore
				fOverlap += iOverlap
				fCorrect += iCorrect
				fSystem += iSystem

				lcWeight = thisWeight
				lcN = thisN
				lcPen = thisPen
				lcScore = thisScore

				lcCgn = iCgn
				lcTotalCgn = iCgnTotal

				lbN = thisN
				lbPen = thisPen
				lbScore = thisScore

				lcOverlap = iOverlap
				lcCorrect = iCorrect
				lcSystem = iSystem
				lbOverlap = iOverlap
				lbCorrect = iCorrect
				lbSystem = iSystem

			}

		}

	}

	fScore = score(fPen, fN)
	bScore = score(bPen, bN)
	cScore = score(cPen, cN)
	if bScore != fScore {
		kappa = 100.0 * (cScore - fScore) / (bScore - fScore)
	} else {
		kappa = math.NaN()
	}
	cAverage = cAv / float64(counter)
	bAverage = bAv / float64(counter)
	fAverage = fAv / float64(counter)
	if bAverage != fAverage {
		kappaAv = 100.0 * (cAverage - fAverage) / (bAverage - fAverage)
	} else {
		kappaAv = math.NaN()
	}

	if !*opt_r {

		fmt.Printf("\n")
		fmt.Printf("exact %6.2f\n", float64(exact)/float64(counter))
		fmt.Printf("first-score %6.2f %6.2f\n", fScore, fAverage)
		fmt.Printf("best-score  %6.2f %6.2f\n", bScore, bAverage)
		fmt.Printf("test-score  %6.2f %6.2f\n", cScore, cAverage)
		fmt.Printf("phi-score   %6.2f %6.2f\n", kappa, kappaAv)
		fmt.Printf("first-p/m:  %v     %v\n", fPen, fN)
		fmt.Printf("best-p/m:   %v     %v\n", bPen, bN)
		fmt.Printf("test-p/m:   %v     %v\n", cPen, cN)
		fmt.Printf("first-av:   %v     %v\n", fAv, counter)
		fmt.Printf("best-av:    %v     %v\n", bAv, counter)
		fmt.Printf("test-av:    %v     %v\n", cAv, counter)

		fmt.Printf("first-overlap     %v\n", fOverlap)
		fmt.Printf("first-correct     %v\n", fCorrect)
		fmt.Printf("first-system      %v\n", fSystem)
		fmt.Printf("best-overlap     %v\n", bOverlap)
		fmt.Printf("best-correct     %v\n", bCorrect)
		fmt.Printf("best-system      %v\n", bSystem)
		fmt.Printf("test-overlap     %v\n", cOverlap)
		fmt.Printf("test-correct     %v\n", cCorrect)
		fmt.Printf("test-system      %v\n", cSystem)

		fPrecision = 100.0 * float64(fOverlap) / float64(fSystem)
		fRecall = 100.0 * float64(fOverlap) / float64(fCorrect)
		fFscore = (2.0 * fPrecision * fRecall) / (fPrecision + fRecall)
		fmt.Printf("first-precision  %6.2f\n", fPrecision)
		fmt.Printf("first-recall     %6.2f\n", fRecall)
		fmt.Printf("first-fscore     %6.2f\n", fFscore)

		bPrecision = 100.0 * float64(bOverlap) / float64(bSystem)
		bRecall = 100.0 * float64(bOverlap) / float64(bCorrect)
		bFscore = (2 * bPrecision * bRecall) / (bPrecision + bRecall)
		fmt.Printf("best-precision   %6.2f\n", bPrecision)
		fmt.Printf("best-recall      %6.2f\n", bRecall)
		fmt.Printf("best-fscore      %6.2f\n", bFscore)

		cPrecision = 100.0 * float64(cOverlap) / float64(cSystem)
		cRecall = 100.0 * float64(cOverlap) / float64(cCorrect)
		cFscore = (2.0 * cPrecision * cRecall) / (cPrecision + cRecall)
		fmt.Printf("test-precision   %6.2f\n", cPrecision)
		fmt.Printf("test-recall      %6.2f\n", cRecall)
		fmt.Printf("test-fscore      %6.2f\n", cFscore)

		fmt.Printf("lemma/pos-score  %6.2f\n", 100.0*float64(tCgn)/float64(tTotalCgn))

	}

}

func max(i, j int64) int64 {
	if i > j {
		return i
	}
	return j
}

func pen(ov, corr, sys int64) int64 {
	return max(corr, sys) - ov
}

func score(pen, n int64) float64 {
	if n != 0 {
		return 100.0 * (1.0 - float64(pen)/float64(n))
	}
	return 100.0
}

/*
output optimized lexer:

    (
        tokenBlockBegin
        [ tokenString ]          // only if opt_d or opt_r: filename
        (
            tokenLineBegin
            tokenInt
            tokenInt
            tokenInt
            tokenInt
            tokenInt
            tokenTokenFloat      // processed features
        ) *
        tokenBlockEnd
    ) *
    tokenFileEnd

output simple lexer (not used):

    (
        tokenBlockBegin
        tokenString           // filename
        (
            tokenLineBegin
            tokenInt
            tokenInt
            tokenInt
            tokenInt
            tokenInt
            (
                tokenFeature
                tokenFloat
                tokenString
            ) *
            tokenLineEnd
        ) *
        tokenBlockEnd
    ) *
    tokenFileEnd

output lexer with error detection (not used):

    (
        tokenBlockBegin
        tokenString           // filename
        (
            tokenLineBegin
            tokenInt
            tokenInt
            tokenInt
            tokenInt
            tokenInt
            (
                tokenFeature
                tokenFloat
                tokenString
            ) *
            ( tokenLineEnd | tokenLineError )
        ) *
        tokenBlockEnd
    ) *
    tokenFileEnd

*/
func lexer(filename string, ch chan<- token) {
	var fp *os.File
	if filename == "" {
		fp = os.Stdin
	} else {
		fp1, err := os.Open(filename)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		fp = fp1
		defer fp.Close()
	}
	rd := bufio.NewReaderSize(fp, 100000)

	var blockname string
	var lineno int64 = 0
	var line string
	inBlock := false
	breaks := make([]int, 0, 1000)

	for {
		bline, err := rd.ReadBytes('\n')
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		bline = bytes.TrimRight(bline, "\n\r")

		lineno++

		line = string(bline)

		breaks = breaks[0:0]
		j := 0
		var p byte = '#'
		for i, c := range bline {
			if c == p {
				breaks = append(breaks, i)
				j++
				switch {
				case j%2 == 1 && j > 6:
					p = '@'
				case j < 2 || j == 6:
					p = '#'
				default:
					p = '|'
				}
			}
		}

		if n := len(breaks); n < 7 || n%2 != 0 {
			fmt.Fprintf(os.Stderr, "Parse failed for line %v: %v\n", lineno, line)
			continue
		}

		if !inBlock || line[:breaks[0]] != blockname {
			if inBlock {
				ch <- token{t: tokenBlockEnd}
			}

			ch <- token{t: tokenBlockBegin}
			inBlock = true
			blockname = line[:breaks[0]]

			if *opt_d || *opt_r {
				ch <- token{t: tokenString, s: blockname}
			}
		}

		ch <- token{t: tokenLineBegin}

		v, _ := strconv.ParseInt(line[breaks[1]+1:breaks[2]], 10, 64)
		ch <- token{t: tokenInt, i: v}
		v, _ = strconv.ParseInt(line[breaks[2]+1:breaks[3]], 10, 64)
		ch <- token{t: tokenInt, i: v}
		v, _ = strconv.ParseInt(line[breaks[3]+1:breaks[4]], 10, 64)
		ch <- token{t: tokenInt, i: v}
		v, _ = strconv.ParseInt(line[breaks[4]+1:breaks[5]], 10, 64)
		ch <- token{t: tokenInt, i: v}
		v, _ = strconv.ParseInt(line[breaks[5]+1:breaks[6]], 10, 64)
		ch <- token{t: tokenInt, i: v}

		var w float64 = 0.0
		var fl float64
		breaks = append(breaks, len(line))
		for n, i := len(breaks)-1, 6; i < n; i += 2 {
			// ParseFloat is slow, so try ParseInt first, since most input values look like ints
			a := line[breaks[i]+1 : breaks[i+1]]
			ii, e := strconv.ParseInt(a, 10, 64)
			if e == nil {
				fl = float64(ii)
			} else {
				fl, _ = strconv.ParseFloat(a, 64)
			}
			if *opt_x {
				w += fl * *opt_u
			} else {
				ff, ok := features[line[breaks[i+1]+1:breaks[i+2]]]
				if ok {
					w += fl * ff
				} else {
					w += fl * *opt_u
				}
			}
		}
		ch <- token{t: tokenFloat, f: w}

	}

	if inBlock {
		ch <- token{t: tokenBlockEnd}
	}

	ch <- token{t: tokenFileEnd}

	close(ch)
}

func getWeights(filename string) {
	fp, err := os.Open(filename)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	defer fp.Close()
	rd := bufio.NewReaderSize(fp, 100000)

	splitBar := []byte{'|'}

	for {
		line, err := rd.ReadBytes('\n')
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		line = bytes.TrimRight(line, "\n\r")

		items := bytes.Split(line, splitBar)
		if len(items) == 2 {
			s := string(items[0])
			_, ok := features[s]
			if !ok {
				f, _ := strconv.ParseFloat(string(items[1]), 64)
				features[s] = f
			}
		}

	}

}

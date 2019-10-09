/*

   Options not implemented:

     -n = naive combined; tags of form chunk_tag*pos_tag (* is separator)
     -e = naive context; create tags of form context_label*pos_tag (* is separator)

*/

package main

import (
	"bufio"
	"github.com/pebbe/util"
	"flag"
	"fmt"
	"io"
	"math"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
)

type tokenType int

type token struct {
	t tokenType
	s string
}

const (
	tokenUndefined tokenType = iota // to catch errors, tokenUndefined must be first
	tokenSentenceBegin
	tokenSentenceEnd
	tokenMultiWordBegin
	tokenMultiWordEnd
	tokenLine
	tokenFileEnd
	tokenString

	SENTENCE_START string = "xxx_sentence_start"
	SENTENCE_END   string = "xxx_sentence_end"
	DUMMY_CONTEXT  string = "<DUMMY_CONTEXT>"
	DUMMY_WORD     string = "<DUMMY_WORD>"
	UNKNOWN_WORD   string = "<UNKNOWN_WORD>"
)

var (
	contextTrigramFreq = make(map[string]int64)
	lexicon            = make(map[string]int64)
	prefixBigramDiv    = make(map[string]int64)
	prefixBigramFreq   = make(map[string]int64)
	prefixTrigramDiv   = make(map[string]int64)
	prefixTrigramFreq  = make(map[string]int64)
	prefixUnigramFreq  = make(map[string]int64)
	tagBigramFreq      = make(map[string]int64)
	tagFourgramFreq    = make(map[string]int64)
	tagTrigramFreq     = make(map[string]int64)
	tagUnigramFreq     = make(map[string]int64)
	usedContext        = make(map[string]int64)
	wordFreq           = make(map[string]int64)
	wordTagFreq        = make(map[string]int64)

	THRESHOLD int64

	opt_c = flag.Int64("c", 0, "use extra context (0 means using dummy everywhere)")
	opt_d = flag.String("d", ".", "directory to write files to")
	opt_f = flag.Int64("f", 0, "minimal frequency - tuple is otherwise ignored")
	opt_g = flag.Int64("g", 0, "maximal -log score - tuple is otherwise ignored")
	opt_s = flag.Bool("s", false, "print statistics: memory and time usage")
)

func stats() {
	if  *opt_s {
		statCmd := exec.Command("ps", "-o", "%mem,etime", fmt.Sprintf("%d", os.Getpid()))
		s, e := statCmd.CombinedOutput()
		if e != nil {
			fmt.Println(e)
		} else {
			fmt.Print(string(s))
		}
	}
}

func main() {

	var inputfile string

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

	ch := make(chan token, 100)
	go lexer(inputfile, ch)

	var baseTag, contextTrigram, firstTag, fourgram string
	var multiTagFirstWord, multiTagRestWords, newContext, oldContext string
	var secondTag, thirdTag, trigram, word, wordTag string
	var inMulti bool
	var inMultiCount int

	re, _ := regexp.Compile("^[0-9][-/0-9]*")

MainLoop:
	for {
		it := <-ch
		switch it.t {

		default:
			fmt.Fprintf(os.Stderr, "\nUnexpected token from lexer: %#v\n\n", it)
			os.Exit(1)

		case tokenFileEnd:
			break MainLoop

		case tokenSentenceBegin:
			// replace tags by default start-of-sentence tags
			firstTag = SENTENCE_START
			secondTag = SENTENCE_START
			oldContext = DUMMY_CONTEXT

			inMulti = false

		case tokenSentenceEnd:

			// take care of ending of previous sentence (with sentenceEnd for third tag)
			fourgram = firstTag + "|" + secondTag + "|" + oldContext + "|" + SENTENCE_END

			tagFourgramFreq[fourgram]++

			// store context n-gram
			contextTrigram = oldContext + "|" + SENTENCE_END + "|" + DUMMY_CONTEXT
			contextTrigramFreq[contextTrigram]++

		case tokenMultiWordBegin:
			inMulti = true
			inMultiCount = 0
			multiTagRestWords = ""

		case tokenMultiWordEnd:
			addToLexicon(&multiTagFirstWord, &baseTag, &multiTagRestWords)
			inMulti = false

		case tokenLine:
			word = (<-ch).s
			thirdTag = (<-ch).s
			newContext = (<-ch).s
			if inMulti {
				inMultiCount++
				switch inMultiCount {
				case 1:
					baseTag = re.ReplaceAllString(thirdTag, "")
					multiTagFirstWord = word
				case 2:
					multiTagRestWords = word
				default:
					multiTagRestWords = multiTagRestWords + "," + word
				}
			} else {
				addToLexicon(&word, &thirdTag, nil)
			}

			// not using context implemented by using dummy everywhere
			if *opt_c == 0 {
				newContext = DUMMY_CONTEXT
			}

			fourgram = firstTag + "|" + secondTag + "|" + oldContext + "|" + thirdTag

			// store tag n-grams
			tagFourgramFreq[fourgram]++

			// store context n-gram
			contextTrigram = oldContext + "|" + thirdTag + "|" + newContext
			contextTrigramFreq[contextTrigram]++

			// keep track of encountered contexts
			usedContext[newContext] = 1

			// store word-tag combination
			wordTag = word + "|" + thirdTag
			wordTagFreq[wordTag]++
			wordFreq[word]++

			// set previous to current
			firstTag = secondTag
			secondTag = thirdTag
			oldContext = newContext

		}

	}

	////////////////

	for fourgram, count := range tagFourgramFreq {
		i := strings.Index(fourgram, "|")
		trigram := fourgram[i+1:]

		// store tag n-grams
		tagTrigramFreq[trigram] += count

		li := strings.LastIndex(fourgram, "|")
		prefixTrigram := fourgram[:li]

		// diversity
		prefixTrigramDiv[prefixTrigram]++

		// store prefix n-grams
		prefixTrigramFreq[prefixTrigram] += count
	}

	for trigram, count := range tagTrigramFreq {
		i := strings.Index(trigram, "|")
		bigram := trigram[i+1:]

		li := strings.LastIndex(trigram, "|")
		prefixBigram := trigram[:li]

		// diversity
		prefixBigramDiv[prefixBigram]++

		// store tag n-grams
		tagBigramFreq[bigram] += count
	}

	for bigram, count := range tagBigramFreq {
		i := strings.Index(bigram, "|")
		unigram := bigram[i+1:]

		// store tag n-grams
		tagUnigramFreq[unigram] += count
	}

	for prefixTrigram, count := range prefixTrigramFreq {
		i := strings.Index(prefixTrigram, "|")
		prefixBigram := prefixTrigram[i+1:]

		// store prefix n-grams
		prefixBigramFreq[prefixBigram] += count
	}

	for prefixBigram, count := range prefixBigramFreq {
		i := strings.Index(prefixBigram, "|")
		prefixUnigram := prefixBigram[i+1:]

		// store prefix n-grams
		prefixUnigramFreq[prefixUnigram] += count
	}

	////////////////

	// open files
	TAG_4_FREQ := openWrite("/tag4")
	TAG_3_FREQ := openWrite("/tag3")
	TAG_2_FREQ := openWrite("/tag2")
	PRE_3_FDIV := openWrite("/prefix3")
	PRE_2_FDIV := openWrite("/prefix2")
	CONTEXT_3_FREQ := openWrite("/context3")
	WORD_TAG_FREQ := openWrite("/wordTag")
	TAG_WORD_FREQ := openWrite("/tagWord")
	USED_CONTEXT := openWrite("/usedContext")
	WORD_TAG_LEX := openWrite("/wordTagLex")

	THRESHOLD := *opt_f

	// 4-gram tag context data
	for fourgram, frequency := range tagFourgramFreq {
		trigram = fourgram[:strings.LastIndex(fourgram, "|")]
		if frequency > THRESHOLD {
			probability := float64(frequency) / float64(prefixTrigramFreq[trigram])
			probabilityInt := int64(-100.0 * math.Log(probability))
			if *opt_g != 0 && probabilityInt < *opt_g {
				fmt.Fprintf(TAG_4_FREQ, "%v|%v\n", fourgram, probabilityInt)
			}
		}
	}

	// 3-gram tag context data
	for trigram, frequency := range tagTrigramFreq {
		bigram := trigram[:strings.LastIndex(trigram, "|")]
		if frequency > THRESHOLD {
			probability := float64(frequency) / float64(prefixBigramFreq[bigram])
			probabilityInt := int64(-100.0 * math.Log(probability))
			if *opt_g != 0 && probabilityInt < *opt_g {
				fmt.Fprintf(TAG_3_FREQ, "%v|%v\n", trigram, probabilityInt)
			}
		}
	}

	// 2-gram tag context data
	for bigram, frequency := range tagBigramFreq {
		unigram := bigram[:strings.LastIndex(bigram, "|")]
		if frequency > THRESHOLD {
			probability := float64(frequency) / float64(prefixUnigramFreq[unigram])
			probabilityInt := int64(-100.0 * math.Log(probability))
			if *opt_g != 0 && probabilityInt < *opt_g {
				fmt.Fprintf(TAG_2_FREQ, "%v|%v\n", bigram, probabilityInt)
			}
		}
	}

	// 3-gram tag context prefix data
	for trigram, frequency := range prefixTrigramFreq {
		if frequency > THRESHOLD {
			diversity := prefixTrigramDiv[trigram]
			fmt.Fprintf(PRE_3_FDIV, "%v|%v|%v\n", trigram, frequency, diversity)
		}
	}

	// 2-gram tag context prefix data
	for bigram, frequency := range prefixBigramFreq {
		if frequency > THRESHOLD {
			diversity := prefixBigramDiv[bigram]
			fmt.Fprintf(PRE_2_FDIV, "%v|%v|%v\n", bigram, frequency, diversity)
		}
	}

	// 3-gram context tag data
	for trigram, frequency := range contextTrigramFreq {
		if frequency > THRESHOLD {
			bigram := trigram[:strings.LastIndex(trigram, "|")]
			probability := float64(frequency) / float64(tagBigramFreq[bigram])
			probabilityInt := int64(-100.0 * math.Log(probability))
			if *opt_g != 0 && probabilityInt < *opt_g {
				fmt.Fprintf(CONTEXT_3_FREQ, "%v|%v\n", trigram, probabilityInt)
			}
		}
	}

	// word-tags lexicon
	for pair, frequency := range lexicon {
		// lexicon (word is printed twice as this format is required by fadd morphology handling)
		if frequency > THRESHOLD {
			a := strings.Split(pair, "|")
			word := a[0]
			tag := a[1]
			fmt.Fprintf(WORD_TAG_LEX, "%v\t%v\t%v\n", word, word, tag)
		}
	}

	// word-tag data, tag-word data
	unknown := make(map[string]int64)
	for pair, frequency := range wordTagFreq {
		a := strings.Split(pair, "|")
		word := a[0]
		tag := a[1]
		wordfreq := wordFreq[word]
		// word-tag data (probability of seeing word given tag)
		if wordfreq > THRESHOLD {
			probability := int64(-100.0 * math.Log(float64(frequency)/float64(tagUnigramFreq[tag])))
			if *opt_g != 0 && probability < *opt_g {
				fmt.Fprintf(WORD_TAG_FREQ, "%v|%v\n", pair, probability)
			}
		}
		// tag-word data (probability of seeing tag given word; used for baseline)
		if frequency > THRESHOLD {
			probability := int64(-100.0 * math.Log(float64(frequency)/float64(wordfreq)))
			reversedPair := tag + "|" + word
			if *opt_g != 0 && probability < *opt_g {
				fmt.Fprintf(TAG_WORD_FREQ, "%v|%v\n", reversedPair, probability)
			}
		}
		// collect data for unknown word; collect tags for all words that occur only once
		//if($UNKNOWN_WORD_TAGPERCENTAGE>0){
		if wordfreq == 1 {
			unknown[tag]++
		}
		//}
	}
	// print tag that was most often assigned to words that occur only once
	mostval := int64(0)
	mosttag := ""
	for s, t := range unknown {
		if t > mostval {
			mostval = t
			mosttag = s
		}
	}
	if mostval > 0 {
		fmt.Fprintf(WORD_TAG_LEX, "%v\t%v\t%v\n", UNKNOWN_WORD, UNKNOWN_WORD, mosttag)
	}
	fmt.Fprintf(WORD_TAG_FREQ, "%v|%v|0\n", DUMMY_WORD, SENTENCE_END)
	fmt.Fprintf(WORD_TAG_FREQ, "%v|%v|0\n", DUMMY_WORD, SENTENCE_START)

	// used context labels
	for label, _ := range usedContext {
		fmt.Fprintln(USED_CONTEXT, label)
	}

	TAG_4_FREQ.Close()
	TAG_3_FREQ.Close()
	TAG_2_FREQ.Close()
	PRE_3_FDIV.Close()
	PRE_2_FDIV.Close()
	CONTEXT_3_FREQ.Close()
	WORD_TAG_FREQ.Close()
	TAG_WORD_FREQ.Close()
	USED_CONTEXT.Close()
	WORD_TAG_LEX.Close()

	stats()
}

func openWrite(filename string) (file *os.File) {
	file, e := os.Create(*opt_d + filename)
	if e != nil {
		fmt.Fprintf(os.Stderr, "Creating file %v: %v\n", *opt_d+filename, e)
		os.Exit(1)
	}
	return
}

func addToLexicon(word, tag, words *string) {
	var w string
	if words == nil {
		w = *word + "|" + *tag
	} else {
		w = *word + "|" + *tag + "^" + *words
	}
	_, ok := lexicon[w]
	if ok {
		lexicon[w]++
	} else {
		lexicon[w] = 1
	}
}

/*
lexer:

    (
        tokenSentenceBegin
        (   tokenLine data
          |
            tokenMultiWordBegin
            ( tokenLine data ) +
            tokenMultiWordEnd
        ) +
        tokenSentenceEnd
    ) +
    tokenFileEnd

data :=

     tokenString  // word
     tokenString  // thirdTag
     tokenString  // newContext
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
	rd := bufio.NewReaderSize(fp, 10000)

	var word, thirdTag, newContext string
	var sentenceKey, sentenceKeyPrev string
	var number, numberPrev int64

	var lineno int64 = 0
	var line string
	inSentence := false
	inMulti := false
	breaks := make([]int, 0, 100)

	re, _ := regexp.Compile("^([0-9]+)[-/]")

	for {
		bline, isP, err := rd.ReadLine()
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		if isP {
			fmt.Fprintln(os.Stderr, "Line too long")
			os.Exit(1)
		}
		lineno++

		if lineno%1000000 == 0 {
			fmt.Fprintf(os.Stderr, "read %d million lines\n", lineno/1000000)
			stats()
		}

		line = string(bline)

		breaks = breaks[0:0]
		for i, c := range bline {
			if c == '|' {
				breaks = append(breaks, i)
			}
		}
		if len(breaks) != 6 {
			fmt.Fprintf(os.Stderr, "Parse failed for line %v: %v\n", lineno, line)
			continue
		}
		// word|thirdTag|sentenceKey|undef|undef|undef|newContext
		//     0        1           2     3     4     5
		word = line[:breaks[0]]
		thirdTag = line[breaks[0]+1 : breaks[1]]
		sentenceKey = line[breaks[1]+1 : breaks[2]]
		newContext = line[breaks[5]+1:]

		if sentenceKey != sentenceKeyPrev {
			if inMulti {
				ch <- token{t: tokenMultiWordEnd}
				inMulti = false
			}
			if inSentence {
				ch <- token{t: tokenSentenceEnd}
			}
			ch <- token{t: tokenSentenceBegin}
			inSentence = true
			sentenceKeyPrev = sentenceKey
		}

		// multiwords...
		if re.MatchString(thirdTag) {

			s := re.FindStringSubmatch(thirdTag)
			number, _ = strconv.ParseInt(s[1], 10, 64)

			if inMulti {
				if number != numberPrev+1 {
					ch <- token{t: tokenMultiWordEnd}
					inMulti = false
				}
			}

			if !inMulti {
				if number != 1 {
					fmt.Fprintf(os.Stderr, "Multiword tag error: %s : %s\n", word, thirdTag)
					continue
				}
				ch <- token{t: tokenMultiWordBegin}
				inMulti = true
			}

			numberPrev = number

		} else {
			if inMulti {
				ch <- token{t: tokenMultiWordEnd}
				inMulti = false
			}

		}

		ch <- token{t: tokenLine}
		ch <- token{t: tokenString, s: word}
		ch <- token{t: tokenString, s: thirdTag}
		ch <- token{t: tokenString, s: newContext}

	}

	fmt.Fprintf(os.Stderr, "read all %d lines\n", lineno)

	if inMulti {
		ch <- token{t: tokenMultiWordEnd}
	}

	if inSentence {
		ch <- token{t: tokenSentenceEnd}
	}

	ch <- token{t: tokenFileEnd}

	close(ch)
}

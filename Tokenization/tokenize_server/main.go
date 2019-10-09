package main

// #include <stdlib.h>
// #include "../tok.h"
// #include "wrap.h"
// #cgo LDFLAGS: -L.. -ltok_breaks
import "C"

import (
	"errors"
	"fmt"
	"html"
	"io/ioutil"
	"log"
	"net/http"
	"regexp"
	"unsafe"
)

// Convert a Go string to an array of wchar_t.
func stringToWChar(s string) (unsafe.Pointer, int) {
	r := []rune(s)
	l := len(r) + 1
	w := C.alloc_wchar(C.size_t(l))

	// Copy runes. XXX - This is not safe on Windows, where wchar_t is 16 bit (UTF-16).
	for idx, val := range r {
		C.set_wchar((*C.wchar_t)(w), C.size_t(idx), C.wchar_t(val))
	}

	// The C string should be null-terminated.
	C.set_wchar((*C.wchar_t)(w), C.size_t(l - 1), 0)

	return unsafe.Pointer(w), l
}

func wCharToString(s unsafe.Pointer) string {
	tokenizedRunes := make([]rune, C.wcslen((*C.wchar_t)(s)))
	for idx, _ := range tokenizedRunes {
		tokenizedRunes[idx] = rune(C.get_wchar((*C.wchar_t)(s), (C.size_t)(idx)))
	}

	return string(tokenizedRunes)
}

// Tokenize a text. Returns sentence per line, tokens separated by spaces.
func tokenize(text string) (string, error) {
	in, l := stringToWChar(text)
	defer C.free(in)

	var out *C.wchar_t = C.alloc_wchar(C.size_t(0))
	defer func() { C.free(unsafe.Pointer(out)) }() // Free the final memory address.

	// We don't know beforehand how large the transduction will be. So, we start with
	// an output array the size of the original and continue duplicating the array until
	// the transduction can be written away. This is the same strategy as used in the
	// Prolog module.
	result := 2
	for result == 2 {
		l *= 2

		// Increase output array.
		out = C.realloc_wchar(out, C.size_t(l))
		if out == nil {
			return "", errors.New("Out of memory, cannot allocate buffer for tokenization output")
		}

		// Apply the transducer.
		result = int(C.t_accepts((*C.wchar_t)(in), (*C.wchar_t)(out), C.int(l)))

		if result == 0 {
			return "", errors.New("Tokenization fails.")
		}
	}

	return wCharToString(unsafe.Pointer(out)), nil
}

func addEnumerationMarkers(s string) string {
	firstExpr, err := regexp.Compile("(\\s?1)[.](\\s.*?\\W2[.])")

	if err != nil {
		return s
	}

	if firstExpr.MatchString(s) {
		s = firstExpr.ReplaceAllString(s, "$1#$2")

		prev := 1
		next := 2
		after := 3

		for {
			sOrig := s
			nextExpr, err := regexp.Compile(fmt.Sprintf("(%d#\\s.*?\\W%d)[.](\\s)", prev, next))

			if err != nil {
				return sOrig
			}

			if !nextExpr.MatchString(s) {
				break
			}

			s = nextExpr.ReplaceAllString(s, "$1#$2")

			prev++
			next++
			after++
		}
	}

	return s
}

func removeEnumerationMarkers(s string) string {
	enumExpr, err := regexp.Compile("([0-9]+)#(\\s)")
	if err != nil {
		// Be robust.
		return s
	}

	return enumExpr.ReplaceAllString(s, "$1.$2")
}

func preProcess(s string) string {
	return addEnumerationMarkers(html.UnescapeString(s))
}

func postProcess(s string) string {
	s = removeEnumerationMarkers(s)

	// ' tuut'-vorm    --> 'tuut'-vorm
	quoteExpr := regexp.MustCompile("' ([[:lower:][:upper:]]+'-)")
	s = quoteExpr.ReplaceAllString(s, "'$1")

	// ( buiten)gewoon --> (buiten)gewoon
	parenExpr := regexp.MustCompile("[(] ([[:lower:][:upper:]]+[)])")
	s = parenExpr.ReplaceAllString(s, "($1")

	// AMSTERDAM - De ...
	placeMarkerExpr := regexp.MustCompile("(?:^|\n)([[:upper:]]{2}[[:upper:]() /,0-9.-]* --*) ")
	s = placeMarkerExpr.ReplaceAllString(s, "$1\n")

	return s
}

func tokenizeHandler(w http.ResponseWriter, r *http.Request) {
	log.Printf("Received a request from: %s", r.Host)

	// Read POST body.
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		log.Print(err)
		http.Error(w, err.Error(), 500)
		return
	}

	text := preProcess(string(body))

	// Tokenize...
	tokens, err := tokenize(text)
	if err != nil {
		log.Print(err)
		http.Error(w, err.Error(), 500)
		return
	}

	tokens = postProcess(tokens)

	fmt.Fprint(w, tokens)
}

func main() {
	http.HandleFunc("/tokenize", tokenizeHandler)
	log.Fatal(http.ListenAndServe(":42425", nil))
}

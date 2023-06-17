package main

import (
	"github.com/pebbe/util"
	"github.com/rug-compling/alpinods"
	"github.com/rug-compling/alud/v2"

	"encoding/xml"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

var (
	sources   = map[string][2]string{}
	sourcesLL = map[string][2]string{
		// type:          source,     description
		"WR-P-E-A":     {"SONAR500", "discussion lists"},
		"WR-P-E-C":     {"SONAR500", "e-magazines"},
		"WR-P-E-E":     {"SONAR500", "newsletters"},
		"WR-P-E-F":     {"SONAR500", "press releases"},
		"WR-P-E-G":     {"SONAR500", "subtitles"},
		"WR-P-E-H":     {"SONAR500", "teletext pages"},
		"WR-P-E-I":     {"SONAR500", "web sites"},
		"WR-P-E-J":     {"SONAR500", "wikipedia"},
		"WR-P-E-K":     {"SONAR500", "blogs"},
		"WR-P-E-L":     {"SONAR NEW MEDIA", "tweets"},
		"WR-P-P-B":     {"SONAR500", "books"},
		"WR-P-P-C":     {"SONAR500", "brochures"},
		"WR-P-P-D":     {"SONAR500", "newsletters"},
		"WR-P-P-E":     {"SONAR500", "guides and manuals"},
		"WR-P-P-F":     {"SONAR500", "legal texts"},
		"WR-P-P-G":     {"SONAR500", "newspapers"},
		"WR-P-P-H":     {"SONAR500", "periodicals and magazines"},
		"WR-P-P-I":     {"SONAR500", "policy documents"},
		"WR-P-P-J":     {"SONAR500", "proceedings"},
		"WR-P-P-K":     {"SONAR500", "reports"},
		"WR-U-E-A":     {"SONAR NEW MEDIA", "chats"},
		"WR-U-E-D":     {"SONAR NEW MEDIA", "sms"},
		"WR-U-E-E":     {"SONAR500", "written assignments"},
		"WS-U-E-A":     {"SONAR500", "auto cues"},
		"WS-U-T-B":     {"SONAR500", "texts for the visually impaired"},
		"cdb":          {"EINDHOVEN", "dagbladen"},
		"gbl":          {"EINDHOVEN", "gezinsbladen"},
		"obl":          {"EINDHOVEN", "opiniebladen"},
		"pwe":          {"EINDHOVEN", "populair-wetenschappelijke boeken"},
		"rno":          {"EINDHOVEN", "romans en novellen"},
		"humandocs":    {"EMEA", ""},
		"pdfs_general": {"EMEA", ""},
		"pdfs_human":   {"EMEA", ""},
		"pdfs_vet":     {"EMEA", ""},
		"vetdocs":      {"EMEA", ""},
		"ep":           {"EUROPARL", ""},
		"wik":          {"NLWIKI20110804", ""},
		"senseval":     {"SENSEVAL", ""},
		"troonrede":    {"TROONREDE", ""},
	}
	sourcesLLX = map[string][2]string{
		// type:              source, description
		"Books":             {"Books", ""},
		"ch":                {"CHILDES", ""},
		"ad":                {"CLEF", ""},
		"nh":                {"CLEF", ""},
		"DGT":               {"DGT", ""},
		"wyt":               {"DutchWebCorpus", ""},
		"reve":              {"GELOOFDERKAMERADEN", ""},
		"GlobalVoices":      {"GlobalVoices", ""},
		"JRC-Acquis":        {"JRC-Acquis", ""},
		"News-Commentary11": {"News-Commentary11", ""},
		"OpenSubtitles2018": {"OpenSubtitles2018", ""},
		"ParaCrawl":         {"ParaCrawl", ""},
		"TED2013":           {"TED2013", ""},
		"Tatoeba":           {"Tatoeba", ""},
		"ep":                {"europarl7", ""},
		"giga-art":          {"gigacorpus.nl", ""},
		"giga-boo":          {"gigacorpus.nl", ""},
		"wiki":              {"wiki2017", ""},
	}

	x = util.CheckErr
)

func main() {
	if len(os.Args) < 2 || len(os.Args) > 3 {
		fmt.Printf(`
Syntax: %s directory [opties]

Voorbeeld: %s /path/naar/WR-P-P-G LL,UD

Het laatste deel van het path wordt gebruikt om het corpus te herkennen

Opties, gescheiden door komma, geen spatie:

    LL  = Metadata voor LassyLarge invoegen
    LLX = Metadata voor LassyLargeExtra invoegen
    UD  = Universal Dependencies invoegen

Je kunt maar één set Metadata kiezen

`, os.Args[0], os.Args[0])
		return
	}

	dirname := os.Args[1]
	suite := filepath.Base(dirname)
	ud := false

	if len(os.Args) > 2 {
		for _, opt := range strings.Split(os.Args[2], ",") {
			switch strings.ToLower(opt) {
			case "ll":
				sources = sourcesLL
			case "llx":
				sources = sourcesLLX
			case "ud":
				ud = true
			}
		}
	}

	meta := make(map[string]string)
	for key, value := range sources {
		if strings.HasPrefix(suite, key) {
			meta["source"] = value[0]
			meta["type"] = key
			meta["description"] = value[1]
			break
		}
	}

	files, err := os.ReadDir(dirname)
	x(err)
	for _, file := range files {
		filename := filepath.Join(dirname, file.Name())
		if !strings.HasSuffix(filename, ".xml") {
			continue
		}

		data, err := os.ReadFile(filename)
		x(err)

		var alpino alpinods.AlpinoDS
		x(xml.Unmarshal(data, &alpino))

		// voeg toe: extra attributen: is_np, is_vorfeld, is_nachfeld
		// dit past ook dtd-versie aan
		alpino.Enhance(alpinods.Fall)

		// voeg toe: metadata
		if len(meta) > 0 {
			if alpino.Metadata == nil {
				alpino.Metadata = &alpinods.Metadata{}
			}
			if alpino.Metadata.Meta == nil {
				alpino.Metadata.Meta = make([]alpinods.Meta, 0)
			}
			for key, value := range meta {
				if value != "" {
					alpino.Metadata.Meta = append(alpino.Metadata.Meta, alpinods.Meta{
						Type:  "text",
						Name:  key,
						Value: value,
					})
				}
			}
		}

		// voeg toe: universal dependencies
		if ud {
			data, err = xml.Marshal(alpino)
			x(err)
			s, err := alud.UdAlpino(data, file.Name(), alpino.Sentence.SentID)
			if err != nil {
				fmt.Println(file.Name(), err)
			}
			if s != "" {
				data = []byte(s)
			}
		} else {
			data = []byte(alpino.String())
		}

		fp, err := os.Create(filename)
		x(err)
		_, err = fp.Write(data)
		x(err)
		x(fp.Close())
	}
}

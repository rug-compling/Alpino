# Afhankelijkheden van alud en alpinods

## Alpino

`alpino_ds.dtd` :

- kopie van https://raw.githubusercontent.com/rug-compling/alpinods/refs/heads/master/alpino_ds.dtd

`TreebankTools/enhance/go.mod` :

- wordt automatisch bijgewerkt door `make` in `TreebankTools/enhance`


## TrEd

In http://www.let.rug.nl/vannoord/alp/Alpino/tred/ na upgrade van dtd

`alpino_full/package.xml` :

- `<version>` ophogen

`alpino_full.zip` :

- `<version>` in `package.xml` ophogen
- nieuwe attributen toevoegen in `resources/alpino2pml.xsl` en `resources/alpino_schema.xml`


## Alpino Demo

Twee manieren om bij te werken naar laatste versie van alud:

    make -C /net/homepages/vannoord/www/bin/cmd/conllu_svg upgrade

    curl -L https://urd2.let.rug.nl/~vannoord/bin/make


## Alpino in Docker

`alpino-in-docker/build/macros.txt`

Zie tekst `## DEP:ALPINODS`

Gewoonlijk:

    make step0


## Overigen

Afhankelijkheden zoeken in Go-programma's buiten Alpino:

    rg 'alud|alpinods' -g go.mod

Upgrade van sources met alud:

    go get -u github.com/rug-compling/alud/v2
    go mod tidy

Upgrade van sources met alpinods, maar zonder alud:

    go get -u github.com/rug-compling/alpinods
    go mod tidy



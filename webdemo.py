#!/usr/bin/env python3

import io
import socket
import sys
import tempfile
from urllib import parse

#sys.stdin.reconfigure(encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.detach(), encoding='utf-8', newline=None, line_buffering=True)
form = {}
for line in sys.stdin:
    for key, val in parse.parse_qsl(line,
                                    strict_parsing=False,
                                    encoding='utf-8',
                                    errors='replace',
                                    max_num_fields=None):
        if key not in form:
            form[key] = []
        form[key].append(val)

words = form.get('words', '')

examples = [
"Leden van de Staten-Generaal,",
"De huidige tijd vraagt om vastberadenheid en bereidheid tot verandering.",
"De wereldwijde financiële en economische crisis heeft ook Nederland hard geraakt.",
"De snelheid waarmee de gebeurtenissen zich in het achter ons liggende jaar hebben voltrokken, was uitzonderlijk.",
"Mensen verliezen hun baan, het aantal faillissementen neemt snel toe, jarenlang opgebouwde vermogens slinken en de overheidsfinanciën vertonen onvermijdelijk grote tekorten",
"De gevolgen zullen nog lang gevoeld worden.",
"Bovendien hebben ontsporingen in de financiële sector het vertrouwen in instituties en hun bestuurders aangetast.",
"Door dit alles groeit bij velen de onzekerheid over de toekomst.",
"Het is de ambitie van de regering onzekerheden om te buigen naar herstel.",
"De noodzakelijke veranderingen bieden perspectief op een economisch en sociaal krachtig Nederland.",
"Wij hebben elkaar en ons land veel te bieden door in saamhorigheid vast te houden aan de traditie van vrijheid, verantwoordelijk burgerschap en een actieve Europese en internationale opstelling.",
"In het najaar van 2008 heeft de regering met kracht ingegrepen in de financiële sector teneinde spaartegoeden van burgers en financiering van bedrijven veilig te stellen en instorten van de economie te voorkomen.",
"Drie miljard euro wordt geïnvesteerd in nieuwbouw en onderhoud van scholen, ziekenhuizen, woningen en infrastructuur en in energiebesparing.",
"Hiermee wil de regering de economie stimuleren.",
"Eveneens wordt drie miljard euro uitgetrokken voor arbeidsmarkt en bedrijfsleven en voor onderwijs en kennis.",
"Bedrijven worden gesteund met ruimere kredietfaciliteiten en met regelingen voor deeltijdwerkloosheid.",
"Daardoor zullen meer werknemers hun baan behouden.",
"Tussen 2008 en 2011 wordt ongeveer acht miljard euro extra uitgegeven aan uitkeringen voor werkloosheid en bijstand.",
"De begroting voor 2010 besteedt bijzondere aandacht aan de bestrijding van de jeugdwerkloosheid.",
"Ook dient de regering vandaag een voorstel in voor een Crisis- en herstelwet, gericht op versnelling van procedures voor infrastructurele projecten.",
"Dit biedt een basis voor meer dynamiek in de economie en daarmee voor meer werkgelegenheid.",
"In de jaren dat de Nederlandse economie krimpt, worden de overheidsuitgaven niet verlaagd.",
"Al deze en andere maatregelen vangen de gevolgen van de recessie op korte termijn zo veel mogelijk op.",
"Voor de jaren daarna bevat deze begroting voorstellen die de basis bieden voor herstel van de overheidsfinanciën.",
"De staatsschuld is fors gestegen en zal niet vanzelf weer verminderen.",
"Het overschot van één procent van het bruto binnenlands product op de begroting van vorig jaar is omgeslagen in een tekort van meer dan zes procent in 2010.",
"De recessie leidt tot aanzienlijk lagere belastingafdrachten door burgers en bedrijven.",
"Zelfs bij een gemiddelde economische groei van twee procent zal de staatsschuld blijven toenemen met ongeveer vijfendertig miljard euro per jaar.",
"Hoewel de economie volgend jaar weer voorzichtig lijkt te verbeteren, blijven de opgaven waar we voor staan aanzienlijk.",
"Als de welvaart achteruitgaat en de staatsschuld stijgt, wordt het bovendien moeilijker om de kosten te dragen van een vergrijzende bevolking en van de noodzakelijke overgang naar een economie waarin voluit recht wordt gedaan aan de eisen die een goed klimaat- en milieubeleid ons stelt.",
"Bij ongewijzigd beleid zullen ernstige en onwenselijke gevolgen optreden voor de hoogte van belastingen en sociale premies, voor de werkgelegenheid en voor de betaalbaarheid van voorzieningen als zorg, onderwijs en pensioenen.",
"De regering acht het niet verantwoord deze rekeningen door te schuiven naar de jonge en toekomstige generaties.",
"Jongeren dreigen nu geen werk te vinden, straks tijdens hun werkzame leven ook de lasten van een vergrijsde bevolking te moeten dragen, en daarna niet meer te kunnen rekenen op goede collectieve voorzieningen.",
"Dit mogen wij niet laten gebeuren!",
"In dit licht heeft de regering al eerder haar voornemens ontvouwd om, rekening houdend met zware beroepen, de AOW-leeftijd te verhogen naar 67 jaar, de kosten van de zorg te beheersen en eigenaren van huizen met een waarde boven 1 miljoen euro zwaarder te belasten.",
"Voor het eind van dit jaar zal een voorstel van een Wet tekortreductie Rijk en medeoverheden bij u worden ingediend, waarvan de regering hoopt dat deze op 1 januari 2011 in werking kan treden.",
"De wet verplicht het saldo tussen uitgaven en inkomsten jaarlijks te verbeteren.",
"De regering zal het komende halfjaar fundamentele heroverwegingen voorbereiden op een twintigtal brede terreinen in de collectieve sector.",
"De Voorjaarsnota 2010 biedt de eerste gelegenheid om tot maatregelen te komen.",
"Om bij te dragen aan het noodzakelijke herstel van de overheidsfinanciën zal onderzocht worden waar met twintig procent besparing maatschappelijke doelen kunnen worden gerealiseerd.",
"Dit moet zicht geven op financieel verantwoorde mogelijkheden om publieke diensten voor burgers effectiever uit te voeren, beleid beter af te stemmen op problemen in de samenleving en verantwoordelijkheden tussen overheden en burgers anders en beter vorm te geven.",
"De heroverwegingen moeten er ook toe leiden dat onderwijs, kennis, innovatie en ondernemerschap doelgerichter worden ingezet om economische groei te bevorderen.",
"Daarnaast zal duidelijk gemaakt worden hoe belangrijke sectoren van het Nederlandse bedrijfsleven, zoals waterbeheer en energie, landbouw en visserij, klimaat en milieu, daaraan kunnen bijdragen.",
"De heroverwegingen hebben tot doel fundamentele keuzes te maken met het oog op een economisch en sociaal krachtig Nederland.",
"Het matigen van de loonontwikkeling draagt bij aan meer werkgelegenheid en aan een eerlijke verdeling van de lasten van de economische recessie tussen werkenden en niet-werkenden, tussen de collectieve sector en de marktsector, tussen hogere en lagere inkomens en tussen jong en oud.",
"De regering roept sociale partners op tot een verantwoorde loonontwikkeling.",
"Indien dit niet gebeurt, zal de regering haar eigen verantwoordelijkheid nemen.",
"De economische recessie heeft ook morele tekortkomingen blootgelegd in het functioneren van markt en maatschappij.",
"De regering heeft gebreken in de financiële sector binnen en buiten Nederland benoemd en vertaald naar voorstellen voor striktere normering en beter toezicht.",
"Bindende afspraken worden gemaakt over begrenzing van te hoge beloningen en bonussen.",
"In deze moeilijke tijd acht de regering het van belang te blijven werken aan een samenleving waarin mensen zich met elkaar verbonden weten, in vrijheid elkaar respecteren en samen verantwoordelijkheid dragen.",
"Goede opvoeding en goed onderwijs liggen ten grondslag aan verantwoordelijk burgerschap.",
"De afgelopen twee jaar heeft de regering maatregelen genomen ter bevordering van sociale samenhang, veiligheid, stabiliteit en respect.",
"Een vasthoudende aanpak over een reeks van jaren is nodig om tot resultaten te komen.",
"Daarom zal de regering bijzondere aandacht blijven schenken aan jeugd en jongeren, aan inburgering en aan kwetsbare wijken in grote steden.",
"Gebrek aan integratie van sommige groepen in de samenleving, onfatsoenlijk en respectloos handelen van velen in de openbare ruimte en crimineel gedrag van groepen jongeren blijken hardnekkig en veroorzaken veel maatschappelijk ongenoegen.",
"De regering treedt daarom niet alleen consequent op tegen plegers van delicten maar pakt ook oorzaken van problematisch gedrag aan.",
"De samenwerking van justitie, politie, gemeenten, reclassering en jeugdzorg is daarvoor essentieel.",
"De regering zal de maatschappelijke weerbaarheid in ons land bevorderen door meer ruimte te geven aan burgers en organisaties en goed met hen samen te werken.",
"Ook met medeoverheden en de publieke sector is goede samenwerking geboden.",
"Vertrouwen in maatschappelijke organisaties, democratie en rechtsstaat is daarbij van onmisbaar belang.",
"Een economisch en sociaal krachtig Nederland vereist samenwerking in Europa en een internationale oriëntatie.",
"Nederland heeft veel te winnen bij een duurzame en open wereldeconomie.",
"Ruim zestig procent van onze werkgelegenheid hangt daar rechtstreeks van af.",
"Nederland zet zich in voor vrije en eerlijke wereldhandel en beter toezicht op de internationale financiële sector.",
"De recessie verzwaart de opgaven waar alle landen samen voor staan om armoede te bestrijden en klimaatverandering aan te pakken.",
"Niettemin zal ons land zich blijven inspannen voor samenwerking met de armste landen en met landen met opkomende economieën, en ook voor drastische verlaging van de uitstoot van schadelijke stoffen.",
"De Algemene Vergadering van de Verenigde Naties, de G20-bijeenkomst in Pittsburgh en de klimaatconferentie in Kopenhagen bieden hiertoe nog dit jaar gelegenheid.",
"Nederland heeft de wereld veel te bieden.",
"Wij zetten ons in voor vrede en veiligheid.",
"Ons land maakt zich sterk voor mensenrechten, vrijheid, democratie en rechtsorde.",
"Deze waarden vinden in Europa hun oorsprong.",
"Juist nu is en blijft Europese samenwerking essentieel.",
"De regering ziet uit naar de inwerkingtreding van het Verdrag van Lissabon.",
"Bij vredes- en veiligheidsmissies zal ons land zijn internationale verantwoordelijkheid blijven nemen.",
"De regering heeft grote waardering voor de militairen die deze zware taken uitvoeren, zoals in Afghanistan.",
"Wij denken met respect aan degenen die hierbij het leven hebben gelaten of gewond zijn geraakt.",
"De regering zal zich ook in Koninkrijksverband blijven inzetten voor gezonde overheidsfinanciën en goede publieke voorzieningen.",
"Nieuwe staatkundige verhoudingen binnen het Koninkrijk moeten daaraan bijdragen.",
"In het belang van de burgers zullen alle betrokkenen zich moeten inspannen om het komend jaar de nieuwe regelingen tot stand te brengen.",
"Leden van de Staten-Generaal,",
"Ons land staat voor een uitzonderlijke, maar geenszins onmogelijke opgave.",
"Met vastberadenheid en met de bereidheid tot verandering kunnen wij de kansen benutten voor een economisch en sociaal krachtig Nederland.",
"De regering doet daartoe een beroep op alle Nederlanders en een ieder die in Nederland woont.",
"Er rust een verantwoordelijkheid op ons allen, jong en oud, burgers en bestuurders, werknemers en werkgevers.",
"De regering spreekt de hoop uit dat iedereen zich daarvan bewust is en ernaar wil handelen.",
"Op u, leden van de Staten-Generaal, rust de zware verantwoordelijkheid om, samen met de regering, initiatief te nemen.",
"U mag zich daarin gesteund weten door het besef dat velen u wijsheid toewensen en met mij om kracht en Gods zegen voor u bidden.",
]



print("Content-type: text/html; charset=UTF-8")
print()

print("""
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
     "http://www.w3.org/TR/REC-html40/loose.dtd">
<HTML>
<head>
<title>Alpino</title>
<link rel="stylesheet" type="text/css" href="/~vannoord/vn3.css"></link>
</head>
<BODY>
<div class="box">
<h1>Alpino: Automatic Syntactic Analysis of Dutch</h1>
<img src=/~vannoord/alp/Alpino/mat2.gif align=left alt=Svejk title=Svejk width=150>
<p>
Sentences which take longer than 20 seconds to parse are ignored.
The input is assumed to be a single sentence. Please type
the sentence as you would do normally, with capitals at the beginning
and for names etc. All sentences are logged and <a href="http://www.let.rug.nl/vannoord/bin/alpinods_dir?webdemo/">visible</a> for others.
<p>
<img src=/~vannoord/alp/mattenklopper.gif width=100 align=right>

<p>
SVG is used to display the resulting dependency structures. Recent browsers support this, but in some cases you might
need to download an SVG plugin.
<p>
For more info on the Alpino Parser visit the <a href="/~vannoord/alp/Alpino/">Alpino homepage</a>
<br clear=all>
<p>

<p>
<form method="post">
<input type="text" name=words size=120/>
<input type="submit" value=" parse! "/>
<p>
<select name="example" onchange="this.form.words.value=options[options.selectedIndex].value" />
<option selected="selected" value="" >Examples</option>

""")


for ex in examples:
    print('<option value="{0}">{0:138.138}</option>'.format(ex))

print("""
</select>
<input type="button" value=" clear " onclick="this.form.words.value=''"/>
<br>
</form>


""")


if len(words)==0:
    print("<p>")
else:
    data=words[0] + "\n\n"
##    print(data)
    host = 'vingolf.let.rug.nl'
    port = 42424
    s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
    s.connect((host,port))
    s.sendall(data.encode('utf-8'))
    total_xml=[]
    while True:
        xml = s.recv(8192)
        if not xml:
            break
        total_xml.append(xml.decode('utf-8'))


    xmlfile,filename = tempfile.mkstemp(suffix=".xml",prefix="alp",dir="../tmp")
    with open(filename,"wt", encoding="utf-8") as xmlout:
        xmlout.write("".join(total_xml) + "\n")

    print("""
<br clear=all>
<a href="{}">XML</a>
<p>
<iframe scrolling="auto" src="http://129.125.14.93/vannoord/bin/alpinods_act2svg?{}" width="100%" height="800">If you don't get to see a dependency structure, your browser does not support SVG, or it does not support IFRAME</iframe>
""".format(filename.replace("/net/homepages/vannoord","/~vannoord"),
            filename.replace("/net/homepages/vannoord/tmp","tmp").replace(".xml","")))


print("""
</div>
</body>
</html>
""")

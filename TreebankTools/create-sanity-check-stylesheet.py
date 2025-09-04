#!/usr/bin/env python3

# Dit script genereert een stylesheet dat gebruikt kan worden voor
# sanity checks op de Treebank.

import time

#----------------------------------------------------------------------

### HIER MOETEN DE CHECKS AAN TOEGEVOEGD WORDEN
def sanity_checks():
    check('/alpino_ds/node[count(node[@cat])>1]','more than one top node???')
    multi_head_check()
    expected_daughters('du', ('dp', 'sat', 'nucl', 'tag','dlink'))
    expected_daughters('mwu', ('mwp',))
    expected_daughters('whq', ('whd', 'body'))
    expected_daughters('oti', ('cmp','body','mod'))
    expected_daughters('ti', ('cmp','body'))
    expected_daughters('pp',('hd','obj1','mod','vc','predc','hdf','pobj1','se'))
    expected_daughters('ap',('hd','mod','vc','pc','obcomp','obj1','predm','predc','me','se','pobj1','obj2'))
    expected_daughters('svan',('cmp','body','mod'))
    expected_daughters('cp',('cmp','body','mod'))
    expected_daughters('ahi',('cmp','body'))
    expected_daughters('rel',('body','rhd'))
    expected_daughters('whrel',('body','rhd'))
    expected_daughters('whsub',('body','whd'))
    expected_daughters('conj',('cnj','crd'))
    expected_daughters('advp',('hd','mod','obcomp','me'))
    expected_daughters('detp',('hd','obcomp','mod','me'))
    expected_daughters('inf',('hd','su','predc','mod','obj1','vc','ld','pc','svp','predm','obj2','se','me','sup','pobj1'))
    expected_daughters('sv1',('hd','su','predc','mod','obj1','vc','ld','pc','svp','predm','obj2','se','me','sup','pobj1'))
    expected_daughters('smain',('hd','su','predc','mod','obj1','vc','ld','pc','svp','predm','obj2','se','me','sup','pobj1'))
    expected_daughters('ssub',('hd','su','predc','mod','obj1','vc','ld','pc','svp','predm','obj2','se','me','sup','pobj1'))
    expected_daughters('ppart',('hd','su','predc','mod','obj1','vc','ld','pc','svp','predm','obj2','se','me','sup','pobj1'))

    expected_categories('hdf',('mwu',))
    expected_categories('hd',('mwu',))
    expected_categories('cmp',('mwu','conj'))
    expected_categories('su',('np','conj','cp','ti','oti','whrel','whsub','svan','mwu',))
    expected_categories('obj2',('np','pp','conj','mwu','whrel'))
    expected_categories('pc',('pp','conj'))
    expected_categories('vc',('cp','ti','ppart','inf','oti','conj','whsub','ahi','svan','smain'))
    expected_categories('svp',('pp','mwu','ti','ahi','inf'))
    expected_categories('predc',('np','ap','ppart','ppres','cp','pp','conj','mwu','whrel','oti'))
    expected_categories('predm',('advp','np','ap','ppart','ppres','cp','pp','conj','mwu','whrel'))
    expected_categories('ld',('pp','np','conj','mwu','ap','advp','whrel'))
    expected_categories('me',('np','ap','conj','mwu'))
    expected_categories('obcomp',('cp','oti','ssub','ti','conj'))
    expected_categories('rhd',('np','pp','ap','conj','advp','mwu','ppart','ppres'))
    expected_categories('whd',('np','pp','ap','conj','advp','cp','mwu','ppart','ppres'))
    expected_categories('mod',('rel','cp','np','advp','ap','ppart','pp','mwu','conj','oti','du','smain','whrel','ti','sv1','ppres'))
    expected_categories('body',('ssub','ti','sv1','inf','np','conj','pp','cp','mwu','du','ppart','smain','whrel','ap','ppart','ppres'))
    expected_categories('det',('detp','mwu','np','conj','ap','pp'))
    expected_categories('app',('mwu','np','conj','smain'))
    expected_categories('crd',('mwu',))

    unexpected_sisters('tag',('tag','nucl','sat','dlink'))
    unexpected_sisters('rhd',('body','rhd'))
    unexpected_sisters('whd',('body','whd'))
    unexpected_sisters('dp',('dp',))
    unexpected_sisters('sat',('sat','nucl','dlink','tag'))
    unexpected_sisters('nucl',('tag','sat','nucl','dlink'))
    unexpected_sisters('body',('body','cmp','rhd','whd','mod'))
    unexpected_sisters('cmp',('body','cmp','mod'))
    unexpected_sisters('crd',('cnj','crd'))
    unexpected_sisters('cnj',('cnj','crd'))
    unexpected_sisters('mwp',('mwp',))
    unexpected_sisters('hdf',('hdf','hd','obj1','mod','se'))

    index_check()

    check('//node[@rel="app" and @begin < ../node[@rel="hd"]/@begin]','appos precedes head')

    check('/alpino_ds[not(@version="1.5" or @version="1.3")]','alpino_ds version is not 1.3 or 1.5')
    check('//node[@lcat]','unexpected lcat attribute')

    check('//node[not(@cat) and node]','non-leaf node without cat?')
    check('//node[@rel="rhd" and not(@index) and ../node[@cat="ssub" or (@cat="conj" and node[@cat="ssub"])]]','rhd without index?')
    check('//node[@rel="whd" and not(@index)]','whd without index?')
    check('//node[(@cat="np" or @cat="smain" or @cat="ppart" or @cat="advp" or @cat="ap" or @cat="ppart" or @cat="ppres" or @cat="pp" or @cat="inf" or @cat="ssub" or @cat="sv1" or @cat="detp") and ./node and not(./node[@rel="hd"])]','No head')
    check('//node[node[@rel="sup"] and not(node[@rel="su"])]','sup without su')
    check('//node[node[@rel="sat"] and not(node[@rel="nucl"])]','sat without nucl')
    check('//node[node[@rel="tag"] and not(node[@rel="nucl"])]','tag without nucl')
    check('//node[node[@rel="pobj1"] and not(node[ @rel="vc"])]','pobj1 without vc')
    check('//node[@index = ancestor::node()/@index]', 'Circularity')
    check('//node[@cat="rel" and not(@rel="mod" or @rel="cnj" or @rel="--")]','Relative not modifier')
    check('//node[count(node) = 1 and not(@cat="top")]', 'Een knoop  met maar 1 dochter')
    check('//node[@pt and not(@end = @begin + 1)]','leaf-node which is not a single word')

    check('//node[@postag="SPEC(deeleigen)" and not(@rel="mwp" or @rel="--") ]','SPEC(deeleigen) is not part of MWU')

    # check('//node[@rel="obj1" and (@cat="cp" or @cat="oti" or @cat="ti" or @cat="ppart") and not(../@cat="pp")]','OBJ1 must be VC?')

    check('//node[@lemma="als" and ../@rel="obcomp" and @rel="cmp" and @pt="vz"]','als is probably VG(onder) here')
    # check('//node[@lemma="als" and (../@rel="predc" or ../@rel="predm" ) and not(@rel="crd") and ../node[@cat="np" or @pt="n"] and not(@postag="VZ(init)") and not ( @begin = //node[@lemma="net"]/@end) ]','als is perhaps VZ(init) here')
    check('//node[@lemma="als" and ../node[@rel="body" and @cat="ssub"] and not(@postag="VG(onder)")]','als is probably VG(onder) here')
    check('//node[@lemma="als" and @pt="vz" and @begin = //node[@lemma="net"]/@end]','als is probably VG(onder) here')
    check('//node[@lemma="als" and @rel="crd" and not(@postag="VG(onder)")]','als is probably VG(onder) here')
    check('//node[@rel="crd" and not (../node[@rel="cnj"])]','A crd without cnj')

    check('//node[ @naamval="gen" and @pt="n" and not(@rel="det" or @rel="mwp" or @rel="hd") ]','Genitive noun, but not determiner?')

    check('//node[@rel="hd" and @cat and not(@cat="mwu")]','Gespleten hoofd')
    check('//node[@cat and not (./node)]','knoop zonder dochters')
    check('//node[@rel="top" and not(@cat="top")]','rel=top, maar cat niet')
    check('//node[@cat="top" and not(@rel="top")]','cat=top, maar rel niet')
    check('//node[@cat="_" or @cat="UNKNOWN"]','wrong value for cat')
    check('//node[@rel="top" and not(./node)]','top-node without daughters?')
    check('//node[@rel="top" and ./node[not(@rel="--")]]','top knoop met andere dan -- dochters')
    check('//node[@rel="top" and not(parent::alpino_ds or parent::top)]', 'rel=top maar de knoop is niet de wortel')
    check('//alpino_ds/node[not(@rel="top")]','top-node heeft geen top relatie')
    check('//node[@rel="--" and not(../@rel="top")]', 'rel=-- aan lage knoop')
    check('//node[@lemma="inclusief" and @pt="vz"]','inclusief is not a prep but an adjective')
    check('//node[@lemma="incluis" and @pt="vz"]','incluis is not a prep but an adjective')
    check('//node[@lemma="uit_zonderen" and @pt="vz"]','uitgezonderd is not a prep but an adjective')
    check('//node[@lemma="voor_behouden" and @pt="vz"]','voorbehouden is not a prep but an adjective')
    check('//node[@lemma="bijgenaamd" and @pt="vz"]','bijgenaamd is not a prep but an adjective')
    check('//node[@word="betreffende" and @pt="vz"]','betreffende is not a prep but an adjective')
    check('//node[@lemma="daar_laten" and @pt="vz"]','daargelaten is not a prep but an adjective')
    check('//node[@lemma="dopen" and @pt="vz"]','gedoopt is not a prep but an adjective')
    check('//node[@lemma="heten" and @pt="vz"]','geheten is not a prep but an adjective')
    check('//node[@lemma="genaamd" and @pt="vz"]','genaamd is not a prep but an adjective')
    check('//node[@lemma="titelen" and @pt="vz"]','getiteld is not a prep but an adjective')
    check('//node[@lemma="noemen" and @pt="vz"]','genoemd is not a prep but an adjective')
    check('//node[@lemma="inbegrepen" and @pt="vz"]','inbegrepen is not a prep but an adjective')
    check('//node[@lemma="zoals" and (@pt="vz" or @rel="hd" or ../@cat="pp")]','zoals is not a prep but a complementizer')
    check('//node[@lemma="als" and (@pt="vz" or @rel="hd" or ../@cat="pp")]','als is not a prep but a complementizer')

    check('//node[@postag="VG(onder)" and not(@rel="cmp") and not(@lemma="als" or @lemma="dan" or @lemma="evenals" or @lemma="behalve" or @lemma="zoals")]','vg(onder) is not cmp')

    # check('//node[@lemma="als" and @pt="vz" and not(@rel="mwp")]','als is not a prep but a complementizer')
    check('//node[@cat="pp" and node[@rel="hd" and @pt="vz"] and not(node[@rel="obj1" or @rel="vc" or @rel="se"])]','preposition without object?')
    check('//node[not(@cat="pp") and node[@rel="hd" and @postag="VZ(init)"]]','preposition head, but not PP?')
    check('//node[@cat="np" and ./node[@cat="whrel"]]','whrel modifies an NP?')
    check('//node[node[@lemma="waard"] and node[@rel="me"]]','adjective waard takes obj1, not me')
    check('//node[not(@cat="svan") and node[@cat="smain" and @rel="body"]]','smain as the body????')
    check('//node[not(@id)]', 'No id-attribute present: probably canonicalization failed for this file')
    check('//node[node[@lemma="geen"] and node[@lemma="veel"]]','geen...meer no longer treated as constituent')
    check('//node[ @cat and @rel="obj1" and not(@cat="np") and not(@cat="conj") and not(@cat="mwu") and not(@cat="whrel") and not(@cat="pp") and not(@cat="cp") and not(@cat="ap") and not((@cat="advp" or @cat="whsub") and ../@cat="pp")]','unexpected obj1-category...')
    check('//node[ ../@cat="np" and not(@rel="hd") and not(@rel="mod") and not(@rel="det") and not(@rel="app") and not(@rel="vc") and not(@rel="pc") and not(@rel="obcomp") and not(@rel="obj1") and not(@rel="su") and not(@rel="predm") and not(@rel="predc") and not(@rel="me") and not((@rel="obj2" or @rel="ld") and ../node[@rel="hd" and (@pt="ww" or @pt="adj")]) and not(@rel="svp" and ../node[@rel="hd" and (@pt="adj" or @pt="ww")]) and not(@rel="se" and ../node[@rel="hd" and (@pt="adj" or @pt="ww")])]','unexpected np-daughter.   Expected rels: hd mod det app vc pc obcomp obj1 su predm predc me')
    check('//node[node[@lemma="willen" or (@lemma="hebben" and node[@rel="obj1" and not(@index)]) or @lemma="mogen" or @lemma="zullen" or @lemma="moeten" or @lemma="hoeven" or @lemma="kunnen" or @lemma="plachten" or @lemma="beginnen" or @lemma="behoeven" or @lemma="behoren" or @lemma="blijven" or @lemma="blijken" or @lemma="zijn" or @lemma="worden" or @lemma="dienen" or @lemma="dreigen" or @lemma="gaan" or @lemma="heten" or @lemma="komen" or @lemma="lijken" or @lemma="mogen" or @lemma="plegen" or @lemma="schijnen" ] and node[@rel="su" and not(@index)] and node[@rel="vc" and not(@cat="cp") and not(@cat="svan")]]','perhaps missing co-indexing??')
    check('//node[node[@rel="mod"] and node[(@lemma="hebben" and node[@rel="obj1" and not(@index)]) or @lemma="zullen" or @lemma="blijven" or @lemma="blijken" or @lemma="zijn" or @lemma="worden" or @lemma="gaan" or @lemma="lijken" or @lemma="schijnen" ] and node[@rel="su" and @index] and not(node[@rel="svp" or @rel="predc"]) and node[@rel="vc" and not(@cat="cp") and not(@cat="svan") and not(@cat="ahi")]]','modifier attached to auxiliary??')
    check('//node[ ../node[@rel="hd" and (@lemma="worden" or @lemma="zijn") ] and ../node[@rel="vc" and node[@rel="obj1" and @index]] and not(@rel="su" or @rel="vc" or @rel="predm" or @rel="hd")]','unexpected relation in passive?')
    check('//node[../@cat="mwu" and not(@end = ../@end) and not(@end = ../node/@begin)]','MWU with hole in it?')
    check('//node[(@cat="ssub" or @cat="sv1" or @cat="smain" or @cat="inf") and node[@rel="mod" and @cat="rel"]]','rel modifies VP?')
    check('//node[node[@lemma="worden" and @rel="hd"] and node[@rel="su" ] and node[@rel="vc" and @cat="ppart" and not(node[(@rel="obj1" or @rel="pobj1") and @index])]]','passive with subject but without main verb object?')
    check('//node[@postag="VG(neven)" and @rel="cnj"]','vg should be crd?')
    check('//node[@cat="inf" and node[@rel="su" and not(@cat="cp" or @cat="oti" or @cat="ti" or @cat="whsub") and not(@index or ../node[@rel="sup"])]]','subject in infinitive clause?')
    check('//node[not(@lemma="er") and @rel="sup" and ../node[@rel="su" and not(@cat="cp" or @cat="oti" or @cat="ti" or @cat="whsub" or @cat="conj")] and not(../node[@rel="hd" and (@lemma="zijn" or @lemma="vallen")] and ../node[@rel="su" and @index])]','unexpected sup/su combination')
    check('//node[node[@rel="hd" and (@lemma="worden" or @lemma="zijn" or @lemma="zullen" or @lemma="lijken" or @lemma="blijken" or @lemma="schijnen" or @lemma="blijven" or @lemma="dunken")] and node[@rel="obj1"]]','unexpected obj1 with copula')
    check('//node[(@cat="cp" or @cat="whsub" or @cat="oti" or @cat="ti") and @rel="obj1"]','this complement is VC perhaps?')
    check('//node[@cat="pp" and @rel="obj1" and not(../@cat="pp")]','this PP is perhaps not an OBJ1?')
    check('//node[@rel="predc" and @cat="ppart" and node[@index and (@rel="obj1" or @rel="su" or @rel="obj2")] and not(../@rel="cnj")]','this looks like a PREDC with controlled arg inside?')
    check('//node[@lemma="er" and @rel="sup"]','er as SUP')
    check('//node[@word and not(@lemma)]','no lemma?')
    check('//node[@word and not(@postag)]','no postag?')
    check('//node[@postag and not(@pt)]','postag but no pt?')
    check('//node[@postag and not(@postag="TSW(dial)" or @postag="N(soort,dial)" or @postag="N(eigen,dial)" or @postag="ADJ(dial)" or @postag="WW(dial)" or @postag="TW(hoofd,dial)" or @postag="TW(rang,dial)" or @postag="VNW(pers,pron,dial)" or @postag="VNW(refl,pron,dial)" or @postag="VNW(recip,pron,dial)" or @postag="VNW(bez,det,dial)" or @postag="VNW(vrag,pron,dial)" or @postag="VNW(vrag,det,dial)" or @postag="VNW(betr,pron,dial)" or @postag="VNW(betr,det,dial)" or @postag="VNW(excl,pron,dial)" or @postag="VNW(excl,det,dial)" or @postag="VNW(aanw,pron,dial)" or @postag="VNW(aanw,det,dial)" or @postag="VNW(onbep,pron,dial)" or @postag="VNW(onbep,det,dial)" or @postag="LID(bep,dial)" or @postag="LID(onbep,dial)" or @postag="VZ(init,dial)" or @postag="VZ(fin,dial)" or @postag="VG(neven,dial)" or @postag="VG(onder,dial)" or @postag="BW(dial)" or @postag="TSW()" or @postag="SPEC(afgebr)" or @postag="SPEC(enof)" or @postag="SPEC(onverst)" or @postag="SPEC(vreemd)" or @postag="SPEC(deeleigen)" or @postag="SPEC(meta)" or @postag="LET()" or @postag="SPEC(comment)" or @postag="SPEC(achter)" or @postag="SPEC(afk)" or @postag="SPEC(symb)" or @postag="N(soort,ev,basis,zijd,stan)" or @postag="N(soort,ev,basis,onz,stan)" or @postag="N(soort,ev,dim,onz,stan)" or @postag="N(soort,ev,basis,gen)" or @postag="N(soort,ev,dim,gen)" or @postag="N(soort,ev,basis,dat)" or @postag="N(soort,mv,basis)" or @postag="N(soort,mv,dim)" or @postag="N(eigen,ev,basis,zijd,stan)" or @postag="N(eigen,ev,basis,onz,stan)" or @postag="N(eigen,ev,dim,onz,stan)" or @postag="N(eigen,ev,basis,gen)" or @postag="N(eigen,ev,dim,gen)" or @postag="N(eigen,ev,basis,dat)" or @postag="N(eigen,mv,basis)" or @postag="N(eigen,mv,dim)" or @postag="ADJ(prenom,basis,zonder)" or @postag="ADJ(prenom,basis,met-e,stan)" or @postag="ADJ(prenom,basis,met-e,bijz)" or @postag="ADJ(prenom,comp,zonder)" or @postag="ADJ(prenom,comp,met-e,stan)" or @postag="ADJ(prenom,comp,met-e,bijz)" or @postag="ADJ(prenom,sup,zonder)" or @postag="ADJ(prenom,sup,met-e,stan)" or @postag="ADJ(prenom,sup,met-e,bijz)" or @postag="ADJ(nom,basis,zonder,zonder-n)" or @postag="ADJ(nom,basis,zonder,mv-n)" or @postag="ADJ(nom,basis,met-e,zonder-n,stan)" or @postag="ADJ(nom,basis,met-e,zonder-n,bijz)" or @postag="ADJ(nom,basis,met-e,mv-n)" or @postag="ADJ(nom,comp,zonder,zonder-n)" or @postag="ADJ(nom,comp,met-e,zonder-n,stan)" or @postag="ADJ(nom,comp,met-e,zonder-n,bijz)" or @postag="ADJ(nom,comp,met-e,mv-n)" or @postag="ADJ(nom,sup,zonder,zonder-n)" or @postag="ADJ(nom,sup,met-e,zonder-n,stan)" or @postag="ADJ(nom,sup,met-e,zonder-n,bijz)" or @postag="ADJ(nom,sup,met-e,mv-n)" or @postag="ADJ(postnom,basis,zonder)" or @postag="ADJ(postnom,basis,met-s)" or @postag="ADJ(postnom,comp,zonder)" or @postag="ADJ(postnom,comp,met-s)" or @postag="ADJ(vrij,basis,zonder)" or @postag="ADJ(vrij,comp,zonder)" or @postag="ADJ(vrij,sup,zonder)" or @postag="ADJ(vrij,dim,zonder)" or @postag="WW(pv,tgw,ev)" or @postag="WW(pv,tgw,mv)" or @postag="WW(pv,tgw,met-t)" or @postag="WW(pv,verl,ev)" or @postag="WW(pv,verl,mv)" or @postag="WW(pv,verl,met-t)" or @postag="WW(pv,conj,ev)" or @postag="WW(inf,prenom,zonder)" or @postag="WW(inf,prenom,met-e)" or @postag="WW(inf,nom,zonder,zonder-n)" or @postag="WW(inf,vrij,zonder)" or @postag="WW(vd,prenom,zonder)" or @postag="WW(vd,prenom,met-e)" or @postag="WW(vd,nom,met-e,zonder-n)" or @postag="WW(vd,nom,met-e,mv-n)" or @postag="WW(vd,vrij,zonder)" or @postag="WW(od,prenom,zonder)" or @postag="WW(od,prenom,met-e)" or @postag="WW(od,nom,met-e,zonder-n)" or @postag="WW(od,nom,met-e,mv-n)" or @postag="WW(od,vrij,zonder)" or @postag="TW(hoofd,prenom,stan)" or @postag="TW(hoofd,prenom,bijz)" or @postag="TW(hoofd,nom,zonder-n,basis)" or @postag="TW(hoofd,nom,mv-n,basis)" or @postag="TW(hoofd,nom,zonder-n,dim)" or @postag="TW(hoofd,nom,mv-n,dim)" or @postag="TW(hoofd,vrij)" or @postag="TW(rang,prenom,stan)" or @postag="TW(rang,prenom,bijz)" or @postag="TW(rang,nom,zonder-n)" or @postag="TW(rang,nom,mv-n)" or @postag="VNW(pers,pron,nomin,vol,1,ev)" or @postag="VNW(pers,pron,nomin,nadr,1,ev)" or @postag="VNW(pers,pron,nomin,red,1,ev)" or @postag="VNW(pers,pron,nomin,vol,1,mv)" or @postag="VNW(pers,pron,nomin,nadr,1,mv)" or @postag="VNW(pers,pron,nomin,red,1,mv)" or @postag="VNW(pers,pron,nomin,vol,2v,ev)" or @postag="VNW(pers,pron,nomin,nadr,2v,ev)" or @postag="VNW(pers,pron,nomin,red,2v,ev)" or @postag="VNW(pers,pron,nomin,nadr,3m,ev,masc)" or @postag="VNW(pers,pron,nomin,vol,3v,ev,fem)" or @postag="VNW(pers,pron,nomin,nadr,3v,ev,fem)" or @postag="VNW(pers,pron,obl,vol,2v,ev)" or @postag="VNW(pers,pron,obl,nadr,3m,ev,masc)" or @postag="VNW(pers,pron,gen,vol,1,ev)" or @postag="VNW(pers,pron,gen,vol,1,mv)" or @postag="VNW(pers,pron,gen,vol,3m,ev)" or @postag="VNW(bez,det,gen,vol,1,ev,prenom,zonder,evmo)" or @postag="VNW(bez,det,gen,vol,1,mv,prenom,met-e,evmo)" or @postag="VNW(bez,det,gen,vol,3v,ev,prenom,zonder,evmo)" or @postag="VNW(bez,det,dat,vol,1,ev,prenom,met-e,evmo)" or @postag="VNW(bez,det,dat,vol,1,ev,prenom,met-e,evf)" or @postag="VNW(bez,det,dat,vol,1,mv,prenom,met-e,evmo)" or @postag="VNW(bez,det,dat,vol,1,mv,prenom,met-e,evf)" or @postag="VNW(bez,det,dat,vol,2v,ev,prenom,met-e,evf)" or @postag="VNW(bez,det,dat,vol,3v,ev,prenom,met-e,evmo)" or @postag="VNW(bez,det,dat,vol,3v,ev,prenom,met-e,evf)" or @postag="VNW(bez,det,dat,vol,1,ev,nom,met-e,zonder-n)" or @postag="VNW(bez,det,dat,vol,1,mv,nom,met-e,zonder-n)" or @postag="VNW(bez,det,dat,vol,3m,ev,nom,met-e,zonder-n)" or @postag="VNW(bez,det,dat,vol,3v,ev,nom,met-e,zonder-n)" or @postag="VNW(betr,pron,gen,vol,3o,ev)" or @postag="VNW(aanw,pron,gen,vol,3m,ev)" or @postag="VNW(aanw,pron,gen,vol,3o,ev)" or @postag="VNW(aanw,det,dat,prenom,met-e,evmo)" or @postag="VNW(aanw,det,dat,prenom,met-e,evf)" or @postag="VNW(aanw,det,gen,nom,met-e,zonder-n)" or @postag="VNW(aanw,det,dat,nom,met-e,zonder-n)" or @postag="VNW(onbep,det,gen,prenom,met-e,mv)" or @postag="VNW(onbep,det,dat,prenom,met-e,evmo)" or @postag="VNW(onbep,det,dat,prenom,met-e,evf)" or @postag="VNW(onbep,det,gen,nom,met-e,mv-n)" or @postag="VNW(onbep,grad,gen,nom,met-e,mv-n,basis)" or @postag="LID(bep,stan,evon)" or @postag="LID(bep,stan,rest)" or @postag="LID(bep,gen,evmo)" or @postag="LID(bep,dat,evmo)" or @postag="LID(bep,dat,evf)" or @postag="LID(bep,dat,mv)" or @postag="LID(onbep,gen,evf)" or @postag="VZ(init)" or @postag="VZ(fin)" or @postag="VZ(versm)" or @postag="VG(neven)" or @postag="VG(onder)" or @postag="BW()" or @postag="N(soort,ev,basis,genus,stan)" or @postag="N(eigen,ev,basis,genus,stan)" or @postag="VNW(pers,pron,nomin,vol,2b,getal)" or @postag="VNW(pers,pron,nomin,nadr,2b,getal)" or @postag="VNW(pers,pron,nomin,vol,2,getal)" or @postag="VNW(pers,pron,nomin,nadr,2,getal)" or @postag="VNW(pers,pron,nomin,red,2,getal)" or @postag="VNW(pers,pron,nomin,vol,3,ev,masc)" or @postag="VNW(pers,pron,nomin,red,3,ev,masc)" or @postag="VNW(pers,pron,nomin,red,3p,ev,masc)" or @postag="VNW(pers,pron,nomin,vol,3p,mv)" or @postag="VNW(pers,pron,nomin,nadr,3p,mv)" or @postag="VNW(pers,pron,obl,vol,3,ev,masc)" or @postag="VNW(pers,pron,obl,red,3,ev,masc)" or @postag="VNW(pers,pron,obl,vol,3,getal,fem)" or @postag="VNW(pers,pron,obl,nadr,3v,getal,fem)" or @postag="VNW(pers,pron,obl,red,3v,getal,fem)" or @postag="VNW(pers,pron,obl,vol,3p,mv)" or @postag="VNW(pers,pron,obl,nadr,3p,mv)" or @postag="VNW(pers,pron,stan,nadr,2v,mv)" or @postag="VNW(pers,pron,stan,red,3,ev,onz)" or @postag="VNW(pers,pron,stan,red,3,ev,fem)" or @postag="VNW(pers,pron,stan,red,3,mv)" or @postag="VNW(pers,pron,gen,vol,2,getal)" or @postag="VNW(pers,pron,gen,vol,3v,getal)" or @postag="VNW(pers,pron,gen,vol,3p,mv)" or @postag="VNW(pr,pron,obl,vol,1,ev)" or @postag="VNW(pr,pron,obl,nadr,1,ev)" or @postag="VNW(pr,pron,obl,red,1,ev)" or @postag="VNW(pr,pron,obl,vol,1,mv)" or @postag="VNW(pr,pron,obl,nadr,1,mv)" or @postag="VNW(pr,pron,obl,red,2v,getal)" or @postag="VNW(pr,pron,obl,nadr,2v,getal)" or @postag="VNW(pr,pron,obl,vol,2,getal)" or @postag="VNW(pr,pron,obl,nadr,2,getal)" or @postag="VNW(refl,pron,obl,red,3,getal)" or @postag="VNW(refl,pron,obl,nadr,3,getal)" or @postag="VNW(recip,pron,obl,vol,persoon,mv)" or @postag="VNW(recip,pron,gen,vol,persoon,mv)" or @postag="VNW(bez,det,stan,vol,1,ev,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,vol,1,ev,prenom,met-e,rest)" or @postag="VNW(bez,det,stan,red,1,ev,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,vol,1,mv,prenom,zonder,evon)" or @postag="VNW(bez,det,stan,vol,1,mv,prenom,met-e,rest)" or @postag="VNW(bez,det,stan,vol,2,getal,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,vol,2,getal,prenom,met-e,rest)" or @postag="VNW(bez,det,stan,vol,2v,ev,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,nadr,2v,mv,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,vol,3,ev,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,vol,3m,ev,prenom,met-e,rest)" or @postag="VNW(bez,det,stan,vol,3v,ev,prenom,met-e,rest)" or @postag="VNW(bez,det,stan,red,3,ev,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,vol,3,mv,prenom,zonder,agr)" or @postag="VNW(bez,det,stan,vol,3p,mv,prenom,met-e,rest)" or @postag="VNW(bez,det,stan,red,3,getal,prenom,zonder,agr)" or @postag="VNW(bez,det,gen,vol,1,ev,prenom,met-e,rest3)" or @postag="VNW(bez,det,gen,vol,1,mv,prenom,met-e,rest3)" or @postag="VNW(bez,det,gen,vol,2,getal,prenom,zonder,evmo)" or @postag="VNW(bez,det,gen,vol,2,getal,prenom,met-e,rest3)" or @postag="VNW(bez,det,gen,vol,2v,ev,prenom,met-e,rest3)" or @postag="VNW(bez,det,gen,vol,3,ev,prenom,zonder,evmo)" or @postag="VNW(bez,det,gen,vol,3,ev,prenom,met-e,rest3)" or @postag="VNW(bez,det,gen,vol,3v,ev,prenom,met-e,rest3)" or @postag="VNW(bez,det,gen,vol,3p,mv,prenom,zonder,evmo)" or @postag="VNW(bez,det,gen,vol,3p,mv,prenom,met-e,rest3)" or @postag="VNW(bez,det,dat,vol,2,getal,prenom,met-e,evmo)" or @postag="VNW(bez,det,dat,vol,2,getal,prenom,met-e,evf)" or @postag="VNW(bez,det,dat,vol,3,ev,prenom,met-e,evmo)" or @postag="VNW(bez,det,dat,vol,3,ev,prenom,met-e,evf)" or @postag="VNW(bez,det,dat,vol,3p,mv,prenom,met-e,evmo)" or @postag="VNW(bez,det,dat,vol,3p,mv,prenom,met-e,evf)" or @postag="VNW(bez,det,stan,vol,1,ev,nom,met-e,zonder-n)" or @postag="VNW(bez,det,stan,vol,1,mv,nom,met-e,zonder-n)" or @postag="VNW(bez,det,stan,vol,2,getal,nom,met-e,zonder-n)" or @postag="VNW(bez,det,stan,vol,2v,ev,nom,met-e,zonder-n)" or @postag="VNW(bez,det,stan,vol,3m,ev,nom,met-e,zonder-n)" or @postag="VNW(bez,det,stan,vol,3v,ev,nom,met-e,zonder-n)" or @postag="VNW(bez,det,stan,vol,3p,mv,nom,met-e,zonder-n)" or @postag="VNW(bez,det,stan,vol,1,ev,nom,met-e,mv-n)" or @postag="VNW(bez,det,stan,vol,1,mv,nom,met-e,mv-n)" or @postag="VNW(bez,det,stan,vol,2,getal,nom,met-e,mv-n)" or @postag="VNW(bez,det,stan,vol,2v,ev,nom,met-e,mv-n)" or @postag="VNW(bez,det,stan,vol,3m,ev,nom,met-e,mv-n)" or @postag="VNW(bez,det,stan,vol,3v,ev,nom,met-e,mv-n)" or @postag="VNW(bez,det,stan,vol,3p,mv,nom,met-e,mv-n)" or @postag="VNW(bez,det,dat,vol,2,getal,nom,met-e,zonder-n)" or @postag="VNW(bez,det,dat,vol,3p,mv,nom,met-e,zonder-n)" or @postag="VNW(vrag,pron,stan,nadr,3o,ev)" or @postag="VNW(betr,pron,stan,vol,persoon,getal)" or @postag="VNW(betr,pron,stan,vol,3,ev)" or @postag="VNW(betr,det,stan,nom,zonder,zonder-n)" or @postag="VNW(betr,det,stan,nom,met-e,zonder-n)" or @postag="VNW(betr,pron,gen,vol,3o,getal)" or @postag="VNW(vb,pron,dat,vol,3p,getal)" or @postag="VNW(vb,pron,stan,vol,3p,getal)" or @postag="VNW(vb,pron,stan,vol,3o,ev)" or @postag="VNW(vb,pron,gen,vol,3m,ev)" or @postag="VNW(vb,pron,gen,vol,3v,ev)" or @postag="VNW(vb,pron,gen,vol,3p,mv)" or @postag="VNW(vb,adv-pron,obl,vol,3o,getal)" or @postag="VNW(excl,pron,stan,vol,3,getal)" or @postag="VNW(vb,det,stan,prenom,zonder,evon)" or @postag="VNW(vb,det,stan,prenom,met-e,rest)" or @postag="VNW(vb,det,stan,nom,met-e,zonder-n)" or @postag="VNW(excl,det,stan,vrij,zonder)" or @postag="VNW(aanw,pron,stan,vol,3o,ev)" or @postag="VNW(aanw,pron,stan,nadr,3o,ev)" or @postag="VNW(aanw,pron,stan,vol,3,getal)" or @postag="VNW(aanw,adv-pron,obl,vol,3o,getal)" or @postag="VNW(aanw,adv-pron,stan,red,3,getal)" or @postag="VNW(aanw,det,stan,prenom,zonder,evon)" or @postag="VNW(aanw,det,stan,prenom,zonder,rest)" or @postag="VNW(aanw,det,stan,prenom,zonder,agr)" or @postag="VNW(aanw,det,stan,prenom,met-e,rest)" or @postag="VNW(aanw,det,gen,prenom,met-e,rest3)" or @postag="VNW(aanw,det,stan,nom,met-e,zonder-n)" or @postag="VNW(aanw,det,stan,nom,met-e,zonder-n)" or @postag="VNW(aanw,det,stan,gen,met-e,mv-n)" or @postag="VNW(aanw,det,stan,nom,met-e,mv-n)" or @postag="VNW(aanw,det,stan,vrij,zonder)" or @postag="VNW(onbep,pron,stan,vol,3p,ev)" or @postag="VNW(onbep,pron,stan,vol,3o,ev)" or @postag="VNW(onbep,pron,gen,vol,3p,ev)" or @postag="VNW(onbep,adv-pron,obl,vol,3o,getal)" or @postag="VNW(onbep,adv-pron,gen,red,3,getal)" or @postag="VNW(onbep,det,stan,prenom,zonder,evon)" or @postag="VNW(onbep,det,stan,prenom,zonder,agr)" or @postag="VNW(onbep,det,stan,prenom,met-e,evz)" or @postag="VNW(onbep,det,stan,prenom,met-e,mv)" or @postag="VNW(onbep,det,stan,prenom,met-e,rest)" or @postag="VNW(onbep,det,stan,prenom,met-e,agr)" or @postag="VNW(onbep,grad,stan,prenom,zonder,agr,basis)" or @postag="VNW(onbep,grad,stan,prenom,met-e,agr,basis)" or @postag="VNW(onbep,grad,stan,prenom,met-e,mv,basis)" or @postag="VNW(onbep,grad,stan,prenom,zonder,agr,comp)" or @postag="VNW(onbep,grad,stan,prenom,met-e,agr,sup)" or @postag="VNW(onbep,grad,stan,prenom,met-e,agr,comp)" or @postag="VNW(onbep,det,stan,nom,met-e,mv-n)" or @postag="VNW(onbep,det,stan,nom,met-e,zonder-n)" or @postag="VNW(onbep,det,stan,nom,zonder,zonder-n)" or @postag="VNW(onbep,grad,stan,nom,met-e,zonder-n,basis)" or @postag="VNW(onbep,grad,stan,nom,met-e,mv-n,basis)" or @postag="VNW(onbep,grad,stan,nom,met-e,zonder-n,sup)" or @postag="VNW(onbep,grad,stan,nom,met-e,mv-n,sup)" or @postag="VNW(onbep,grad,stan,nom,zonder,mv-n,dim)" or @postag="VNW(onbep,det,stan,vrij,zonder)" or @postag="VNW(onbep,grad,stan,vrij,zonder,basis)" or @postag="VNW(onbep,grad,stan,vrij,zonder,sup)" or @postag="VNW(onbep,grad,stan,vrij,zonder,comp)" or @postag="LID(bep,gen,rest3)" or @postag="LID(onbep,stan,agr)" or @postag="VNW(onbep,grad,stan,nom,zonder,zonder-n,sup)")]','wrong value for postag')
    check('//node[node[@rel="hd" and @pt and not(@pt="ww")] and node[@rel="su"]]','subject without a verb?')
    check('//node[@postag and @postag=""]','empty postag?')
    check('//node[@lemma and @lemma=""]','empty lemma?')
    check('//node[@word and @word=""]','empty word?')
    check('//node[contains(@word," ")]','space in word????')
    check('//node[contains(@postag," ")]','space in postag????')
    check('//node[@cat="smain" and not(node[@rel="su"]) and node[@rel="obj1"]]','no subject in smain?')
    check('//node[@cat="ssub" and not(node[@rel="su"]) and node[@rel="obj1"]]','no subject in ssub?')
    check('//node[@wvorm="pv" and not(../@cat="smain" or ../@cat="ssub" or ../@cat="sv1" or ../@cat="mwu" or ../@cat="top" or ../@cat="du") ]','unexpected finite verb?')
    check('//node[@cat="ppart" and node[@rel="hd" and @pt="ww" and not(@wvorm="vd")]]','ppart without vd?')
    check('//node[@cat="ppart" and node[@rel="hd" and @pt="adj" ]]','ppart with adjectival head?')
    check('//node[@cat="ap" and node[@rel="hd" and @pt="ww" ]]','ap with verbal head?')
    check('//node[@cat="inf" and node[@rel="hd" and @pt and not(@postag="WW(inf,vrij,zonder)" or @postag="WW(inf,prenom,zonder)" or @postag="WW(inf,prenom,met-e)")]]','inf cat without inf hd?')
    check('//node[@rel="hd" and @postag="WW(inf,nom,zonder,zonder-n)" and not(../@cat="np")]','nominalized infinitival head, but not np?')
    check('//node[(@cat="sv1" or @cat="smain" or @cat="ssub") and node[@rel="hd" and @pt and not(@wvorm="pv")]]','s without pv?')
    check('//node[@cat="ti" and node[@rel="body" and @cat and not(@cat="inf" or @cat="conj")]]','te-inf without inf?')
    check('//node[@cat="oti" and node[@rel="body" and @cat and not(@cat="ti" or @cat="conj")]]','oti without ti')
    check('//node[contains(@frame,"read_from_treebank")]','read_from_treebank frame????')
    check('//node[@lcat="UNKNOWN"]','unknown lcat????')
    check('//node[@word="dat" and @rel="cmp" and not(@pt="vg")]','dat is cmp, but not VG?')
    check('//node[@word="dat" and @rel="rhd" and not(@pt="vnw" and @vwtype="betr")]','dat is rhd, but not vnw(betr...)?')
    check('//node[@rel="det" and @vwtype="betr" and not(../@rel="rhd")]','relative as determiner???')
    check('//node[@rel="rhd" and @pos and not(@vwtype="betr" or @vwtype="vb" or @pt="bw")]','rhd without relative pronoun?')
    check('//node[@rel="whd" and @pos and not(@vwtype="vb" or @pt="bw" or @lemma="hoeveel")]','whd without wh postag?')
    check('//node[@rel="det" and @postag and @vwtype="pers"]','personal pronoun as determiner?')
    check('//node[@vwtype="betr" and not(@rel="rhd" or ../@rel="rhd")]','relative not RHD?')
    check('//node[@rel="det" and @pos and not(@pt="lid" or @pt="vnw" or @pt="tw" or (@pt="n" and @naamval="gen") or @pt="spec" or @pt="adj" or @lemma="genoeg" or (@pt="n" and @ntype="eigen"))]','unexpected postag for determiner')
    check('//node[node[@getal="mv" and @rel="hd"] and node[@rel="det" and (@lemma="het" or @lemma="een" or @lemma="dit" or @lemma="dat")]]','plural head noun with singular determiner?')
    check('//node[@pt="n" and @rel="det" and not(@naamval="gen")]','unexpected postag for det')
    check('//node[@pt="lid" and not(@rel="det" or ../@rel="det" or @rel="mwp" or @rel="--") ]','unexpected role for determiner')
    check('//node[(@cat="pp" or @cat="detp" or @cat="ap") and node[@cat="rel"]]','unexpected relative clause')
    check('//node[(@cat="rel" or @cat="whrel" or @cat="whsub") and node[@index] and node[@rel="body" and not(@cat="ssub" or (@cat="conj" and node[@cat="ssub"]))]]','unexpected relative')
    check('//node[@cat="cp" and not(node[@rel="body"])]','cp without body')
    check('//node[@cat="cp" and not(node[@rel="cmp"])]','cp without cmp')
    # thanks to Jan Odijk:
    check('//node[@cat="ppart" and node[@rel="hd" and @postag and not(@pt="ww" and @wvorm="vd")]]','ppart without participle')
    # thanks to Gosse Bouma:
    check('//node[@cat="ap" and node[@rel="hd" and @pt="n"]]','noun heads an ap')
    #
    check('//node[@rel="hd" and ../@cat="np" and @pt and not(@pt="n" or @pt="tw" or @pt="spec" or @pt="adj" or @pt="ww" or @pt="vnw" or @pt="bw")]','unexpected postag for head of np')
    check('//node[@pt="adj" and @positie="vrij" and @rel="hd" and ../@cat="np"]','free adjective as head of NP?')
    check('//node[@pt="adj" and @positie="vrij" and (@rel="su" or (@rel="obj1" and not(../@cat="pp")) or @rel="obj2") ]','free adjective as NP?')
    check('//node[@rel="hd" and ../@cat="ap" and @pt and not(@pt="vz" or @pt="tw" or @pt="bw" or @pt="vnw" or @pt="adj")]','unexpected postag for head of ap')
    check('//node[@rel="hd" and ../@cat="advp" and @pt and not(@pt="bw" or @pt="n" or @pt="adj" or @pt="vnw" or @pt="tw" or @postag="VZ(fin)")]','unexpected postag for head of advp')
    check('//node[@rel="hd" and ../@cat="pp" and @pt and not(@pt="vz" or @pt="spec" or @pt="bw" or @pt="adj" or @lemma="richting" or @lemma="naargelang" or @lemma="getuige" or @lemma="zien")]','unexpected postag for head of pp')
    check('//node[@rel="hd" and (../@cat="ssub" or ../@cat="smain" or ../@cat="sv1") and @pt and not(@pt="ww")]','unexpected pt for head of finite sentence')
    check('//node[@naamval="obl" and (@rel="su" or (@rel="hd"and ../@rel="su") or (@rel="cnj" and ../@rel="su")) and (@lemma="u" or @lemma="uzelf" or @lemma="je")]','obl case in nominative position?')
    check('//node[@naamval="nomin" and not(@rel="su" or (@rel="hd"and ../@rel="su") or (@rel="cnj" and ../@rel="su")) and (@lemma="u" or @lemma="uzelf" or @lemma="je")]','nominative case in obl position?')

#----------------------------------------------------------------------
# SANITY CHECKS

# check if rel occurs more than once (hd, hdf, rhd, whd, obj1, obj2, pobj1 ...):
# ./dtv2 '//node[count(node[@rel="hd"]) > 1]'
def multi_head_check():
    rels = ('hd', 'hdf', 'rhd', 'whd', 'obj1', 'obj2', 'pobj1','pc','se',
            'cmp','sup','su','vc','predc','ld','me','obcomp','body','nucl',
            'sat','tag','dlink')
    for rel in rels:
        query = '//node[count(node[@rel="%s"]) > 1]/node[@rel="%s"]' % (rel, rel)
        comment = 'rel "%s" occurs more than once' % (rel)
        check(query, comment)


# display all unexpected daughters:
# ./dtv2 '//node[ ../@cat="du" and not(@rel="dp") and not(@rel="sat") and not(@rel="nucl") and not(@rel="tag")]'
def expected_daughters(parentcat, expecteddaughterrels):
    query = '//node[ ../@cat="' + parentcat + '"'
    descr = 'unexpected ' + parentcat + '-daughter.   Expected rels:'
    for rel in expecteddaughterrels :
        query = query + ' and not(@rel="%s")' % (rel)
        descr = descr + " %s" % (rel)
    query = query + ']'
    check(query, descr)

def expected_categories(parentrel, expecteddaughtercats):
    query = '//node[ @cat and @rel="' + parentrel + '"'
    descr = 'unexpected ' + parentrel + '-category.   Expected cats:'
    for rel in expecteddaughtercats :
        query = query + ' and not(@cat="%s")' % (rel)
        descr = descr + " %s" % (rel)
    query = query + ']'
    check(query, descr)

# display all unexpected sisters:
# //node[../node[@rel="crd"] and not(@rel="crd") and not(@rel="cnj")]
def unexpected_sisters(sisterrel, expectedrels):
    query = '//node[../node[@rel="%s"]' % (sisterrel)
    descr = 'unexpected ' + sisterrel + '-sister.   Expected rels:'
    for rel in expectedrels:
        query = query + ' and not(@rel="%s")' % (rel)
        descr = descr + " %s" % (rel)
    query = query + ']'
    check(query, descr)



# We drukken het de inhoud van het index-test-stylesheet hier bijin:

def index_check():
    print('    <xsl:apply-templates select="//node[@index]" mode="index_check"/>')

def index_check_templates():
    print("""

  <!-- templates for index checking -->

  <!-- indexed-nodes -->
  <xsl:template match="node[@index and (@pos or @cat)]" mode="index_check">
    <xsl:variable name="index">
      <xsl:value-of select="@index"/>
    </xsl:variable>
    <xsl:if test="not(//node[@index[. = $index] and not(@pos or @cat)])">
      <!-- Alles met zo'n index: 'filename //node[@index = $index]' -->
      <xsl:value-of select="$filename"/>
      <xsl:text>&#x9;</xsl:text>
      <xsl:text>//node[@index = </xsl:text>
      <xsl:value-of select="$index"/>
      <xsl:text> ]&#x9;indexed-node without index-node&#xA;</xsl:text>
    </xsl:if>
    <xsl:if test="count(//node[@index = $index and (@pos or @cat)]) > 1">
      <xsl:value-of select="$filename"/>
      <xsl:text>&#x9;</xsl:text>
      <xsl:text>//node[@index = </xsl:text>
      <xsl:value-of select="$index"/>
      <xsl:text> and (@pos or @cat)]&#x9;multiple use of index&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- index-nodes -->
  <xsl:template match="node[@index and not(@pos or @cat)]" mode="index_check">
    <xsl:variable name="index">
      <xsl:value-of select="@index"/>
    </xsl:variable>
    <xsl:if test="not(//node[@index[. = $index] and (@pos or @cat)])">
      <!-- Alles met die index: 'filename //node[@index = $index]' -->
      <xsl:value-of select="$filename"/>
      <xsl:text>&#x9;</xsl:text>
      <xsl:text>//node[@index = </xsl:text>
      <xsl:value-of select="$index"/>
      <xsl:text> ]&#x9;index-node without indexed-node&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>
  """)

#----------------------------------------------------------------------
# UTILITY FUNCTIONS

def printfilepart():
    print('      <xsl:value-of select="$filename"/>')
    print('      <xsl:text>&#x9;</xsl:text>')

def printquerypart (query):
    print('      <xsl:text>' + escape_meta(query) + '&#x9;</xsl:text>')

def printdescription (description=""):
    print('      <xsl:text>' + escape_meta(description) + '&#xA;</xsl:text>')

def check(query, description):
    "Bruikbaar wanneer zoekexpressie hetzelfde is als highlight-expressie"
    print("    <xsl:if test='" + escape_meta(query) + "'>")
    printfilepart()
    printquerypart(query)
    printdescription(description)
    print("    </xsl:if>\n")

def escape_meta(query):
    query = query.replace("<", "&lt;")
    query = query.replace("'", "&apos;")
    query = query.replace('"', "&quot;")
    return query


def print_header():
    print('''<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="text" encoding="UTF-8"/>

  <!-- This file was generated by create-sanity-check-stylesheet.py on %s. -->


  <!-- Usage:

          xmlmatch -q / -s <this file> */*.xml | dtview -file-query-pairs

    -->

  <xsl:param name="filename"/>''' % (time.ctime(time.time())))

def print_footer():
    print("</xsl:stylesheet>")

def print_root_template():
    print('  <xsl:template match="/">')
    sanity_checks()
    print('  </xsl:template>')


#----------------------------------------------------------------------

if __name__ == '__main__' :
    print_header()
    print_root_template()
    index_check_templates()
    print_footer()

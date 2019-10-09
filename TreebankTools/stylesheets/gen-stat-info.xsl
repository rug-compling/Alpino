<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:saxon="http://icl.com/saxon"
    extension-element-prefixes="saxon">

  <xsl:output method="text" encoding="UTF-8"/>
  <xsl:param name='expr'/>
  <xsl:param name='filename'/>

  <!-- Voor gebruik met dt_search in combinatie met xmlmatch -->

  <!-- 
       Als debugout.xsl, maar dan met minder output.
       Met zinnen van gemiddeld 13 woorden, waarvan gemiddeld 5
       attributes per node minder uitgeprint hoeven te worden, 
       (word, root, begin, end, hd, soms index), scheelt dat op een
       treebank van 5000 zinnen al gauw 5 * 13 * 5000 = 32500
       template calls...   En een stuk minder I/O.

       Op een pentium II 300 betekent dat 55 seconden in plaats van
       anderhalve minuut.  (//node[@word])

       print-sentence.xsl doet er 30 seconden over, zonder stylesheet
       doet xmlmatch er 25 seconden over, met  de debugdump van de
       show-match optie 42 seconden.

       Met de query '//node' zijn de verschillen groter. 
       De show-match optie doet er nu 45 seconden over.  Het
       debugout.xsl stylesheet 2 minuten, dit stylesheet iets meer
       dan een minuut.

       117171 nodes bijlangs i.p.v. 71063.
   -->

  <xsl:variable name="selectedNodes" select="saxon:evaluate($expr)"/>

  <xsl:template match="/">
    <xsl:text>filename=</xsl:text>
    <xsl:value-of select="$filename"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:apply-templates select="/top/sentence"/>
    <xsl:for-each select="$selectedNodes">
      <xsl:apply-templates select="@rel"/>
      <xsl:apply-templates select="@cat"/>
      <xsl:apply-templates select="@pos"/>

      <xsl:apply-templates select="@poscat"/>

      <xsl:apply-templates select="@case"/>
      <xsl:apply-templates select="@comparative"/>
      <xsl:apply-templates select="@def"/>
      <xsl:apply-templates select="@frame"/>
      <xsl:apply-templates select="@gen"/>
      <xsl:apply-templates select="@infl"/>
      <xsl:apply-templates select="@lcat"/>
      <xsl:apply-templates select="@neclass"/>
      <xsl:apply-templates select="@num"/>
      <xsl:apply-templates select="@per"/>
      <xsl:apply-templates select="@refl"/>
      <xsl:apply-templates select="@sc"/>
      <xsl:apply-templates select="@special"/>
      <xsl:apply-templates select="@tense"/>
      <xsl:apply-templates select="@wh"/>

      <xsl:apply-templates select="@lemma"/>
      <xsl:apply-templates select="@root"/>
      <xsl:apply-templates select="@sense"/>
      <xsl:apply-templates select="@word"/>
    </xsl:for-each>
  </xsl:template>


  <xsl:template match="@*|sentence">
    <xsl:value-of select="name()"/>
    <xsl:text>=</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

</xsl:stylesheet>

<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:saxon="http://icl.com/saxon"
    extension-element-prefixes="saxon">

  <xsl:output method="text" encoding="UTF-8"/>
  <xsl:param name='expr'/>
  <xsl:param name='filename'/>

  <!-- 
       Dit stylesheet is bedoeld voor dt_search in combinatie met
       xmlmatch.

       We geven als uitvoer de filename, de zin, en de begin en end
       posities.

       Het aantal matches kan geteld worden door of begin of end te
       tellen.  Merk op dat index nodes op het moment geen begin en
       end posities hebben.

       We gebruiken een apart stylesheet omdat alle attributen
       uitprinten igv. van een stylesheet errug duur is.  Voor ieder
       attribute een template call.  Dus zo min mogelijk info
       genereren.

       filename=...
       sentence=...
       begin=...
       end=...
       ...
   -->

  <xsl:variable name="selectedNodes" select="saxon:evaluate($expr)"/>

  <xsl:template match="/">
    <xsl:text>filename=</xsl:text>
    <xsl:value-of select="$filename"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:apply-templates select="/top/sentence|/alpino_ds/sentence"/>
    <xsl:for-each select="$selectedNodes">
      <xsl:apply-templates select="@begin"/>
      <xsl:apply-templates select="@end"/>
    </xsl:for-each>
    <xsl:text>-&#xA;</xsl:text>
  </xsl:template>


  <xsl:template match="@*|sentence">
    <xsl:value-of select="name()"/>
    <xsl:text>=</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>


</xsl:stylesheet>

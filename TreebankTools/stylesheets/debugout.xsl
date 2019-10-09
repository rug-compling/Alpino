<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:saxon="http://icl.com/saxon"
    extension-element-prefixes="saxon">

  <xsl:output method="text" encoding="UTF-8"/>
  <xsl:param name='expr'/>
  <xsl:param name='filename'/>

  <!-- Voor gebruik met xmlmatch -->

  <!-- 
       Bedoeld om te gemakkelijk te parsen met bijvoorbeeld Perl.  Het
       begin van een match is te herkennen aan /^filename=/, het eind
       aan /^-/ De individuele nodes staan tussen '<' en '>'.  Merk op
       dat '<' redundant is om dat iedere node in principe een rel
       heeft en dat deze in de dump vooraan staat.
   -->

  <xsl:variable name="selectedNodes" select="saxon:evaluate($expr)"/>

  <xsl:template match="/">
    <xsl:text>filename=</xsl:text>
    <xsl:value-of select="$filename"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:apply-templates select="/top/sentence|/alpino_ds/sentence"/>
    <xsl:for-each select="$selectedNodes">
      <xsl:text>&lt;&#xA;</xsl:text>
      <xsl:apply-templates select="@*"/>
      <xsl:text>&gt;&#xA;</xsl:text>
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

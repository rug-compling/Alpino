<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:saxon="http://icl.com/saxon"
    extension-element-prefixes="saxon">

  <!-- voor gebruik met xmlmatch -->

  <xsl:output method="text" encoding="iso-8859-1"/>
  <xsl:param name='expr'/>
  <xsl:param name='filename'/>

  <xsl:variable name="selectedNodes" select="saxon:evaluate($expr)"/>

  <xsl:template match="/">
    <xsl:text>%% FILE </xsl:text>
    <xsl:value-of select="$filename"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:for-each select="$selectedNodes">
      <xsl:text>&#xA;</xsl:text>
      <xsl:apply-templates select="story/p"/>
      <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
  </xsl:template>



  <xsl:template match="p">
  <xsl:text>&#xA;</xsl:text> <!-- een newline -->
  <xsl:value-of select="text()"/>
  <xsl:text>&#xA;</xsl:text> <!-- een newline -->
  </xsl:template>


</xsl:stylesheet>

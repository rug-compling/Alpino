<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  

  <xsl:output method="text" encoding="iso-8859-1"/>

  <xsl:template match="/">
    <xsl:apply-templates select="/kranten/DOC"/>
  </xsl:template>

  <xsl:template match="DOC">
    <xsl:value-of select="DOCNO"/>
    <xsl:text>|</xsl:text>
    <xsl:value-of select="HEAD/RU"/>
    <xsl:text>&#xA;</xsl:text> <!-- een newline -->
  </xsl:template>

</xsl:stylesheet>

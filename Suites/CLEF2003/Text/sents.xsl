<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates select="/kranten/DOC"/>
  </xsl:template>

  <xsl:template match="DOC">
    <xsl:for-each select="BODY/LE/P|BODY/TE/P">
      <xsl:text>key|</xsl:text>
      <xsl:value-of select="../../../DOCNO"/>
      <xsl:text>-</xsl:text> <!-- een newline -->
      <xsl:value-of select="position()"/>
      <xsl:text>&#xA;</xsl:text> <!-- een newline -->
      <xsl:value-of select="."/>
      <xsl:text>&#xA;</xsl:text> 
    </xsl:for-each>
  </xsl:template>





</xsl:stylesheet>

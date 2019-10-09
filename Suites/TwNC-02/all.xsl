<?xml version="1.0"?>
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates select="//kranten/artikel"/>
  </xsl:template>

  <xsl:template match="artikel">
    <xsl:for-each select="body/le/p|body/te/p">
      <xsl:text>key|</xsl:text>
      <xsl:value-of select="../../../head/ui"/>
      <xsl:value-of select="../../../head/dat"/>
      <xsl:number count="body" level="any" format="-1"/>
      <xsl:number count="body/le/p|body/te/p" level="any" from="body/he|body/le" format="-1"/>
      <xsl:text>&#xA;</xsl:text> <!-- een newline -->
      <xsl:value-of select="."/>
      <xsl:text>&#xA;</xsl:text> 
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>

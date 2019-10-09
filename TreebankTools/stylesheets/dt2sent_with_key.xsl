<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:str="http://exslt.org/strings"
  xmlns:func="http://exslt.org/functions"
  xmlns:alp="http://let.rug.nl/alpino"
  extension-element-prefixes="alp str func">
  <xsl:import href="alp.basename.xsl"/>

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>
  <xsl:template match="/">
    <xsl:apply-templates select="/top/sentence|/alpino_ds/sentence"/>
  </xsl:template>

  <xsl:template match="sentence">
    <xsl:value-of select="alp:noxml(alp:basename($filename))"/>
    <xsl:text>|</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>
</xsl:text>
  </xsl:template>

</xsl:stylesheet>



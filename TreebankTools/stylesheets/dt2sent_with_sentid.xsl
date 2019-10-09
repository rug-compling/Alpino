<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:str="http://exslt.org/strings"
  xmlns:func="http://exslt.org/functions">
  <xsl:import href="alp.basename.xsl"/>

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates select="/alpino_ds/sentence"/>
  </xsl:template>

  <xsl:template match="sentence">
    <xsl:value-of select="@sentid"/>
    <xsl:text>|</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>
</xsl:text>
  </xsl:template>

</xsl:stylesheet>



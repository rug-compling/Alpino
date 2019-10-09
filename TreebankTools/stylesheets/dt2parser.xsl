<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates select="/alpino_ds/parser"/>
  </xsl:template>

  <xsl:template match="parser">
    <xsl:text>skips: </xsl:text>
    <xsl:value-of select="@skips"/>
    <xsl:text>
</xsl:text>
    <xsl:text>cats: </xsl:text>
    <xsl:value-of select="@cats"/>
    <xsl:text>
</xsl:text>
  </xsl:template>

</xsl:stylesheet>


<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>
  <xsl:template match="/">
    <xsl:for-each select="alpino_ds/metadata/meta">
      <!-- <xsl:text>[</xsl:text> -->
      <!-- <xsl:value-of select="@type"/> -->
      <!-- <xsl:text>] </xsl:text> -->
      <xsl:value-of select="@name"/>
      <xsl:text> = </xsl:text>
      <xsl:value-of select="@value"/>
      <xsl:text>
</xsl:text>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>

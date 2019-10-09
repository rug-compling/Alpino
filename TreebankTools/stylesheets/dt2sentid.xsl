<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates select="/top/sentence|/alpino_ds/sentence"/>
  </xsl:template>

  <xsl:template match="sentence">
    <xsl:value-of select="@sentid"/>
    <xsl:text>
</xsl:text>
  </xsl:template>

</xsl:stylesheet>



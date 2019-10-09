<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- voor gebruik met xmlmatch -->

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:apply-templates select="./top/sentence|./alpino_ds/sentence" mode="print"/>
  </xsl:template>

  <xsl:template match="sentence" mode="print">
    <xsl:value-of select="$filename"/>
    <xsl:text>&#x9;</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>&#xA;</xsl:text> <!-- een newline -->
  </xsl:template>

</xsl:stylesheet>

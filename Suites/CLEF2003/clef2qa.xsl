<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" encoding="iso-8859-1"/>

  <xsl:template match="/">
    <xsl:apply-templates select="/DISEQuA/qa/language[@val='DUT']"/>
  </xsl:template>

  <xsl:template match="question">
    <xsl:text>QUESTION|</xsl:text>
    <xsl:value-of select="../../@cnt"/><xsl:text>|</xsl:text>
    <xsl:value-of select="../../@type"/><xsl:text>|</xsl:text>
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:template>

<xsl:template match="answer">
    <xsl:text>answer(</xsl:text>
    <xsl:value-of select="../../@cnt"/><xsl:text>,</xsl:text>
    <xsl:value-of select="@n"/><xsl:text>,'</xsl:text>
    <xsl:value-of select="@idx"/><xsl:text>',"</xsl:text>
    <xsl:value-of select="normalize-space(.)"/><xsl:text>").</xsl:text>
  </xsl:template>

</xsl:stylesheet>



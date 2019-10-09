<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates select="/top/comments/comment|/alpino_ds/comments/comment"/>
  </xsl:template>

  <xsl:template match="comment">
    <xsl:value-of select="."/>
  <xsl:text>&#xA;</xsl:text>
  </xsl:template>

</xsl:stylesheet>



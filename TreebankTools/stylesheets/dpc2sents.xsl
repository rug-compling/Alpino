<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns="http://www.tei-c.org/ns/1.0">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates select="//text"/>
  </xsl:template>

  <xsl:template match="text">
    <xsl:value-of select="."/> 
  </xsl:template>

<!--   <xsl:template match="text"> -->
<!--     <xsl:value-of select="@xml:id"/> -->
<!--     <xsl:apply-templates select=".//s"/> -->
<!--     <xsl:text> -->
<!-- </xsl:text> -->
<!--   </xsl:template> -->

<!--   <xsl:template match="s"> -->
<!--     <xsl:value-of select="@n"/> -->
<!--     <xsl:text>|</xsl:text> -->
<!--     <xsl:apply-templates select=".//w"/> -->
<!--   </xsl:template> -->

<!--   <xsl:template match="w"> -->
<!--     <xsl:value-of select="."/> -->
<!--     <xsl:text> </xsl:text> -->
<!--   </xsl:template> -->

</xsl:stylesheet>

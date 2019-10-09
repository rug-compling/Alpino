<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- voor gebruik met xmlmatch -->

  <xsl:output method="text"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:value-of select="$filename"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="count(//node[@word])"/>
    <xsl:text>&#xA;</xsl:text> <!-- een newline -->
  </xsl:template>

</xsl:stylesheet>

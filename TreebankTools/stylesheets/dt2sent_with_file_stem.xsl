<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:text>FILE: </xsl:text>
    <xsl:value-of select="$filename"/>
    <xsl:text>&#xA;</xsl:text>

    <xsl:apply-templates select="/top/sentence|/alpino_ds/sentence"/>
    <xsl:text> </xsl:text>

    <xsl:call-template name="print-root"/>
    <xsl:call-template name="print-lemma"/>
    <xsl:text>&#xA;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="sentence">
    <xsl:value-of select="."/>
  </xsl:template>
  
  <xsl:template name="print-root">
    <xsl:for-each select="//node[@root and not(@root=@word)]">
      <xsl:value-of select="@root"/>
      <xsl:text> </xsl:text>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template name="print-lemma">
    <xsl:for-each select="//node[@lemma and not(@lemma=@word) and not(@lemma=@root)]">
      <xsl:value-of select="@lemma"/>
      <xsl:text> </xsl:text>
    </xsl:for-each>
  </xsl:template>
  
</xsl:stylesheet>


      <!-- <xsl:sort select="@begin" data-type="number"/> -->
      <!-- <xsl:if test="position() != last()"> -->
      <!--   <xsl:text> </xsl:text> -->
      <!-- </xsl:if> -->


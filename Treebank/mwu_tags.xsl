<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:str="http://exslt.org/strings"
  xmlns:func="http://exslt.org/functions"
  xmlns:alp="http://let.rug.nl/alpino"
  extension-element-prefixes="alp str func">
  <xsl:import href="alp.basename.xsl"/>

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:call-template name="print-atts"/>
  </xsl:template>

  
  <xsl:template name="print-atts">
    <xsl:for-each select="//node[@cat='mwu']">
      <xsl:call-template name="print-ds"/>      
    </xsl:for-each>
  </xsl:template>


<xsl:template name="print-ds">
  <xsl:for-each select="node[@rel='mwp' and @word and @postag]">
    <xsl:sort select="@begin"  data-type="number"/>
    <xsl:apply-templates select="@word"/>
    <xsl:text> </xsl:text>
  </xsl:for-each>
  <xsl:text>	</xsl:text>
  <xsl:for-each select="node[@rel='mwp' and @word and @postag]">
    <xsl:sort select="@begin"   data-type="number"/>
    <xsl:apply-templates select="@postag"/>
    <xsl:text>:</xsl:text>
    <xsl:apply-templates select="@lemma"/>
    <xsl:text> </xsl:text>
  </xsl:for-each>
  <xsl:text>
</xsl:text>
</xsl:template>

  
 <xsl:template match="@*">
   <xsl:value-of select="."/>
 </xsl:template>

</xsl:stylesheet>



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
    <xsl:for-each select="//node[@lemma]">
      <xsl:sort select="@begin" data-type = "number" />
       <xsl:value-of select="alp:noxml(alp:basename($filename))"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="@begin"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="@lemma"/>
       <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
  </xsl:template>
  
 <xsl:template match="@*">
   <xsl:value-of select="."/>
 </xsl:template>

</xsl:stylesheet>



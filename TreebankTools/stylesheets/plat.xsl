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
    <xsl:text>&lt;sent id=&quot;</xsl:text>
    <xsl:value-of select="alp:noxml(alp:basename($filename))"/>
    <xsl:text>&quot;&gt;
</xsl:text>
    <xsl:call-template name="print-atts"/>
    <xsl:text>&lt;/sent&gt;
</xsl:text>
  </xsl:template>

  
  <xsl:template name="print-atts">
    <xsl:for-each select="//node[@word and @postag]">
      <xsl:sort select="@begin" data-type="number"/>
      <xsl:apply-templates select="@word"/>
      <xsl:text>	</xsl:text>
      <xsl:apply-templates select="@postag"/>
      <xsl:text>	</xsl:text>
      <xsl:apply-templates select="@lemma"/>
      <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
  </xsl:template>
  
 <xsl:template match="@*">
   <xsl:value-of select="."/>
 </xsl:template>

</xsl:stylesheet>



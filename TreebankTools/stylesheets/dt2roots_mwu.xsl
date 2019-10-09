<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:call-template name="print-atts"/>
  </xsl:template>

  
  <xsl:template name="print-atts">
  <xsl:value-of select="$filename"/>
       <xsl:text>|</xsl:text>
    <xsl:for-each select="//node[(@root and not(@rel='mwp')) or @cat='mwu']">
      <xsl:sort select="@begin" data-type="number"/>
       <xsl:apply-templates select="."/>
    </xsl:for-each>
       <xsl:text>
</xsl:text>
  </xsl:template>
  
 <xsl:template match="node[@root]">
   <xsl:value-of select="@root"/>
       <xsl:text> </xsl:text>
 </xsl:template>

 <xsl:template match="node[@cat='mwu']">
       <xsl:text>[ </xsl:text>
    <xsl:for-each select="node[@word]">
       <xsl:apply-templates select="@word"/>
       <xsl:text> </xsl:text>
    </xsl:for-each>
       <xsl:text>] </xsl:text>
 </xsl:template>

</xsl:stylesheet>



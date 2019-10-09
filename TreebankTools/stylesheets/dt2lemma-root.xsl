<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:call-template name="print-atts"/>
  </xsl:template>

  
  <xsl:template name="print-atts">
    <xsl:for-each select="//node[@root and @lemma and not (@root = @lemma)]">
       <xsl:apply-templates select="@lemma"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="@root"/>
       <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
    <xsl:for-each select="//node[@root and @sense and not (@root = @sense or @root=@lemma)]">
       <xsl:apply-templates select="@sense"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="@root"/>
       <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
  </xsl:template>
  
 <xsl:template match="@*">
   <xsl:value-of select="."/>
 </xsl:template>

</xsl:stylesheet>



<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:call-template name="print-atts"/>
  </xsl:template>

  
  <xsl:template name="print-atts">
    <xsl:for-each select="//node[node[1][@word] and node[2][@word] and node[3][@word] and not(node[4])]">
       <xsl:apply-templates select="node[1]/@word"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[2]/@word"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[3]/@word"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[1]/@rel"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[2]/@rel"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[3]/@rel"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[1]/@lemma"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[2]/@lemma"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[3]/@lemma"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[1]/@postag"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[2]/@postag"/>
       <xsl:text>	</xsl:text>
       <xsl:apply-templates select="node[3]/@postag"/>
       <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
  </xsl:template>
  
 <xsl:template match="@*">
   <xsl:value-of select="."/>
 </xsl:template>

</xsl:stylesheet>



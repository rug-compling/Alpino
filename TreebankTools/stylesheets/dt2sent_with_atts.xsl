<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:text>FILE: </xsl:text>
    <xsl:value-of select="$filename"/>
    <xsl:text>&#xA;</xsl:text>

    <xsl:apply-templates select="/top/sentence|/alpino_ds/sentence"/>
    <xsl:text>&#xA;</xsl:text>

    <xsl:call-template name="print-atts"/>
    <xsl:text>&#xA;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="sentence">
    <xsl:value-of select="."/>
  </xsl:template>
  
  <xsl:template name="print-atts">
    <xsl:for-each select="//node">
       <xsl:apply-templates select="@*[not(name() = 'begin')   and 
                                       not(name() = 'end'  )   and
                                       not(name() = 'index')   and
                                       not(name() = 'id'   )   and
                                       not(name() = 'word' )   
                                       ]"/>
    </xsl:for-each>
    <xsl:text> </xsl:text>
  </xsl:template>
  
 <xsl:template match="@*">
   <xsl:value-of select="."/>
   <xsl:text> </xsl:text>
 </xsl:template>

</xsl:stylesheet>



<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:str="http://exslt.org/strings"
                xmlns:func="http://exslt.org/functions"
                xmlns:alp="http://let.rug.nl/alpino"
                extension-element-prefixes="alp str func">


  <!-- 
       om dit stylesheet te gebruiken in andere stylesheets:

       - voeg bovenstaande namespaces toe aan het stylesheet
       - <xsl:import href="alp.basename.xsl"/>

   -->


  <func:function name="alp:basename">
    <xsl:param name="str"/>
    <xsl:variable name="parts" select="str:tokenize($str, '/')"/>
    <func:result select="string( $parts[ last() ] )"/>
  </func:function>

  <func:function name="alp:noextension">
    <xsl:param name="str"/>
    <xsl:variable name="parts" select="str:tokenize($str, '.')"/>
    <func:result select="string( $parts[ 1 ] )"/>
  </func:function>

  <func:function name="alp:noxml">
    <xsl:param name="str"/>
    <func:result select="substring-before($str, '.xml')"/>
  </func:function>


  <!-- test code

  <xsl:template match="/">
    <xsl:value-of select="alp:basename('/home/geertk/iets')"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:value-of select="alp:basename('nog iets')"/>
  </xsl:template>

  -->

</xsl:stylesheet>


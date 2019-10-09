<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="text"/>

  <!-- 

       Kijkt of iedere indexed-node wel een index heeft en vice versa.

       Te gebruiken met xmlmatch zodat de filename param gevuld kan
       worden:
       
           xmlmatch -q / -s stylesheets/index-test.xsl */*.xml

       ("oneigenlijk gebruik" van xmlmatch, maar het werkt...)

       TODO:
       - checken op niet-numerieke/lege indices (gebeurt dat ondertussen 
         m.b.v. de Thistle spec file?)

       Dit stylesheet heeft uitvoer voor "dtview -file-query-pairs"
    -->

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <xsl:apply-templates select="//node[@index]"/>
  </xsl:template>

  <!-- indexed-nodes -->
  <xsl:template match="node[@index and (@pos or @cat)]">
    <xsl:variable name="index">
      <xsl:value-of select="@index"/>
    </xsl:variable>
    <xsl:if test="not(//node[@index[. = $index] and not(@pos or @cat)])">
      <!-- Alles met zo'n index: 'filename //node[@index = $index]' -->
      <xsl:value-of select="$filename"/>
      <xsl:text>&#x9;</xsl:text>
      <xsl:text>//node[@index = </xsl:text>
      <xsl:value-of select="$index"/>
      <xsl:text> ]&#x9;indexed-node without index-node&#xA;</xsl:text>
    </xsl:if>
    <xsl:if test="count(//node[@index = $index and (@pos or @cat)]) > 1">
      <xsl:value-of select="$filename"/>
      <xsl:text>&#x9;</xsl:text>
      <xsl:text>//node[@index = </xsl:text>
      <xsl:value-of select="$index"/>
      <xsl:text> and (@pos or @cat)]&#x9;multiple use of index&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- index-nodes -->
  <xsl:template match="node[@index and not(@pos or @cat)]">
    <xsl:variable name="index">
      <xsl:value-of select="@index"/>
    </xsl:variable>
    <xsl:if test="not(//node[@index[. = $index] and (@pos or @cat)])">
      <!-- Alles met die index: 'filename //node[@index = $index]' -->
      <xsl:value-of select="$filename"/>
      <xsl:text>&#x9;</xsl:text>
      <xsl:text>//node[@index = </xsl:text>
      <xsl:value-of select="$index"/>
      <xsl:text> ]&#x9;index-node without indexed-node&#xA;</xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>

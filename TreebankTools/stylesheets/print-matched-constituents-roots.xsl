<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:saxon="http://icl.com/saxon"
                extension-element-prefixes="saxon">

  <!-- 
       Print de gematchte onderdelen elk op een regel voor zich.
       (inclusief de filenaam)
       Voor gebruik met xmlmatch.
  -->

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name='expr'/>
  <xsl:param name='filename'/>

  <xsl:variable name="selectedNodes" select="saxon:evaluate($expr)"/>
  <!-- <xsl:variable name="selectedNodes" select="$expr"/> -->

  <!-- FIXME: print nu kale bestandsnaam igv. index knoop.
       (heeft geen word-attribuut onder zich) -->
  <xsl:template match="/">
    <xsl:for-each select="$selectedNodes">
      <xsl:value-of select="$filename"/>
      <xsl:text>&#x9;</xsl:text>
      <xsl:for-each select=".//@root">
        <xsl:sort select="../@begin" data-type="number" order="ascending"/>
        <xsl:value-of select="."/>
        <xsl:if test="position() != last()">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:for-each>
      <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>

<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:fo="http://www.w3.org/1999/XSL/Format">
  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>

  <xsl:output encoding="UTF-8" />

  <xsl:param name="fop1.extensions">1</xsl:param>
  <xsl:param name="fop.extensions">0</xsl:param>
  
  <xsl:param name="page.height">11.7in</xsl:param>
  <xsl:param name="page.width">8.3in</xsl:param>

  <xsl:param name="double.sided" select="1"/>

  <xsl:param name="keep.together"></xsl:param>

  <xsl:attribute-set name="formal.object.properties">
    <xsl:attribute name="space-before.minimum">0.5em</xsl:attribute>
    <xsl:attribute name="space-before.optimum">1em</xsl:attribute>
    <xsl:attribute name="space-before.maximum">2em</xsl:attribute>
    <xsl:attribute name="space-after.minimum">0.5em</xsl:attribute>
    <xsl:attribute name="space-after.optimum">1em</xsl:attribute>
    <xsl:attribute name="space-after.maximum">2em</xsl:attribute>
    <xsl:attribute name="keep-together.within-column">auto</xsl:attribute>
  </xsl:attribute-set>
</xsl:stylesheet>

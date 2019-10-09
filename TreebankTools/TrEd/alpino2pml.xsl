<?xml version="1.0" encoding="utf-8"?>
<!-- -*- mode: xsl; coding: utf8; -*- -->
<!-- Author: pajas@ufal.mff.cuni.cz -->

<xsl:stylesheet
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform' 
    xmlns='http://ufal.mff.cuni.cz/pdt/pml/'
    version='1.0'>
  <xsl:output method="xml" encoding="utf-8" indent="yes"/>
  <xsl:strip-space elements="alpino_ds node"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="alpino_ds">
    <alpino_ds_pml>
      <head>
        <schema href="alpino_schema.xml" />
      </head>
      <version><xsl:value-of select="@version"/></version>
      <metadata>
        <xsl:apply-templates select="metadata/meta"/>
      </metadata>
      <xsl:apply-templates select="sentence"/>
      <sentid><xsl:value-of select="sentence/@sentid"/></sentid>
      <xsl:apply-templates select="comments"/>
      <trees>
        <xsl:apply-templates select="node"/>
      </trees>
    </alpino_ds_pml>
  </xsl:template>

  <xsl:template match="meta">
    <xsl:element name="meta">
      <xsl:element name="type">
        <xsl:value-of select="@type"/>
      </xsl:element>
      <xsl:element name="name">
        <xsl:value-of select="@name"/>
      </xsl:element>
      <xsl:element name="value">
        <xsl:value-of select="@value"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <!-- skip ud -->
  <xsl:template match="ud">
  </xsl:template>

  <xsl:template match="*">
    <xsl:element name="{name()}">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>


  <!-- copy these attributes -->
  <!-- all alpino_ds 1.5 attributes except these: begin end id -->
  <!-- not needed:
       buiging
       conjtype
       dial
       genus
       getal
       getal-n
       graad
       lwtype
       naamval
       npagr
       ntype
       numtype
       pdtype
       persoon
       positie
       pt
       pvagr
       pvtijd
       spectype
       status
       vwtype
       vztype
       wvorm
  -->
  <xsl:template match="@aform|@case|@cat|@comparative|@def|@frame|@gen|@iets|@index|@infl|@lcat|@lemma|@mwu_root|@mwu_sense|@neclass|@num|@pb|@per|@personalized|@pos|@postag|@pron|@refl|@rel|@rnum|@root|@sc|@sense|@special|@stype|@tense|@vform|@wh|@wk|@word|@wordid">
    <xsl:copy/> 
  </xsl:template>
  

  <!-- replace @end with wordno -->
  <xsl:template match="@end">
    <!-- only add wordno for word nodes (leaf nodes) -->
    <xsl:if test="../@word">
      <xsl:attribute name="wordno">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>

  <!-- skip any other attributes -->
  <xsl:template match="@*">
  </xsl:template>



</xsl:stylesheet>

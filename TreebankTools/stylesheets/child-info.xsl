<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:param name="filename"/>

  <xsl:template match="/">
    <!--    <xsl:text># rel cat/pos childrel childcat/pos&#xA;</xsl:text> -->
    <xsl:apply-templates select="/top/node/node|/alpino_ds/node/node" mode="child"/>
  </xsl:template>

  <!-- We slaan de indices eerst maar even over -->
  <xsl:template match="node[@cat or @pos]" mode="parent">
    <!--    <xsl:value-of select="$filename"/>
             <xsl:text> </xsl:text>
      -->
    <xsl:choose>
      <xsl:when test="@pos">    <!-- moet vanwege toegevoegde cats -->
        <xsl:text>parentpos=</xsl:text>
        <xsl:value-of select="@pos"/>
      </xsl:when>
      <xsl:otherwise test="@cat">
        <xsl:text>parentcat=</xsl:text>
        <xsl:value-of select="@cat"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:text> parentrel=</xsl:text>
    <xsl:value-of select="@rel"/>
    <xsl:text> </xsl:text>

  </xsl:template>

  <xsl:template match="node[@cat or @pos]" mode="child">
    <xsl:apply-templates select="parent::node()[name() = 'node']" mode="parent"/>
    <xsl:text>childrel=</xsl:text>
    <xsl:value-of select="@rel"/>
    <xsl:choose>
      <xsl:when test="@pos">
        <xsl:text> childpos=</xsl:text>
        <xsl:value-of select="@pos"/>
      </xsl:when>
      <xsl:otherwise test="@cat">
        <xsl:text> childcat=</xsl:text>
        <xsl:value-of select="@cat"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#xA;</xsl:text>
    <xsl:apply-templates select="node" mode="child"/>
  </xsl:template>

  <!-- niks doen in de overige gevallen (indices e.d.) -->
  <xsl:template match="*" mode="child"/>
  <xsl:template match="*" mode="parent"/>

</xsl:stylesheet>

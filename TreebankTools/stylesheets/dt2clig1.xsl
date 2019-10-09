<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:saxon="http://icl.com/saxon"
    xmlns:set="http://exslt.org/sets"
    xmlns:exslt="http://exslt.org/common"
    extension-element-prefixes="saxon set exslt">

  <xsl:output method="text" encoding="UTF-8"/>
  <xsl:param name='expr'>/..</xsl:param> <!-- matcht nooit -->

  <xsl:param name="extended-attributes" select ="1"/>

  <xsl:variable name="newexpr">
    <!-- 
         Wanneer de expr parameter niet tot een nodeset evalueert,
         moeten we zorgen dat de expressie die gebruikt wordt om te
         kijken of er gehighlight moet worden evalueert tot een lege
         nodeset.
     -->
    <xsl:choose>
      <xsl:when test="exslt:object-type(saxon:evaluate($expr)) = 'node-set'">
        <xsl:value-of select="$expr"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>/..</xsl:text>
      </xsl:otherwise>
    </xsl:choose>    
  </xsl:variable>

  <xsl:variable name="selectedNodes" select="saxon:evaluate($newexpr)"/>

  <xsl:template match="/">
    <xsl:text>clig </xsl:text>
    <xsl:text>{stack </xsl:text>
    <xsl:text>{vspace 5}&#xA;</xsl:text>
    <xsl:apply-templates select="/top/node|/alpino_ds/node|/alpino_adt/node"/>
    <xsl:text>{vspace 20}&#xA;</xsl:text>
    <xsl:apply-templates select="/top/comments|/alpino_ds/comments|/alpino_adt/comments"/>
    <xsl:text>{vspace 5}&#xA;</xsl:text>
    <xsl:apply-templates select="/top/sentence|/alpino_ds/sentence|/alpino_adt/sentence"/>
    <xsl:text>}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="node[./node]">       <!-- non-terminal -->
    <xsl:text>{tree {color-area </xsl:text>
    <xsl:call-template name="print-tree-color"/>
    <xsl:text> { drs </xsl:text>

    <xsl:apply-templates select="@rel"/>
    <xsl:apply-templates select="@index"/>
    <xsl:apply-templates select="@cat"/>
    <xsl:if test="$extended-attributes">
      <xsl:apply-templates select="@*[not(name() = 'rel')   and 
                                      not(name() = 'cat')   and 
                                      not(name() = 'begin') and 
                                      not(name() = 'end')   and 
                                      not(name() = 'index')   and 
                                      not(name() = 'id')   and 
                                      not(name() = 'word')  and 
                                      not(name() = 'root') ]"/>
    </xsl:if>
    <xsl:text>}}&#xA;</xsl:text>
    <xsl:apply-templates select="node"/>
    <xsl:text>}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="node[not(./node)]">  <!-- een terminal -->
    <xsl:text>{color-area </xsl:text>
    <xsl:call-template name="print-tree-color"/>
    <xsl:text> {drs </xsl:text>
    <xsl:apply-templates select="@rel"/>
    <xsl:apply-templates select="@index"/>
    <xsl:apply-templates select="@cat"/>
    <xsl:text>{stack </xsl:text>
    <!-- <xsl:apply-templates select="@pos"/> -->
    <xsl:apply-templates select="@lemma"/>
    <xsl:apply-templates select="@postag"/>
    <xsl:if test="$extended-attributes">
      <xsl:apply-templates select="@*[not(name() = 'rel')   and 
                                      not(name() = 'cat')   and 
                                      not(name() = 'pos')   and 
                                      not(name() = 'postag')   and 
                                      not(name() = 'lemma')   and 
                                      not(name() = 'begin') and 
                                      not(name() = 'end')   and 
                                      not(name() = 'index')   and 
                                      not(name() = 'id')   and 
                                      not(name() = 'word')  and 
                                      not(name() = 'root') ]"/>
    </xsl:if>
    <xsl:apply-templates select="@word"/>
    <xsl:text>}}}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="@rel|@cat|@frame|@pos|@postag|@lemma|@root">
    <xsl:text>{color </xsl:text>
    <xsl:call-template name="print-text-color"/>
    <xsl:text> {plain-text {</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>}}}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="@*">
    <xsl:text>{color </xsl:text>
    <xsl:call-template name="print-text-color"/>
    <xsl:text> {plain-text {</xsl:text>
    <xsl:value-of select="name()"/>
    <xsl:text>=</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>}}}&#xA;</xsl:text>
  </xsl:template>


  <xsl:template match="@word" mode="DISABLED">   <!-- apart i.v.m. begin, end -->
    <xsl:text>{color </xsl:text>
    <xsl:call-template name="print-text-color"/>
    <xsl:text> {plain-text {</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>/[</xsl:text>
    <xsl:value-of select="../@begin"/>
    <xsl:text>,</xsl:text>
    <xsl:value-of select="../@end"/>
    <xsl:text>]</xsl:text>
    <xsl:text>}}}&#xA;</xsl:text>
  </xsl:template>


  <xsl:template match="@word">          <!-- apart i.v.m. begin, end -->
    <xsl:text>{color </xsl:text>
    <xsl:call-template name="print-text-color"/>
    
    <xsl:text> {subscript {plain-text {</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>}}</xsl:text>
    <xsl:text> {plain-text { </xsl:text>
    <xsl:value-of select="../@begin"/>
    <xsl:text>}}}}&#xA;</xsl:text>
  </xsl:template>


  <xsl:template match="@index">         <!-- Indices doen we bold -->
    <xsl:text>{color </xsl:text>
    <xsl:call-template name="print-text-color"/>
    <xsl:text> {bold-text </xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>}}&#xA;</xsl:text>
  </xsl:template>


  <xsl:template name="print-tree-color">
      <xsl:choose>
        <!-- <xsl:when test="count($selectedNodes | .)  = count($selectedNodes)">  -->
        <xsl:when test="set:intersection($selectedNodes, .)">
          <xsl:text>yellow</xsl:text>
        </xsl:when>
        <xsl:otherwise>white</xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="print-text-color">
      <xsl:choose>
        <!-- <xsl:when test="count($selectedNodes | .)  = count($selectedNodes)"> -->
        <xsl:when test="set:intersection($selectedNodes, .)">
          <xsl:text>red</xsl:text>
        </xsl:when>
        <xsl:otherwise>black</xsl:otherwise>
      </xsl:choose>
  </xsl:template>


  <xsl:template match="sentence">
    <xsl:text>{color-area white {smallbox {big-plain-text {</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>}}}}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="comment">
    <xsl:text>{vspace 5}&#xA;</xsl:text>
    <xsl:text>{color-area red {smallbox {big-plain-text {</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>}}}}&#xA;</xsl:text>
  </xsl:template>

</xsl:stylesheet>


<?xml version="1.0"?>

<!-- Adapted from Alpino dt2tree for ADTs. -->

<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:svg="http://www.w3.org/2000/svg"
  xmlns:exslt="http://exslt.org/common">
  <xsl:output method="xml" encoding="UTF-8" indent="yes"/>

  <!-- adapted from Jirka Kosek's page at 
       http://www.xml.com/pub/a/2004/09/08/tree.html -->


  <xsl:variable name="xunit" select="35"/>
  <xsl:variable name="yunit" select="40"/>

  <xsl:template match="/">
    <xsl:variable name="layoutTree">
      <xsl:apply-templates select="/alpino_adt/node/node" mode="xml2layout"/>
    </xsl:variable>
    <xsl:call-template name="layout2svg">
      <xsl:with-param name="layout" select="exslt:node-set($layoutTree)"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="comment">
    <xsl:text>comment: </xsl:text>
    <xsl:apply-templates select="text()"/>
    <xsl:text>  </xsl:text>
  </xsl:template>

  <xsl:template match="node[node]" mode="xml2layout">
    <xsl:param name="depth" select="1"/>
    <xsl:variable name="subTree">
      <xsl:apply-templates select="node" mode="xml2layout">
        <xsl:with-param name="depth" select="$depth+1"/>
      </xsl:apply-templates>
    </xsl:variable>
    
    <!-- Add layout attributes to the existing node -->
    <node depth="{$depth}" width="{sum(exslt:node-set($subTree)/node/@width)}">
      <!-- Copy original attributes and content -->
      <xsl:copy-of select="@*"/>
      <xsl:copy-of select="$subTree"/>
    </node>
    
  </xsl:template>
  
  <!-- Add layout attributes to leaf nodes -->
  <xsl:template match="node" mode="xml2layout">
    <xsl:param name="depth" select="1"/>
    <xsl:variable name="label">
      <xsl:choose>
        <xsl:when test = "string-length(@cat) &gt; string-length(@root) 
          and string-length(@cat) &gt; string-length(@rel)">
          <xsl:value-of select="@cat"/>
        </xsl:when>
        <xsl:when test = "string-length(@root) &gt; string-length(@rel) ">
          <xsl:value-of select="@root"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@rel"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <node depth="{$depth}" width="{0.2 + string-length($label) *0.12}">
      <xsl:copy-of select="@*"/>
    </node>
  </xsl:template>
  
  <!-- Convert layout to SVG -->
  <xsl:template name="layout2svg">
    <xsl:param name="layout"/>
    
    <!-- Find depth of the tree -->
    <xsl:variable name = "maxDepth">
      <xsl:for-each select = "$layout//node">
        <xsl:sort select = "@depth" data-type = "number" order = "descending"/>
        <xsl:if test = "position() = 1">
          <xsl:value-of select = "@depth"/>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>       

    <!-- Create SVG wrapper -->
    <svg:svg viewBox = "0 0 {sum($layout/node/@width) * 2 * $xunit + $xunit} {$maxDepth * 2 * $yunit + $yunit}" style="text-anchor:middle">
      <xsl:apply-templates select = "$layout/node" mode = "layout2svg"/>            
    </svg:svg>
  </xsl:template>
        
  <!-- Draw one node -->
  <xsl:template match = "node" mode = "layout2svg">
    <!-- Calculate X coordinate -->
    <xsl:variable name="x" select = "(sum(preceding::node[@depth = current()/@depth or (not(node) 
      and @depth &lt;= current()/@depth)]/@width) + (@width div 2)) * 2 * $xunit"/>
    <!-- Calculate Y coordinate -->
    <xsl:variable name = "y" select = "@depth * 2 * $yunit"/>
    <!-- Draw label of node -->
    <svg:text x = "{$x}" y = "{$y - 30}" font-style="italic">
      <xsl:if test ="@rel != 'top' and @rel != '--'">
      <xsl:value-of select="@rel"/>
      </xsl:if>
    </svg:text>
              
    <svg:text x = "{$x}" y = "{$y - 10}">
      <svg:tspan font-weight="bold" fill="red">
      <xsl:value-of select="@index"/>
      </svg:tspan>
      <xsl:if test = "@index and @cat">
        <xsl:text>:</xsl:text>
      </xsl:if>
      <xsl:if test = "not(@root)">
      <xsl:value-of select="@cat"/>
      </xsl:if>
    </svg:text>

    <xsl:if test = "@root">
      <svg:text x = "{$x}" y = "{$y - 10}">
        <svg:tspan>
        <xsl:value-of select="@root"/>
        </svg:tspan>
      </svg:text>
    </xsl:if>

      <svg:text x = "{$x}" y = "{$y}" font-size="80%">
	<xsl:for-each select="@*">
	  <xsl:if test ="name() != 'id'
			 and name() != 'sense'
			 and name() != 'root'
			 and name() != 'cat'
			 and name() != 'index'
			 and name() != 'rel'
			 and name() != 'depth'
			 and name() != 'mwu_sense'
			 and name() != 'mwu_root'
			 and name() != 'width'"
		  >
            <svg:tspan x = "{$x}" dy="8"><xsl:value-of select="."/></svg:tspan>
	  </xsl:if>
	</xsl:for-each>
      </svg:text>

    
    <!-- Draw connector lines to all sub-nodes -->
    <xsl:for-each select="node">
      <svg:line x1 = "{$x}" y1 = "{$y}"
        x2 = "{(sum(preceding::node[@depth = current()/@depth or (not(node) 
        and @depth &lt;= current()/@depth)]/@width) + (@width div 2)) * 2 * $xunit}"
        y2 = "{@depth * 2 * $yunit - 50}"  stroke="black"/>
     </xsl:for-each>
     <!-- Draw sub-nodes -->
     <xsl:apply-templates select = "node" mode = "layout2svg"/>
  </xsl:template>
              
</xsl:stylesheet>



<?xml version="1.0"?>
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
      <xsl:apply-templates select="/alpino_ds/node" mode="xml2layout"/>
    </xsl:variable>
    <xsl:call-template name="layout2svg">
      <xsl:with-param name="layout" select="exslt:node-set($layoutTree)"/>
      <xsl:with-param name="sentence" select="/alpino_ds/sentence/text()"/>
      <xsl:with-param name="comments">
        <xsl:apply-templates select="/alpino_ds/comments/comment"/>
      </xsl:with-param>
    </xsl:call-template>

   <xsl:text>
   </xsl:text>

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
        <xsl:when test = "@pt and string-length(@pt) &gt; string-length(@word) and string-length(@pt) &gt; string-length(@rel) and string-length(@pt) &gt; string-length(@lemma) and string-length(@pt) &gt; string-length(@sense)">
          <xsl:value-of select="@pt"/>
        </xsl:when>
        <xsl:when test = "not(@pt) and string-length(@pos) &gt; string-length(@word) and string-length(@pos) &gt; string-length(@rel) and string-length(@pos) &gt; string-length(@sense) and string-length(@pos) &gt; string-length(@lemma)">
          <xsl:value-of select="@pos"/>
        </xsl:when>
        <xsl:when test = "string-length(@word) &gt; string-length(@rel) and string-length(@word) &gt; string-length(@lemma) and string-length(@word) &gt; string-length(@sense)">
          <xsl:value-of select="@word"/>
        </xsl:when> 
        <xsl:when test = "string-length(@lemma) &gt; string-length(@rel) and string-length(@lemma) &gt; string-length(@sense)">
          <xsl:value-of select="@lemma"/>
        </xsl:when> 
        <xsl:when test = "string-length(@sense) &gt; string-length(@rel)" >
          <xsl:value-of select="@sense"/>
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
    <xsl:param name="sentence"/>
    <xsl:param name="comments"/>
    
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
    <svg:svg width="{sum($layout/node/@width) * 2 * $xunit + $xunit}"  height="{$maxDepth * 2 * $yunit + 4 * $yunit}" viewBox = "{-1 * $xunit} {-1 * $yunit} {sum($layout/node/@width) * 2 * $xunit + $xunit} {$maxDepth * 2 * $yunit + 4 * $yunit}" >
      <xsl:apply-templates select = "$layout/node" mode = "layout2svg"/>
          
      <svg:text  x="10" y="10" text-anchor="start">
        <xsl:value-of select="$sentence"/>
      </svg:text>
<!--       <text x="10" y="{$maxDepth * 2 * $yunit + $yunit + $yunit}" text-anchor="start"> -->
<!--         <xsl:value-of select="$comments"/> -->
<!--       </text> -->
            
    </svg:svg>
  </xsl:template>
        


<!--Here is a template from Jeni Tennison - works great for escaping all sorts of things. Just call it from wherever you need it. -->

<xsl:template name="escape-apos">
 <xsl:param name="string"/>
<xsl:variable name="apos" select='"&apos;"' />
<xsl:choose>
 <xsl:when test='contains($string, $apos)'>
  <xsl:value-of select="substring-before($string,$apos)" />
	<xsl:text>\'</xsl:text>
	<xsl:call-template name="escape-apos">
	 <xsl:with-param name="string"
          select="substring-after($string, $apos)" />
	</xsl:call-template>
 </xsl:when>
 <xsl:otherwise>
  <xsl:value-of select="$string" />
 </xsl:otherwise>
</xsl:choose>
</xsl:template>


  <!-- Draw one node -->
  <xsl:template match = "node" mode = "layout2svg">
    <!-- Calculate X coordinate -->
    <xsl:variable name="x" select = "(sum(preceding::node[@depth = current()/@depth or (not(node) 
      and @depth &lt;= current()/@depth)]/@width) + (@width div 2)) * 2 * $xunit"/>
    <!-- Calculate Y coordinate -->
    <xsl:variable name = "y" select = "@depth * 2 * $yunit"/>
    <!-- Draw label of node -->
    <svg:text x = "{$x}" y = "{$y - 30}" font-style="italic" text-anchor="middle" fill="blue" >
      <xsl:if test ="@rel != 'top'">
      <xsl:value-of select="@rel"/>
      </xsl:if>
    </svg:text>
              
    <xsl:variable name="postag">
     <xsl:if test="@postag">
        <xsl:text>postag=</xsl:text>
     <xsl:call-template  name="escape-apos">
      <xsl:with-param name="string" select="@postag"/>
     </xsl:call-template>
     </xsl:if>
     <xsl:if test="@postag and @frame">
        <xsl:text>  </xsl:text>
     </xsl:if>
     <xsl:if test="@frame">
        <xsl:text>frame=</xsl:text>
     <xsl:call-template  name="escape-apos">
      <xsl:with-param name="string" select="@frame"/>
     </xsl:call-template>
     </xsl:if>
    </xsl:variable>

    <xsl:choose>
    <xsl:when test="@postag or @frame">


    <svg:text x = "{$x}" y = "{$y - 10}" text-anchor="middle" onmouseover="tooltip.show('{$postag}')" onmouseout="tooltip.hide()">
      <svg:tspan font-weight="bold" fill="red">
      <xsl:value-of select="@index"/>
      </svg:tspan>
      <xsl:if test = "@index and (@cat|@pt|@pos)">
        <xsl:text>:</xsl:text>
      </xsl:if>
      <xsl:choose>
      <xsl:when test = "@cat">
      <xsl:value-of select="@cat"/>
      </xsl:when>
      <xsl:when test = "@pt">
      <xsl:value-of select="@pt"/>
      </xsl:when>
      <xsl:otherwise>
      <xsl:value-of select="@pos"/>	
      </xsl:otherwise>
      </xsl:choose>
    </svg:text>


      </xsl:when>

      <xsl:otherwise>
    <svg:text x = "{$x}" y = "{$y - 10}" text-anchor="middle">
      <svg:tspan font-weight="bold" fill="red">
      <xsl:value-of select="@index"/>
      </svg:tspan>
      <xsl:if test = "@index and (@cat|@pt|@pos)">
        <xsl:text>:</xsl:text>
      </xsl:if>
      <xsl:choose>
      <xsl:when test = "@cat">
      <xsl:value-of select="@cat"/>
      </xsl:when>
      <xsl:when test = "@pt">
      <xsl:value-of select="@pt"/>
      </xsl:when>
      <xsl:otherwise>
      <xsl:value-of select="@pos"/>	
      </xsl:otherwise>
      </xsl:choose>
    </svg:text>


      </xsl:otherwise>

    </xsl:choose>



    <xsl:if test = "@word">
      <svg:text x = "{$x}" y = "{$y + 10}" text-anchor="middle">
        <svg:tspan>
        <xsl:value-of select="@word"/>
        </svg:tspan>
      </svg:text>
    </xsl:if>

    <xsl:if test = "@lemma and not(@lemma=@word)">
      <svg:text x = "{$x}" y = "{$y + 25}" text-anchor="middle">
        <svg:tspan>
        <xsl:value-of select="@lemma"/>
        </svg:tspan>
      </svg:text>
    </xsl:if>

    <xsl:if test = "@sense and not(@lemma=@sense) and not(@sense=@word)">
      <svg:text x = "{$x}" y = "{$y + 40}" text-anchor="middle">
        <svg:tspan>
        <xsl:value-of select="@sense"/>
        </svg:tspan>
      </svg:text>
    </xsl:if>

    <!-- Draw connector lines to all sub-nodes -->
    <xsl:for-each select="node">
      <svg:line x1 = "{$x}" y1 = "{$y}"
        x2 = "{(sum(preceding::node[@depth = current()/@depth or (not(node) 
        and @depth &lt;= current()/@depth)]/@width) + (@width div 2)) * 2 * $xunit}"
        y2 = "{@depth * 2 * $yunit - 50}"  style="stroke:#006600;"/>
     </xsl:for-each>
     <!-- Draw sub-nodes -->
     <xsl:apply-templates select = "node" mode = "layout2svg"/>
  </xsl:template>
              
</xsl:stylesheet>



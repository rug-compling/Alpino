<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:include href="replace.xsl"/>

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:text>

\documentclass{article}
\setlength{\textwidth}{20in}\setlength{\textheight}{20in}
\usepackage[latin1]{inputenc}
\usepackage{rtrees}
\usepackage{pslatex}
\pagestyle{empty}
\begin{document}
\begin{tree}
\psset{levelsep=*1.15cm}
\psset{treesep=0.75cm}
% \psset{treefit=tight}
% \psset{linecolor=red}
% \psset{linewidth=1pt}
\setlength{\fboxsep}{1pt}
%\setlength{\fboxrule}{0.2pt}

    </xsl:text>
    <xsl:apply-templates select="/top/node|/alpino_ds/node"/>
    <xsl:text>

\end{tree}
\end{document}

    </xsl:text>
  </xsl:template>


  <xsl:template match="node[./node]">       <!-- non-terminal -->
    <xsl:text>
\br{
\lf{\fbox{
\begin{tabular}{@{}c@{~}}
{\sf </xsl:text><xsl:value-of select="@rel"/><xsl:text>}\\
</xsl:text><xsl:choose><xsl:when test="@index">
<xsl:text>{\bf </xsl:text><xsl:value-of select="@index"/><xsl:text>}\\
</xsl:text></xsl:when></xsl:choose>
<xsl:value-of select="@cat"/><xsl:text>\\
\end{tabular}}}
}{</xsl:text>
<xsl:apply-templates select="node"/>
<xsl:text>}</xsl:text>
  </xsl:template>


  <xsl:template match="node[not(./node) and @index and not(@pos)]">  
    <xsl:text>
\lf{\fbox{
\begin{tabular}{@{}c@{~}}
{\sf </xsl:text><xsl:value-of select="@rel"/><xsl:text>}\\
{\bf </xsl:text><xsl:value-of select="@index"/><xsl:text>}\\
\end{tabular}}} </xsl:text>
  </xsl:template>

  <xsl:template match="node[not(./node) and @cat and not(@pos)]">  
    <xsl:text>
\lf{\fbox{
\begin{tabular}{@{}c@{~}}
{\sf </xsl:text><xsl:value-of select="@rel"/><xsl:text>}\\
</xsl:text><xsl:value-of select="@cat"/><xsl:text>\\
\end{tabular}}} </xsl:text>
  </xsl:template>



  <xsl:template match="node[not(./node) and @pos]">  <!-- een terminal -->
    <xsl:text>
\lf{\fbox{
\begin{tabular}{@{}c@{~}}
{\sf </xsl:text><xsl:value-of select="@rel"/><xsl:text>}\\
</xsl:text><xsl:choose><xsl:when test="@index">
<xsl:text>{\bf </xsl:text><xsl:value-of select="@index"/><xsl:text>}\\
</xsl:text></xsl:when></xsl:choose>
<xsl:text>{\footnotesize\em </xsl:text>
    <xsl:call-template name="replace-substring">
      <xsl:with-param name="original">
        <xsl:call-template name="replace-substring">
          <xsl:with-param name="original" select="@root"/>
          <xsl:with-param name="substring" select="'_'"/>
          <xsl:with-param name="replacement" select="'\_'"/>
        </xsl:call-template> 
      </xsl:with-param>
      <xsl:with-param name="substring" select="'&amp;'"/>
      <xsl:with-param name="replacement" select="'\&amp;'"/>
    </xsl:call-template> 

    <xsl:text>$_{</xsl:text><xsl:value-of select="@begin"/>
<xsl:text>}$}\\\end{tabular}}} </xsl:text>
  </xsl:template>

</xsl:stylesheet>



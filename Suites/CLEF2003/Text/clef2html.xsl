<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    
  <xsl:template match="/">

<html>
<head>
  <title>CLEF corpus</title>
</head>

<body bgcolor="#FFFFEC">
  <font face="Arial, Helvetica, sans-serif" size="7" color="#0000AA">	
  <ul>
  <p align="center">	
  <u>
        CLEF corpus
  </u>
  </p>
  </ul>
  </font>
 <ul>
  <font face="Arial, Helvetica, sans-serif" size="3">
  <table width="40%" border="0">
    <tr> 
      <td width="10%"><font color="navy"><b>Type:</b></font></td>
      <td width="90%"><font color="navy"><b>Kranten</b></font></td>
    </tr>
    <tr> 
      <td width="10%"><font color="navy"><b>Naam:</b></font></td>
      <td width="90%"><font color="navy"><b><xsl:value-of select="kranten//COP"/></b></font></td>
    </tr>
    <tr> 
      <td width="10%"><font color="navy"><b>Datum:</b></font></td>
      <td width="90%"><font color="navy"><b><xsl:value-of select="kranten//DAT"/></b></font></td>
    </tr>
  </table>
  </font>  
  <br/>
  <hr/> 
  <xsl:for-each select="kranten/DOC">
  <font face="Arial, Helvetica, sans-serif">
  <table width="90%" border="0">
    <tr> 
      <td><font size="2" color="red">Doc-id: <b><xsl:value-of select="DOCID"/></b></font></td>
      <td><font size="2" color="red">Sectie: <xsl:value-of select="INFO/HTR"/>
      (
      <xsl:value-of select="HEAD/RU"/>
      )
      </font>
      </td>
      <td><font size="2" color="red">Auteur: <xsl:value-of select="INFO/BY"/></font></td>
    </tr>
  </table>
  </font>
  <br/>
   <b><font face="Arial, Helvetica, sans-serif" size="3" color="navy">
   <xsl:value-of select="BODY//TI"/>
   </font></b>
   <br/>
   <br/>
   <b><font face="Arial, Helvetica, sans-serif" size="2" color="black">
        <xsl:for-each select="BODY//LE/P">
    <p>
    <xsl:value-of select="."/>
    </p>
        </xsl:for-each>
   </font></b>
    <font face="Arial, Helvetica, sans-serif" size="1" color="black">   
        <xsl:for-each select="BODY//TE/P">
    <p>
    <xsl:value-of select="."/>
    </p>
        </xsl:for-each>
    </font>

   <hr/>
  </xsl:for-each>

  <br /> 
</ul>
</body>

</html>

</xsl:template>



</xsl:stylesheet> 


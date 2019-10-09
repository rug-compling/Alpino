<?xml version="1.0"?>
<pmlbackend
  xmlns="http://ufal.mff.cuni.cz/pdt/pml/"
  xmlns:xi="http://www.w3.org/2001/XInclude">
  <head>
    <schema href="pmlbackend_conf_schema.xml"/>
  </head>
  <transform_map>
    <!-- 
      Conversion from Alpino XML format.
    -->
    <transform id="alpino" test="alpino_ds[starts-with(@version, '1.')]">
      <in type="xslt" href="alpino2pml.xsl"/>
      <out type="xslt" href="pml2alpino.xsl"/>
    </transform>
  </transform_map>
</pmlbackend>

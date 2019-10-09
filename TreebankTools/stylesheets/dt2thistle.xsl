<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <!-- het compacte formaat ==> thistle -->

  <xsl:output method="xml" 
              indent="yes"
              encoding="UTF-8"
              omit-xml-declaration="yes"
              doctype-system="cgn_dt.dtd"/>

  <xsl:template match="/">
    <diagrams>
      <diagram>
        <top_type>
          <xsl:apply-templates select="top|alpino_ds"/>
        </top_type>
      </diagram>
    </diagrams>
  </xsl:template>

  <xsl:template match="top|alpino_ds">
    <xsl:apply-templates select="sentence"/>
    <xsl:apply-templates select="comments"/>
    <tree_x_top>
      <xsl:apply-templates select="node"/>
    </tree_x_top>
  </xsl:template>

  <xsl:template match="sentence">
    <sentence_x_top>
      <sentence_type>
        <sentence_x_sentence>
          <xsl:value-of select="."/>
        </sentence_x_sentence>
      </sentence_type>
    </sentence_x_top>
  </xsl:template>

  <xsl:template match="comments">
    <comments_x_top>
      <xsl:apply-templates select="comment"/>
    </comments_x_top>
  </xsl:template>

  <xsl:template match="comment">
    <comments_type>
      <comments_x_comments>
        <xsl:value-of select="."/>
      </comments_x_comments>
    </comments_type>
  </xsl:template>


  <!-- OK, nu kunnen we bezig met de tree en leaf types 

       resumerende (zie ook thistle2dt.xsl):
       - het geheel staat binnen een tree_x_top
       - we hebben dan of een `leaf_type' of een `tree_type' 
  -->


  <xsl:template match="node[node]"> <!-- Een tree-type -->
    <!-- Een tree type bestaat uit 
          - een mother_x_tree
          - een daughters_x_tree (die weer bestaat uit leaf en tree types)
         
          - Opbouw mother_x_tree:

            mother_x_tree/pnode_type/
                                  pcategory_x_pnode 
                                        indexed_cat_type|cat_type
                                  deprel_x_pnode

         grep '<node rel=.*/>$' /tmp/cdb-nieuwestijl2/* | grep -v pos |grep -v index
         /tmp/cdb-nieuwestijl2/1084:      <node rel="mod" cat="pp"/>
         /tmp/cdb-nieuwestijl2/1304:          <node rel=""/>
         /tmp/cdb-nieuwestijl2/135:      <node rel="mod" cat="pp"/>
         /tmp/cdb-nieuwestijl2/1784:      <node rel="mod" cat="smain"/>
         
         Deze check komt overeen met '//node[not(node) and not(@pos) and not(@index)]'
         ==> geeft matches!
         
         Of bomen niet correct of selectiecriterium niet correct.  Dit zou
         betekenen dat op [cat and not(pos)] onderscheid gemaakt moet worden.
         
         Het kan idd voorkomen in een "lege" dependency tree.
         FIXME: het criterium is dus eigenlijk niet goed
         N.B. Het probleem is ondervangen door bij de bladeren een
         extra regel toe te voegen (zie verderop).
         -->
    <tree_type>
      <mother_x_tree>
        <pnode_type>

          <deprel_x_pnode>
            <deprel_type>
              <rel_x_deprel>
                <xsl:value-of select="@rel"/>
              </rel_x_deprel>
            </deprel_type>
          </deprel_x_pnode>

          <xsl:choose>

            <!-- Zonder index -->
            <xsl:when test="not(@index)">
              <pcategory_x_pnode>
                <cat_type>
                  <cat_x_cat><xsl:value-of select="@cat"/></cat_x_cat>
                </cat_type>
              </pcategory_x_pnode>
            </xsl:when>

            <!-- Met een index: -->
            <xsl:otherwise>
              <pcategory_x_pnode>
                <indexed_cat_type>
                  <cat_x_indexed_cat>
                    <cat_type>
                      <cat_x_cat>
                        <xsl:value-of select="@cat"/>
                      </cat_x_cat>
                    </cat_type>
                  </cat_x_indexed_cat>
                  <index_x_indexed_cat>
                    <index_type>
                      <index_x_index>
                        <xsl:value-of select="@index"/>
                      </index_x_index>
                    </index_type>
                  </index_x_indexed_cat>
                </indexed_cat_type>
              </pcategory_x_pnode>
            </xsl:otherwise>

          </xsl:choose>

        </pnode_type>
      </mother_x_tree>

      <daughters_x_tree>
        <xsl:apply-templates select="node"/>
      </daughters_x_tree>
    </tree_type>
  </xsl:template>

  <!-- De bladeren -->
  <xsl:template match="node[not(node)]">
    <leaf_type>
      <tcategory_x_leaf>
        
        <xsl:choose>
          <!-- Eerst de knopen zonder index -->
          <xsl:when test="not(@index)">
            <tnode_type>
              <pos_x_tnode>
                <pos_type>
                  <pos_x_pos>
                    <xsl:value-of select="@pos"/>
                  </pos_x_pos>
                </pos_type>
              </pos_x_tnode>
              <word_x_tnode>
                <word_type>
                  <lex_x_word>
                    <xsl:value-of select="@root"/>
                    <xsl:text>/[</xsl:text>
                    <xsl:value-of select="@begin"/>
                    <xsl:text>,</xsl:text>
                    <xsl:value-of select="@end"/>
                    <xsl:text>]</xsl:text>
                  </lex_x_word>             
                </word_type>
              </word_x_tnode>
            </tnode_type>
          </xsl:when>

          <!-- Dan de indexed-nodes -->
          <xsl:when test="@pos and @index">
            <indexed_tnode_type>
              <index_x_indexed_tnode>
                <index_type>
                  <index_x_index>
                    <xsl:value-of select="@index"/>
                  </index_x_index>
                </index_type>
              </index_x_indexed_tnode>
              <tnode_x_indexed_tnode>
                <tnode_type>
                  <pos_x_tnode>
                    <pos_type>
                      <pos_x_pos>
                        <xsl:value-of select="@pos"/>
                      </pos_x_pos>
                    </pos_type>
                  </pos_x_tnode>
                  <word_x_tnode>
                    <word_type>
                      <lex_x_word>
                        <xsl:value-of select="@root"/>
                        <xsl:text>/[</xsl:text>
                        <xsl:value-of select="@begin"/>
                        <xsl:text>,</xsl:text>
                        <xsl:value-of select="@end"/>
                        <xsl:text>]</xsl:text>
                      </lex_x_word>             
                    </word_type>
                  </word_x_tnode>
                </tnode_type>
              </tnode_x_indexed_tnode>
            </indexed_tnode_type>
          </xsl:when>

          <!-- En dan de kale indices (index-nodes) -->
          <xsl:otherwise>
            <index_type>
              <index_x_index>
                <xsl:value-of select="@index"/>
              </index_x_index>
            </index_type>
          </xsl:otherwise>
        </xsl:choose>
        
      </tcategory_x_leaf>
      <deprel_x_leaf>
        <deprel_type>
          <rel_x_deprel>
            <xsl:value-of select="@rel"/>
          </rel_x_deprel>
        </deprel_type>
      </deprel_x_leaf>
    </leaf_type>
  </xsl:template>

  <!-- Het randgeval: de lege dependency 

       Het is een tree_type zonder kinderen, dus wel een blad. -->

  <xsl:template match="node[not(node) and @cat and not(@pos)]">
    <tree_type>
      <!-- De volgende knoop kan door een xslt-processor afgekort
           worden tot <daughters_x_tree/>, wat weer niet gesnapt
           wordt door Thistle -->
      <daughters_x_tree>
        <!-- Workaround:
             We forceren een expliciete sluittag door een spatie toe
             te voegen.  De knoop heeft nu een text-node als kind en
             zal niet vereenvoudigd worden.  De whitespace zal daarentegen
             weer genegeerd worden door Thistle. -->
        <xsl:text> </xsl:text>
      </daughters_x_tree>
      <mother_x_tree>
        <pnode_type>
          <deprel_x_pnode>
            <deprel_type>
              <rel_x_deprel>
                <xsl:value-of select="@rel"/>
              </rel_x_deprel>
            </deprel_type>
          </deprel_x_pnode>
          <pcategory_x_pnode>
            <cat_type>
              <cat_x_cat>
                <xsl:value-of select="@cat"/>
              </cat_x_cat>
            </cat_type>
          </pcategory_x_pnode>
        </pnode_type>
      </mother_x_tree>
    </tree_type>
  </xsl:template>

</xsl:stylesheet>

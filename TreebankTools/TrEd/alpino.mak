# -*- mode:cperl; cperl-indent-level:4 -*-

package Alpino;

=pod

=head1 Alpino

Alpino (F<alpino.mak>) - implements macros for editing Alpino dependency trees

=over 4

=cut

import TredMacro;
import PML;


sub get_value_line_hook
{
    my ($fsfile,$treeNo)=@_;
    return unless $fsfile;
    my $tree = $fsfile->tree($treeNo);
    my @nodes = GetNodes($tree);

    # sort @nodes array to sentence order

    my @wordnodes = ();

    for my $node (@nodes) {
        if (defined($node->{word})) {
            push(@wordnodes, $node);
        }
    }
    @wordnodes = sort {$a->{wordno} <=> $b->{wordno}} @wordnodes;


    # now we return an arrayref of [ string, tag ] pairs,
    # if tag is a FSNode, than the string is clickable
    # but you can also use tags like '-foreground=>red' to set colors
    # etc.

    # [FileMetaData('pml_root')->{sentence}."\n", 'sentence'],

    return [
            #[FileMetaData('pml_root')->{sentence}."\n", 'sentence'],
            map { [ $_->{word}, $_ ], [ " ", 'space' ] }  @wordnodes
           ];
}


# display comments in the status line
sub get_status_line_hook
{
    return unless $root;

    my $sequence = FileMetaData('pml_root')->{comments};
    my $comment_string;
    if ($sequence) {
        my @comments = FileMetaData('pml_root')->{comments}->values();
        $comment_string = join('    ', @comments);
    } else {
        $comment_string = '';
    }
    return [ [$comment_string, "comment" ], ['comment' => [-foreground => 'red'] ] ];
}


# #bind create_new_rbrother to Ctrl+Shift+Right menu New right brother node
# #bind create_new_lbrother to Ctrl+Shift+Left menu New left brother node

#bind create_new_son to Ctrl+Shift+Down menu New child node
#bind create_new_son to Ctrl+Alt+c menu New child node

#bind create_new_parent to Ctrl+Shift+Up menu New parent node
#bind create_new_parent to Ctrl+Alt+p menu New parent node

## FIXME??:  things break when we don't set the type here
sub create_new_rbrother
{
    my $node = myTredMacro::NewRBrother();
    my $schema = PML::Schema();
    $node->set_type_by_name($schema, 'node.type');
    $node->{'#name'} = 'node';
}

sub create_new_lbrother
{
    my $node = TredMacro::NewLBrother();
    my $schema = PML::Schema();
    $node->set_type_by_name($schema, 'node.type');
    $node->{'#name'} = 'node';
}

sub create_new_son
{
    # don't add children to leaf nodes (i.e. with a "word" attribute)
    if (defined($this->{word})) {
        ChangingFile(0);
        return;
    }

    my $node = TredMacro::NewSon();
    my $schema = PML::Schema();
    $node->set_type_by_name($schema, 'node.type');
    $node->{'#name'} = 'node';
}


=item create_new_parent()

The create_new_parent() macro creates a new parent node for the
current node.

The I<rel> attribute is initialized with the I<rel> attribute of the
current node.  If the current node has an I<index> attribute, this I<index>
attribute is moved to the new parent node.

=cut

sub create_new_parent
{
    # create a new left brother first and copy yourself there:
    # workaround to fix node shifting problem.

    my $node = TredMacro::NewLBrother($this);
    my $schema = PML::Schema();
    $node->set_type_by_name($schema, 'node.type');
    $node->{'#name'} = 'node';

    # initialize the parent's rel with the current rel
    $node->{rel} = $this->{rel};

    # often that means the current rel should be become head
    $this->{rel} = 'hd';
    if (defined($this->{pt}) && $this->{pt} eq 'name')
      { $node->{cat} = 'np' }
    if (defined($this->{pt}) && $this->{pt} eq 'noun')
      { $node->{cat} = 'np' }
    if (defined($this->{pt}) && $this->{pt} eq 'adj')
      { $node->{cat} = 'ap' }
    if (defined($this->{pt}) && $this->{pt} eq 'bw')
      { $node->{cat} = 'advp' }
    if (defined($this->{pt}) && $this->{pt} eq 'lid')
      { $node->{cat} = 'detp' }


    # if this node has an index, move the index to the new parent
    if (defined($this->{index})) {
        $node->{index} = $this->{index};
        delete $this->{index};
    }

    TredMacro::PasteNode($this,$node);
}


=item Flatten()

The Flatten() macro removes an intermediate node from a dependency tree.
The children of the current node are copied to the parent node.
After that the current node is removed.

If the current node has only one child node, its I<rel> and I<index>
attributes (if present) are copied to this child node.

=cut


#bind Flatten to Ctrl+Alt+f menu Flatten
sub Flatten
{
    # kopieer de kinderen naar de parent en wis de huidige node

    if (!$this->parent) {
        ChangingFile(0);
        return;
    }

    my @children;

    @children = $this->children;

    if (!@children) {
        ChangingFile(0);
        return;
    }

    # als we maar een kind hebben willen we dat het kind
    # de relatie krijgt van de ouderknoop

    if (@children == 1) {
        $children[0]->{rel} = $this->{rel};
        $children[0]->{index} = $this->{index};
    }


    foreach my $kid (@children) {
        TredMacro::PasteNodeBefore($kid, $this);
    }
    CutNode($this);
    ChangingFile(1);
}

sub RecursiveFlatten {
    my ($node) = @_;
    if (!$node -> parent) {
        ChangingFile(0);
        return;
    }
    my @children;
    @children = $node->children;

    foreach my $kid (@children) {
        RecursiveFlatten($kid)
    }

    @children = $node->children;
    foreach my $kid (@children) {
        TredMacro::PasteNodeBefore($kid, $node);
    }
    if (@children) { CutNode($node) };
    ChangingFile(1);


}

=item MwuFlatten()

The MwuFlatten macro flattens all dominated nodes into a single 
multi-word-unit structure.

=cut
#bind MwuFlatten to Ctrl+Alt+m menu MwuFlatten


sub MwuFlatten {

    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }

    my @children;
    @children = $this->children;


    foreach my $kid (@children) {
        RecursiveFlatten($kid);
    }


    @children = $this->children;

    foreach my $kid (@children) {
        $kid->{rel} = "mwp";
#	if ($kid->{postag}) {
#	    $kid->{postag} = "SPEC(deeleigen)"
#	}
	if ($kid->{lemma}) {
	    $kid->{lemma} = $kid->{word}
	}
	if ($kid->{root}) {
	    $kid->{root} = $kid->{word}
	}
#	if ($kid->{pos}) {
#	    $kid->{pos} = "name"
#	}
    }

    if (@children) { $this->{cat} = "mwu" };

}

=item MwuFlattenSpec()

The MwuFlatten macro flattens all dominated nodes into a single 
multi-word-unit structure where each word has postag SPEC(deeleigen)

=cut
#bind MwuFlattenSpec to m menu MwuFlattenSpec



sub MwuFlattenSpec {

    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }

    my @children;
    @children = $this->children;


    foreach my $kid (@children) {
        RecursiveFlatten($kid);
    }


    @children = $this->children;

    foreach my $kid (@children) {
        $kid->{rel} = "mwp";
	if ($kid->{postag}) {
	    $kid->{postag} = "SPEC(deeleigen)"
	}
	if ($kid->{lemma}) {
	    $kid->{lemma} = $kid->{word}
	}
	if ($kid->{root}) {
	    $kid->{root} = $kid->{word}
	}
	if ($kid->{pos}) {
	    $kid->{pos} = "name"
	}
    }

    if (@children) { $this->{cat} = "mwu" };

}

=item MwuFlattenVreemd()

The MwuFlattenVreemd macro flattens all dominated nodes into a single 
multi-word-unit structure where each word has postag SPEC(vreemd)

=cut
#bind MwuFlattenVreemd to v menu MwuFlattenVreemd



sub MwuFlattenVreemd {

    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }

    my @children;
    @children = $this->children;


    foreach my $kid (@children) {
        RecursiveFlatten($kid);
    }


    @children = $this->children;

    foreach my $kid (@children) {
        $kid->{rel} = "mwp";
	if ($kid->{postag}) {
	    $kid->{postag} = "SPEC(vreemd)"
	}
	if ($kid->{lemma}) {
	    $kid->{lemma} = $kid->{word}
	}
	if ($kid->{root}) {
	    $kid->{root} = $kid->{word}
	}
	if ($kid->{pos}) {
	    $kid->{pos} = "name"
	}
    }

    if (@children) { $this->{cat} = "mwu" };

}

=item DpDp()

The DpDp macro changes the local tree into DP-DP structure where
each daughter has relation DP and the mother node has category DU

=cut
#bind DpDp to alt+d menu DpDp

sub DpDp  {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }

    my @children;
    @children = $this->children;

    foreach my $kid (@children) {
        $kid->{rel} = "dp";
    }

    if (@children) { $this->{cat} = "du" };
}

=item Mod()
The Mod macro changes the local relation to MOD
=cut
#bind Mod to M menu Relation MOD
sub Mod {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "mod";
}

=item Ld()
The Ld macro changes the local relation to LD
=cut
#bind Ld to L menu Relation LD
sub Ld {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "ld";
}


=item Subj()
The Subj macro changes the local relation to SU
=cut
#bind Subj to S menu Relation SU
sub Subj {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "su";
}

=item Obj1()
The Obj1 macro changes the local relation to OBJ1
=cut
#bind Obj1 to O menu Relation OBJ1
sub Obj1 {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "obj1";
}

=item Pc()
The Pc macro changes the local relation to PC
=cut
#bind Pc to P menu Relation PC
sub Pc {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "pc";
}

=item Svp()
The Svp macro changes the local relation to SVP
=cut
#bind Svp to F menu Relation Svp
sub Svp {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "svp";
}

=item Hd()
The Hd macro changes the local relation to HD
=cut
#bind Hd to H menu Relation Hd
sub Hd {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "hd";
}

=item Predc()
The Predc macro changes the local relation to PREDC
=cut
#bind Predc to E menu Relation Predc
sub Predc {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "predc";
}

=item Det()
The Det macro changes the local relation to DET
=cut
#bind Det to D menu Relation Det
sub Det {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "det";
}

=item App()
The App macro changes the local relation to APP
=cut
#bind App to A menu Relation App
sub App {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "app";
}

=item Vc()
The Vc macro changes the local relation to VC
=cut
#bind Vc to V menu Relation Vc
sub Vc {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "vc";
}

=item Nucl()
The Nucl macro changes the local relation to NUCL
=cut
#bind Nucl to N menu Relation Nucl
sub Nucl {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "nucl";
}

=item Tag()
The Tag macro changes the local relation to TAG
=cut
#bind Tag to T menu Relation Tag
sub Tag {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "tag";
}

=item Body()
The Body macro changes the local relation to BODY
=cut
#bind Body to B menu Relation Body
sub Body {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "body";
}


=item PosTag()
The PosTag macro changes the local postag to its most frequent confusable
=cut
#bind PosTag to X menu Relation PosTag
sub PosTag {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }

    if    ($this->{postag} eq "WW(inf,vrij,zonder)") {
	$this->{postag}="WW(pv,tgw,mv)";
    } 
    elsif ($this->{postag} eq "WW(pv,tgw,mv)") {
	$this->{postag}="WW(inf,vrij,zonder)";
    } 
    elsif ($this->{postag} eq "WW(pv,tgw,met-t)"){
	$this->{postag}="WW(vd,vrij,zonder)";
    }
    elsif ($this->{postag} eq "WW(vd,vrij,zonder)"){
	$this->{postag}="WW(pv,tgw,met-t)";
    }
    elsif ($this->{postag} eq "VNW(pers,pron,nomin,red,2v,ev)"){
	$this->{postag}="VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)";
    }
    elsif ($this->{postag} eq "VNW(bez,det,stan,red,2v,ev,prenom,zonder,agr)"){
	$this->{postag}="VNW(pr,pron,obl,red,2v,getal)";
    }
    elsif ($this->{postag} eq "VNW(pr,pron,obl,red,2v,getal)"){
	$this->{postag}="VNW(pers,pron,nomin,red,2v,ev)";
    }
    elsif ($this->{postag} eq "LID(bep,stan,evon)"){
	$this->{postag}="VNW(pers,pron,stan,red,3,ev,onz)";
    }
    elsif ($this->{postag} eq "VNW(pers,pron,stan,red,3,ev,onz)"){
	$this->{postag}="LID(bep,stan,evon)";
    }
    elsif ($this->{postag} eq "VG(onder)"){
	$this->{postag}="VNW(betr,pron,stan,vol,3,ev)";
    }
    elsif ($this->{postag} eq "VNW(betr,pron,stan,vol,3,ev)"){
	$this->{postag}="VNW(aanw,pron,stan,vol,3o,ev)";
    }
    elsif ($this->{postag} eq "VNW(aanw,pron,stan,vol,3o,ev)"){
	$this->{postag}="VNW(aanw,det,stan,prenom,zonder,evon)";
    }
    elsif ($this->{postag} eq "VNW(aanw,det,stan,prenom,zonder,evon)"){
	$this->{postag}="VG(onder)";
    }
    elsif ($this->{postag} eq "VNW(betr,pron,stan,vol,persoon,getal)"){
	$this->{postag}="VNW(aanw,det,stan,prenom,zonder,rest)";
    }
    elsif ($this->{postag} eq "VNW(aanw,det,stan,prenom,zonder,rest)"){
	$this->{postag}="VNW(aanw,pron,stan,vol,3,getal)";
    }
    elsif ($this->{postag} eq "VNW(aanw,pron,stan,vol,3,getal)"){
	$this->{postag}="VNW(betr,pron,stan,vol,persoon,getal)";
    }
    elsif ($this->{postag} eq "SPEC(deeleigen)"){
        $this->{postag}="N(soort,ev,basis,zijd,stan)";
    }
    # elsif ($this->{postag} eq ""){
    # 	$this->{postag}="";
    # }


}

=item Sat()
The Sat macro changes the local relation to SAT
=cut
#bind Sat to Z menu Relation Sat
sub Sat {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }
    $this->{rel} = "sat";
}


=item CjCj()

The CjCj macro changes the local tree into coordination structure where
each daughter has relation CNJ and the mother node has category CONJ

=cut
#bind CjCj to alt+c menu CjCj

sub CjCj {
    if (!$this -> parent) {
        ChangingFile(0);
        return;
    }

    my @children;
    @children = $this->children;

    foreach my $kid (@children) {
        $kid->{rel} = "cnj";
    }

    if (@children) { $this->{cat} = "conj" };


}


#bind EditRel to r menu Edit Rel
sub EditRel{
    ChangingFile(EditAttribute($this,'rel'));
}

#bind EditCat to c menu Edit Cat
sub EditCat{
    if ($this->children) {
        ChangingFile(EditAttribute($this, 'cat'));
    } else {
        ChangingFile(0);
    }
}

#bind EditPos to p menu Edit Pos
sub EditPos{
    if (!$this->children) {
        ChangingFile(EditAttribute($this,'postag'));
    } else {
        ChangingFile(0);
    }
}

#bind EditIndex to i menu Edit Index
sub EditIndex{
    ChangingFile(EditAttribute($this,'index'));
}

#bind EditLemma to l menu Edit Lemma
sub EditLemma{
    if (!$this->children) {
        ChangingFile(EditAttribute($this,'lemma'));
    } else {
        ChangingFile(0);
    }
}

#bind EditWord to w menu Edit Word
sub EditWord{
    if (!$this->children) {
        ChangingFile(EditAttribute($this,'word'));
    } else {
        ChangingFile(0);
    }
}

## Voorlopig alleen bij Utrecht actief
## #bind EditSemantics to s menu Edit Semantics
sub EditSemantics{
    ChangingFile(EditAttribute($this,'pb'));
}


#bind RemoveOrEditIndex to ctrl+alt+i menu Remove or Edit Index
sub RemoveOrEditIndex
{
    # als er een index is, verwijder die
    # is er geen index, dan doen we edit index

    if (defined($this->{index})) {
        delete $this->{index};
        ChangingFile(1);
    } else {
        ChangingFile(EditIndex())
    }
}


#bind Undo to Ctrl+z menu Undo
#bind Redo to Ctrl+r menu Redo

#bind CutToClipboard to Ctrl+x menu Cut Subtree
#bind AlpinoPasteFromClipboard to Ctrl+v menu Paste Subtree

sub AlpinoPasteFromClipboard
{
    # like PasteFromClipboard, but with a workaround to avoid pasting
    # a node onto a leaf.  With a proper PML schema this would not be needed.

    # don't add children to leaf nodes (i.e. with a "word" attribute)
    if (defined($this->{word})) {
        ChangingFile(0);
        return;
    }

    return PasteFromClipboard(@_);
}

#bind EditComments to ctrl+alt+a menu Edit Comments (Annotate)
sub EditComments {
    my $pml_root = FileMetaData('pml_root');
    my $pml_root_type = PML::Schema()->type(PML::Schema()->get_root_type);
    ChangingFile(EditAttribute($pml_root, 'comments', $pml_root_type));
}


sub CreateStylesheet
{
    # force update of stylesheet for now... well eh... don't force update...
    if (1) {
    #unless (StylesheetExists('Alpino')) {
        SetStylesheetPatterns(<<'EOF','Alpino',1);
style:<? $${word}.$${cat} eq '' ? "#{Node-shape:rectangle}#{CurrentOval-fill:red}#{Oval-fill:green}" : '' ?>
rootstyle: #{balance:spread}#{Line-coords:n,n,n,p,p,p}#{EdgeLabel-coords:n,n-25}#{NodeLabel-halign:center}#{NodeLabel-yadj:-7}#{Node-textalign:center}#{Node-width:15}#{Node-height:15}#{lineSpacing:1.2}#{nodeYSkip:30}
node: #{brown}${cat}${postag}
node: #{grey40}${lemma}
edge: ${rel} <? '#{black}:#{darkgreen}${index} ' if $${index}  ?>
style: <? !$this->parent ? '' : '#{Node-rellevel:-0.30}' ?>

style:<? "#{Node-hide:1}" if FileUserData('hide')->{$this}; ?>
style:<? "#{Oval-fill:blue}#{Node-shape:rectangle}"
           if FileUserData('fold')->{$this} ?>
EOF
    }
}


sub switch_context_hook
{
    CreateStylesheet();
    my $cur_stylesheet = GetCurrentStylesheet();
    SetCurrentStylesheet('Alpino'),Redraw() 
      if $cur_stylesheet eq STYLESHEET_FROM_FILE() or
        $cur_stylesheet !~ /^[Aa]lpino/;
}


sub after_save_hook {
    my ($filename, $saved_ok)=@_;
    if ($saved_ok) {

        # is ALPINO_HOME wel gezet?
        if (!$ENV{'ALPINO_HOME'}) {
            die "Error: Please set your ALPINO_HOME environment variable.\n" .
              "Error: Can't find canonicalization tool: canonicalization failed.\n";
        }

        my $canonicalize = $ENV{'ALPINO_HOME'} . "/TreebankTools/bin/dtcanonicalize.py";

        # kunnen we canonicalize.py wel vinden?
        if (! -x $canonicalize) {
            die "Error: Can't access canonicalization script ($canonicalize)\n" .
              "Error: canonicalization failed\n";
        }

        # laat het script los op het bestand

        my $errormsg = `$canonicalize $filename 2>&1`;

        if ($?) {
            warn "ERROR: canonicalization failed, please fix any problems:\n\n$errormsg\n";
            warn "WARNING: the file has been saved, but is *not* in canonical form!\n";
            return 'stop';
        }
    }
    return;
}


# validate the tree: "all nodes at a time" method
#bind validate_tree to Alt+v menu Validate Tree
sub validate_tree
{
    my $log = [];
    $root->validate_subtree($log); 


    if (@$log) {

        # now @$log contains error messages
        # which you can report e.g. with something like
        ErrorMessage("VALIDATION ERRORS:\n", join "\n", @$log);

        my $first_err = $log->[0];
        my $node = $root;

        if ($first_err) {       # nu redundant?
            while ($node and $first_err=~s{^/?#content/\[(\d+)\]}{}) {
                $node = $node->firstson;
                $node = $node ? $node->rbrother : $node for 2..$1;
                $this = $node if $node;
            }
        }
    }

    ChangingFile(0);
}


# When we use validate_tree in file_save_hook and don't return "stop"
# we'll get two error messages; one from validate_tree and one from
# the validation code that runs when saving.
#
# If we return "stop" it isn't possible to save an invalid file; i'm not
# sure if that's what we want.
#
# As a workaround we use the following code to get to offending node
# but rely on the error reporting from the saving code.
sub validate_tree_silent
{
#    my $log = [];
    $root->validate_subtree(my $log); 


    if (@$log) {

        # now @$log contains error messages
        # which you can report e.g. with something like

        # ErrorMessage("VALIDATION ERRORS:\n", join "\n", @$log);

        my $first_err = $log->[0];
        my $node = $root;

        if ($first_err) {       # nu redundant?
            while ($node and $first_err=~s{^/?#content/\[(\d+)\]}{}) {
                $node = $node->firstson;
                $node = $node ? $node->rbrother : $node for 2..$1;
                $this = $node if $node;
            }
        }
    }

    ChangingFile(0);
}

# FIXME: is it better (since we save anyway) to run validate_tree()
#        from after_save_hook and skip canonicalization in case of error?
sub file_save_hook {
    validate_tree_silent();

    # make sure $this is propagated
    SetCurrentNodeInOtherWin($grp,$this);
    CenterOtherWinTo($grp,$this);
}


#
# Don't allow drop on nodes with a non-empty root attribute
#

sub node_release_hook {
    my ($node, $target_node, $mod) = @_;

    if (defined($target_node->{word}))
    {
        ChangingFile(0);
        return "stop";
    }
}


#
# Folding subtrees.  Thanks again to Petr Pajas for his help.
#

#bind toggle_fold to space menu Fold/unfold current subtree
sub toggle_fold {
    my $hide = FileUserData("hide") || {}; # hash nodes not to show
    my $fold = FileUserData("fold") || {}; # fold-state of a subtree
    my $folded = $fold->{$this}; # currently folded

    $fold->{$this}=!$folded;     # toggle
    my $node=$this->firstson;
    while ($node) {
        $hide->{$node}=!$folded; # toggle visibility

        if ($fold->{$node}) {
            # this is a folded subtree within our folded subtree
            # we skip it, keeping it folded and the nodes hidden
            $node=$node->following_right_or_up($this);
        } else {
            # next node
            $node=$node->following($this);
        }
    }
    # store the data
    FileUserData("hide",$hide);
    FileUserData("fold",$fold);
    ChangingFile(0);
}


# FIXME: hernoemen?

=item MoveToParentDirectory()

If the current directory is named F<uncorrected>, make sure the
current file is saved, and move the current treebank file to the
parent directory.

If the current directory is not named F<uncorrected>, just save the
file if necessary.  The file is not moved to the parent directory.

The renaming itself is implemented by first using SaveAs() to save the
file in the parent directory.  If succesful, the original file is
removed.

Returns 1 if the file was saved successfully.

=cut


sub MoveToParentDirectory {

    my $filename = $grp->{FSFile}->filename;

    my $basename = File::Basename::basename($filename);
    my $dirname = File::Basename::dirname($filename);
    my @pathcomponents = File::Spec->splitdir($dirname);
    my $current_dirname = pop(@pathcomponents);
    my $newfile = File::Spec->catfile(@pathcomponents, $basename);

    if ($current_dirname eq "uncorrected" ) {

        if (-e $newfile) {      # FIXME?  This does not work with URLs (like the IOBackend functions)

            my $overwrite =
                main::userQuery($grp,
                                "A file with the same name already exists in the parent directory.\n".
                                "Overwrite?",
                                -bitmap=> 'question',
                                -title => "Move to parent directory: overwrite?",
                                -buttons => ['Yes', 'No']);

            if ($overwrite eq 'No') {
                ChangingFile(0);
                return 0;
            }
        }

        $ret = SaveAs({filename => $newfile,
                       update_refs => 'all',
                       update_filelist => 'current'});

        if ($ret) {
            # print "unlinking the original file ($filename)\n";
            Treex::PML::IO::unlink_uri($filename);
        }

    } else {

        # save if necessary
        if (GetFileSaveStatus() || $FileNotSaved) {
            $ret = Save();
        } else {
            $ret = 1;
        }
    }

    return $ret;
}



=item MoveToParentAndNextFile()

Mark the current file as done by moving it to the parent directory
(see MoveToParentDirectory()) and proceed with the next file.

=cut

#bind MoveToParentAndNextFile to F12 menu Move to parent directory and Next File
sub MoveToParentAndNextFile {
    if (GetFileSaveStatus() || $FileNotSaved) {
        return unless MoveToParentDirectory();
    }
    NextFile();
}


=item MoveToSkippedDirectory()

If the subdirectory F<skipped> does not exist, create this.
Signal an error when this fails.

The renaming itself is implemented by first using SaveAs() to save the
file in the parent directory.  If succesful, the original file is
removed.

Returns 1 if the file was saved successfully, 0 on failure.

=cut

sub MoveToSkippedDirectory {

    my $filename = $grp->{FSFile}->filename;

    # construct filenames
    my $basename = File::Basename::basename($filename);
    my $dirname = File::Basename::dirname($filename);
    my $skipped_dir = File::Spec->catfile($dirname, "skipped");
    my $newfile = File::Spec->catfile($skipped_dir, $basename);

    # FIXME: evt. kijken of de huidige directory "skipped" heet?

    # create the directory if necessary
    if (!-d $skipped_dir) {
        if (!mkdir($skipped_dir)) {
            ErrorMessage("Could not create \"$skipped_dir\": $!");
            ChangingFile(0);
            return 0;
        }
    }

    # ask the user to overwrite if the file exists
    if (-e $newfile) { # FIXME?  This does not work with URLs (like the IOBackend functions)

        my $overwrite =
          main::userQuery($grp,
                          "A file with the same name already exists in the skipped directory.\n".
                          "Overwrite?",
                          -bitmap=> 'question',
                          -title => "Move to skipped directory: overwrite?",
                          -buttons => ['Yes', 'No']);

        if ($overwrite eq 'No') {
            ChangingFile(0);
            return 0;
        }
    }

    # rename the file
    $ret = SaveAs({filename => $newfile,
                   update_refs => 'all',
                   update_filelist => 'current'});

    if ($ret) {
        # print "unlinking the original file ($filename)\n";
        # IOBackend::unlink_uri($filename);
	Treex::PML::IO::unlink_uri($filename);
    }

    return $ret;
}



=item MoveToSkippedAndNextFile()

Mark the current file as skipped by moving it to the "skipped" subdirectory
(see MoveToSkippedDirectory()) and proceed with the next file.

=cut

#bind MoveToSkippedAndNextFile to Alt+s menu Move to skipped directory and Next File
sub MoveToSkippedAndNextFile {
    if (GetFileSaveStatus() || $FileNotSaved) {
        return unless MoveToSkippedDirectory();
    }
    NextFile();
}


=item RemoveAllStructure()

Remove all structure of the tree.  Only word nodes are
kept and put under the node with rel=top and cat=top.

For every word the rel and pos attribute is set to "--" and
any index attributes are removed.

=cut

# bind RemoveAllStructure to ctrl+alt+r menu Remove all structure
sub RemoveAllStructure {
    my @nodes = GetNodes($root);

    my $topnode = shift @nodes;

    # collect the word nodes
    my @wordnodes = ();
    for my $node (@nodes) {
        if (defined($node->{word})) {
            push(@wordnodes, $node);
        }
    }

    # sort in reverse order; this  makes the nodes end
    # up in the right order when pasting them
    @wordnodes = sort {$b->{wordno} <=> $a->{wordno}} @wordnodes;


    # remove existing nodes from the topnode
    my @children = $topnode->children;
    for my $child (@children) {
	CutNode($child);
    }

    # add the word nodes to the topnode
    for my $node (@wordnodes) {

        # remove any indexes and set rel to "--"
	if (defined($node->{index})) {
            delete $node->{index};
	}
	$node->{rel} = "--";
	$node->{pos} = "--";

	PasteNode($node, $topnode);
    }

}

#bind add_terminal_l to Alt+l menu Add Terminal Left
sub add_terminal_l  {
    if (defined ($this->{wordno})) {
	my $wordno=$this->{wordno};
	my $new = NewLBrother();
	my $schema = PML::Schema();
	$new->set_type_by_name($schema, 'node.type');
	$new->{'#name'} = 'node';

	my @nodes = GetNodes($root);
	for my $node (@nodes) {
	    if (defined ($node->{wordno})
		&&
		$node->{wordno} >= $wordno
	       ) {
		$node->{wordno}++;
	    }
	}
	$new->{wordno} = $wordno;
	$new->{word} = "???";
    }
}

#bind add_terminal_r to Alt+r menu Add Terminal Right
sub add_terminal_r  {
    if (defined ($this->{wordno})) {
	my $wordno=$this->{wordno};
	my $new = NewRBrother();
	my $schema = PML::Schema();
	$new->set_type_by_name($schema, 'node.type');
	$new->{'#name'} = 'node';

	my @nodes = GetNodes($root);
	for my $node (@nodes) {
	    if (defined ($node->{wordno})
		&&
		$node->{wordno} > $wordno
	       ) {
		$node->{wordno}++;
	    }
	}
	$new->{wordno} = $wordno+1;
	$new->{word} = "???";
    }
}


#bind add_terminal_r_p to Alt+p menu Add Terminal Right Punt
sub add_terminal_r_p  {
    if (defined ($this->{wordno})) {
	my $wordno=$this->{wordno};
	my $new = NewSon($root);
	my $schema = PML::Schema();
	$new->set_type_by_name($schema, 'node.type');
	$new->{'#name'} = 'node';

	my @nodes = GetNodes($root);
	for my $node (@nodes) {
	    if (defined ($node->{wordno})
		&&
		$node->{wordno} > $wordno
	       ) {
		$node->{wordno}++;
	    }
	}
	$new->{wordno} = $wordno+1;
	$new->{rel} = "--";
	$new->{root} = ".";
	$new->{pos} = "punct";
	$new->{word} = ".";
	if (defined ($this->{lemma})) {
	    $new->{lemma} = ".";
	}
	if (defined ($this->{postag})) {
	    $new->{postag} = "LET()";
	}
    }
}


#bind cut_terminal to Alt+x menu Cut Terminal
sub cut_terminal
  {
      if (defined ($this->{wordno})) {
	  $wordno=$this->{wordno};
	  DeleteLeafNode($this);

	  my @nodes = GetNodes($root);
	  for my $node (@nodes) {
	      if (defined ($node->{wordno})
                  &&
		  $node->{wordno} > $wordno
		 ) {
		  $node->{wordno}--;
	      }
	  }
      }
}

=item AssignCatOfParent()

The current node is assumed to be the head-node with a pos label.
On the basis of the pos label, the cat node of the mother is assigned.

=cut

#bind AssignCatOfParent to U menu AssignCatOfParent
sub AssignCatOfParent {
    if (!$this -> parent || !defined($this -> {pos})) {
        ChangingFile(0);
        return;
    }
    my $pos = $this -> {pt};

    if ($pos eq "n" )
      { $this -> parent -> {cat} = "np" }
    if ($pos eq "vz")
      { $this -> parent -> {cat} = "pp" }
    if ($pos eq "adj")
      { $this -> parent -> {cat} = "ap" }
    if ($pos eq "bw")
      { $this -> parent -> {cat} = "advp" }
    if ($pos eq "vg")
      { $this -> parent -> {cat} = "cp" }

}

sub add_comment {
    my ($comment)=@_;
    my $pml_root = FileMetaData('pml_root');
    my $sequence = $pml_root->{comments};
    unless (defined $sequence) {
        $sequence = $pml_root->{comments} = Fslib::Seq->new();
    }
    $sequence->push_element('comment', $comment);
}

1;

=back

=cut

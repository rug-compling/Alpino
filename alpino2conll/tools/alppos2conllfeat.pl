#!/usr/bin/perl
# converts alpino postags into conll feats
# Example: zie@verb(hebben,past(sg),aci) -> zie \t hebben|past(sg)|aci

#use matchpairs ('('=>')'); only in perl 6!

# extract all between outermost ()
# replace comma by |
# adds word in column2 and wordcount in column1
while(<>) {
    chomp;
    @words = split;
    my $i = 1;
    foreach $w (@words) {
	my ($stem,$fullpos) = split("@",$w);
	my $shortpos = substr($fullpos,0,index($fullpos,'('));
	print $i . "\t" .$stem ."\t". $shortpos ."\t". getFeat($fullpos) ."\n";
	$i++;
    }
}

# returns values separated by |
# if no (), returns original w
sub getFeat() {
    my $w = shift;
    my $leftChar = '(';
    my $rightChar = ')';
    my $startpos = index($w, $leftChar);
    my $endpos = rindex($w, $rightChar);
    my $length = $endpos - $startpos;

    if ($startpos != -1 && $endpos != -1){
	my $new_w = substr($w, $startpos+1, $length-1);

	$new_w = replace_sqbracket($new_w);
	$new_w = replace_inner_comma($new_w);
	$new_w =~ s/,/|/g;
	return $new_w;
    }
    else {
	return $w;
    }
}

#for values in square bracket, replace comma with _
# [aan,door,uit,[in,de,plaats]] -> aan_door_uit_[in_de_plaats_]
sub replace_sqbracket() {
    my $w = shift;
    my $leftChar = '[';
    my $rightChar = ']';
    my $startpos = index($w, $leftChar);
    my $endpos = rindex($w, $rightChar);
    my $length = $endpos - $startpos;

    if ($startpos != -1 && $endpos != -1 && $length > 1){
	# replace comma in substructure
        my $substr = substr($w, $startpos, $length);
	if (index($substr, ",") != -1) {
            my $old_sub = $substr;
            $substr =~ s/,/\_/g;
            $w =~ s/\Q$old_sub/$substr/g;
        }
#	$w =~ s/,/\_/g;
	return $w;
    }
    else {
	return $w;
    }
}
# if substructure, replace commas with __ (double _)
# slinger_voort@verb(hebben,past(sg),ninv(refl,part_refl(voort))) ->
# hebben|past(sg)|ninv(refl__part_refl(coord)))
##todo: find matching bracket instead of next with position
# e.g. geef_les@verb(hebben,psp,ninv(pc_pp(in),part_pc_pp(les,in)))
sub replace_inner_comma() {
    my $w = shift;
    my $offset = shift;
    #print "offset: $offset\n";

    my $leftChar = '(';
    my $rightChar = ')';
    my $startpos;
    my $endpos;
    if ($offset ne "") {
        $startpos = index($w, $leftChar, $offset+1);
	if ($startpos == -1) {
	    return $w;
	}
        $endpos = index($w, $rightChar, $offset+1);

    } else {
        $startpos = index($w, $leftChar);
	if ($startpos == -1) {
	    return $w;
	}
        #$endpos = index($w, $rightChar);
	$endpos = indexMatchingBracket($w, $startpos);
	#print "endpos $endpos epos $epos \n";
    }
    my $length = $endpos - $startpos + 1; #including ()
    my $sym = ',';

    if ($startpos != -1 && $endpos != -1) {
        #feature contains substructure, check for commas
        my $substr = substr($w, $startpos, $length);
        if (index($substr, $sym) != -1) {
            my $old_sub = $substr;
            $substr =~ s/,/__/g;
            $w =~ s/\Q$old_sub/$substr/g;
        }
    }
   # check if another (), then recursively iter
    if (index($w, $leftChar, $endpos) != -1) {
        replace_inner_comma($w, $endpos);
    }
    else {
	return $w;
    }
}
sub indexMatchingBracket {
    my $w = shift;
    my $startpos = shift;
    my @arr = ();
    my $leftChar='(';
    my $rightChar=')';

    my $w_length = length($w);
    for ($i=$startpos;$i<$w_length;$i++) {
	# $w[$i] = substr($w, $i, 1)
	my $w_i = substr($w, $i, 1) . "\n";
	chomp($w_i);
	if ($w_i eq "(") {
	    push(@arr,$i);
	} elsif ($w_i eq ")") {
	    pop(@arr);
	    $array_length = scalar(@arr);
	    if ($array_length == 0) {
		return $i;
	    }
	}
	
    }
}

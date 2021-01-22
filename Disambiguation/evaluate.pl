#!/usr/bin/perl -w

use Getopt::Std;
getopts('dw:ru:x');

## opt_w: file containing features and their weights
## opt_r: if set, only the raw number of overlap/correct/system triples 
#         per sentence is reported. 
## opt_d: display continuation messages to standard output
## opt_u: weight of unknown features

$opt_w ||= "data.weights";

$opt_u ||=0;

if (! $opt_x) {
    %weights=();
    open(WEIGHTS,"$opt_w") or die "Can't find weights file $opt_w: $!\n";
    while (<WEIGHTS>) {
	chomp;
	($key,$weight) = split/\|/;
	$weights{$key} = $weight unless $weights{$key};  # first one wins
    }
    close WEIGHTS;
    print STDERR "reading weights file $opt_w done;\n" if $opt_d;
}

$prevkey=undef;
$f_pen=0;
$b_pen=0;
$c_pen=0;

$f_n=0;
$b_n=0;
$c_n=0;

$c_av=0;
$f_av=0;
$b_av=0;

$c_overlap=0;
$c_correct=0;
$c_system=0;
$b_overlap=0;
$b_correct=0;
$b_system=0;
$f_overlap=0;
$f_correct=0;
$f_system=0;

$exact=0;

sub score {
    ($pen,$n) = @_;
    if ($n) {
	return 100*(1-$pen/$n);
    } else {
	return 100;
    }
}

sub pen {
  ($ov,$corr,$sys) = @_;
  return max($corr,$sys)-$ov;
}

sub min {
  ($a,$b) = @_;
  return $a<$b ? $a : $b;
}

sub max {
  ($a,$b) = @_;
  return $a>$b ? $a : $b;
}

sub weight {
  ($lpairs) = @_;
  return $lpairs if ($opt_x);
  @lpairs = split/\|/,$lpairs;
  $lweight=0;
  foreach $lpair (@lpairs) {
    ($ln,$lfeat) = split/\@/,$lpair;
    if ($weights{$lfeat}) {
      $lweight += $ln*$weights{$lfeat}
    } else { $lweight += $ln*$opt_u}
  }
  return $lweight;
}

$lb_pen=0;
$lc_pen=0;
$lb_n=0;
$lc_n=0;
$lb_score=0;
$lc_score=0;
$counter=0;
$lc_cgn=0;       # number correct lemma of current best
$lc_total_cgn=0; # number total lemma of current best
$t_cgn=0;        # summed correct lemma of all best
$t_total_cgn=0;  # summed total lemma of all best

while(<>) {
  next if /^[%\#]/;
  next if !/\|/;
  chomp;
  ($key,undef,$triple,$pairs) = split/\#/;
  ($overlap,$correct,$system,$cgn,$cgn_total) = split/\|/,$triple;
  
  $this_weight=weight($pairs);
  $this_pen=pen($overlap,$correct,$system);
  $this_n=max($correct,$system);
  $this_score=score($this_pen,$this_n);

###  print STDERR "$key: $this_score\n";

  if (defined($prevkey) && $key eq $prevkey) {


      # another parse for same sentence
      if ($this_weight > $lc_weight) {
	  $lc_weight=$this_weight;
          $lc_pen=$this_pen;
          $lc_n=$this_n;
          $lc_score=$this_score;
	  $lc_overlap=$overlap;
	  $lc_correct=$correct;
	  $lc_system=$system;
	  $lc_cgn=$cgn;
	  $lc_total_cgn=$cgn_total;
      }
      if ($this_score > $lb_score) {
          $lb_score=$this_score;
	  $lb_pen=$this_pen;
	  $lb_n=$this_n;
	  $lb_overlap=$overlap;
	  $lb_correct=$correct;
	  $lb_system=$system;
      }
  }
  else {

      # new sentence
      $f_pen += $this_pen;
      $f_n += $this_n;
      $f_av += $this_score;
      $f_overlap += $overlap;
      $f_correct += $correct;
      $f_system += $system;
      
      if (defined $prevkey) {

	  $t_cgn += $lc_cgn;
	  $t_total_cgn += $lc_total_cgn;
	  
	  $c_pen += $lc_pen;
	  $c_n += $lc_n;
	  $c_av+=score($lc_pen,$lc_n);

	  $b_pen += $lb_pen;
	  $b_n += $lb_n;
	  $b_av+=score($lb_pen,$lb_n);

	  if (score($lc_pen,$lc_n) >= score($lb_pen,$lb_n)) {
	      $exact++;
	  }

	  $b_overlap += $lb_overlap;
	  $b_correct += $lb_correct;
	  $b_system += $lb_system;

	  $c_overlap += $lc_overlap;
	  $c_correct += $lc_correct;
	  $c_system += $lc_system;

      }

      if ($opt_d && $prevkey) {
	  $c_score=score($c_pen,$c_n);
	  printf STDERR "\t%s test-score\t%6.2f\t(%6.2f) (exact: %6.2f)\t lemma/pos-score\t%6.2f\t(%6.2f)\n",
             $prevkey,$lc_score,$c_score,$exact/$counter,100*$lc_cgn/$lc_total_cgn,100*$t_cgn/$t_total_cgn;
      }
      if ($opt_r && $prevkey) {
	  print "$prevkey\t$lc_pen\t$lc_n\n";
      }
      
      $lc_weight = $this_weight;
      $lc_n = $this_n;
      $lc_pen = $this_pen;
      $lc_score = $this_score;
      $lc_cgn = $cgn;
      $lc_total_cgn= $cgn_total;

      $lb_n = $this_n;
      $lb_pen = $this_pen;
      $lb_score = $this_score;

      $lc_overlap=$overlap;
      $lc_correct=$correct;
      $lc_system=$system;
      $lb_overlap=$overlap;
      $lb_correct=$correct;
      $lb_system=$system;

      $counter++;

  }
  $prevkey = $key;
}

$t_cgn += $lc_cgn;
$t_total_cgn += $lc_total_cgn;

$c_pen += $lc_pen;
$c_n += $lc_n;
$c_av+=score($lc_pen,$lc_n);

$b_pen += $lb_pen;
$b_n += $lb_n;
$b_av+=score($lb_pen,$lb_n);

#if ($c_pen >= $b_pen) {
#    $exact++;
#}
if (score($lc_pen,$lc_n) >= score($lb_pen,$lb_n)) {
    $exact++;
}

$b_overlap += $lb_overlap;
$b_correct += $lb_correct;
$b_system += $lb_system;

$c_overlap += $lc_overlap;
$c_correct += $lc_correct;
$c_system += $lc_system;

if ($opt_d && $prevkey) {
    $c_score=score($c_pen,$c_n);
    $lc_score=score($lc_pen,$lc_n);
	  printf STDERR "\t%s test-score\t%6.2f\t(%6.2f) (exact: %6.2f)\t lemma/pos-score\t%6.2f\t(%6.2f)\n",
            $prevkey,$lc_score,$c_score,$exact/$counter,100*$lc_cgn/$lc_total_cgn,100*$t_cgn/$t_total_cgn;
}

if ($opt_r) {
    print "$key\t$lc_pen\t$lc_n\n";
}
      
$f_score=score($f_pen,$f_n);
$b_score=score($b_pen,$b_n);
$c_score=score($c_pen,$c_n);
if ($b_score != $f_score) {
    $kappa=100*($c_score-$f_score)/($b_score-$f_score);
} else { $kappa="undefined" }
$c_average=$c_av/$counter;
$b_average=$b_av/$counter;
$f_average=$f_av/$counter;
if ($b_average != $f_average) {
    $kappa_av=100*($c_average-$f_average)/($b_average-$f_average);
} else { $kappa_av="undefined" }

if (!$opt_r) {
  print "\n";
  printf "exact %6.2f\n",$exact/$counter;
  printf "first-score %6.2f %6.2f\n", $f_score,$f_average;
  printf "best-score  %6.2f %6.2f\n", $b_score,$b_average;
  printf "test-score  %6.2f %6.2f\n", $c_score,$c_average;
  printf "phi-score   %6.2f %6.2f\n", $kappa,$kappa_av;
  printf "first-p/m:  %s     %s\n", $f_pen, $f_n;
  printf "best-p/m:   %s     %s\n", $b_pen, $b_n; 
  printf "test-p/m:   %s     %s\n", $c_pen, $c_n;
  printf "first-av:   %s     %s\n", $f_av,$counter;
  printf "best-av:    %s     %s\n", $b_av,$counter;
  printf "test-av:    %s     %s\n", $c_av,$counter;

  printf "first-overlap     %s\n",$f_overlap;
  printf "first-correct     %s\n",$f_correct;
  printf "first-system      %s\n",$f_system;
  printf "best-overlap     %s\n",$b_overlap;
  printf "best-correct     %s\n",$b_correct;
  printf "best-system      %s\n",$b_system;
  printf "test-overlap     %s\n",$c_overlap;
  printf "test-correct     %s\n",$c_correct;
  printf "test-system      %s\n",$c_system;

  $f_precision=100*$f_overlap/$f_system;
  $f_recall=100*$f_overlap/$f_correct;
  $f_fscore=(2*$f_precision*$f_recall)/($f_precision+$f_recall);
  printf "first-precision  %6.2f\n",$f_precision;
  printf "first-recall     %6.2f\n",$f_recall;
  printf "first-fscore     %6.2f\n",$f_fscore;

  $b_precision=100*$b_overlap/$b_system;
  $b_recall=100*$b_overlap/$b_correct;
  $b_fscore=(2*$b_precision*$b_recall)/($b_precision+$b_recall);
  printf "best-precision   %6.2f\n",$b_precision;
  printf "best-recall      %6.2f\n",$b_recall;
  printf "best-fscore      %6.2f\n",$b_fscore;

  $c_precision=100*$c_overlap/$c_system;
  $c_recall=100*$c_overlap/$c_correct;
  $c_fscore=(2*$c_precision*$c_recall)/($c_precision+$c_recall);
  printf "test-precision   %6.2f\n",$c_precision;
  printf "test-recall      %6.2f\n",$c_recall;
  printf "test-fscore      %6.2f\n",$c_fscore;

  printf "lemma/pos-score  %6.2f\n",100*$t_cgn/$t_total_cgn;

}


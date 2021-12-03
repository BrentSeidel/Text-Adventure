#!/usr/bin/perl -w
#
#	This program fixes the XML in adventure.xml.
#
my	$line;
my	$dir;
my	$flags;
my	$len;
my	$space;
open(IN, "adventure.xml") || die "Can't open output file, $!\n";
open(OUT, ">adventure.new") || die "Can't open output file, $!\n";
while ($line = <IN>)
{
	if ($line =~ /\s+\<exit/)
	{
		chomp $line;
		print "Processing exit line '$line'\n";
		$line =~ s/\s+\<exit//;
		$line =~ s/\s*(dir=\"[^"]*\")//;
		$dir = $1;
		$line =~ s/\s*(flags=\"[^"]*\")//;
		$flags = $1;
		print "  Found direction ($dir) and flags ($flags)\n";
		$line =~ s/\"\>/\"\/\>/;
		$len = 11 - length($dir);
		$spaces = " " x $len;
		print OUT "    <exit $dir$spaces$flags $line\n";
	}
	else
	{
		print OUT $line;
	}
}
close(IN);
close(OUT);

#!/usr/bin/perl-w
#
#                      File:    Polynomial.pm
#                      Author:  brent seidel
#
#	This package implements the polynomial object
#
#M o d i f i c a t i o n   H i s t o r y
#Version		When	Who	What
#V00.01		 6-Aug-2001	BBS	Version history started
#V00.02		19-Sep-2001	BBS	Add function to dump a polynomial
#
package Math::Symbolic::Polynomial;
use strict;
use overload 	fallback => 0,			# Don't autogenerate.
				'""' => \&format,
				"*"  => \&multiply,
				"+"  => \&add,
				"-"  => \&subtract;
##########################################################
#	Basic functions to setup objects
#
sub new
{
	my $class = shift;
	my $value = shift;

	if (defined($value))
	{
		my @poly = parse($value);
		my $self = \@poly;
		bless $self, $class;
		return $self;
	}
	else
	{
		my $self = [];
		if (defined($class))
		{
			bless $self, $class;
		}
		else
		{
			bless $self;
		}
		return $self;
	}
}
#
#	Converts a polynomial to a string suitable for printing
#
sub format
{
	my $ref = shift;
	my @poly = @$ref;
	my $exp = $#poly + 1;
	my $num;
	my $str;

	$str = "";
	foreach $num (reverse(@poly))
	{
		$exp--;
		next					if !defined($num);
		next					if $num == 0;
		$str .= "+"				if $num > 0;
		$str .= "${num}X^$exp"	if $exp > 1;
		$str .= "${num}X"		if $exp == 1;
		$str .= "$num"			if $exp == 0;
	}
	return $str;
}
#
#	Parses a string containing a polynomial into a polynomial
#	object.
#
sub parse
{
    my $input = shift;
    my $num;
    my $sign;
    my $exp;
    my @poly;
    my $len;

    $input =~ s/\s+//g;			# Eliminate spaces
    $input =~ tr/a-z/A-Z/;		# Uppercase everyting
    $len = -1;
    while ($input)
    {
    	if ($len == length($input))
    	{
    		die "Failure parsing <$input>\n";
    	}
    	$len = length($input);
    	$sign = 1;
    	if ($input =~ s/^([+-])//)
    	{
    		$sign = -1	if ($1 eq "-");
    	}
        if ($input =~ s/^(\d+\.?\d*([Ee][+-]?\d+)?)//)
        {
			$num = $1;
        }
        else
        {
			$num = 1;
        }
        $num = $sign * $num;
		if ($input =~ s/^\*?X//)
		{
			if ($input =~s/^\^(\d+)//)
			{
				$exp = $1;
			}
			else
			{
				$exp = 1;
			}
		}
		else
		{
			$exp = 0;
		}
		$poly[$exp] += $num;
    }
    return (@poly);
}
###################################################################
#	Functions to perform operations on objects
#
#
#	This function evaluates a polynomial at a specific value
#	using Newton's method.
#
#	a0 + a1X + a2X^2 + a3X^3... = a0 + X(a1 + X(a2 + X(a3 + X...)))
#
sub evaluate
{
	my $self = shift;
	my $x = shift;
	my @poly = @$self;
	my $max_exp = $#poly;
	my $temp_exp;
	my $result;

	$result = $poly[$max_exp];
	for ($temp_exp = ($max_exp - 1); $temp_exp >= 0; $temp_exp--)
	{
		$result *= $x;
		$result += $poly[$temp_exp]	if defined($poly[$temp_exp]);
	}
	return($result);
}
#
#	Add two polynomials together
#
sub add
{
	my $p1 = shift;
	my $p2 = shift;
	my $rev = shift;
	my $p3 = new();
	my $max_exp;
	my $exp;

	$max_exp = $#{$p1};
	$max_exp = $#{$p2}	if $#{$p2} > $max_exp;
	for ($exp = 0; $exp <= $max_exp; $exp++)
	{
		$$p3[$exp]  = 0;
		$$p3[$exp]  = $$p1[$exp]		if defined($$p1[$exp]);
		$$p3[$exp] += $$p2[$exp]		if defined($$p2[$exp]);
	}
	return $p3
}

sub subtract
{
	my $p1 = shift;
	my $p2 = shift;
	my $rev = shift;
	my $p3 = new();
	my $max_exp;
	my $exp;

	($p2, $p1) = ($p1, $p2)		if $rev;
	$max_exp = $#{$p1};
	$max_exp = $#{$p2}			if $#{$p2} > $max_exp;
	for ($exp = 0; $exp <= $max_exp; $exp++)
	{
		$$p3[$exp]  = 0;
		$$p3[$exp]  = $$p1[$exp]		if defined($$p1[$exp]);
		$$p3[$exp] -= $$p2[$exp]		if defined($$p2[$exp]);
	}
	return $p3
}

sub multiply
{
	my $p1 = shift;
	my $p2 = shift;
	my $rev = shift;
	my $p3 = new;
	my $max1 = $#{$p1};
	my $max2 = $#{$p2};
	my $exp1;
	my $exp2;

	for ($exp1 = 0; $exp1 <= $max1; $exp1++)
	{
		for ($exp2 = 0; $exp2 <= $max2; $exp2++)
		{
			$$p3[$exp1 + $exp2] += $$p1[$exp1] * $$p2[$exp2];
		}
	}
	return $p3;
}

sub integrate
{
	my $poly = shift;
	my $const = shift;
	my $max = $#{$poly};
	my $exp;
	my $result = new();

	for ($exp = 0; $exp <= $max; $exp++)
	{
		$$result[$exp + 1] = $$poly[$exp] / ($exp + 1)
				if defined($$poly[$exp]);
	}
	$$result[0] = $const	if defined($const);
	return $result;
}

sub derivitive
{
	my $poly = shift;
	my $max = $#{$poly};
	my $exp;
	my $result = new();

	for ($exp = 1; $exp <= $max; $exp++)
	{
		$$result[$exp - 1] = $exp * $$poly[$exp]
				if defined($$poly[$exp]);
	}
	return $result;
}

sub dump
{
	my	$self = shift;
	my	$file = shift;
	my	$exp;
	my	$max = $#{$self};

	open(DMP, ">>$file")  || warn "Can't open $file, $!\n";
	print DMP "  <poly>\n";
	for ($exp = $max; $exp >= 0; $exp--)
	{
		print DMP "    <coeff power=\"$exp\" value=\"$$self[$exp]\"/>\n";
	}
	print DMP "  </poly>\n";
	close(DMP);
}


1;

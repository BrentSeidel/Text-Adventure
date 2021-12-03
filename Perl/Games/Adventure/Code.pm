#!/usr/bin/perl
#
#	This package is used to encapsulate the code table
#
use	strict;
use	Carp;

package	Code;

{
	my	%code_table;		# Table containing references to subroutines

	sub set_code
	{
		my	$class = shift;
		my	$code_id = shift;
		my	$code_ref = shift;

		$code_table{$code_id} = $code_ref;
	}

	sub get
	{
		my	$class = shift;
		my	$code_id = shift;
		my	$code = $code_table{$code_id};

		return ($code);
	}

	sub reset_code
	{
		undef %code_table;
	}
}

sub new
{
		my	$class = shift;
		my	$code_id = shift;
		my	$code = shift;
		my	$code_ref;

		$code = "sub { " . $code . " }";
		$code_ref = eval $code;
		bless $code_ref, $class;
		Code->set_code($code_id, $code_ref);
		return $code_ref;
}
sub BEGIN
{
	print "Loaded Package Code\n";
}
1;

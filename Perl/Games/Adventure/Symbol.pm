#!/usr/bin/perl
#
#	This package is used to encapsulate the symbol table
#
use	strict;
use	Carp;

package	Symbol;

{
	my	%Symbol_table;

	sub set
	{
		my	$class = shift;
		my	$Symbol_id = shift;
		my	$Symbol_ref = shift;

		$Symbol_table{$Symbol_id} = $Symbol_ref;
	}

	sub get
	{
		my	$class = shift;
		my	$Symbol_id = shift;
		my	$Symbol = $Symbol_table{$Symbol_id};

		return ($Symbol);
	}

	sub delete
	{
		my	$class = shift;
		my	$name = shift;

		delete $Symbol_table{$name};
	}

	sub list
	{
		my	$name;

		print "Polynomial table:\n";
		foreach $name (sort(keys(%Symbol_table)))
		{
			print "  Polynomial $name is: ", $symtab{$name}, "\n";
		}
	}

	sub reset
	{
		undef %Symbol_table;
	}
}
sub BEGIN
{
	print "Loaded package Symbol\n";
}
0;

#!/usr/bin/perl -w
#                      File:    adventure.pl
#                      Author:  brent seidel
#
#	This program runs a combination adventure game and math package
#
#M o d i f i c a t i o n   H i s t o r y
#Version		When	Who	What
#V00.01		 6-Aug-2001	BBS	Version history started
#V00.02		 9-Aug-2001	BBS	Replace literals with messages
#V00.03		10-Aug-2001	BBS	Use identify_object in function turn
#V00.03		14-Aug-2001	BBS	Move packages into Games::Adventure:: namespace
#V00.04		20-Aug-2001	BBS	Add Math::Symbolic::Expression package
#V00.05		18-Sep-2001	BBS	Move file name from load_xml.pm into this file
#V00.06		19-Sep-2001	BBS	Add dump command for debugging purposes
#V00.07		20-Sep-2001	BBS	Add function to drink an object
#
use strict;
#
#	Define packages used
#
use Math::Symbolic::Polynomial;
use	Math::Symbolic::Expression;
use	Math::Symbolic::Expression_ops;
use	Math::Symbolic::Expression_basic;
use Games::Adventure::Symbol;
use Games::Adventure::Load_xml;
use Games::Adventure::Objects;
use Games::Adventure::Location;
use	Games::Adventure::Messages;
use Games::Adventure::Code;
use Games::Adventure::Command;
use Games::Adventure::Denizen;
#
#	Declare subroutines
#
sub main;
sub move;
sub create;
sub list;
sub add;
sub evaluate;
sub remove;
sub subtract;
sub multiply;
sub derive;
sub integrate;
sub	simplify;
sub quit;
#
#	Define some constants
#
sub xml_file	{ ":adventure.xml"	}


main();

sub main
{
	my	$test;
	my	$poly;
	my	$cmd;
	my	$cmd_ref;
	my	@params;
	my	$line;
#
#	Initialization stuff
#
	Games::Adventure::Load_xml->load(xml_file);
	Games::Adventure::Location->set_location("start");
	Games::Adventure::Messages->print("start");
	Games::Adventure::Objects->list("start",
		Games::Adventure::Messages->get("object-list"), "long");
#
#	Enter main loop
#
	while (1)
	{
		print "> ";
		$test = <STDIN>;
		next			unless defined($test);	# Skip undefined lines
		$test =~ s/^\s+//;						# Strip leading whitespace
		$test =~ s/\s+$//;						# Strip trailing whitespace
		next			if ($test eq "");		# Skip blank lines
		$test =~ tr/a-z/A-Z/;					# Convert to all caps
		($cmd, @params) = split(/\s+/, $test);	# Split out the command and parameters
		$cmd_ref = Games::Adventure::Command->get($cmd);
		if (defined($cmd_ref))					# See if the command is defined
		{
			my	$func = $cmd_ref->get_func();	# Get function name
			my	$type = $cmd_ref->get_type();	# Get function type
#
#	If the type is "ext", then the function is the name of some code that
#	was loaded.  Otherwise, function is the name of an internal function to
#	be called.
#
			if (defined($type) && ($type eq "ext"))
			{
				my	$code_ref = Code->get($func);	# Get the code reference

				&$code_ref($cmd, @params)	if (defined($code_ref));
			}
			else
			{
no strict;		# We need to call a function by name
				&{$func}($cmd, @params);
use strict;
			}
			
		}
		else
		{
			Games::Adventure::Messages->print("bad-cmd", $cmd);
		}
	}
}
###############################
#	General functions
#
#
#	List various items
#
sub list
{
	my	$cmd = shift;
	my	$table = shift;
	my	$name;
	my	$id;
	my	$poly;

#
#	Check for listing commands
#
	if (defined($table) && ($table =~ /^CMD/))
	{
		Games::Adventure::Command->list();
		return;
	}
#
#	Check for listing objects
#
	if (defined($table) && ($table =~ /^OBJ/))
	{
		Games::Adventure::Objects->debug_list_objects();
		return;
	}
#
#	Check for listing location
#
	if (defined($table) && ($table =~ /^LOC/))
	{
		my	$current_loc = Location->get_loc_ref();
		my	$exits = $current_loc->get_exits();
		my	$dir;

		print "Current location is: $current_loc\n";
		print "  Location ID:       ", $current_loc->get_id(), "\n";
		print "  Flags are:         ", $current_loc->get_flags(), "\n";
		print "  Exits are:\n";
		foreach $dir (sort(keys(%{$exits})))
		{
			print "    Direction $dir exit is:\n";
			print "      Flags:  ", $exits->{$dir}->get_flags(), "\n";
			if (defined($exits->{$dir}->get_dest()))
			{
				print "      Destination:  ", $exits->{$dir}->get_dest(), "\n";
			}
			else
			{
				print "      Destination:  undefined\n";
			}
		}
		return;
	}
#
#	By default, list symbol table
#
	Games::Adventure::Symbol->list();
}
#
#	Dump an object for debugging purposes
#
sub dump
{
	my	$cmd = shift;
	my	$type = shift;
	my	$name = shift;
	my	$file = shift;
	my	$obj_ref;

	$file = "Dev:Console:Dumped $type"							unless defined($file);
	$obj_ref = Games::Adventure::Symbol->get($name)				if $type =~ /^MATH/;
	$obj_ref = Games::Adventure::Objects->get($name)			if $type =~ /^OBJ/;
	$obj_ref = Games::Adventure::Location->get_loc_data($name)	if $type =~ /^LOC/;
	if (defined($obj_ref))
	{
		$obj_ref->dump($file);
	}
	else
	{
		print "Warning:  Unable to find <$type> entity <$name>\n";
	}
}
#
#	Quit the program
#
sub quit
{
	exit(0);
}
################################
#	Math functions
#
#
#	Create a symbol table entry
#
sub create
{
	my	$cmd	= shift;
	my	$type	= shift;
	my	$name	= shift;
	my	$value	= shift;
	my	@vars	= @_;

	CASE:
	{
		Games::Adventure::Symbol->set($name,
			Math::Symbolic::Polynomial->new($value)),
			last CASE							if $type =~ /^POLY/;
#
		Games::Adventure::Symbol->set($name,
			Math::Symbolic::Expression->new($value, @vars)),
			last CASE							if $type =~ /^EXPR/;
#
		Games::Adventure::Symbol->set($name,
			Games::Adventure::Const->new($value)),
			last CASE							if $type =~ /^CONST/;
#
		print "Cannot create a symbol table entry of type <$type>\n";
	}
}
sub evaluate
{
	my	$cmd = shift;
	my	$symbol = shift;
	my  $obj = Games::Adventure::Symbol->get($symbol);
	my  $type = ref($obj);
	my	$value;

	if ($type =~ /Poly/)
	{
        foreach $value (@_)
        {
            print "The value of $obj at $value is: ";
            print $obj->evaluate($value), "\n";
        }
		return;
    }
	if ($type =~ /Expr/)
	{
	    $value = $obj->evaluate(@_);
		print "<", $obj, "> Evaluates to: <", $value, ">\n";
		return;
	}
	if ($type =~ /Const/)
	{
	    $value = $obj->get();
	    print "$symbol is a constant with the value $value\n";
	}
}
#
#	Remove a symbol table entry
#
sub remove
{
	my	$cmd = shift;
	my	@vars = @_;
	my	$name;

	foreach $name (@vars)
	{
		Games::Adventure::Symbol->delete($name);
	}
}
#
#	Add two symbol table entries
#
sub add
{
	my	$cmd = shift;
	my	$poly1 = shift;
	my	$poly2 = shift;
	my	$poly3 = shift;

	if (ref(Games::Adventure::Symbol->get($poly1)) ne
		ref(Games::Adventure::Symbol->get($poly1)))
	{
		print "Arguments are not of the same type\n";
	}
	else
	{
		Games::Adventure::Symbol->set($poly3,
			Games::Adventure::Symbol->get($poly1) +
			Games::Adventure::Symbol->get($poly2));
	}
}
#
#	Subtract two symbol table entries
#
sub subtract
{
	my	$cmd = shift;
	my	$poly1 = shift;
	my	$poly2 = shift;
	my	$poly3 = shift;

	if (ref(Games::Adventure::Symbol->get($poly1)) ne
		ref(Games::Adventure::Symbol->get($poly1)))
	{
		print "Arguments are not of the same type\n";
	}
	else
	{
		Games::Adventure::Symbol->set($poly3,
			Games::Adventure::Symbol->get($poly1) -
			Games::Adventure::Symbol->get($poly2));
	}
}
#
#	Multiply two symbol table entries
#
sub multiply
{
	my	$cmd = shift;
	my	$poly1 = shift;
	my	$poly2 = shift;
	my	$poly3 = shift;

	if (ref(Games::Adventure::Symbol->get($poly1)) ne
		ref(Games::Adventure::Symbol->get($poly1)))
	{
		print "Arguments are not of the same type\n";
	}
	else
	{
		Games::Adventure::Symbol->set($poly3,
			Games::Adventure::Symbol->get($poly1) *
			Games::Adventure::Symbol->get($poly2));
	}
}
#
#	Compute the derivitive of a symbol table entry
#
sub derive
{
	my	$cmd = shift;
	my	$in = shift;
	my	$out = shift;

	Games::Adventure::Symbol->set($out,
		Games::Adventure::Symbol->get($in)->derivitive());
}
#
#	Compute the integral of a symbol table entry
#
sub integrate
{
	my	$cmd = shift;
	my	$in = shift;
	my	$out = shift;
	my	$const = shift;

	Games::Adventure::Symbol->set($out,
		Games::Adventure::Symbol->get($in)->integrate($const));
}
#
#	Simplify an expression
#
sub	simplify
{
	my	$cmd = shift;
	my	$in = shift;
	my	$out = shift;
	my	$const = shift;

	Games::Adventure::Symbol->set($out,
		Games::Adventure::Symbol->get($in)->consolidate($const));
}
##############################################
#	Functions for adventure game
#
#
#	Move from one location to another
#
sub move
{
	my	$cmd = shift;
	my	$dir = shift;

	$dir = $cmd		unless defined($dir);
	Games::Adventure::Location->get_loc_ref()->move($dir);
	Games::Adventure::Objects->list(Games::Adventure::Location->get_location(),
		Games::Adventure::Messages->get("object-list"));
}
#
#	Look at the current location
#
sub look
{
	my	$loc_ref = Games::Adventure::Location->get_loc_ref();

	if ($loc_ref->is_light() || Games::Adventure::Objects->find_lights())
	{
		print $loc_ref->get_long(), "\n";
		Games::Adventure::Objects->list($loc_ref->get_id(),
			Games::Adventure::Messages->get("object-list"), "long");
	}
	else
	{
		Games::Adventure::Messages->print("dark");
	}
}
#
#	Helper function to locate a specified object
#
sub identify_object
{
	my	$adj = shift;
	my	$name = shift;
	my	@obj_id;
	my	$obj;

	if (!defined($adj))
	{
		Games::Adventure::Messages->print("specify");
		return;
	}
	if (defined($name))
	{
		@obj_id = Objects->locate($name, $adj);
		Games::Adventure::Messages->print("no-find-2", $adj, $name)	unless @obj_id > 0;
	}
	else
	{
		$name = $adj;
		@obj_id = Games::Adventure::Objects->locate($name);
		Games::Adventure::Messages->print("no-find-1", $name)	unless @obj_id > 0;
	}
	if (@obj_id > 1)
	{
		Games::Adventure::Messages->print("which-name", $name);
		foreach $obj (@obj_id)
		{
			Games::Adventure::Messages->print("list-name-adj", Objects->get($obj)->get_adj(),
				Objects->get($obj)->get_name());
		}
		return undef;
	}
	return $obj_id[0];
}
#
#	Get a specified object
#
sub get
{
	my	$cmd = shift;
	my	@params = @_;
	my	$obj_id;
	my	$obj;
	my	$name;

	$obj_id = identify_object(@params);
	if (defined($obj_id))
	{
		$obj = Games::Adventure::Objects->get($obj_id);
		$name = $obj->get_name();
		if ($obj->is_immobile())
		{
			Games::Adventure::Messages->print("immobile", $name);
		}
		else
		{
			$obj->location("player1");
		}
	}
}
#
#	Drop a specified object
#
sub drop
{
	my	$cmd = shift;
	my	@params = @_;
	my	$obj_id;
	my	$loc = Games::Adventure::Location->get_location();

	$obj_id = identify_object(@params);
	Games::Adventure::Objects->get($obj_id)->location($loc)	if defined($obj_id);
}
#
#	Print the list of things you are carrying
#
sub inventory
{
	Games::Adventure::Objects->list("player1",
		Games::Adventure::Messages->get("inventory"), "short");
}
#
#	Open an object (if it can be opened)
#
sub open_obj
{
	my	$cmd = shift;
	my	@params = @_;
	my	$obj_id;
	my	$obj;

	$obj_id = identify_object(@params);
	if (defined($obj_id))
	{
		$obj = Games::Adventure::Objects->get($obj_id);
		if ($obj->is_openable())
		{
			if ($obj->is_locked())
			{
				Games::Adventure::Messages->print("locked", $obj->get_name());
			}
			else
			{
				if ($obj->is_open())
				{
					Games::Adventure::Messages->print("already", $obj->get_name(), "open");
				}
				else
				{
					$obj->set_open();
					print $obj->get_long(), "\n";
				}
			}
		}
		else
		{
			Games::Adventure::Messages->print("unable",  "open", $obj->get_name());
		}
	}
}
#
#	Close an object (if it can be closed)
#
sub close_obj
{
	my	$cmd = shift;
	my	@params = @_;
	my	$obj_id;
	my	$obj;

	$obj_id = identify_object(@params);
	if (defined($obj_id))
	{
		$obj = Games::Adventure::Objects->get($obj_id);
		if ($obj->is_openable())
		{
			if ($obj->is_open())
			{
				$obj->set_closed();
				print $obj->get_short(), "\n";
			}
			else
			{
				Games::Adventure::Messages->print("already", $obj->get_name(), "closed");
			}
		}
		else
		{
			Games::Adventure::Messages->print("unable",  "close", $obj->get_name());
		}
	}
}
#
#	Examine an object
#
sub examine
{
	my	$cmd = shift;
	my	@params = @_;
	my	$obj_id;
	my	$obj;

	$obj_id = identify_object(@params);
	if (defined($obj_id))
	{
		$obj = Games::Adventure::Objects->get($obj_id);
		if ($cmd =~ /EXAM/)
		{
			if ($obj->is_read())
			{
				print $obj->get_long(), "\n";
			}
			else
			{
				print $obj->get_detail(), "\n";
			}
		}
		else
		{
			if ($cmd =~ /READ/)
			{
				if ($obj->is_read())
				{
					print $obj->get_detail(), "\n";
				}
				else
				{
					Games::Adventure::Messages->print("unable",  "read", $obj->get_name());
				}
			}
			else
			{
				Games::Adventure::Messages->print("can't", $cmd);
			}
		}
	}
}
#
#	Turn an object on or off
#
sub turn
{
	my	$cmd = shift;
	my	@params = @_;
	my	$name;
	my	$adj;
	my	$state;
	my	$obj_id;
	my	$obj;

	if (@params == 3)
	{
		($adj, $name, $state) = @params;
		$obj_id = identify_object($adj, $name);
	}
	else
	{
		if (@params == 2)
		{
			($name, $state) = @params;
			$obj_id = identify_object($name);
		}
		else
		{
			Games::Adventure::Messages->print("can't", "do what you want");
		}
	}
	if (($state ne "ON") && ($state ne "OFF"))
	{
		Games::Adventure::Messages->print("can't", "turn a $name $state");
		return;
	}
	return		unless defined($obj_id);
	$obj = Games::Adventure::Objects->get($obj_id);
	if ($obj->is_switchable())
	{
		if ((($state eq "ON") && ($obj->is_on())) ||
		    (($state eq "OFF") && (!$obj->is_on())))
		{
			Games::Adventure::Messages->print("already", $name, $state);
			return;
		}
		$obj->set_on()		if $state eq "ON";
		$obj->set_off()		if $state eq "OFF";
	}
	else
	{
		Games::Adventure::Messages->print("no-switch", $name);
	}
}
#
#	Drink an object, if drinkable
#
sub drink
{
	my	$cmd = shift;
	my	@params = @_;
	my	$obj_id = identify_object(@params);

	if (defined($obj_id))
	{
		my	$obj_ref = Games::Adventure::Objects->get($obj_id);
		my	$name = $obj_ref->get_name();

		if ($obj_ref->is_drink())
		{
			Games::Adventure::Messages->print("drunk", $name);
			$obj_ref->location("none")		if $obj_ref->is_consumed();
#
#	Stick code here for poison
#
		}
		else
		{
			Games::Adventure::Messages->print("no drink", $name);
		}
	}
}
#
#	Eat an object, if edable
#
sub eat
{
	my	$cmd = shift;
	my	@params = @_;
	my	$obj_id = identify_object(@params);

	if (defined($obj_id))
	{
		my	$obj_ref = Games::Adventure::Objects->get($obj_id);
		my	$name = $obj_ref->get_name();

		if ($obj_ref->is_food())
		{
			Games::Adventure::Messages->print("eat", $name);
			$obj_ref->location("none")		if $obj_ref->is_consumed();
#
#	Stick code here for poison
#
		}
		else
		{
			Games::Adventure::Messages->print("no eat", $name);
		}
	}
}
#
#	Print a help message
#
sub help
{
	Games::Adventure::Messages->print("help");
}
#
#	Reload the XML file and restart the game
#
sub reset_reload
{
	Games::Adventure::Objects->reset();
	Games::Adventure::Messages->reset();
	Games::Adventure::Location->reset();
	Games::Adventure::Load_xml->load(xml_file);
	Games::Adventure::Location->set_location("start");
	Games::Adventure::Messages->print("start");
	Games::Adventure::Objects->list("start",
		Games::Adventure::Messages->get("object-list"), "long");
}

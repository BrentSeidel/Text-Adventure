#!/usr/bin/perl -w
#
#	This module implements methods for location objects in the
#	adventure game.
#
use	Carp;
use strict;

package	Location;
#
#	Package variables and accessors
#
{
	my	%loctab;		# Table containing all the location objects
	my	$current_loc;	# Location ID of the current location

	sub	get_location
	{
		return ($current_loc);
	}

	sub get_loc_ref
	{
		return ($loctab{$current_loc});
	}

	sub	set_location
	{
		my	$class = shift;
		my	$temp_loc = shift;

		if (defined($loctab{$temp_loc}))
		{
			$current_loc = $temp_loc;
		}
		else
		{
			Carp::carp("Attempt to set current location to <$temp_loc> - an invalid value\n");
		}
	}

	sub set_loc_data
	{
		my	$class = shift;
		my	$loc_id = shift;
		my	$loc_ref = shift;

		$loctab{$loc_id} = $loc_ref;
	}

	sub get_loc_data
	{
		my	$class = shift;
		my	$loc_id = shift;

		return ($loctab{$loc_id});
	}

	sub reset_locations
	{
		undef %loctab;
	}
}
#
#	Create a new location object
#
sub new
{
	my	$class		= shift;
	my	$id			= shift;
	my	$flags		= shift;
	my	$exits		= shift;
	my	$long		= shift;
	my	$short		= shift;
	my	$self = {};

	$self->{"_id"} = $id;
	$self->{"_flags"} = $flags;
	$self->{"_exits"} = $exits;
	$self->{"_long"} = $long;
	$self->{"_short"} = $short;
	bless $self, $class;
	Location->set_loc_data($id, $self);
	return $self;
}
#
#	Accessor methods
#
sub get_id			{ $_[0]->{"_id"}}
sub get_flags		{ $_[0]->{"_flags"}}
sub	get_exits		{ $_[0]->{"_exits"}}
sub get_long		{ $_[0]->{"_long"}}
sub get_short		{ $_[0]->{"_short"}}
#
#	Flag methods
#
sub	is_entered0		{$_[0]->{"_flags"} & oct("0x00000001")}
sub	is_entered1		{$_[0]->{"_flags"} & oct("0x00000002")}
sub	is_entered2		{$_[0]->{"_flags"} & oct("0x00000004")}
sub	is_entered3		{$_[0]->{"_flags"} & oct("0x00000008")}
sub	is_entered4		{$_[0]->{"_flags"} & oct("0x00000010")}
sub	is_entered5		{$_[0]->{"_flags"} & oct("0x00000020")}
sub	is_entered6		{$_[0]->{"_flags"} & oct("0x00000040")}
sub	is_entered7		{$_[0]->{"_flags"} & oct("0x00000080")}
sub	is_light		{$_[0]->{"_flags"} & oct("0x00000100")}
sub	is_air			{$_[0]->{"_flags"} & oct("0x00000200")}
sub	is_prints		{$_[0]->{"_flags"} & oct("0x00000400")}
sub	is_ship			{$_[0]->{"_flags"} & oct("0x00000800")}

sub	set_entered0	{$_[0]->{"_flags"} |= oct("0x00000001")}
sub	set_entered1	{$_[0]->{"_flags"} |= oct("0x00000002")}
sub	set_entered2	{$_[0]->{"_flags"} |= oct("0x00000004")}
sub	set_entered3	{$_[0]->{"_flags"} |= oct("0x00000008")}
sub	set_entered4	{$_[0]->{"_flags"} |= oct("0x00000010")}
sub	set_entered5	{$_[0]->{"_flags"} |= oct("0x00000020")}
sub	set_entered6	{$_[0]->{"_flags"} |= oct("0x00000040")}
sub	set_entered7	{$_[0]->{"_flags"} |= oct("0x00000080")}

#
#	Other methods
#
sub	move
{
	my	$self = shift;
	my	$dir = shift;
	my	$exits = $self->get_exits();
	my	$exit = $exits->{$dir};
	my	$dest;
	my	$closed;

	$dest = $exit->get_dest();
	$closed = $exit->get_closed();
#
#	Check if there is potentially some way to move in that direction
#
	if (defined($dest))
	{
		my	$obj_id = $exit->get_obj();
		my	$new_loc = Location->get_loc_data($dest);
		my	$desc;

#
#	Check if the way is closed
#
		if (defined($obj_id) && (!Objects->get_object($obj_id)->is_open()))
		{
			if (defined($exit->get_closed()))
			{
				print Messages->get_msg($exit->get_closed()), "\n";
			}
			else
			{
				print "The ", Objects->get_oject($obj_id)->get_name(), " is closed\n";
			}
			return;
		}
		Location->set_location($new_loc->get_id());
#
#	Check if the way closes behind you
#
		if ($exit->is_closes())
		{
			Objects->get_object($obj_id)->set_closed();
			print Objects->get_object($obj_id)->get_short(), "\n";
		}
#
#	Check if the new location is illuminated
#
		if ($new_loc->is_light() || Objects->find_lights())
		{
			if ($new_loc->is_entered0())
			{
				print $new_loc->get_short(), "\n";
			}
			else
			{
				print $new_loc->get_long(), "\n";
			}
			$new_loc->set_entered0();
		}
		else
		{
			print "It's dark in here.\n";
		}
		return;
	}
#
#	Otherwise print a message indicating the move cannot be made.
#
	if (defined($closed))
	{
		print Messages->get_msg($closed), "\n";
		return;
	}
	print "You are unable to move in that direction\n";
	print "Missing message for room described by:  \n", $self->get_long();
}

#
#	The objects for exits are intimately connected
#	with locations since each location has 10 exits
#	associated with it.
#
package Exits;
sub new
{
	my	$class		= shift;
	my	$dest		= shift;
	my	$flags		= shift;
	my	$closed		= shift;
	my	$obj		= shift;
	my	$self = {};

	$self->{"_dest"} = $dest;
	$self->{"_flags"} = $flags;
	$self->{"_closed"} = $closed;
	$self->{"_obj"} = $obj;
	bless $self, $class;
	return $self;
}
#
#	Accessor methods
#
sub	get_dest		{ $_[0]->{"_dest"}}
sub get_flags		{ $_[0]->{"_flags"}}
sub get_closed		{ $_[0]->{"_closed"}}
sub	get_obj			{ $_[0]->{"_obj"}}
#
#	Flag methods
#
sub	is_closes		{$_[0]->{"_flags"} & oct("0x0001")}
sub	is_flag01		{$_[0]->{"_flags"} & oct("0x0002")}
sub	is_flag02		{$_[0]->{"_flags"} & oct("0x0004")}
sub	is_flag03		{$_[0]->{"_flags"} & oct("0x0008")}
sub	is_flag04		{$_[0]->{"_flags"} & oct("0x0010")}
sub	is_flag05		{$_[0]->{"_flags"} & oct("0x0020")}
sub	is_flag06		{$_[0]->{"_flags"} & oct("0x0040")}
sub	is_flag07		{$_[0]->{"_flags"} & oct("0x0080")}
sub	is_flag08		{$_[0]->{"_flags"} & oct("0x0100")}
sub	is_flag09		{$_[0]->{"_flags"} & oct("0x0200")}
sub	is_flag10		{$_[0]->{"_flags"} & oct("0x0400")}
sub	is_flag11		{$_[0]->{"_flags"} & oct("0x0800")}
sub	is_flag12		{$_[0]->{"_flags"} & oct("0x1000")}
sub	is_flag13		{$_[0]->{"_flags"} & oct("0x2000")}
sub	is_flag14		{$_[0]->{"_flags"} & oct("0x4000")}
sub	is_flag15		{$_[0]->{"_flags"} & oct("0x8000")}

1;
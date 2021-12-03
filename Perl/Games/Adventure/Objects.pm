#!/usr/bin/perl -w
#
#	This module implements methods for object objects in the
#	adventure game.
#
use strict;

package	Objects;
{
	my	%objtab;

	sub get_object
	{
		my $class = shift;
		my $obj_id = shift;

		return ($objtab{$obj_id});
	}

	sub set_object
	{
		my	$class = shift;
		my	$obj_id = shift;
		my	$obj_ref = shift;

		$objtab{$obj_id} = $obj_ref;
	}

	sub debug_list_objects
	{
		my	$id;

		print "Object table:\n";
		foreach $id (sort(keys(%objtab)))
		{
			print "  Object $id is a ", $objtab{$id}->get_name, "\n";
		}
		return;
	}

	sub reset_objects
	{
		undef %objtab;
	}
#
#	Selects objects by name and optional adjective.
#
	sub	locate_object
	{
		my	$class = shift;
		my	$name = shift;
		my	$adj = shift;
		my	@found;
		my	$obj_id;
		my	$obj;


		undef @found;
		if (defined($adj))
		{
			foreach $obj_id (keys(%objtab))
			{
				$obj = $objtab{$obj_id};
				push @found, $obj_id	if (($obj->get_name() eq $name) &&
											($obj->get_adjective() eq $adj));
			}
		}
		else
		{
			foreach $obj_id (sort(keys(%objtab)))
			{
				$obj = $objtab{$obj_id};
				push @found, $obj_id	if ($obj->get_name() eq $name);
			}
		}
		return (Objects->is_here(@found));
	}
#
#	Find if there is a light in the current location
#
	sub	find_lights
	{
		my	$class = shift;
		my	@found;
		my	$obj_id;
		my	$obj;


		undef @found;
		foreach $obj_id (keys(%objtab))
		{
			$obj = $objtab{$obj_id};
			if ($obj->is_light())
			{
				if ($obj->is_switchable())
				{
					push @found, $obj_id	if $obj->is_on();
				}
				else
				{
					push @found, $obj_id;
				}
			}
		}
		return (Objects->is_here(@found));
	}
#
#	List the objects in the current location
#
	sub	list
	{
		my	$class = shift;
		my	$location = shift;
		my	$msg = shift;
		my	$desc = shift;
		my	$obj;

		foreach $obj (sort(keys(%objtab)))
		{
			if (($objtab{$obj}->location() eq $location) &&
				(!$objtab{$obj}->is_special()))
			{
				if (defined($msg))
				{
					print "$msg\n";
					undef $msg;
				}
				if (defined($desc) && ($desc eq "long"))
				{
					print $objtab{$obj}->get_long(), "\n";
				}
				else
				{
					print $objtab{$obj}->get_short(), "\n";
				}
			}
		}
	}
}
sub BEGIN
{
	print "Loading package Objects\n";
}
#
#	Create a new object object
#
sub new
{
	my	$class		= shift;
	my	$id			= shift;
	my	$name		= shift;
	my	$adjective	= shift;
	my	$flags		= shift;
	my	$value		= shift;
	my	$mass		= shift;
	my	$capacity	= shift;
	my	$power		= shift;
	my	$location	= shift;
	my	$long		= shift;
	my	$short		= shift;
	my	$detail		= shift;
	my	$self = {};

	$self->{"_id"} = $id;
	$self->{"_name"} = $name;
	$self->{"_adj"} = $adjective;
	$self->{"_flags"} = $flags;
	$self->{"_value"} = $value;
	$self->{"_mass"} = $mass;
	$self->{"_capacity"} = $capacity;
	$self->{"_power"} = $power;
	$self->{"_location"} = $location;
	$self->{"_long"} = $long;
	$self->{"_short"} = $short;
	$self->{"_detail"} = $detail;
	bless $self, $class;
	Objects->set_object($id, $self);
	return $self;
}

#
#	Accessor methods
#
sub get_id			{ $_[0]->{"_id"}}
sub get_name		{ $_[0]->{"_name"}}
sub get_adjective	{ $_[0]->{"_adj"}}
sub get_value		{ $_[0]->{"_value"}}
sub get_mass		{ $_[0]->{"_mass"}}
sub get_capacity	{ $_[0]->{"_capacity"}}
sub get_long		{ $_[0]->{"_long"}}
sub get_short		{ $_[0]->{"_short"}}
sub get_detail		{ $_[0]->{"_detail"}}
sub	location
{
	my	($self, $new) = @_;
	defined($new) and $self->{"_location"} = $new;
	return($self->{"_location"});
}

sub	power
{
	my	($self, $new) = @_;
	defined($new) and $self->{"_power"} = $new;
	return($self->{"_location"});
}
#
#	Flag methods
#
sub	is_immobile		{$_[0]->{"_flags"} & oct("0x00000001")}
sub is_key			{$_[0]->{"_flags"} & oct("0x00000002")}
sub is_weapon		{$_[0]->{"_flags"} & oct("0x00000004")}
sub is_food			{$_[0]->{"_flags"} & oct("0x00000008")}
sub	is_drink		{$_[0]->{"_flags"} & oct("0x00000010")}
sub	is_switchable	{$_[0]->{"_flags"} & oct("0x00000020")}
sub	is_poison		{$_[0]->{"_flags"} & oct("0x00000040")}
sub	is_read			{$_[0]->{"_flags"} & oct("0x00000080")}
sub	is_on			{$_[0]->{"_flags"} & oct("0x00000100")}
sub	is_denzin		{$_[0]->{"_flags"} & oct("0x00000200")}
sub	is_vehicle		{$_[0]->{"_flags"} & oct("0x00000400")}
sub is_door			{$_[0]->{"_flags"} & oct("0x00000800")}
sub	is_open			{$_[0]->{"_flags"} & oct("0x00001000")}
sub	is_locked		{$_[0]->{"_flags"} & oct("0x00002000")}
sub	is_openable		{$_[0]->{"_flags"} & oct("0x00004000")}
sub	is_lockable		{$_[0]->{"_flags"} & oct("0x00008000")}
sub	is_special		{$_[0]->{"_flags"} & oct("0x00010000")}
sub	is_light		{$_[0]->{"_flags"} & oct("0x00020000")}

sub	set_on			{$_[0]->{"_flags"} |= oct("0x00000100")}
sub set_off			{$_[0]->{"_flags"} &= ~oct("0x00000100")}
sub	set_open		{$_[0]->{"_flags"} |= oct("0x00001000")}
sub	set_closed		{$_[0]->{"_flags"} &= ~oct("0x00001000")}
sub	set_locked		{$_[0]->{"_flags"} |= oct("0x00002000")}
sub	set_unlocked	{$_[0]->{"_flags"} &= ~oct("0x00002000")}
#
#	Other methods
#
#
#	See if any of the items can be found in the current location.  First see
#	if the location matches, then check the exits from the location.  Finally
#	see if the player is carrying the object.
#
sub	is_here
{
	my	$class = shift;
	my	@found = @_;
	my	$location = Location->get_location();
	my	$loc_ref = Location->get_loc_ref();
	my	$exits = $loc_ref->get_exits();
	my	@obj_here;
	my	$obj_id;
	my	$obj_loc;
	my	$obj;
	my	$exit;
	my	$exit_obj;

	undef @obj_here;
	if (@found > 0)
	{
		foreach $obj_id (@found)
		{
			$obj = Objects->get_object($obj_id);
			$obj_loc = $obj->location();
			push @obj_here, $obj_id		if ($obj_loc eq $location);
			push @obj_here, $obj_id		if ($obj_loc eq "player1");
			foreach $exit (keys(%{$exits}))
			{
				my	$exit_obj = $exits->{$exit}->get_obj();
				if (defined($exit_obj) && ($obj_id eq $exit_obj))
				{
					push @obj_here, $obj_id;
				}
			}
		}
	}
	return @obj_here;
}

1;

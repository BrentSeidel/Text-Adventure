#!/usr/bin/perl
#
#	This package is used to encapsulate the message table
#
use	strict;
use	Carp;

package	Messages;

{
	my	%msgtab;		# Table containing all the messages

	sub set_msg
	{
		my	$class = shift;
		my	$msg_id = shift;
		my	$msg = shift;

		$msgtab{$msg_id} = $msg;
	}

	sub get_msg
	{
		my	$class = shift;
		my	$msg_id = shift;
		my	$msg = $msgtab{$msg_id};

		Carp::carp "Attempt to get undefined message ID <$msg_id>\n"
				unless defined($msg);
		return ($msg);
	}

	sub reset_messages
	{
		undef %msgtab;
	}
}

1;
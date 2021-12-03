#!/usr/bin/perl -w
#
#	This package contains functions to load XML for the adventure game
#
use strict;

use Objects;
use Location;
use	Messages;
use	Code;

package Load_xml;

{
	my	$version;
	my	$title;

	sub	set_version	{ $version = $_[1] }
	sub get_version { $version }
	sub	set_title { $title = $_[1] }
	sub get_title { $title }
}

sub load_xml;
#sub extract_id;
sub decode_text;
sub extract_value;

sub load_xml
{
	my	$cmdtab = shift;
	my	$xml_file = ":adventure.xml";
	my	$line;
	my	$state;
	my	$cmd;
	my	$func;
	my	$version;
#
#	Read commands from an XML like file
#
	print "Loading data...\n";
	open(IN, $xml_file) || die "Can't open $xml_file, $!\n";
	$/ = ">";
	$state = 0;
	$line = <IN>;		# The first thing should be the XML version number
	if ($line =~ /\<\?xml version=\"(.*)\"\?\>/)
	{
		$version = $1;
		if ($version ne "1.0")
		{
			die "Incorrect XML version ($version) found in $xml_file\n";
		}
	}
	else
	{
		die "No XML version found in $xml_file\n";
	}
	$line = <IN>;		# The second line should indicate the doc type
	if ($line !~ /\<adventure\>/)
	{
		die "Invalid document type ($line) found in $xml_file\n";
	}
	while (defined($line = <IN>))
	{
		$line =~ s/^\s+//;
		if ($line =~ /^\<\!\-\-\s/)	# Ignore comments
		{
			next;
		}
		if ($line =~ /^\<print/)	# Element to be printed
		{
			print $line, "\n";
			next;
		}
#
#	XML Element is a file definition
#
		if ($line =~ /^\<file /)
		{
			set_version(extract_value("version", $line, 1));
			set_title(extract_value("title", $line, 1));
			next;
		}
#
#	XML Element is a code definition
#
		if ($line =~ /^\<code /)
		{
			my	$code_id = extract_value("id", $line, 1);
			my	$code = <IN>;

			$code =~ s/\s+\<\/code\>$//;
			$code =~ s/^\s+//;
			$code = decode_text($code);
			Code->new($code_id, $code);
			next;
		}
#
#	XML Element is a command definition
#
		if ($line =~ /^\<cmd /)	# if element is a command definition
		{
			$cmd = extract_value("name", $line, 1);
			$func = extract_value("func", $line, 1);
			$cmd =~ tr/a-z/A-Z/;
			if (length($cmd) > 4)	# Create command abbreviations
			{
				my ($len, $str);
				for ($len = 4; $len <= length($cmd); $len++)
				{
					$str = substr($cmd, 0, $len);
					$cmdtab->{$str} = $func;
				}
			}
			else
			{
				$cmdtab->{$cmd} = $func;
			}
			next;
		}
#
#	XML element is an object definition
#
		if ($line =~ /^\<obj /)	# if element is an object definition
		{
			my	$id			= extract_value("id", $line, 1);
			my	$name		= extract_value("name", $line, 1);
			my	$adjective	= extract_value("adj", $line, 0);
			my	$flags		= oct(extract_value("flags", $line, 1));
			my	$value		= extract_value("value", $line, 1);
			my	$mass		= extract_value("mass", $line, 1);
			my	$capacity	= extract_value("capacity", $line, 1);
			my	$power		= extract_value("power", $line, 1);
			my	$location	= extract_value("location", $line, 1);
			my	$long;
			my	$short;
			my	$detail;

			$name =~ tr/a-z/A-Z/			if defined($name);
			$adjective =~ tr/a-z/A-Z/		if defined($adjective);
			while (defined($line = <IN>))
			{
				last		if ($line =~ /\<\/obj\>/);
				if ($line =~ /\<long/)
				{
					$long = <IN>;
					$long =~ s/\s+\<\/long\>$//;
					$long =~ s/^\s+//;
					$long = decode_text($long);
					next;
				}
				if ($line =~ /\<short/)
				{
					$short = <IN>;
					$short =~ s/\s+\<\/short\>$//;
					$short =~ s/^\s+//;
					$short = decode_text($short);
					next;
				}
				if ($line =~ /<detail/)
				{
					$detail = <IN>;
					$detail =~ s/\s+\<\/detail\>$//;
					$detail =~ s/^\s+//;
					$detail = decode_text($detail);
					next;
				}
				print "\"$line\" is an unrecognized XML token in an object\n";
			}
			Objects->new($id, $name, $adjective, $flags, $value,
				$mass, $capacity, $power, $location, $long, $short, $detail);
			next;
		}
#
#	XML Element is a location definition
#
		if ($line =~ /^\<loc /)	# if element is a location definition
		{
			my	$id			= extract_value("id", $line, 1);
			my	$flags		= oct(extract_value("flags", $line, 1));
			my	$long;
			my	$short;
			my	$exits = {};

			while (defined($line = <IN>))
			{
				last		if ($line =~ /\<\/loc\>/);
				if ($line =~ /\<exit/)
				{
					my	$dir	= extract_value("dir", $line, 1);
					my	$dest	= extract_value("dest", $line, 0);
					my 	$flags	= oct(extract_value("flags", $line, 1));
					my	$closed	= extract_value("closed", $line, 0);
					my	$obj	= extract_value("object", $line, 0);

					$dir =~ tr/a-z/A-Z/;
					$exits->{$dir} = Exits->new($dest, $flags, $closed, $obj);
					next;
				}
				if ($line =~ /\<long/)
				{
					$long = <IN>;
					$long =~ s/\s+\<\/long\>$//;
					$long =~ s/^\s+//;
					$long = decode_text($long);
					next;
				}
				if ($line =~ /\<short/)
				{
					$short = <IN>;
					$short =~ s/\s+\<\/short\>$//;
					$short =~ s/^\s+//;
					$short = decode_text($short);
					next;
				}
			}
			Location->new($id, $flags, $exits, $long, $short);
			next;
		}
#
#	XML element is a message definition
#
		if ($line =~ /^\<msg /)	# if element is a message definition
		{
			my	$id			= extract_value("id", $line, 1);
			my	$msg;

			$msg = <IN>;
			$msg =~ s/\s+\<\/msg\>$//;
			$msg =~ s/^\s+//;
			$msg = decode_text($msg);
			Messages->set_msg($id, $msg);
			next;
		}
		last	if $line =~ /\<\/adventure\>/;
		print "\"$line\" is an unrecognized XML token\n";
	}
	close(IN);
	$/ = "\n";
}

#sub extract_id
#{
#	my	$line = shift;
#	my	$id;
#
#	if ($line =~ /id\=\"([^"]*)\"/)
#	{
#		$id = $1
#	}
#	else
#	{
#		warn "ID not found for object in line $line\n";
#	}
#}

sub extract_value
{
	my	$name = shift;
	my	$line = shift;
	my	$required = shift;
	my	$result;
	my	$cmd = "(\$result) = (\$line =~ / $name=\"([^\"]*)\"/);";

	if ($required)
	{
		unless (eval "$cmd")
		{
			warn "Value for $name not found in line $line\n";
		}
	}
	else
	{
		eval "$cmd";
	}
	return($result);
}

sub decode_text
{
	my	$text = shift;

	$text =~ s/\&lt\;/\</g;		# Translate element &lt;
	$text =~ s/\&gt\;/\>/g;		# Translate element &gt;
	$text =~ s/\&amp\;/\&/g;	# Translate element &amp; (must be last one);
	return ($text);
}
1;
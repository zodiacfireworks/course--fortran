#!/usr/bin/env perl

# Copyright (C) 2011
# Free Software Foundation, Inc.
#
# This file is part of the gtk-fortran gtk+ Fortran Interface library.
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License along with
# this program; see the files COPYING3 and COPYING.RUNTIME respectively.
# If not, see <http://www.gnu.org/licenses/>.
#
# Contributed by James Tappin 05/11/2011.

# Extracts the structure definitions for Gdk events from the gdk 
# header files.

my $gdkvers="3";
my $gdktypes="/usr/include/gtk-".$gdkvers.".0/gdk/gdktypes.h";
my $gdkevents="/usr/include/gtk-".$gdkvers.".0/gdk/gdkevents.h";
my $ftninterface="gdkevents-auto".$gdkvers.".f90";

# Type conversions.
# Defaults are pointers "type(c_ptr)", actual objects "integer(kind=c_int)" 
# (i.e. enumeration) (We assume that gdk-auto.f90 contains the actual 
# enumerations). As structures are defined, they are added to the hash.

my %conversions = ("GdkAtom" => "type(c_ptr)",
		   "GdkNativeWindow" => "type(c_ptr)",
		   "cairo_region_t" => "type(c_ptr)",
		   "gint" => "integer(kind=c_int)",
		   "guint" => "integer(kind=c_int)",
		   "gint8" => "integer(kind=c_int8_t)",
		   "guint8" => "integer(kind=c_int8_t)",
		   "gint16" => "integer(kind=c_int16_t)",
		   "guint16" => "integer(kind=c_int16_t)",
		   "guint32" => "integer(kind=c_int32_t)",
		   "gdouble" => "real(kind=c_double)",
		   "gboolean" => "integer(kind=c_int)",
		   "gshort" => "integer(kind=c_short)",
		   "gushort" => "integer(kind=c_short)",
		   "char" => "character(kind=c_char)",
		   "short" => "integer(kind=c_short)",
		   "long" => "integer(kind=c_long)");

# GDK structure declarations have the form:
# struct _Name
# {
#   type name;
#   ...
# }

my $sspattern = "^struct _([a-zA-Z]+)";
my $sepattern = "};";
my $sflag = 0;
my $dpattern = "^ +([a-zA-Z0-9_]+) +(.+);";  # A "direct" type
my $dppattern = "^ +([a-zA-Z0-9_]+) +\\*(.+);"; # A pointer to something
my $tname;
my $now = gmtime;

rename($ftninterface, "${ftninterface}.old") if ( -f $ftninterface);

open(FGDKE, ">", $ftninterface) || die "Failed to open $ftninterface:$!\n";
print FGDKE "! Automatically generated by extract_events.pl on $now Z\n";
print FGDKE "! Please do not modify (unless you really know what you're doing).\n";
print FGDKE "! This file is part of the gtk-fortran GTK+ Fortran Interface library.\n";
print FGDKE "! GNU General Public License version 3\n\n";

print FGDKE "module gdk_events\n";
print FDGKE "  ! GDK events and related structures\n";
print FDGKE "  ! Automatically extracted from gdktypes.h & gdkevents.h\n";
print FGDKE "  use iso_c_binding\n\n";
print FGDKE "  implicit none\n\n";

foreach $hfile ($gdktypes, $gdkevents) {
    open(GDKE, "<", $hfile) || die "Failed to open $hfile:$!\n";
  LINE: 
    while (<GDKE>) {
	chop();
	if ( /$sspattern/ ) {  # Start a new definition
	    print FGDKE "  type, bind(c) :: $1\n";
	    $sflag = 1;
	    $tname = $1;
	    $conversions{$1} = "type($1)";
	    next LINE;
	}
	if ( $sflag ) {  # We are defining a structure
	    next LINE  if ($_ eq "{"); # Skip the starting delimiter 
	    next LINE if ($_ eq ""); # Skip blank lines

	    if ($_ eq $sepattern) { # Ending delimiter close out the definition
		$sflag = 0;
		print FGDKE "  end type $tname\n\n";
		next LINE;
	    }
	    if ( /$dppattern/ ) { # A pointer to something 
                                           #-- always a c_ptr
		$list = $2;
		$list =~ tr/:/=/;
		print FGDKE "    type(c_ptr) :: $list   ! -> $1\n";
	    }
	    elsif ( /$dpattern/ ) { # A direct declaration
		$list = $2;
		$list =~ tr/:/=/;
		if (defined($conversions{$1})) { # A known type
		    print FGDKE "    $conversions{$1} :: $list  ! $1\n";
		} else { # Unknown type assume it's an enum
		    print FGDKE "    integer(kind=c_int) :: $list    ! enum $1\n";
		}
	    } else {
		print FGDKE "!$_ ******\n";
		print "Unrecognized construct in $tname\n$_\n";
		print "You may need to edit $ftninterface to resolve this\n";
	    }
	}
    }
    close GDKE;
}

print FGDKE "end module gdk_events\n";
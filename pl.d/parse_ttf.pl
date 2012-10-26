#!/usr/bin/perl -s
use 5.10.0;
use utf8;
use strict;
use warnings;
use open qw( :std :utf8 );
use Data::Dumper;
use Fatal qw( open close sysseek sysread );

$Data::Dumper::Terse    = 1;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent   = 1;

my $filename = ( our $f );
my $buf;
my %ttf;

open my $FH, "<:raw", $filename;

$ttf{offset_subtable} = read_offset_subtable( $FH );

foreach ( 1 .. $ttf{offset_subtable}{num_tables} ) {
    my $table_ref = read_table( $FH );
    $ttf{ $table_ref->{tag} } = $table_ref;
}

read_post_table( $FH, %ttf );

say Dumper \%ttf;

exit;

sub read_post_table {
    my( $FH, %ttf ) = @_;
    my $buf;
say Dumper $ttf{post};

    sysseek $FH, $ttf{post}{offset}, 0;

    sysread $FH, $buf, 2;
    my $format = unpack "s>", $buf;
say "format: $format";

    sysread $FH, $buf, 2;
    my $italic_angle = unpack "s>", $buf;

    sysread $FH, $buf, 2;
    my $underline_position = unpack "s>", $buf;

    sysread $FH, $buf, 2;
    my $underline_thickness = unpack "s>", $buf;

    sysread $FH, $buf, 4;
    my $is_fixed_pitch = unpack "L>", $buf;

    sysread $FH, $buf, 4;
    my $min_mem_type42 = unpack "L>", $buf;

    sysread $FH, $buf, 4;
    my $max_mem_type42 = unpack "L>", $buf;

    sysread $FH, $buf, 4;
    my $min_mem_type1 = unpack "L>", $buf;

    sysread $FH, $buf, 4;
    my $max_mem_type1 = unpack "L>", $buf;


}

sub read_table {
    my $FH = shift;
    my %table;

    sysread $FH, $buf, 4;
    $table{tag} = $buf;

    sysread $FH, $buf, 4;
    $table{check_sum} = unpack "L>", $buf;

    sysread $FH, $buf, 4;
    $table{offset} = unpack "L>", $buf;

    sysread $FH, $buf, 4;
    $table{length} = unpack "L>", $buf;

    return \%table;
}

sub read_offset_subtable {
    my $FH = shift;
    my %offset;

    sysread $FH, $buf, 4;
    $offset{scalar_type} = unpack "L>", $buf;

    sysread $FH, $buf, 2;
    $offset{num_tables} = unpack "S>", $buf;

    sysread $FH, $buf, 2;
    $offset{search_range} = unpack "S>", $buf;

    sysread $FH, $buf, 2;
    $offset{entry_selector} = unpack "S>", $buf;

    sysread $FH, $buf, 2;
    $offset{range_shift} = unpack "S>", $buf;

    return \%offset;
}


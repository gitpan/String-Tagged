#!/usr/bin/perl -w

use strict;

use Test::More tests => 6;

use String::Tagged;

my $str = String::Tagged->new( "A string with BOLD and ITAL tags" );

$str->apply_tag( -1, -1, message => 1 );
$str->apply_tag( 14,  4, bold    => 1 );
$str->apply_tag( 23,  4, italic  => 1 );

my @tags;

undef @tags;
$str->iter_tags( sub { push @tags, [ @_ ] }, start => 20 );
is_deeply( \@tags, 
           [
              [  0, 32, message => 1 ],
              [ 23,  4, italic  => 1 ],
           ],
           'tags list with start offset' );

undef @tags;
$str->iter_tags( sub { push @tags, [ @_ ] }, end => 20 );
is_deeply( \@tags, 
           [
              [  0, 32, message => 1 ],
              [ 14,  4, bold    => 1 ],
           ],
           'tags list with end limit' );

sub fetch_tags
{
   my ( $start, $len, %tags ) = @_;
   push @tags, [ $start, $len, map { $_ => $tags{$_} } sort keys %tags ]
}

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags, start => 20 );
is_deeply( \@tags, 
           [
              [ 20, 3, message => 1 ],
              [ 23, 4, italic  => 1, message => 1 ],
              [ 27, 5, message => 1 ],
           ],
           'tags list non-overlapping with start offset' );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags, end => 20 );
is_deeply( \@tags, 
           [
              [  0, 14, message => 1 ],
              [ 14,  4, bold    => 1, message => 1 ],
              [ 18,  2, message => 1 ],
           ],
           'tags list non-overlapping with end limit' );

my @substrs;
sub fetch_substrs
{
   my ( $substr, %tags ) = @_;
   push @substrs, [ $substr, map { $_ => $tags{$_} } sort keys %tags ]
}

undef @substrs;
$str->iter_substr_nooverlap( \&fetch_substrs, start => 20 );
is_deeply( \@substrs, 
           [
              [ "nd ",   message => 1 ],
              [ "ITAL",  italic  => 1, message => 1 ],
              [ " tags", message => 1 ],
           ],
           'substrs non-overlapping with start offset' );

undef @substrs;
$str->iter_substr_nooverlap( \&fetch_substrs, end => 20 );
is_deeply( \@substrs, 
           [
              [ "A string with ",   message => 1 ],
              [ "BOLD",             bold    => 1, message => 1 ],
              [ " a",               message => 1 ],
           ],
           'substrs non-overlapping with start offset' );

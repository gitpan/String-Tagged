#!/usr/bin/perl -w

use strict;

use Test::More tests => 6;

use String::Tagged;

my $str = String::Tagged->new( "Hello, world" );
$str->apply_tag(  1, 1, e => 1 );
$str->apply_tag( -1, -1, message => 1 );

my @tags;
sub fetch_tags
{
   my ( $start, $len, %tags ) = @_;
   push @tags, [ $start, $len, map { $_ => $tags{$_} } sort keys %tags ]
}

$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [ 0,  1, message => 1 ],
              [ 1,  1, e => 1, message => 1 ],
              [ 2, 10, message => 1 ],
           ],
           'tags list initially' );

$str->set_substr( 7, 5, "planet" );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [ 0,  1, message => 1 ],
              [ 1,  1, e => 1, message => 1 ],
              [ 2, 11, message => 1 ],
           ],
           'tags list after first substr' );

$str->apply_tag( 5, 1, comma => 1 );

$str->set_substr( 0, 5, "Goodbye" );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [ 0, 7, message => 1 ],
              [ 7, 1, comma => 1, message => 1 ],
              [ 8, 7, message => 1 ],
           ],
           'tags list after second substr' );

$str->set_substr( 7, 1, "" );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [ 0, 14, message => 1 ],
           ],
           'tags list after collapsing substr' );

$str->apply_tag( 0, 7, goodbye => 1 );
$str->apply_tag( 8, 6, planet => 1 );

$str->set_substr( 2, 10, "urm" );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [ 0, 2, goodbye => 1, message => 1 ],
              [ 2, 3, message => 1 ],
              [ 5, 2, message => 1, planet => 1 ],
           ],
           'tags list after straddling substr' );

$str->set_substr( 0, 0, "I say, " );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [  0, 7, message => 1 ],
              [  7, 2, goodbye => 1, message => 1 ],
              [  9, 3, message => 1 ],
              [ 12, 2, message => 1, planet => 1 ],
           ],
           'tags list after prepend substr' );

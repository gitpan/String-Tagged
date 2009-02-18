#!/usr/bin/perl -w

use strict;

use Test::More tests => 25;

use String::Tagged;

my $str = String::Tagged->new( "Hello, world" );

is_deeply( [ $str->tagnames ], [], 'No tags defined initially' );

$str->apply_tag( 0, 12, message => 1 );

is_deeply( [ $str->tagnames ], [qw( message )], 'message tag now defined' );

my @tags;
$str->iter_tags( sub { push @tags, [ @_ ] } );
is_deeply( \@tags, 
           [
              [ 0, 12, message => 1 ],
           ],
           'tags list after apply message' );

my @extents;
$str->iter_extents( sub { push @extents, $_[0] } );

is( scalar @extents, 1, 'one extent from iter_extents' );

my $e = $extents[0];
can_ok( $e, qw( string start length end substr ) );

is( $e->string, $str, '$e->string' );
is( $e->start,   0, '$e->start' );
is( $e->length, 12, '$e->length' );
is( $e->end,    12, '$e->end' );
is( $e->substr, "Hello, world", '$e->substr' );

is_deeply( $str->get_tags_at( 0 ), 
           { message => 1 },
           'tags at pos 0' );

is( $str->get_tag_at( 0, "message" ), 1, 'message tag is 1 at pos 0' );

$str->apply_tag( 6, 1, space => 1 );

is_deeply( [ sort $str->tagnames ], [qw( message space )], 'space tag now also defined' );

undef @tags;
$str->iter_tags( sub { push @tags, [ @_ ] } );
is_deeply( \@tags, 
           [
              [ 0, 12, message => 1 ],
              [ 6, 1,  space => 1 ],
           ],
           'tags list after apply space' );

undef @extents;
$str->iter_extents( sub { push @extents, $_[0] } );

is( scalar @extents, 2, 'two extent from iter_extents' );

is( $extents[0]->substr, "Hello, world", '$e[0]->substr' );
is( $extents[1]->substr, " ", '$e[1]->substr' );

sub fetch_tags
{
   my ( $start, $len, %tags ) = @_;
   push @tags, [ $start, $len, map { $_ => $tags{$_} } sort keys %tags ]
}

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [ 0, 6, message => 1 ],
              [ 6, 1, message => 1, space => 1 ],
              [ 7, 5, message => 1 ],
           ],
           'tags list non-overlapping after apply space' );

my @substrs;
sub fetch_substrs
{
   my ( $substr, %tags ) = @_;
   push @substrs, [ $substr, map { $_ => $tags{$_} } sort keys %tags ]
}

$str->iter_substr_nooverlap( \&fetch_substrs );
is_deeply( \@substrs, 
           [
              [ "Hello,", message => 1 ],
              [ " ",      message => 1, space => 1 ],
              [ "world",  message => 1 ],
           ],
           'substrs non-overlapping after apply space' );

$str->apply_tag( 0, 1, capital => 1 );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [ 0, 1, capital => 1, message => 1 ],
              [ 1, 5, message => 1 ],
              [ 6, 1, message => 1, space => 1 ],
              [ 7, 5, message => 1 ],
           ],
           'tags list non-overlapping after apply space' );

undef @substrs;
$str->iter_substr_nooverlap( \&fetch_substrs );
is_deeply( \@substrs, 
           [
              [ "H",     capital => 1, message => 1 ],
              [ "ello,", message => 1 ],
              [ " ",     message => 1, space => 1 ],
              [ "world", message => 1 ],
           ],
           'substrs non-overlapping after apply space' );

$str = String::Tagged->new( "my BIG message" );

$str->apply_tag( 0, 14, size => 1 );
$str->apply_tag( 3,  3, size => 2 );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [ 0, 3, size => 1 ],
              [ 3, 3, size => 2 ],
              [ 6, 8, size => 1 ],
           ],
           'tags list with overridden tag' );

$str = String::Tagged->new( "BEGIN middle END" );

$str->apply_tag( -1, -1, everywhere => 1 );
$str->apply_tag( -1,  5, begin => 1 );
$str->apply_tag( 13, -1, end => 1);

is_deeply( $str->get_tags_at( 0 ), 
           { everywhere => 1, begin => 1 },
           'tags at pos 0 of edge-anchored' );

is( $str->get_tag_at( 0, "everywhere" ), 1, 'everywhere tag is 1 at pos 0 of edge-anchored' );

undef @tags;
$str->iter_tags_nooverlap( \&fetch_tags );
is_deeply( \@tags, 
           [
              [  0, 5, begin => 1, everywhere => 1 ],
              [  5, 8, everywhere => 1 ],
              [ 13, 3, end => 1, everywhere => 1 ],
           ],
           'tags list with edge-anchored tags' );

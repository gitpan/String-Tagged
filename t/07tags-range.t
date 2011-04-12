#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 21;
use Test::Identity;

use String::Tagged;

my $str = String::Tagged->new( "some BIG words" );

$str->apply_tag( -1, -1, everywhere => 1 );
$str->apply_tag(  5,  3, big => 1 );

my $e = $str->get_tag_extent( 7, 'everywhere' );

ok( defined $e, 'Got an extent' );

identical( $e->string, $str, '$e->str' );

is( $e->start,   0, '$e->start' );
is( $e->end,    14, '$e->end' );
is( $e->length, 14, '$e->length' );
is( $e->substr, "some BIG words", '$e->substr' );
ok( $e->anchor_before, '$e->anchor_before' );
ok( $e->anchor_after,  '$e->anchor_after' );

$e = $str->get_tag_extent( 7, 'big' );

is( $e->start, 5, '$e->start' );
is( $e->end,   8, '$e->end' );
is( $e->substr, "BIG", '$e->substr of 7/big' );
ok( !$e->anchor_before, '$e->anchor_before' );
ok( !$e->anchor_after,  '$e->anchor_after' );

$e = $str->get_tag_extent( 3, 'big' );

ok( !defined $e, '$e not defined for 3/big' );

$e = $str->get_tag_missing_extent( 3, 'big' );

ok( defined $e, '$e missing defined for 3/big' );

is( $e->start, 0, '$e->start' );
is( $e->end,   5, '$e->end' );

$e = $str->get_tag_missing_extent( 7, 'big' );

ok( !defined $e, '$e missing not defined for 7/big' );

$e = $str->get_tag_missing_extent( 10, 'big' );

ok( defined $e, '$e missing defined for 10/big' );

is( $e->start,  8, '$e->start' );
is( $e->end,   14, '$e->end' );

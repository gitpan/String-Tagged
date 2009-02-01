#!/usr/bin/perl -w

use strict;

use Test::More tests => 6;

use String::Tagged;

my $str = String::Tagged->new( "Hello, world" );

is( $str->str, "Hello, world", 'Plain string accessor' );

is( $str->substr( 0, 5 ), "Hello", 'Plain substring accessor' );

$str->set_substr( 7, 5, "planet" );
is( $str->str, "Hello, planet", "After set_substr" );

$str->insert( 7, "lovely " );
is( $str->str, "Hello, lovely planet", 'After insert' );

$str->append( "!" );
is( $str->str, "Hello, lovely planet!", 'After append' );

is( "message is $str",
    "message is Hello, lovely planet!",
    'STRINGify operator' );

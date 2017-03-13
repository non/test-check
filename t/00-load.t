#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Test::Check' );
}

diag( "Testing Test::Check $Test::Check::VERSION, Perl $], $^X" );

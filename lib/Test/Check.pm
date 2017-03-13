package Test::Check;

use warnings;
use strict;

our $VERSION = '0.01';

#use Data::Compare qw(Compare);
#use Scalar::Quote qw(quote);
#use Scalar::Util qw(blessed looks_like_number reftype);
use Test::More import => ['is_deeply'];

use Test::Check::Gen qw(/.*/);
use Test::Check::Prop qw(/.*/);

use base 'Test::Builder::Module';

our @EXPORT = qw(test prop);

sub test($$) {
    my ($name, $body) = @_;
    my ($p, @gens) = @$body;
    my $prop = Test::Check::Prop->new($name, $p, @gens);
    my $seed = randomseed();


    my $t = Test::Builder->new();

    my ($passed, $failseed) = $prop->test($seed);

    $t->level(1);
    $t->ok($passed, $name);
    unless ($passed) {
        $t->diag("  failing seed: $failseed");
        my ($value, $next) = $prop->{gen}->($seed);
        if ($prop->{isnamed}) {
            $t->diag("  named arguments were:");
            my %h = %$value;
            foreach my $key (keys(%h)) {
                $t->diag("    $key: $value->{$key}");
            }
        } else {
            $t->diag("  positional arguments were:");
            my $i = 0;
            while ($i < scalar(@$value)) {
                $t->diag("    $i: $value->[$i]");
                $i += 1;
            }
        }
    }
}

sub prop(&@) {
    my ($p, @gens) = @_;
    return [$p, @gens];
}

## helper function for test, notest and pretest
#sub _test {
#    my ($pre, $testfunc, $cmpfunc, $name) = @_;
#    if(scalar(@_) == 3) {
#        $name = $_[-1];
#        $cmpfunc = noop();
#    } elsif(ref($cmpfunc) ne 'CODE') {
#        $cmpfunc = eqv($cmpfunc);
#    }
#
#    my $result = eval { &$testfunc() };
#    $name = @STACK ? join(".", @STACK) . ".$name" : $name;
#
#    my $t = __PACKAGE__->builder();
#    $t->level(3);
#    return _ok($@, $name, "    failed to die") if $cmpfunc eq $dies;
#    my $ok;
#    if($@) {
#        _fail($name, "    died: $@") if $@;
#        $ok = 0;
#    } else {
#        $t->level(4);
#        $ok = &$cmpfunc($result, $name);
#    }
#    die if $pre && !$ok && @STACK;
#    $t->BAIL_OUT("pretest failed") if !$ok && $pre;
#    $t->BAIL_OUT("fastout is on") if !$ok && $FASTOUT;
#    return $result;
#}
#
## helper function; wraps calls to builder->ok, displays failure messages, and
## helps keep our builder->level consistent.
#sub _ok {
#    my ($ok, $name, $failmsg) = @_;
#    my $t = __PACKAGE__->builder;
#    $t->ok($ok, $name);
#    $t->diag($failmsg) if !$ok && $failmsg;
#    return $ok;
#}
#
## similar to _ok, but deals with known failure.
#sub _fail {
#    my ($name, $failmsg) = @_;
#    my $t = __PACKAGE__->builder;
#    $t->ok(0, $name);
#    $t->diag($failmsg) if $failmsg;
#    return 0;
#}
#
#=item B<group { BLOCK } NAME>
#
#Groups are blocks which wrap associated tests. Groups can be used to namespace
#tests as well as to allow groups of tests to fail together. Here is a short
#example:
#
#    group {
#        my $a = coretest { Adder->new } typeqv 'Adder', "new";
#
#        test { $a->add(4, 6) } 10, "4 + 6";
#        test { $a->add("cat", "dog") } dies, "mass hysteria";
#        test { $a->add() } isundef, "not a number";
#
#    } "adder";
#
#If C<< Adder->new >> fails, the rest of the tests aren't producing useful
#results, so they will be skipped. See the L<ETHOS> section for a more in-depth
#discussion of the package in general, and the implications of test
#short-circuiting in particular.
#
#=cut
#sub group(&$) {
#    my ($func, $name) = @_;
#
#    push(@STACK, $name);
#    eval { &$func() };
#    pop(@STACK);
#
#    die if $@ && @STACK;
#}
#
#=back
#
#=head1 TEST CONDITIONS
#
#=over
#
#=item B<eqv OBJECT>
#
#Creates a function which tests that the result is exactly equivalent (eqv) to
#I<OBJECT> (using Test::More::is_deeply). It works for both simple values and
#nested data structures. See L<Test::More> for more details.
#
#If I<test> receives a condition which isn't a code-ref, it will be wrapped in an
#I<eqv> call, since this is the most common case (testing that a result is the
#expected value).
#
#=cut
#sub eqv($) {
#    my ($other) = @_;
#    return sub {
#        my ($got, $name) = @_;
#        return is_deeply($got, $other, $name);
#    };
#}
#
#=item B<ineqv OBJECT>
#
#Tests whether the result differs from (is inequivalent to) I<OBJECT> according
#to Data::Compare. This is expected (hoped?) to be inverse of I<eqv>.
#
#=cut
#sub ineqv($) {
#    my ($other) = @_;
#    return sub {
#        my ($got, $name) = @_;
#        return _ok(!Compare($got, $other), $name, "    objects were the same");
#    };
#}
#
#=item B<typeqv TYPE>
#
#Creates a function which tests that the result is of (or inhereits from) the
#provided I<TYPE> (that the result's type is equivalent to I<TYPE>). For
#unblessed references, it checks that
#C<ref($result) eq $type>. For blessed references it checks that
#C<< $result->isa($type) >>. Results which are not references will always be
#false.
#
#=cut
#sub typeqv($) {
#    my($type) = @_;
#    return sub {
#        my ($got, $name) = @_;
#        return _fail($name, "    result was undef") unless defined($got);
#        return _fail($name, "    result was not a ref") unless ref($got);
#        my $ok = ref($got) eq $type || blessed($got) && $got->isa($type);
#        return _ok($ok, $name, "    result was not of type $type");
#    };
#}
#
#=item B<dies>
#
#Verifies that the test's code block died. It is unique amongst test conditions
#in that it doesn't test the result, but rather tests C<$@>. Any result other
#than a die succeeds.
#
#=cut
#sub dies() {
#    return $dies;
#};
#
#=item B<noop>
#
#This is the "default" condition; if no condition is given to a test then this
#condition is used. As long as the code block does not die, the test passes.
#
#=cut
#sub noop()  {
#    return sub {
#        my ($got, $name) = @_;
#        return _ok(1, $name);
#    };
#}
#
#=item B<true>
#
#Verifies that the result is a true value.
#
#=cut
#sub true()  {
#    return sub {
#        my ($got, $name) = @_;
#        return _ok($got, $name);
#    };
#}
#
#=item B<false>
#
#Verifies that the result is a false value.
#
#=cut
#sub false() {
#    return sub {
#        my ($got, $name) = @_;
#        return _ok(!$got, $name);
#    };
#}
#
#=item B<isdef>
#
#Checks that the result is defined (not undef).
#
#=cut
#sub isdef() {
#    return sub {
#        my ($got, $name) = @_;
#        return _ok(defined($got), $name);
#    };
#}
#
#=item B<isundef>
#
#Checks that the result is undefined.
#
#=cut
#sub isundef() {
#    return sub {
#        my ($got, $name) = @_;
#        return _ok(!defined($got), $name);
#    };
#}
#
#=back
#
#=head1 CUSTOM TEST CONDITIONS
#
#Anonymous subroutines can be used in place of the provided test conditions.
#These functions take two arguments: the test result and the test's name. Here
#are some examples:
#
#  use Test::More;
#
#  sub over21 {
#      my ($result, $name) = @_;
#      return cmp_ok($result, '>=', 21, $name);
#  }
#  test { $alice->age } \&over21, 'can alice drink?';
#  test { $bob->age } \&over21, 'can bob drink?';
#
#These examples are kind of clunky, but you get the idea. Using anything
#complicated will probably require reading the source, and/or learning how to
#use L<Test::Builder>. In particular, it's important to make sure
#C<< builder->level >> is set correctly.
#
#=head1 ETHOS
#
#This package exists to address some specific concerns I've had while writing
#tests using other frameworks. As such, it has some pretty major differences from
#the other testing frameworks out there.
#
#Most Perl tests are written as perl scripts which test Perl code by calling
#functions or methods, and then using various Test packages to look at the
#result. This approach has some problems:
#
#=over
#
#=item 1
#
#Test scripts can make bad assumptions or have bugs, causing problems that
#aren't obviously linked to a particular test clause and which can be hard to
#track down and fix.
#
#=item 2
#
#Writing defensive test scripts involves a bunch of relatively boiler-plate
#eval-blocks and C<$@> tests, as well as effectively doubling the number of tests
#that are "run" without meaningfully doubling the test coverage.
#
#=item 3
#
#In some cases a small early error causes tons of test clauses to spew useless
#messages about failing; this loses sight of the basic issue that caused the
#problem (syntax error, missing module, etc).
#
#=back
#
#Test::Functional addresses these concerns: it enables the programmer to write
#all the "meat" of the test script inside anonymous subs which are tests [1].
#Since each test checks both that the code did not die and that the result was
#what was expected, the tester doesn't have to worry about what kind of failure
#might occur, just about the expected outcome [2]. Especially when trying to test
#other people's code (gray box testing?) this feature is invaluable.
#
#The various features to prematurely end the test (using I<pretest()> and/or
#C<< $Test::Functional::Conf->fastout >>) can help the developer to focus on the
#problem at hand, rather than having to filter through spew [3]. This is
#especially nice during test-driven development, or when trying to increase
#coverage for an old and crufty module.
#
#=head1 AUTHOR
#
#Erik Osheim C<< <erik at osheim.org> >>
#
#=head1 BUGS
#
#The syntax takes some getting used to.
#
#I should create default wrappers for things such as I<like> and I<compare> from
#L<Test::More>. Currently I mostly use I<true> but that gives less debugging
#information.
#
#I wrote these tests to suit my needs, so I am sure there are cases I haven't
#thought of or encountered. Also, I'm sure I have a lot to learn about the
#intricacies of L<Test::Harness> and L<Test::Module>. Please contact me (via
#email or L<http://rt.cpan.org>) with any comments, advice, or problems.
#
#=head1 ACKNOWLEDGEMENTS
#
#This module is based on Test::Builder::Module, and relies heavily on the work
#done by Michael Schwern. It also uses Data::Compare by David Cantrell.
#
#=head1 COPYRIGHT & LICENSE
#
#Copyright 2009 Erik Osheim, all rights reserved.
#
#This program is free software; you can redistribute it and/or modify it
#under the same terms as Perl itself.
#
#=cut
#
#1;

1;
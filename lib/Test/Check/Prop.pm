package Test::Check::Prop;

use warnings;
use strict;

our $VERSION = '0.01';

=head1 NAME

Test::Check::Prop

=head1 SYNOPSIS

  use Test::Check::Prop;

  die "write this!";

=head1 DESCRIPTION

We should write a description.

=over

=cut
use Carp;
use Exporter;
use Memoize;

use Test::Check::Gen qw(/.*/);

our @ISA = qw(Exporter);

use feature 'unicode_strings';

=item B<new NAME PREDICATE [GEN1, GEN2...]>

Test a property.

=cut
sub new {
    my ($class, $name, $predicate, @gens) = @_;
    die unless scalar(@gens);
    my $isnamed = !ref($gens[0]);
    my $self = {
        name => $name,
        predicate => $predicate,
        isnamed => $isnamed,
        gen => $isnamed ? record(@gens) : tuple(@gens),
        args => \@gens,
    };
    bless($self, $class);
    return $self;
}

=item B<run TESTER [SEED]>

Do a complete test run, using the given TESTER (an instance of Test::Builder),
as well an optional seed. This will register whether or not the test passed,
as well as displaying any necessary failure output (such as the failing seed).

=cut
sub run {
    my ($self, $t, $seed) = @_;
    $seed = randomseed() unless defined($seed);

    my ($passed, $failseed) = $self->test($seed);

    $t->level(1);
    $t->ok($passed, $self->{name});
    unless ($passed) {
        $t->diag("  failing seed: $failseed");
        my ($value, $next) = $self->{gen}->($seed);
        if ($self->{isnamed}) {
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

=item B<test SEED>

Test a property with a given starting seed.

By default this will check 100 test cases (with 100 different seeds, starting
with SEED). If any test case fails, this function will return immediately.

The return value is a list with two members: ($passed, $failingseed). If
$passed is true, $failingseed will be undef. Otherwise, $failingseed will be
the seed for the failing test case (which can be used to reproduce the
failure).

=cut
sub test {
    my ($self, $seed) = @_;
    my $runs = 0;
    while ($runs < 100) {
        my ($res, $next) = $self->check($seed);
        return (undef, $seed) unless $res;
        $seed = $next;
        $runs += 1;
    }
    return (1, undef);
}

=item B<check SEED>

Generate a test case using SEED and check to see if the property is true.

Returns a list of two elements: ($passed, $nextseed).

Note that unlike B<test>, even if $passed is false, the seed returned is NOT
the failing seed; it is always the next seed for the RNG.

=cut
sub check {
    my ($self, $seed) = @_;
    my ($value, $next) = $self->{gen}->($seed);
    my @args = $self->{isnamed} ? %$value : @$value;
    my $res = $self->{predicate}->(@args);
    return ($res, $next);
}

=back

=head1 Conclusion

The end.

=cut

1;

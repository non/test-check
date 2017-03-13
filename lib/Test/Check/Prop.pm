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

=item B<run SEED>

Test a property.

=cut
sub run {
    my ($self, $seed) = @_;
    my ($value, $next) = $self->{gen}->($seed);
    my @args = $self->{isnamed} ? %$value : @$value;
    #my $res = eval { $self->{predicate}->(@args) };
    #return ($@ ? undef : $res, $next);
    my $res = $self->{predicate}->(@args);
    return ($res, $next);
}

=item B<test SEED>

Test a property.

=cut
sub test {
    my ($self, $seed) = @_;
    my $runs = 0;
    while ($runs < 100) {
        my ($res, $next) = $self->run($seed);
        return (undef, $seed) unless $res;
        $seed = $next;
        $runs += 1;
    }
    return (1, undef);
}

=back

=head1 Conclusion

The end.

=cut

1;

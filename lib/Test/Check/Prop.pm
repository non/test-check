package Test::Check::Prop;

use warnings;
use strict;

use Carp;
use Exporter;
use Memoize;

use Test::Check::Gen qw(/.*/);

our @ISA = qw(Exporter);

use feature 'unicode_strings';

our $VERSION = '0.01';

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

sub run {
    my ($self, $seed) = @_;
    my ($value, $next) = $self->{gen}->($seed);
    my @args = $self->{isnamed} ? %$value : @$value;
    #my $res = eval { $self->{predicate}->(@args) };
    #return ($@ ? undef : $res, $next);
    my $res = $self->{predicate}->(@args);
    return ($res, $next);
}

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

sub summary {
    my ($self, $seed) = @_;
    $seed = randomseed() unless defined($seed);
    my ($passed, $failseed) = $self->test($seed);
    if ($passed) {
        print "property $self->{name} passed\n";
    } else {
        print "property $self->{name} failed\n";
        print "  failing seed was $failseed\n";
        print "  arguments were:\n";
        my ($value, $next) = $self->{gen}->($seed);
        if ($self->{isnamed}) {
            my %h = %$value;
            foreach my $key (keys(%h)) {
                print "    $key: $value->{$key}\n";
            }
        } else {
            my $i = 0;
            while ($i < scalar(@$value)) {
                print "    $i: $value->[$i]\n";
                $i += 1;
            }
        }
    }
}

1;

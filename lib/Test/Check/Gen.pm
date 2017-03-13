package Test::Check::Gen;

use warnings;
use strict;

use Carp;
use Exporter;
use Memoize;
use Test::Deep qw(eq_deeply);

our @ISA = qw(Exporter);

our @EXPORT_OK = qw(comap flatmap flatten filter const onein bool whole fraction range
    size codepoint identifier string tuple record vector array table hash anything
    oneof frequency optional sample stringof ascii function generators randomseed gengen genseed eq_gen);

use feature 'unicode_strings';

our $VERSION = '0.01';

use constant SEED_MODULUS => 2147483647;

use constant MAX_RETRIES => 20;

# Fundamental generator constructors and combinators

# Generators are blessed function references, which take an RNG seed as input,
# and return a list containing a generated value and a new RNG seed.
sub gen(&) {
    my ($function) = @_;
    bless($function, 'Test::Check::Gen');
    return $function;
}

sub comap(&$) {
    my ($f, $gen) = @_;
    return gen {
        my ($seed0) = @_;
        my ($value, $seed1) = $gen->($seed0);
        $_ = $value;
        return (&$f, $seed1);
    };
}

sub flatmap(&$) {
    my ($f, $gen) = @_;
    return gen {
        my ($seed0) = @_;
        my ($value, $seed1) = $gen->($seed0);
        $_ = $value;
        return (&$f)->($seed1);
    };
}

sub flatten {
    my ($gen) = @_;
    return flatmap { $_ } $gen;
}

sub filter(&$) {
    my ($p, $gen) = @_;
    return gen {
        my ($seed) = @_;
        my $failures = 0;
        while ($failures < MAX_RETRIES) {
            my ($value, $next) = $gen->($seed);
            return ($value, $next) if $p->($value);
            $seed = $next;
            $failures += 1;
        }
        die;
    };
}

sub const {
    my ($value) = @_;
    return gen { return ($value, $_[0]) };
}

sub oneof(@) {
    my (@choices) = @_;
    my $limit = scalar(@choices);
    return comap { $choices[$_] } range(0, $limit);
}

sub generators(@) {
    my (@gens) = @_;
    return flatten(oneof(@gens));
}

sub frequency(@) {
    my (@pairs) = @_;
    my $total = 0;
    foreach my $pair (@pairs) {
        my ($weight, $item) = @$pair;
        die unless $weight > 0;
        $total += $weight;
    }
    return comap {
        my $total = $_;
        foreach my $pair (@pairs) {
            my ($weight, $item) = @$pair;
            return $item if $total < $weight;
            $total -= $weight;
        }
        return $pairs[0]->[1];
    } fraction($total);
}

sub optional {
    my ($gen) = @_;
    return flatmap { $_ ? const(undef) : $gen } onein(10);
}

# Boolean and numeric generators

sub onein($) {
    my ($n) = @_;
    die unless $n > 0;
    return comap { $_ == 0 } range(0, $n);
}

sub bool() { return onein(2) }
memoize('bool');

sub fraction {
    my ($start, $limit) = @_;
    if (defined($limit)) {
        my $delta = $limit - $start;
        return gen {
            my ($seed) = @_;
            return (($seed / SEED_MODULUS) * $delta + $start, nextseed($seed));
        }
    } else {
        return fraction(-$start, $start);
    }
}

sub whole {
    return comap { int($_) } fraction(@_);
}

sub range {
    my ($start, $limit) = @_;
    die "invalid range: ($start, $limit)" unless $start < $limit;
    my $diff = $limit - $start;
    die if $diff > SEED_MODULUS;
    return gen {
        my ($seed) = @_;
        my $remainder = SEED_MODULUS % $diff;
        my $top = SEED_MODULUS - $remainder;
        while ($seed >= $top) {
            $seed = nextseed($seed);
        }
        my $mod = $seed % $diff;
        return ($start + $mod, nextseed($seed));
    };
}

sub size() {
    return generators(range(0, 3), range(0, 11), range(0, 35));
}
memoize('size');

# Character and string generators

sub concat(@) {
    return comap { join('', @$_) } tuple(@_);
}

sub char {
    my ($start, $limit) = @_;
    return comap { chr($_) } range($start, $limit);
}

sub ascii() {
    return char(0, 128)
}
memoize('ascii');

sub wordchar() {
    return oneof('A'..'Z', 'a'..'z', '0'..'9');
}
memoize('wordchar');

sub codepoint() {
    my $b10 = char(128, 192);
    return generators(
        concat(ascii()),
        concat(char(192, 224), $b10),
        concat(char(224, 240), $b10, $b10),
        concat(char(240, 248), $b10, $b10, $b10));
}
memoize('codepoint');

sub stringof {
    my ($cgen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    return comap {
        join('', @$_)
    } flatmap {
        vector($cgen, $_)
    } $sgen;
}

sub identifier() {
    return stringof(wordchar(), range(1, 8));
}
memoize('identifier');

sub string() {
    return stringof(codepoint());
}
memoize('string');

# Arary generators

sub tuple(@) {
    my (@gens) = @_;
    return gen {
        my ($seed) = @_;
        my $res = [];
        for my $gen (@gens) {
            confess unless $gen;
            my ($value, $next) = $gen->($seed);
            push(@$res, $value);
            $seed = $next;
        }
        return ($res, $seed);
    };
}

sub record(@) {
    my (@items) = @_;
    return gen {
        my ($seed) = @_;
        my $res = {};
        my $i = 0;
        while ($i < scalar(@items)) {
            my $key = $items[$i];
            my $gen = $items[$i + 1];
            $i += 2;
            my ($value, $next) = $gen->($seed);
            $res->{$key} = $value;
            $seed = $next;
        }
        return ($res, $seed);
    };
}

sub vector {
    my ($gen, $size) = @_;
    die "invalid size: $size" unless $size >= 0;
    return gen {
        my ($seed) = @_;
        my $res = [];
        while ($size > 0) {
            my ($value, $next) = $gen->($seed);
            push(@$res, $value);
            $seed = $next;
            $size -= 1;
        }
        return ($res, $seed);
    };
}

sub array {
    my ($egen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    return flatmap { vector($egen, $_) } $sgen;
}

# Hash generators

sub table {
    my ($kgen, $vgen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    comap {
        my $res = {};
        foreach my $pair (@$_) {
             my ($key, $value) = @$pair;
             $res->{$key} = $value;
        }
        return $res;
    } array(tuple($kgen, $vgen), $sgen);
}

sub hash {
    my ($gen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    return table(identifier(), $gen, $sgen);
}

# Function generator

sub function {
    my ($gen) = @_;
    return gen {
        my ($seed0) = @_;
        my $f = sub {
            my (@inputs) = @_;
            my $seed = $seed0;
            foreach my $input (@inputs) {
                $seed = absorb($seed, $input);
            }
            my ($value, $next) = $gen->($seed);
            return $value;
        };
        return ($f, nextseed($seed0));
    };
}

# arbitrary generator

sub gengen {
    return comap {
        if    ($_ <= 10) { bool() }
        elsif ($_ <= 20) { flatmap { fraction($_) } range(10, 1000000) }
        elsif ($_ <= 30) { flatmap { whole($_) } range(10, 1000000) }
        elsif ($_ <= 40) { identifier() }
        elsif ($_ <= 50) { string() }
        elsif ($_ <= 55) { flatmap { tuple(@$_) } tuple(gengen(), gengen()) }
        elsif ($_ <= 60) { flatmap { record(@$_) } tuple(identifier(), gengen(), identifier(), gengen()) }
        elsif ($_ <= 65) { flatmap { vector(@$_) } tuple(gengen(), size()) }
        else             { const(undef) }
    } range(1, 75);
}
memoize('gengen');

sub anything {
    return flatten(gengen());
}
memoize('anything');

sub genseed {
    return gen {
        my ($seed) = @_;
        return ($seed, nextseed($seed));
    }
}
memoize('genseed');

# Interactive method for testing generators

sub sample {
    my ($gen, $count) = @_;
    my $seed = randomseed();
    if (defined($count) && $count > 1) {
        my @res;
        while ($count > 0) {
            my ($value, $next) = $gen->($seed);
            push(@res, $value);
            $seed = $next;
            $count -= 1;
        }
        return @res;
    } else {
        my ($value, $next) = $gen->($seed);
        return $value;
    }
}

# Low-level RNG / seed manipulation

sub randomseed {
    return int(rand(SEED_MODULUS));
}

sub nextseed {
    my ($seed) = @_;
    my $next = (16807 * $seed) % SEED_MODULUS;
    my $m = int($next % 4);
    return $next
}

sub adjustseed {
    my ($seed, $n) = @_;
    return int($seed + $n) % SEED_MODULUS;
}

sub absorb {
    my ($seed, $input) = @_;
    if (!defined($input)) {
    } elsif (ref($input) eq 'ARRAY') {
        foreach my $item (@$input) {
            $seed = absorb($seed, $item);
        }
    } elsif (ref($input) eq 'HASH') {
        foreach my $key (keys(%$input)) {
            $seed = absorb($seed, $key);
            $seed = absorb($seed, $input->{$key});
        }
    } elsif (ref($input) eq 'SCALAR') {
        return absorb($seed, $$input);
    } else {
        my $str = defined($input) ? "$input" : "";
        my $i = 0;
        while ($i < length($str)) {
            my $c = substr($str, $i, 1);
            $seed = adjustseed($seed, ord($c));
            $i += 1
        }
    }
    return $seed;
}

sub eq_gen {
    my ($gen1, $gen2, $seed) = @_;
    my ($x1, $s1) = $gen1->($seed);
    my ($x2, $s2) = $gen2->($seed);
    return eq_deeply($x1, $x2) && $s1 == $s2;
}

1;

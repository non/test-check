package Test::Check::Gen;

use warnings;
use strict;

our $VERSION = '0.01';

=head1 NAME

Test::Check::Gen

=head1 SYNOPSIS

    use Test::Check::Gen;

    die "write this!";

=head1 DESCRIPTION

A generator produces arbitrary (but deterministic) values given an input seed
and shrinking parameter. Generators are used to produce test cases for
properties -- if a property passes some set number of generated test cases, we
say that the property has passed.

Generators are represented as blessed function references. The function should
accept two values (C<$seed> and C<$shrinks>) and return two values (C<$value>
and C<$nextseed>). Given the same inputs a generator is required to always
produce the same outputs.

Most generators are defined in terms of other functions (referred to as
"generator combinators"), for example:

 * generators($gen1, $gen2, ...)
 * comap { ... } $gen
 * flatmap { ... } $gen
 * flatten $gengen
 * filter { ... } $gen
 * optional($gen)
 * concat($gen1, $gen2, ...)
 * stringof($chargen)
 * tuple($gen1, $gen2, ...)
 * record(k1 => $gen1, k2 => $gen2, ...)
 * vector($gen, $size)
 * array()
 * table($keygen, $gen)
 * hash($gen)
 * function($gen)


Generators can also be written from scratch (using the C<gen> method), which
requires implementing an anonymous function of the form:

    gen {
        my ($seed, $shrinks) = @_;
        ... # generation goes here
        return ($generated_value, $nextseed);
    };

The C<$seed> parameter can be used directly with the following RNG-related
methods:

 * nextseed($seed) - given a seed, get the next seed in the RNG sequence
 * randomfloat($seed) - given a seed, produce a random float in [0.0, 1.0).
 * absorb($seed, $input) - given a seed and a value, produce a new seed

Instead of calling C<rand()> (or similar built-in functions), it's important
to use these methods, which ensure that the random generation is reproducable
from a starting seed.

The C<$shrinks> parameter should be passed to any other generators used. It's
primary purpose is to decrease the size of any numbers/structures generated.
For example, when set to 2, the numeric range for generated numbers should
contract by 2.

=over

=cut
use Carp;
use Exporter;
use Memoize;
use Test::Deep qw(eq_deeply);

our @ISA = qw(Exporter);

our @EXPORT_OK = qw(comap flatmap flatten filter const onein bool whole float range
    size codepoint identifier string tuple record vector array table hash anything
    oneof frequency optional sample stringof ascii function generators randomseed
    gengen genseed number fraction printable);

use feature 'unicode_strings';

use constant SEED_MODULUS => 2147483647;

use constant SHRINK_DENOM => 1.18920711500272; # sqrt(sqrt(2))

use constant MAX_RETRIES => 20;

# Fundamental generator constructors and combinators

# Generators are blessed function references, which take an RNG seed as input,
# and return a list containing a generated value and a new RNG seed.


=item B<gen { FN }>

Define a primitive generator using the given FN function.

The BODY function should take a seed and return a value and a new seed in a
list. For example:

    my $mygen = gen {
      my ($seed) = @_;
      my ($value, $nextseed);
      ...
      return ($value, $nextseed);
    };

Generators are blessed function references. Since they return a pair of
$value, $nextseed, $value will be a reference or a scalar.

Generators are described in terms of the kinds of values they generate.

=cut
sub gen(&) {
    my ($function) = @_;
    bless($function, 'Test::Check::Gen');
    return $function;
}

=item B<comap { FN } GEN>

Create a new generator from GEN using the given FN function.

    my $g0 = oneof(1, 2, 3);        // produces 1, 2, or 3.
    my $g1 = comap { $_ * 2 } $g0;  // produces 2, 4, or 6.
    my $g2 = comap { $_ != 0 } $g0; // true or false, unevenly

This is an analogue of Perl's built-in map function, and the fmap function in
Haskell. Generators are covariant functors, so the name "comap" refers to a
covariant map operation.

=cut
sub comap(&$) {
    my ($f, $gen) = @_;
    return gen {
        my ($seed0, $shrinks) = @_;
        my ($value, $seed1) = $gen->($seed0, $shrinks);
        $_ = $value;
        return (&$f, $seed1);
    };
}

=item B<flatmap { GENFN } GEN>

Create a new generator from GEN by sequencing its generated values through a
generator-producing function GENFN.

C<flatmap { ... } $g> is equivalent to C<flatten(comap { ... } $g)>.

This is an analogue to the flatMap method in Scala, and hte bind function in
Haskell.

=cut
sub flatmap(&$) {
    my ($f, $gen) = @_;
    return gen {
        my ($seed0, $shrinks) = @_;
        my ($value, $seed1) = $gen->($seed0, $shrinks);
        $_ = $value;
        return (&$f)->($seed1, $shrinks);
    };
}

=item B<flatten(GEN)>

Flatten the given generator GEN by one level.

    my $gen0 = whole();        // produces whole-numbers
    my $gen1 = const($gen0);   // produces whole-number generators
    my $gen2 = flatten($gen1); // equivalent to $gen0

Note that this method does not "deeply-flatten" generators:

    my $gen0 = whole();                // produces whole-numbers
    my $gen1 = const(const($gen0));    // so nested!
    my $gen2 = flatten($gen1);         // not equivalent to $gen0
    my $gen  = flatten(flatten($gen1)) // equivalent to $gen0

C<filter($gen)> is equvialent to C<flatmap { $_ } $gen>.

=cut
sub flatten {
    my ($gen) = @_;
    return flatmap { $_ } $gen;
}

=item B<filter { PRED } GEN>

Create a new generator which produces values from the generator GEN which
satisfy the predicate PRED.

If the underlying generator GEN fails to satisfy the predicate many times in a
row, the generator fails. This means C<filter> can only make a best effort,
since some predicates are unsatisfiable:

    filter { 1 } $g     // identical to 
    filter { undef } $g // will crash instead of producing values

=cut
sub filter(&$) {
    my ($p, $gen) = @_;
    return gen {
        my ($seed, $shrinks) = @_;
        my $failures = 0;
        while ($failures < MAX_RETRIES) {
            my ($value, $next) = $gen->($seed, $shrinks);
            return ($value, $next) if $p->($value);
            $seed = $next;
            $failures += 1;
        }
        die;
    };
}

=item B<const(VALUE)>

Create a generator that always produces the same value.

=cut
sub const {
    my ($value) = @_;
    return gen { return ($value, $_[0]) };
}

=item B<oneof(VALUES...)>

Create a generator that produces any of the given values with equal
probability.

=cut
sub oneof(@) {
    my (@choices) = @_;
    my $limit = scalar(@choices);
    return comap { $choices[$_] } range(0, $limit);
}

=item B<generators(GENS...)>

Create a generator which produces any of the values which can be produced by
the given generators.

The generators will be used with equal probability (which does not mean that
the underlying values will be chosen with equal probability across
generators).

=cut
sub generators(@) {
    my (@gens) = @_;
    return flatten(oneof(@gens));
}

=item B<frequency(PAIRS...)>

=cut
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
    } float($total);
}

=item B<optional(GEN)>

=cut
sub optional {
    my ($gen) = @_;
    return flatmap { $_ ? const(undef) : $gen } onein(10);
}

# Boolean and numeric generators

=item B<onein(N)>

=cut
sub onein($) {
    my ($n) = @_;
    die unless $n > 0;
    return comap { $_ == 0 } range(0, $n);
}

=item B<bool()>

=cut
sub bool() { return onein(2) }
memoize('bool');

=item B<number()>

=cut
sub number {
    return generators(
        float(1), float(1000), float(1000000000),
        whole(), whole(1000), whole(1000000000));
}
memoize('number');

=item B<float()>

Produce floating point values in the interal [start, limit).

=cut
sub float {
    my ($start, $limit) = @_;
    $start = 1000000000 unless defined($start);
    if (defined($limit)) {
        my $delta = $limit - $start;
        return gen {
            my ($seed, $shrinks) = @_;
            my $d = $shrinks ? $delta / (SHRINK_DENOM ** $shrinks) : $delta;
            my $x = randomfloat($seed, $d, $start);
            return ($x, nextseed($seed));
        }
    } else {
        return float(-$start, $start);
    }
}

=item B<whole()>

Produce fractional values in the interval [start, limit).

=cut
sub whole {
    my ($start, $limit) = @_;
    $start = 1000000000 unless defined($start);
    if (defined($limit)) {
        my $delta = $limit - $start;
        return comap { int($_ + $start) } float(0, $delta);
    } else {
        return whole(-$start + 1, $start);
    }
}

=item B<fraction()>

Produce fractional values in the interval [start, limit).

=cut
sub fraction {
    my ($start, $limit) = @_;
    $start = 1000000000 unless defined($start);
    if (defined($limit)) {
        my $delta = $limit - $start;
        return flatmap {
            my $scale = 10 ** $_;
            comap {
                $_->[0] / $_->[1]
            } tuple(whole(0, $delta * $scale), whole(1, $scale + 1));
        } whole(0, 11);
    } else {
        return fraction(-$start + 1, $start);
    }
}

=item B<range(START, LIMIT)>

range(x, y) produces values in the same range as whole(x, y). The difference
is that unlike whole, range will not contract during shrinking. When you want
to produce integers that shrink toward their minimum value, use whole.
Otherwise, use range.

=cut
sub range {
    my ($start, $limit) = @_;
    die "invalid range: ($start, $limit)" unless $start < $limit;
    my $diff = $limit - $start;
    die if $diff > SEED_MODULUS;
    return gen {
        my ($seed, $shrinks) = @_; # shrink is unused
        my $remainder = SEED_MODULUS % $diff;
        my $top = SEED_MODULUS - $remainder;
        while ($seed >= $top) {
            $seed = nextseed($seed);
        }
        my $mod = $seed % $diff;
        return ($start + $mod, nextseed($seed));
    };
}

=item B<size()>

=cut
sub size() {
    return whole(0, 64);
}
memoize('size');

# Character and string generators

=item B<concat(STRGENS...)>

=cut
sub concat(@) {
    return comap { join('', @$_) } tuple(@_);
}

=item B<char(START, LIMIT)>

=cut
sub char {
    my ($start, $limit) = @_;
    return comap { chr($_) } range($start, $limit);
}

=item B<ascii()>

=cut
sub ascii() {
    return char(0, 128)
}
memoize('ascii');

=item B<printable()>

=cut
sub printable() {
    return char(32, 127)
}
memoize('printable');

=item B<wordchar()>

=cut
sub wordchar() {
    return oneof('A'..'Z', 'a'..'z', '0'..'9', '_');
}
memoize('wordchar');

=item B<codepoint()>

=cut
sub codepoint() {
    my $b10 = char(128, 192);
    return generators(
        concat(ascii()),
        concat(char(192, 224), $b10),
        concat(char(224, 240), $b10, $b10),
        concat(char(240, 248), $b10, $b10, $b10));
}
memoize('codepoint');

=item B<stringof(CHARGEN, SIZEGEN)>

=cut
sub stringof {
    my ($cgen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    return comap {
        join('', @$_)
    } flatmap {
        vector($cgen, $_)
    } $sgen;
}

=item B<identifier()>

=cut
sub identifier() {
    return stringof(wordchar(), whole(1, 8));
}
memoize('identifier');

=item B<string()>

=cut
sub string() {
    return stringof(codepoint());
}
memoize('string');

=item B<anyscalar()>

=cut
sub anyscalar() {
    return generators(
        const(undef),
        bool(),
        number(),
        identifier(),
        string());
}
memoize('anyscalar');

# Arary generators

=item B<tuple(GENS...)>

=cut
sub tuple(@) {
    my (@gens) = @_;
    return gen {
        my ($seed, $shrinks) = @_;
        my $res = [];
        for my $gen (@gens) {
            confess unless $gen;
            my ($value, $next) = $gen->($seed, $shrinks);
            push(@$res, $value);
            $seed = $next;
        }
        return ($res, $seed);
    };
}

=item B<record(ITEMGENS...)>

=cut
sub record(@) {
    my (@items) = @_;
    return gen {
        my ($seed, $shrinks) = @_;
        my $res = {};
        my $i = 0;
        while ($i < scalar(@items)) {
            my $key = $items[$i];
            my $gen = $items[$i + 1];
            $i += 2;
            my ($value, $next) = $gen->($seed, $shrinks);
            $res->{$key} = $value;
            $seed = $next;
        }
        return ($res, $seed);
    };
}

=item B<vector(GEN, SIZE)>

=cut
sub vector {
    my ($gen, $size) = @_;
    die "invalid size: $size" unless defined($size) && $size >= 0;
    return gen {
        my ($seed, $shrinks) = @_;
        my $res = [];
        while ($size > 0) {
            my ($value, $next) = $gen->($seed, $shrinks);
            push(@$res, $value);
            $seed = $next;
            $size -= 1;
        }
        return ($res, $seed);
    };
}

=item B<array(GEN, SIZEGEN)>

=cut
sub array {
    my ($egen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    $egen = anything() unless defined($egen);
    return flatmap { vector($egen, $_) } $sgen;
}

# Hash generators

=item B<table(KEYGEN, GEN, SIZEGEN)>

=cut
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

=item B<hash(GEN, SIZEGEN)>

=cut
sub hash {
    my ($gen, $sgen) = @_;
    $sgen = size() unless defined($sgen);
    return table(identifier(), $gen, $sgen);
}

# Function generator

=item B<function(GEN)>

=cut
sub function {
    my ($gen) = @_;
    return gen {
        my ($seed0, $shrinks) = @_;
        my $f = sub {
            my (@inputs) = @_;
            my $seed = $seed0;
            foreach my $input (@inputs) {
                $seed = absorb($seed, $input);
            }
            my ($value, $next) = $gen->($seed, $shrinks);
            return $value;
        };
        return ($f, nextseed($seed0));
    };
}

# arbitrary generator

=item B<gengen()>

=cut
sub gengen {
    return comap {
        if    ($_ <= 10) { bool() }
        elsif ($_ <= 20) { flatmap { float($_) } whole(1, 1000000000) }
        elsif ($_ <= 30) { flatmap { whole($_) } whole(1, 1000000000) }
        elsif ($_ <= 40) { identifier() }
        elsif ($_ <= 50) { string() }
        elsif ($_ <= 55) { flatmap { tuple(@$_) } tuple(gengen(), gengen()) }
        elsif ($_ <= 60) { flatmap { record(@$_) } tuple(identifier(), gengen(), identifier(), gengen()) }
        elsif ($_ <= 65) { flatmap { vector(@$_) } tuple(gengen(), whole(1, 6)) }
        else             { const(undef) }
    } whole(1, 75);
}
memoize('gengen');

=item B<anything()>

=cut
sub anything {
    return flatten(gengen());
}
memoize('anything');

=item B<genseed()>

=cut
sub genseed {
    return gen {
        my ($seed, $shrinks) = @_;
        return ($seed, nextseed($seed));
    }
}
memoize('genseed');

# Interactive method for testing generators

=item B<sample(GEN, COUNT)>

=cut
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

=item B<randomseed()>

=cut
sub randomseed {
    return int(rand(SEED_MODULUS));
}

=item B<nextseed(SEED)>

=cut
sub nextseed {
    my ($seed) = @_;
    my $next = (16807 * $seed) % SEED_MODULUS;
    my $m = int($next % 4);
    return $next
}

=item B<randomfloat(SEED, DELTA, START)>

=cut
sub randomfloat {
    my ($seed, $delta, $start) = @_;
    return ($seed / SEED_MODULUS) * $delta + $start;
}

=item B<absorb(SEED, VALUE)>

=cut
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
            $seed = ($seed + ord($c)) % SEED_MODULUS;
            $i += 1
        }
    }
    return $seed;
}

=back

=head1 Conclusion

The end.

=cut

1;

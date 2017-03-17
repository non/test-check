package Test::Check;

use warnings;
use strict;

our $VERSION = '0.01';

=head1 NAME

Test::Check - Property-based testing for Perl.

=head1 SYNOPSIS

    use Test::Check tests => 2;
    use Test::Check::Gen qw(/.*/);

    my $g1 = whole(1000); // generate integers -999 to 999

    test "+ is associative" => prop {
        my ($x, $y, $z) = @_;
        my $lhs = (($x + $y) + $z);
        my $rhs = ($x + ($y + $z));
        $lhs == $rhs;
    } $g1, $g1, $g1;

    my $g2 = oneof(0, 1, 1000); // generate one of 3 numbers

    test "0 is unique" => prop {
        my (%o) = @_;
        my $isunchanged = $o{x} + $o{y} == $o{x};
        my $iszero = $o{y} == 0;
        $unchanged == $iszero;
    } x => $g2, y => $g2;

=head1 DESCRIPTION

We should write a description.

=over

=cut
use Test::More import => ['is_deeply'];

use Test::Check::Gen qw(/.*/);
use Test::Check::Prop qw(/.*/);

use base 'Test::Builder::Module';

our @EXPORT = qw(test prop);

=item B<test NAME PROPERTY>

Test a property.

=cut
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

=item B<prop { BLOCK } [GEN1, GEN2...]>

Define a property.

=cut
sub prop(&@) {
    my ($p, @gens) = @_;
    return [$p, @gens];
}

=back

=head1 Conclusion

The end.

=cut

1;

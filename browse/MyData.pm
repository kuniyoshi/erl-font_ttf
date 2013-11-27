package MyData;
use utf8;
use strict;
use warnings;

our @CHARS;
our %CHAR;

my $CHARS_REF = do "jis_x_0208.pl";

my %code = (
    kana   => [ 1  .. 8  ],
    kanji1 => [ 16 .. 47 ],
    kanji2 => [ 48 .. 84 ],
);

for my $key ( keys %code ) {
    for my $num ( @{ $code{ $key } } ) {
        $code{ $num } = $key;
    }
}

for my $ku ( sort { $a <=> $b } keys %{ $CHARS_REF } ) {
    my @chars;

    for my $ten ( sort { $a <=> $b } keys %{ $CHARS_REF->{ $ku } } ) {
        push @chars, $CHARS_REF->{ $ku }{ $ten };
    }

    push @CHARS, @chars;

    unless ( $CHAR{ $code{ $ku } } ) {
        $CHAR{ $code{ $ku } } = [ ];
    }

    push @{ $CHAR{ $code{ $ku } } }, @chars;
}

1;

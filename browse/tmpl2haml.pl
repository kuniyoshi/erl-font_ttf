#!/usr/bin/perl
use 5.10.0;
use utf8;
use strict;
use warnings;
use open qw( :std :utf8 );
use autodie qw( open close );
use Data::Dumper;
use File::Basename qw( fileparse );
use Template;
use MyData;
use Data::Page;

$Data::Dumper::Terse    = 1;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent   = 1;

my $filename = shift
    or die "usage: $0 <tmpl filename>";

my $tmpl = Template->new;
my $input = do {
    open my $FH, "<", $filename;
    local $/;
    my $data = <$FH>;
    close $FH;
    $data;
};
my $output;
my %param;
my( $basename, $path, $suffix ) = fileparse( $filename, qw( .tmpl ) );
$param{basename} = $param{font_family} = $basename;

my %chars;

foreach my $code ( keys %MyData::CHAR ) {
    my @chars = @{ $MyData::CHAR{ $code } };
    $chars{ $code } = [ ];

    my $page = Data::Page->new;
    $page->total_entries( scalar @chars );
    $page->entries_per_page( 20 );
    $page->current_page( 1 );

    foreach my $current ( $page->first_page .. $page->last_page ) {
        $page->current_page( $current );
        my @parts = @chars[ ( $page->first .. $page->last ) ];
        push @{ $chars{ $code } }, \@parts;
    }
}

$param{chars} = \%chars;

$tmpl->process(
    \$input,
    \%param,
    \$output,
)
    or die "process(): ", $tmpl->error;

say $output;

exit;

__END__
	perl -MyData -MTemplate -e 'Template->new->process(do { local $$/; my $$d = <>; \$$d }, { title => "$<", chars => \@yData::CHARS }, \*STDOUT) or die "Could not process."' $< > $$(echo $< | sed 's/\.tmpl$$/.haml/')

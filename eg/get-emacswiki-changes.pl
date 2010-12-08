#!/usr/bin/env perl
use strict;
use warnings;
use LWP::UserAgent;
use Getopt::Long;

sub listup_changes {
    my $ua = LWP::UserAgent->new;
    my $response = $ua->get(qw!http://www.emacswiki.org/emacs/?action=rc;raw=1!);
    for (split /\n/, $response->decoded_content) {
        next unless /link: (.+)$/;
        print $1, "\n";
    }
}

sub open_it {
    my $url = shift;
    `open $url`;
}

GetOptions("init=s" => \my $init,
           "action=s"   => \my $action);
if ($init) {
    if ($init eq 'list') {
        listup_changes;
    }
}elsif ($action) {
    my $candidate = shift;
    if ($action eq 'open') {
        open_it($candidate);
    }
}

#!/usr/bin/env perl
use strict;
use warnings;
use Encode;
use JSON;
use opts;
use Regexp::Common qw/URI/;
use URI::Fetch;
use XML::Feed;


sub listup_entries {
    my $url = shift;
    my $res = URI::Fetch->fetch($url)
      or die URI::Fetch->errstr;
    my $feed = XML::Feed->parse(\($res->content));
    my $ns = $feed->{rss}->{namespaces}->{hatena};
    for my $entry ($feed->entries) {
        printf "(%dusers) %s (%s)\n", $entry->{entry}->{$ns}->{bookmarkcount},
          encode_utf8($entry->title), $entry->link;
    }
}

sub listup_hotentries {
    my $url = 'http://b.hatena.ne.jp/hotentry.rss';
    listup_entries($url);
}

sub listup_myentries {
    my $url = 'http://b.hatena.ne.jp/yaotti/rss';
    listup_entries($url);
}

sub view_entry {
    my $url = shift;
    warn "Open ", $url;
    `open $url`;
}

sub view_comment {
    my $url = shift;
    my $comment_url = 'http://b.hatena.ne.jp/entry/'. $url;
    warn "Open ", $comment_url;
    `open $comment_url`;
}


opts my $init => { isa => 'Str' },
  my $action => { isa => 'Str' };
if ($init) {
    if ($init eq 'list') {
        listup_hotentries;
    }elsif ($init eq 'my_entries') {
        listup_myentries;
    }
}elsif ($action) {
    my $candidate = shift;
    my ($url) = $candidate =~ /\(($RE{URI}{HTTP})\)$/;
    if ($action eq 'open') {
        view_entry($url);
    }elsif ($action eq 'view_comment') {
        view_comment($url);
    }
}

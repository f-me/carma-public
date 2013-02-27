#!/usr/bin/env perl

use JSON;
use Data::Dumper;

use strict;

open F, '<', '../srv/resources/site-config/dictionaries/Programs.json';

my $j = decode_json join '', <F>;

foreach(@{$j->{'entries'}}) {
    my $j = encode_json $_;
    `curl -XPOST 127.0.0.1:8000/_/program -d '$j'`
}

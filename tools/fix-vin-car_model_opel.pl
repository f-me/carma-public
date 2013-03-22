#!/usr/bin/env perl

use JSON;
use Data::Dumper;

use strict;

open F, '<', '../srv/resources/site-config/dictionaries/CarModels.json';

my $j = decode_json join '', <F>;

my @keys = split '\n', `redis-cli keys vin:*`;

foreach( @keys ) {
  my $make = `redis-cli hget $_ car_make`;
  chomp($make);
  next unless $make;
  my $d = $j->{'entries'}->{$make};
  print "$_ $make\n" unless $d;

  if ($make eq 'Opel') {
    my $lcmake = lc($make);
    `redis-cli hset $_ car_make $lcmake`
  }
}

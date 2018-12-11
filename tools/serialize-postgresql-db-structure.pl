#!/usr/bin/env perl
# serialize-postgresql-db-structure.pl
# (see bottom of this script for license and usage info)
use v5.10; use warnings; use strict; use autodie qw(:all);
use utf8; no warnings 'utf8';
use Encode qw(decode_utf8);
use Pod::Usage;
use Getopt::Long;
use Scalar::Util qw(openhandle);
use List::Util qw(first);
use IPC::Open2;
use JSON;

# db connection parameters
GetOptions(
	'help|?'            => \my $show_help,
	'host|h=s'          => \my $host,
	'port|p=i'          => \my $port,
	'user|u=s'          => \my $user,
	'dbname|d=s'        => \my $dbname,
	'output-format|o=s' => \my $output_format,
) || sub {
	say STDERR '';
	pod2usage(-exitval => 1, -verbose => 1, -output => \*STDERR)
}->();

if (scalar @ARGV) {
	say STDERR 'Unexpected arguments: ', join ' ', @ARGV;
	say STDERR '';
	pod2usage(-exitval => 1, -verbose => 1, -output => \*STDERR)
}

# default values for command-line options
$show_help = 0           unless defined $show_help;
$host      = 'localhost' unless defined $host;
$port      = 5432        unless defined $port;
$user      = `whoami`    unless defined $user;

pod2usage(-exitval => 0, -verbose => 2) if $show_help;
my @errors = ();

push @errors, '--dbname=[string] is required command-line option'
	unless defined $dbname;
push @errors, '--output-format=(json|md|html) is required command-line option'
	unless defined $output_format;
push @errors,
	qq{--output_format must be "json" or "md" or "html" (not "$output_format")}
	if defined($output_format)
	&& !(first {$output_format eq $_} qw(json md html));

if (scalar @errors) {
	say STDERR for @errors;
	say STDERR '';
	pod2usage(-exitval => 1, -verbose => 1, -output => \*STDERR);
}

# opening bi-directional pipe
my @args = ('psql', '-h', $host, '-p', $port, '-d', $dbname, '-U', $user);
open2 \*PSQL_OUT, \*PSQL_IN, @args;

END { # cleanup
	close PSQL_OUT if defined openhandle *PSQL_OUT;
	close PSQL_IN  if defined openhandle *PSQL_IN;
}

my $sep = ('~' x 50).'SEPARATOR'.('~' x 50);
sub separate {say PSQL_IN "SELECT '$sep';"}

say PSQL_IN '\d+'; # requesting list of tables
separate; # marking end of output
my @tables = (); # list of names (strings)
my @views = (); # list of names (strings)
{
	local $_;
	my $is_started = 0;
	while (chomp($_ = decode_utf8 <PSQL_OUT>)) {
		last if /$sep/; # end of output of a command

		unless ($is_started) {
			# detecting table header and table body separator
			$is_started = 1 if /^[-+]+$/ && /\+/;
			next;
		}

		my @columns = split /\|/;
		map s/(^\s+|\s+$)//g, @columns;
		next if scalar(@columns) < 2;

		if ($columns[2] eq 'table') {
			push @tables, $columns[1];
		} elsif ($columns[2] eq 'view') {
			push @views, $columns[1];
		}
	}
}

# put labeled debugging separator to log (STDERR)
sub stitle {say STDERR (('-' x 20).' '.shift.' '.('-' x 20))}

# info just for debug
stitle 'Tables';
say STDERR for @tables;
stitle 'Views';
say STDERR for @views;

sub get_table_hash {local $_ = shift; s/ /_/g; "table__$_"}

sub get_field_hash {
	my ($table, $field) = (shift, shift); $table =~ s/ /_/g; $field =~ s/ /_/g;
	"table__${table}__field__${field}"
}

my %tables_data = ();
my %views_data = ();

foreach my $table (@tables) {
	say PSQL_IN '\d+ "', $table, '"';
	separate; # marking end of output
	my $is_fields_started = 0;
	my $is_meta_started = 0;
	my $is_foreign_keys_meta = 0;
	my $is_done = 0;

	my @fields = ();
	my %foreign_refs = ();
	my %table = (fields => \@fields, foreign_refs => \%foreign_refs);

	while (chomp($_ = decode_utf8 <PSQL_OUT>)) {
		if (/$sep/) {$is_done = 1; last} # end of output of a command

		unless ($is_fields_started) {
			# detecting table header and table body separator
			$is_fields_started = 1 if /^[-+]+$/ && /\+/;
			next
		}

		my @columns = split /\|/;
		map s/(^\s+|\s+$)//g, @columns;

		if (scalar(@columns) < 4) {
			if (/^[^|]+:$/) {
				$is_meta_started = 1;
				$is_foreign_keys_meta = 1 if $_ eq 'Foreign-key constraints:';
				last # in this case we're done with fields
			} else {next}
		}

		my %field = (
			name     => $columns[0],
			type     => $columns[1],
			optional => $columns[3],
		);

		push @fields, \%field;
	}

	if (!$is_done && $is_meta_started) {
		while (chomp($_ = decode_utf8 <PSQL_OUT>)) {
			if (/$sep/) {$is_done = 1; last} # end of output of a command

			if (/^[^|]+:$/) {
				$is_foreign_keys_meta = $_ eq 'Foreign-key constraints:'
			} elsif (/^\s{4}\S/ && $is_foreign_keys_meta) {
				my ($a, $b) = ('FOREIGN KEY', 'REFERENCES');
				/\s+$a\s+\(([^)]+)\)\s+$b\s+("[^"]+"|[^(]+)\(([^)]+)\)/;
				my $field = $1;
				my $ref_table = $2;
				my $ref_field = $3;
				$ref_table =~ s/^"(.*)"$/$1/;

				my %ref = (
					table => $ref_table,
					field => $ref_field,
				);

				$foreign_refs{$field} = \%ref;
			}
		}
	}

	$tables_data{$table} = \%table
}

foreach my $view (@views) {
	my @fields = ();

	say PSQL_IN '\d+ "', $view, '"';
	separate; # marking end of output
	my $is_fields_started = 0;
	my $is_meta_started = 0;

	while (chomp($_ = decode_utf8 <PSQL_OUT>)) {
		last if /$sep/; # end of output of a command

		# meta blocks of a view is just a view definition.
		# just skipping this, we don't need it...
		next if $is_meta_started;

		unless ($is_fields_started) {
			# detecting table header and table body separator
			$is_fields_started = 1 if /^[-+]+$/ && /\+/;
			next
		}

		my @columns = split /\|/;
		map s/(^\s+|\s+$)//g, @columns;

		if (/^[^|]+:$/) {$is_meta_started = 1; next}
		next if scalar(@columns) < 4;

		my %field = (
			name => $columns[0],
			type => $columns[1],
		);

		push @fields, \%field;
	}

	$views_data{$view} = \@fields
}

if ($output_format eq 'json') {

	my %all = (tables => \%tables_data, views => \%views_data);
	say encode_json \%all

} elsif ($output_format eq 'md') {

	say "# Tables";
	say '';
	while (my ($_table_name, $table_data) = each %tables_data) {
		my $table_hash = get_table_hash $_table_name;
		my @fields = @{$table_data->{fields}};
		my %foreign_refs = %{$table_data->{foreign_refs}};

		my $table_name = "`$_table_name`" if $_table_name ne '';
		say "- <a name='$table_hash'>$table_name</a>";
		say "  - Fields:";
		foreach my $field (@fields) {
			my ($name, $type, $optional) =
				($field->{name}, $field->{type}, $field->{optional});
			my $field_hash = get_field_hash $_table_name, $name;
			$name = "`$name`" if $name ne '';
			$type = "`$type`" if $type ne '';
			$optional = "`$optional`" if $optional ne '';
			say "    - <a name='$field_hash'>$name</a>";
			say "      - Type: $type";
			say "      - Nullable: $optional";
		}

		if (scalar(keys(%foreign_refs)) > 0) {
			say "  - Foreign keys of the table";
			while (my ($k, $v) = each %foreign_refs) {
				my ($table, $field) = ($v->{table}, $v->{field});
				my $table_hash = get_table_hash $table;
				my $field_hash = get_field_hash $table, $field;
				$k = "`$k`" if $k ne '';
				$table = "`$table`" if $table ne '';
				$field = "`$field`" if $field ne '';
				say "    - $k";
				say "      - Foreign table reference: <a href='#$table_hash'>$table</a>";
				say "      - Foreign field reference: <a href='#$field_hash'>$field</a>";
			}
		}
	}

	say '';
	say "# Views";
	say '';
	while (my ($view_name, $view_fields) = each %views_data) {
		my $hash = $view_name; $hash =~ s/ /_/g;
		my @fields = @{$view_fields};
		$view_name = "`$view_name`" if $view_name ne '';
		say "- <a name='$hash'>$view_name</a>";
		foreach my $field (@fields) {
			my ($name, $type) = ($field->{name}, $field->{type});
			$name = "`$name`" if $name ne '';
			$type = "`$type`" if $type ne '';
			say "  - $name";
			say "    - Type: $type";
		}
	}

} elsif ($output_format eq 'html') {
	say "<!doctype html>";
	say "<html>";
	say "<head>";
	say "<meta charset='utf-8'>";
	say "<style>table th, table td { border: 1px solid gray; padding: 5px; }</style>";
	say "</head>";
	say "<body>";

	say "<h1>Tables</h1>";
	while (my ($table_name, $table_data) = each %tables_data) {
		my $table_hash = get_table_hash $table_name;
		my @fields = @{$table_data->{fields}};
		my %foreign_refs = %{$table_data->{foreign_refs}};

		say "<h2><a name='$table_hash'>$table_name</a></h2>";
		say "<table>";
			say "<thead><tr>";
				say "<th>Field name</th>";
				say "<th>Type</th>";
				say "<th>Nullable</th>";
			say "</tr></thead>";
			say "<tbody>";
				foreach my $field (@fields) {
					my ($name, $type, $optional) =
						($field->{name}, $field->{type}, $field->{optional});
					my $field_hash = get_field_hash $table_name, $name;
					say "<tr>";
						say "<td><a name='$field_hash'>$name</a></td>";
						say "<td>$type</td>";
						say "<td>$optional</td>";
					say "</tr>";
				}
		say "</tbody>";
		say "</table>";

		if (scalar(keys(%foreign_refs)) > 0) {
			say "<h3>Foreign keys of the table</h3>";
			say "<table>";
				say "<thead><tr>";
					say "<th>Field name</th>";
					say "<th>Foreign table reference</th>";
					say "<th>Foreign field reference</th>";
				say "</tr></thead>";
				say "<tbody>";
					while (my ($k, $v) = each %foreign_refs) {
						my ($table, $field) = ($v->{table}, $v->{field});
						my $table_hash = get_table_hash $table;
					    my $field_hash = get_field_hash $table, $field;
						say "<tr>";
							say "<td>$k</td>";
							say "<td><a href='#$table_hash'>$table</a></td>";
							say "<td><a href='#$field_hash'>$field</a></td>";
						say "</tr>";
					}
			say "</tbody>";
			say "</table>";
		}
	}

	say "<hr>";
	say "<h1>Views</h1>";
	while (my ($view_name, $view_fields) = each %views_data) {
		my $hash = $view_name; $hash =~ s/ /_/g;
		my @fields = @{$view_fields};

		say "<h2><a name='$hash'>$view_name</a></h2>";
		say "<table>";
			say "<thead><tr>";
				say "<th>Field name</th>";
				say "<th>Type</th>";
			say "</tr></thead>";
			say "<tbody>";
				foreach my $field (@fields) {
					my ($name, $type) = ($field->{name}, $field->{type});
					say "<tr>";
						say "<td>$name</td>";
						say "<td>$type</td>";
					say "</tr>";
				}
		say "</tbody>";
		say "</table>";
	}

	say "</body>";
	say "</html>";

} else {die 'Unexpected behavior'}

__END__

=head1 NAME

serialize-postgresql-db-structure.pl

=head1 SYNOPSIS

serialize-postgresql-db-structure.pl [options]

=head1 OPTIONS

=over 6

=item B<--help> B<-?>

Shows expanded usage info, description and license of this program.

=item B<--host>=I<string> B<-h>=I<string>

Hostname or IP-address of the PostgreSQL server (default is "localhost").

=item B<--port>=I<string> B<-p>=I<string>

Port of PostgreSQL server (default is 5432).

=item B<--user>=I<string> B<-u>=I<string>

Username of PostgreSQL database
(default is current username who runs this script).

=item B<--dbname>=I<string> B<-d>=I<string>

PostgreSQL database name.

=item B<--output-format>=I<string> B<-o>=I<string>

Output format (could be one of "json", "md" or "html").

=back

=head1 DESCRIPTION

B<This script> helps to produce serialized structure of a PostgreSQL database.

It supports different output standards: I<json>, I<markdown> and I<html>.

It serializes separately tables and views, fields and foreign references.

See also https://gist.github.com/unclechu/bc5b587e8095cf7bf525fdd535bea3b0

=head1 LICENSE B<(BSD3)>

Copyright (c) 2018 Viacheslav Lotsmanov

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Max nor the names of other contributors may
      be used to endorse or promote products derived from this
      software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut

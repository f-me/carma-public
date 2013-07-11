create or replace language plperlu;
create or replace function remove_cancel(text) returns text
  strict immutable
  language plperlu as $$
    use JSON;
    my $j = JSON->new->utf8(0);
    return $j->encode([ grep {!$_->{'type'}} @{$j->decode($_[0])} ])
  $$;

update casetbl set comments = remove_cancel(comments) where comments is not null;
drop function remove_cancel(text);
drop language plperlu;

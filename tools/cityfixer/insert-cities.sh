#!/usr/bin/env bash

set -e

CITY_LIST=$1

psql carma << EOF
create temporary table city_raw (json json);
copy city_raw from stdin;
`cat $CITY_LIST`
\.

insert into "City" (label, value, coords)
  select
      json->>'label',
      json->>'value',
      ST_SetSRID(
        ST_MakePoint(
          (json->>'lon')::float8,
          (json->>'lat')::float8),
        4326)
    from city_raw
    where (json->>'country' ~ 'RU'
        or json->>'country' ~ 'Russia')
      and not exists (select 1 from "City" where label = json->>'label');

insert into "City" (label, value, coords)
  select
      json->>'label',
      json->>'value',
      ST_SetSRID(
        ST_MakePoint(
          (json->>'lon')::float8,
          (json->>'lat')::float8),
        4326)
    from city_raw
    where not (json->>'country' ~ 'RU'
        or json->>'country' ~ 'Russia')
      and not exists (select 1 from "City" where label = json->>'label')
      and regexp_replace(json->>'pop', '\D', '', 'g')::int4 > 500000;


update "City" c
  set coords = ST_SetSRID(
    ST_MakePoint(
      (json->>'lon')::float8,
      (json->>'lat')::float8),
    4326)
  from city_raw r
    where c.label = r.json->>'label'
      and c.coords is null;
EOF


#!/bin/bash

$PSQL << EOF
  alter table "PartnerService"
    add column tarifoptions text not null default ''
EOF

$PSQL << EOF
  update "PartnerService" new
    set tarifoptions = coalesce(old.tarifoptions, '')
    from partner_servicetbl old
    where old.id = new.id
EOF

$PSQL -f baseline/5-views/8-ru-services-priority.sql
$PSQL -f baseline/5-views/5-ru-partners.sql
$PSQL -f baseline/2-functions/1-geo-within.sql

#!/bin/bash -e

for role in {programman,head,director,analyst,back,supervisor,parguy,account,front,vwfake,admin,partner};
do
    cat <<EOF
  insert into "FieldPermission" (role, model, field, r, w) values
    ('$role', 'rent', 'towDealer_partnerId', true, true);
EOF
done | $PSQL

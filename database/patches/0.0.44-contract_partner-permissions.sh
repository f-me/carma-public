#!/bin/bash -e


for model in {bank,consultation,continue,deliverCar,deliverClient,deliverParts,hotel,insurance,tech,taxi,tech1,tickets,transportation,sober,ken,rent,averageCommissioner} ; do
  cat <<EOF
    insert into "FieldPermission" (role, model, field, r, w) values
        ('all', '$model', 'contractor_partner', true, true),
        ('all', '$model', 'contractor_partnerId', true, true),
        ('all', '$model', 'contractor_partnerTable', true, true),
        ('all', '$model', 'contractor_partnerMap', true, true),
        ('all', '$model', 'contractor_coords', true, true),
        ('all', '$model', 'contractor_partnerCancel', true, true),
        ('all', '$model', 'contractor_address', true, true);
EOF
done | $PSQL

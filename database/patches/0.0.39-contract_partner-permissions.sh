#!/bin/bash -e


for model in {bank,consultation,continue,deliverCar,deliverClient,deliverParts,hotel,insurance,tech,taxi,tech1,tickets,transportation,sober,ken,rent,averageCommissioner} ; do
  cat <<EOF
    insert into "FieldPermission" (role, model, field, r, w) values
        ('front', '$model', 'contractor_partner', true, true),
        ('front', '$model', 'contractor_partnerId', true, true),
        ('front', '$model', 'contractor_partnerTable', true, true),
        ('front', '$model', 'contractor_partnerMap', true, true),
        ('front', '$model', 'contractor_coords', true, true),
        ('front', '$model', 'contractor_partnerCancel', true, true),
        ('front', '$model', 'contractor_address', true, true),
        ('back',  '$model', 'contractor_partner', true, true),
        ('back',  '$model', 'contractor_partnerId', true, true),
        ('back',  '$model', 'contractor_partnerTable', true, true),
        ('back',  '$model', 'contractor_partnerMap', true, true),
        ('back',  '$model', 'contractor_coords', true, true),
        ('back',  '$model', 'contractor_partnerCancel', true, true),
        ('back',  '$model', 'contractor_address', true, true);
EOF
done | $PSQL

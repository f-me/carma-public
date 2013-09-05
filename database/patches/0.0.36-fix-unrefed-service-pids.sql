UPDATE towagetbl 
SET towDealer_partnerId = concat('partner:',towDealer_partnerId)
WHERE towDealer_partnerId NOT LIKE '%:%'
AND towDealer_partnerId <> '';

UPDATE towagetbl 
SET contractor_partnerId = concat('partner:',contractor_partnerId)
WHERE contractor_partnerId NOT LIKE '%:%'
AND contractor_partnerId <> '';

UPDATE techtbl
SET contractor_partnerId = concat('partner:',contractor_partnerId)
WHERE contractor_partnerId NOT LIKE '%:%'
AND contractor_partnerId <> '';

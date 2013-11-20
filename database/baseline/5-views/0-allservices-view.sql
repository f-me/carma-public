CREATE VIEW allservicesview AS

SELECT averagecommissionertbl."id"
	,averagecommissionertbl."type"
	,averagecommissionertbl.contractor_partner "towdealer_partner"
	,averagecommissionertbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,NULL::TEXT AS techtype
	,NULL::TEXT AS towtype
	,averagecommissionertbl.contractor_address "towaddress_address"
	,averagecommissionertbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM averagecommissionertbl

UNION ALL

SELECT banktbl."id"
	,banktbl."type"
	,banktbl.contractor_partner "towdealer_partner"
	,banktbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,NULL::TEXT AS "techtype"
	,NULL::TEXT AS towtype
	,NULL::TEXT AS "towaddress_address"
	,banktbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM banktbl

UNION ALL

SELECT consultationtbl."id"
	,consultationtbl."type"
	,consultationtbl.contractor_partner "towdealer_partner"
	,consultationtbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,consultationtbl.constype "techtype"
	,NULL::TEXT AS towtype
	,NULL::TEXT AS "towaddress_address"
	,consultationtbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM consultationtbl

UNION ALL

SELECT deliverclienttbl."id"
	,deliverclienttbl."type"
	,deliverclienttbl.contractor_partner "towdealer_partner"
	,deliverclienttbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,deliverclienttbl.deliverytype "techtype"
	,NULL::TEXT AS towtype
	,deliverclienttbl.contractor_address "towaddress_address"
	,deliverclienttbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM deliverclienttbl

UNION ALL

SELECT informationtbl."id"
	,informationtbl."type"
	,informationtbl.contractor_partner "towdealer_partner"
	,informationtbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,NULL::TEXT AS techtype
	,NULL::TEXT AS towtype
	,informationtbl.contractor_address "towaddress_address"
	,informationtbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM informationtbl

UNION ALL

SELECT deliverpartstbl."id"
	,deliverpartstbl."type"
	,deliverpartstbl.contractor_partner "towdealer_partner"
	,deliverpartstbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,NULL::TEXT AS techtype
	,NULL::TEXT AS towtype
	,deliverpartstbl.contractor_address "towaddress_address"
	,deliverpartstbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM deliverpartstbl

UNION ALL

SELECT hoteltbl."id"
	,hoteltbl."type"
	,hoteltbl.contractor_partner "towdealer_partner"
	,hoteltbl.suburbanmilage
	,hoteltbl.providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,NULL::TEXT AS techtype
	,NULL::TEXT AS towtype
	,hoteltbl.contractor_address "towaddress_address"
	,hoteltbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM hoteltbl

UNION ALL

SELECT insurancetbl."id"
	,insurancetbl."type"
	,insurancetbl.contractor_partner "towdealer_partner"
	,insurancetbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,NULL::TEXT AS techtype
	,NULL::TEXT AS towtype
	,insurancetbl.contractor_address "towaddress_address"
	,insurancetbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM insurancetbl

UNION ALL

SELECT sobertbl."id"
	,sobertbl."type"
	,sobertbl.contractor_partner "towdealer_partner"
	,sobertbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,NULL::TEXT AS techtype
	,NULL::TEXT AS towtype
	,sobertbl.contractor_address "towaddress_address"
	,sobertbl.contractor_partnerid "towdealer_partnerid",
	assignedto
FROM sobertbl

UNION ALL

SELECT renttbl.ID
	,renttbl.TYPE
	,renttbl.towdealer_partner
	,NULL::TEXT AS suburbanmilage
	,renttbl.providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,NULL::TEXT AS techtype
	,NULL::TEXT AS towtype
	,NULL::TEXT AS towaddress_address
	,renttbl.towdealer_partnerid,
	assignedto
FROM renttbl

UNION ALL

SELECT towagetbl.ID
	,towagetbl.TYPE
	,towagetbl.towdealer_partner
	,towagetbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,towagetbl.repairenddate
	,NULL::TEXT AS techtype
	,towagetbl.towtype
	,towagetbl.towaddress_address
	,towagetbl.towdealer_partnerid,
	assignedto
FROM towagetbl

UNION ALL

SELECT techtbl.ID
	,techtbl.TYPE
	,NULL::TEXT AS towdealer_partner
	,techtbl.suburbanmilage
	,NULL::TEXT AS providedfor
	,NULL::TIMESTAMP WITH TIME ZONE AS repairenddate
	,techtbl.techtype
	,NULL::TEXT AS towtype
	,NULL::TEXT AS towaddress_address
	,NULL::TEXT AS towdealer_partnerid,
	assignedto
FROM techtbl;

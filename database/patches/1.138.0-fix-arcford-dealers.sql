UPDATE "Contract" SET lastcheckdealer = seller
WHERE fromArc AND subprogram = 14 AND seller IS NOT NULL;

UPDATE "Contract" SET seller = NULL
WHERE fromArc AND subprogram = 14 AND seller IS NOT NULL;

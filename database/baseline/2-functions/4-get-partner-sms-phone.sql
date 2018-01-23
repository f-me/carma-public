BEGIN;

CREATE OR REPLACE FUNCTION
  GetPartnerSmsPhone (partnerId INT)
RETURNS TEXT
AS $$

  SELECT
    item->>'value' AS phone
  FROM
    partnertbl                    AS p,
    JSON_ARRAY_ELEMENTS(p.phones) AS item,
    JSON_EACH_TEXT(item)          AS pair
  WHERE TRUE
    AND p.id       = partnerId
    AND pair.key   = 'key'
    AND pair.value = 'fax'
  LIMIT 1

$$ LANGUAGE SQL
SECURITY DEFINER;

REVOKE ALL ON FUNCTION GetPartnerSmsPhone
  (INT) FROM PUBLIC;

GRANT EXECUTE ON FUNCTION GetPartnerSmsPhone
  (INT) TO carma_db_sync;

COMMIT;

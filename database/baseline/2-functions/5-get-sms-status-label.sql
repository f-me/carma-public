BEGIN;

CREATE OR REPLACE FUNCTION
  GetSmsStatusLabel (status TEXT)
RETURNS TEXT
AS $$

  SELECT
    CASE
      WHEN status = 'sent'        THEN 'Отправлено'
      WHEN status = 'please-send' THEN 'В очереди на отправку'
      WHEN status = 'draft'       THEN 'Черновик'
      WHEN status = 'error'       THEN 'Ошибка'
      WHEN status = 'processing'  THEN 'Отправляется'
    END

$$ LANGUAGE SQL
SECURITY DEFINER;

REVOKE ALL ON FUNCTION GetSmsStatusLabel (TEXT) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION GetSmsStatusLabel (TEXT) TO PUBLIC;

COMMIT;

UPDATE usermetatbl SET isDealer = 'f' WHERE isDealer IS NULL;
ALTER TABLE usermetatbl ALTER COLUMN isDealer SET NOT NULL;

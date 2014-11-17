ALTER TABLE servicetbl
ALTER COLUMN parentId type int4
USING substring(parentId, ':(.*)')::int;

ALTER TABLE servicetbl
ADD CONSTRAINT "servicetbl_parentId_fkey"
FOREIGN KEY (parentId) REFERENCES casetbl (id);

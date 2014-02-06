ALTER TABLE casetbl ADD COLUMN contract int4 REFERENCES "Contract";
ALTER TABLE casetbl ADD COLUMN contractIdentifier text;

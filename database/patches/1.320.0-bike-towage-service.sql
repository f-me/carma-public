BEGIN;

CREATE TABLE "BikeTowage" (
    towerType integer REFERENCES "TowerType",
    bikeTowType integer REFERENCES "BikeTowType",

    towDealer_partner text,
    towDealer_partnerid integer REFERENCES partnertbl,
    towDealer_address text,
    towDealer_coords text,

    dealerdistance text,

    towAddress_address text,
    towAddress_comment text,
    towAddress_coords text,
    towAddress_map text,

    toweraddress_address text,
    toweraddress_comment text,
    toweraddress_coords text,
    toweraddress_map text,

    wheelsBlocked integer,

    orderNumber text,
    repairEndDate date,

    isCountryRide boolean DEFAULT false NOT NULL,
    suburbanMilage numeric(7,2),
    totalMilage numeric(7,2),
    partnerWarnedInTime boolean
)
INHERITS (servicetbl);

ALTER TABLE public."BikeTowage" OWNER TO carma;
ALTER TABLE ONLY "BikeTowage" ALTER COLUMN type SET DEFAULT 19;
ALTER TABLE ONLY "BikeTowage" ALTER COLUMN id SET DEFAULT nextval('servicetbl_id_seq'::regclass);

CREATE INDEX BikeTowage_createtime_idx ON "BikeTowage" USING btree (createtime);
CREATE INDEX BikeTowage_parentid_id_type_idx ON "BikeTowage" USING btree (parentid, id, type);

REVOKE ALL ON TABLE "BikeTowage" FROM PUBLIC;
GRANT ALL ON TABLE "BikeTowage" TO carma;
GRANT ALL ON TABLE "BikeTowage" TO carma_db_sync;
GRANT SELECT ON TABLE "BikeTowage" TO reportgen;

INSERT INTO "CtrModel" (id, value, label) VALUES (20, 'BikeTowage', 'Мотоэвакуация');
INSERT INTO "ServiceType" (id, label, icon, fdds, model) VALUES (19, 'Мотоэвакуация', '', '', 20);

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'BikeTowage', field, r, w
 FROM "FieldPermission" WHERE model='Towage' AND field NOT IN ('vandalism', 'accident', 'canNeutral', 'towingPointPresent', 'manipulatorPossible', 'companion', 'check1', 'check2', 'towType', 'towSort'));

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'BikeTowage', 'bikeTowType', r, w
 FROM "FieldPermission" WHERE model='Towage' AND field='towSort');

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, info, required, r, w)
(SELECT 20, program, ord, field, label, info, required, r, w
 FROM "ConstructorFieldOption" WHERE model=17 AND field NOT IN ('vandalism', 'accident', 'canNeutral', 'towingPointPresent', 'manipulatorPossible', 'companion', 'check1', 'check2', 'towType', 'towSort'));

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, info, required, r, w)
(SELECT 20, program, ord, 'bikeTowType', 'Тип мотоэвакуации', info, required, r, w
 FROM "ConstructorFieldOption" WHERE model=17 AND field='towSort');

COMMIT;

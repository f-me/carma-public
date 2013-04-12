CREATE EXTENSION postgis;

CREATE ROLE carma_search PASSWORD 'md568023aeacae5a76b23b958eb5da1a994' NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO carma_search;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO carma_search;

CREATE ROLE carma_db_sync PASSWORD 'md556d33ece5e1452257fa0a086e7945c0b' NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO carma_db_sync; -- FIXME:

CREATE ROLE carma_geo PASSWORD 'md5a73940ffdfdd8d8b9ecfbfba6cc3e2ab' NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN;

CREATE ROLE carma_action_assignment ENCRYPTED PASSWORD 'md5039cf6a6d8de18b95bd103f64c1dfab9' NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN;
GRANT SELECT, UPDATE ON actiontbl TO carma_action_assignment;
GRANT SELECT ON servicetbl TO carma_action_assignment;
GRANT SELECT ON casetbl TO carma_action_assignment;
-- Run this after first sync

GRANT SELECT, UPDATE ON partnertbl TO carma_geo;
GRANT SELECT ON partnerMessageTbl TO carma_geo;
GRANT ALL PRIVILEGES ON spatial_ref_sys TO carma_geo;


-- create indices
CREATE INDEX ON calltbl USING hash (callerName_phone1);

CREATE INDEX ON casetbl USING btree (callDate);

CREATE INDEX ON partnertbl USING hash (isActive);
CREATE INDEX ON partnertbl USING hash (isDealer) where isActive = true;
CREATE INDEX ON partnertbl USING hash (city) where isActive = true;

CREATE INDEX ON actiontbl USING hash (closed);
CREATE INDEX ON actiontbl USING hash (targetGroup);
CREATE INDEX ON actiontbl USING hash (caseId);
CREATE INDEX ON actiontbl USING btree (duetime) where closed = false;
CREATE INDEX ON actiontbl USING btree (priority) where closed = false;

CREATE INDEX ON servicetbl USING hash (urgentService);

-- case + service view
create view servicesview as
    select
        c.id as caseid,
        c.calldate,
        c.calltaker,
        c.comment,
        c.diagnosis1,
        c.diagnosis2,
        c.diagnosis3,
        c.diagnosis4,
        c.contact_name,
        c.contact_phone1,
        c.contact_phone2,
        c.contact_phone3,
        c.contact_phone4,
        c.contact_email,
        c.contact_contactowner,
        c.contact_ownername,
        c.contact_ownerphone1,
        c.contact_ownerphone2,
        c.contact_ownerphone3,
        c.contact_ownerphone4,
        c.contact_owneremail,
        c.program,
        c.car_vin,
        c.car_seller,
        c.car_make,
        c.car_model,
        c.car_platenum,
        c.car_makeyear,
        c.car_color,
        c.car_buydate,
        c.car_checkupdate,
        c.car_dealerto,
        c.car_mileage,
        c.car_checkupmileage,
        c.car_transmission,
        c.car_engine,
        c.car_liters,
        c.car_capacity,
        c.car_dims,
        c.car_weight,
        c.car_checkperiod,
        c.car_class,
        c.car_makecode,
        c.car_modelcode,
        c.car_faultcode,
        c.cardnumber_cardnumber,
        c.cardnumber_validfrom,
        c.cardnumber_validuntil,
        c.cardnumber_validuntilmilage,
        c.cardnumber_milageto,
        c.cardnumber_serviceinterval,
        c.cardnumber_cardowner,
        c.cardnumber_manager,
        c.vinchecked,
        c.caseaddress_address,
        c.caseaddress_comment,
        c.caseaddress_coords,
        c.caseaddress_map,
        c.city,
        c.temperature,
        c.dealercause,
        c.casestatus,
        c.claim,
        c.betacomment,
        c.services,
        c.actions,
        c.files as casefiles,
        c.comments,
        c.repair,

        s.type,
        s.id,
        s.parentid,
        s.createtime,
        s.paytype,
        s.payment_expectedcost,
        s.payment_costtranscript,
        s.payment_partnercost,
        s.payment_calculatedcost,
        s.payment_limitedcost,
        s.payment_overcosted,
        s.payment_paidbyruamc,
        s.payment_paidbyclient,
        s.times_expectedservicestart,
        s.times_factservicestart,
        s.times_expectedserviceend,
        s.times_factserviceend,
        s.times_expecteddealerinfo,
        s.times_factdealerinfo,
        s.times_expectedserviceclosure,
        s.times_factserviceclosure,
        s.falsecall,
        s.bill_billnumber,
        s.bill_billingcost,
        s.bill_billingdate,
        s.status,
        s.clientsatisfied,
        s.files,
        s.contractor_partner,
        s.contractor_partnerId,
        s.contractor_address,
        s.contractor_coords,
        s.contractor_partnermap,
        s.contractor_partnertable,
        s.warrantycase,

        t.towDealer_partner,
        t.suburbanMilage,
        t.providedFor,
        t.repairEndDate,
        t.techType,
        t.towType,
        t.towAddress_address,

        a.assignedTo as backoperator,

        p1.code as partner_code,
        p2.code as dealer_code

    from casetbl c, servicetbl s
        left outer join (
            select id, type, towDealer_partner, null as suburbanMilage, providedFor, null as repairEndDate, null as techType, null as towType, null as towAddress_address, towDealer_partnerId from renttbl
            union all
            select id, type, towDealer_partner, suburbanMilage, null as providedFor, repairEndDate, null as techType, towType, towAddress_address, towDealer_partnerId from towagetbl
            union all
            select id, type, null as towDealer_partner, suburbanMilage, null as providedFor, null as repairEndDate, techType, null as towType, null as towAddress_address, null as towDealer_partnerId from techtbl
            union all
            select id, type, null as towDealer_partner, null as suburbanMilage, providedFor, null as repairEndDate, null as techType, null as towType, null as towAddress_address, null as towDealer_partnerId from hoteltbl
            ) t on t.id = s.id and t.type = s.type
        left outer join actiontbl a on s.type || ':' || s.id = a.parentId and a.name = 'orderService'
        left outer join partnertbl p1 on s.contractor_partnerId = 'partner:' || p1.id
        left outer join partnertbl p2 on t.towDealer_partnerId = 'partner:' || p2.id
    where c.id::text = substring(s.parentId, ':(.*)');

GRANT SELECT ON servicesview TO carma_db_sync;

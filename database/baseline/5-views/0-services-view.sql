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
        c.car_make,
        c.car_model,
        c.car_platenum,
        c.car_makeyear,
        c.car_color,
        c.car_buydate,
        c.car_checkupdate,
        c.car_warrantystart,
        c.car_warrantyend,
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
        s.times_expecteddispatch,
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
        s.paid,
        s.scan,
        s.original,

        t.towDealer_partner,
        t.suburbanMilage,
        t.providedFor,
        t.repairEndDate,
        t.techType,
        t.towType,
        t.towAddress_address,

        a.assignedTo as backoperator,

        p1.code as partner_code,
        p2.code as dealer_code,
        p3.code as seller_code,
        p3.name as car_seller_name,
        p4.code as to_dealer_code,
        p4.name as car_dealerto_name

    from casetbl c
        left outer join partnertbl p3 on c.car_seller = p3.id::text
        left outer join partnertbl p4 on c.car_dealerTO = p4.id::text
      , servicetbl s
        left outer join (
            select   id
                   , type
                   , towDealer_partner
                   , null as suburbanMilage
                   , providedFor
                   , null as repairEndDate
                   , null as techType
                   , null as towType
                   , null as towAddress_address
                   , towDealer_partnerId
            from renttbl
            union all
            select   id
                   , type
                   , towDealer_partner
                   , suburbanMilage
                   , null as providedFor
                   , repairEndDate
                   , null as techType
                   , towType
                   , towAddress_address
                   , towDealer_partnerId from towagetbl
            union all
            select   id
                   , type
                   , null as towDealer_partner
                   , suburbanMilage
                   , null as providedFor
                   , null as repairEndDate
                   , techType
                   , null as towType
                   , null as towAddress_address
                   , null as towDealer_partnerId from techtbl
            union all
            select   id
                   , type
                   , null as towDealer_partner
                   , null as suburbanMilage
                   , providedFor
                   , null as repairEndDate
                   , null as techType
                   , null as towType
                   , null as towAddress_address
                   , null as towDealer_partnerId from hoteltbl
            ) t
            on t.id = s.id and t.type = s.type
        left outer join actiontbl a
        on s.type || ':' || s.id = a.parentId and a.name = 'orderService'
        left outer join partnertbl p1
        on s.contractor_partnerId = 'partner:' || p1.id
        left outer join partnertbl p2
        on t.towDealer_partnerId = 'partner:' || p2.id
    where c.id::text = substring(s.parentId, ':(.*)');

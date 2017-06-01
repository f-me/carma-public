DROP VIEW IF EXISTS "Услуги";
CREATE VIEW "Услуги" AS
WITH servicecounts AS (
         SELECT servicetbl_1.parentid,
            count(*) AS amount
           FROM servicetbl servicetbl_1
          GROUP BY servicetbl_1.parentid
        ),
     partner_with_priorities AS (
         SELECT
            p.id as id,
            p.name as name,
            svc->>'type' as type,
            svc->>'priority1' as priority1,
            svc->>'priority2' as priority2,
            svc->>'priority3' as priority3
          FROM partnertbl p, json_array_elements(p.services) x(svc)
        ),
     commentLists AS (
         SELECT caseId, array_agg(usermetatbl.login || ' в ' ||
                                  to_char(cc.ctime, 'DD.MM.YYYY HH:MI') ||
                                  ': ' || trim(cc.comment) ORDER BY cc.ctime)
                                  AS txt
         FROM "CaseComment" cc, casetbl, usermetatbl
         WHERE usermetatbl.id = cc.author AND casetbl.id = cc.caseId
         GROUP BY caseId
        ),
     orderActions AS (
         SELECT DISTINCT ON (serviceId) serviceId, assignedTo
         FROM actiontbl
         WHERE result IN (1, 2)
         ORDER BY serviceId, closeTime DESC),
     cities_regions AS (
        SELECT
            unnest(cities) AS city,
            string_agg(label, E'\n') AS regionlist
        FROM "Region" GROUP BY city),
	-- сложности 
	comps (tech_type_id, name1, name2, name3, name4, name5) as (
		select 
			 27
			,'Двери открываются'
			,'Капот открывается'
			,'Есть место для подъезда ТП спереди/ сбоку 6 метров'
			,'Сложность №4'
			,'Сложность №5'
		union all 
		select 
			 28
			,'Автомобиль не посреди дороги/ автомагистрали'
			,'Автомобиль стоит на твердом ровном покрытии, без уклона'	
			,'Есть запасное колесо'	
			,'Секреток нет'	
			,'Есть ключ от секретки'
		union all 
		select 
			 32
			,'Ключ в автомобиле'
			,'Двигатель не заведен'	
			,'Есть дополнительное запирающее устройство капота'	
			,'Есть документы на автомобиль'	
			,'Сложность №5'
		union all 
		select 
			 33
			,'Двери открываются'	
			,'Сложность №2'	
			,'Сложность №3'	
			,'Сложность №4'	
			,'Сложность №5'
		union all 
		select 
			 31
			,'Открывается лючок бензобака'	
			,'Сложность №2'	
			,'Сложность №3'	
			,'Сложность №4'	
			,'Сложность №5'
		union all 
		select 
			 35
			,'Авто на твердой поверхности'	
			,'Авто на колесах'	
			,'Расстояние от авто до дороги меньше 10 метров'	
			,'Сложность №4'	
			,'Сложность №5'
		union all 
		select 
			 34
			,'Высота потолка ниже 2м20 см'	
			,'Сложность №2'	
			,'Сложность №3'	
			,'Сложность №4'	
			,'Сложность №5'
		union all 
		select 
			 37
			,'Сложность №1'	
			,'Сложность №2'	
			,'Сложность №3'	
			,'Сложность №4'	
			,'Сложность №5'
		union all 
		select 
			 36
			,'Топливо вытекает, когда двигатель заведен'	
			,'Топливо вытекает когда двигатель заглушен'	
			,'Сложность №3'	
			,'Сложность №4'	
			,'Сложность №5'
		union all 
		select 
			 41
			,'Наличие запасных частей'	
			,'Сложность №2'	
			,'Сложность №3'	
			,'Сложность №4'	
			,'Сложность №5'
	)
 SELECT
    "PaymentType".label AS "Тип оплаты",
        servicetbl.parentid || COALESCE(('/'::text || rank() OVER (PARTITION BY servicetbl.parentid ORDER BY servicetbl.createtime ASC)) ||
        CASE
            WHEN servicecounts.amount < 2 THEN NULL::text
            ELSE ''::text
        END, ''::text) AS "Номер происшествия",
    '1-RAMC'::text AS "Орг-ия, обрабатывающая происшеств.",
    timezone('Europe/Moscow'::text, casetbl.calldate) AS "Дата и время звонка (МСК)", --"Дата звонка"
    timezone('Europe/Moscow'::text, servicetbl.times_expectedservicestart) AS "Ожид. время нач. оказания услуги",
    case when json_array_length(servicetbl.times_expectedserviceStartHistory) > 0
      then timezone('Europe/Moscow'::text,
        (servicetbl.times_expectedServiceStartHistory->0) :: text :: timestamp at time zone 'UTC')
      else timezone('Europe/Moscow'::text, servicetbl.times_expectedServiceStart)
      end AS "Исходное Ожид. время нач. оказания",
    timezone('Europe/Moscow'::text, servicetbl.createtime) AS "Дата и время создания услуги",
    upper(casetbl.car_platenum) AS "Регистрационный номер автомобиля",
    DATE(timezone('Europe/Moscow'::text, casetbl.car_buydate::timestamp with time zone)) AS "Дата продажи автомобиля",
    upper(casetbl.car_vin) AS "VIN автомобиля",
    upper("Contract".vin) AS "VIN автомобиля(контракт)",
    "ContractCheckStatus".label AS "VIN проверен(Участие в программе)",
    casetbl.car_mileage AS "Пробег автомобиля",
    "CarMake".label AS "Марка автомобиля",
    "CarModel".label AS "Модель автомобиля",
    p3.name AS "Дилер, продавший автомобиль",
    "Wazzup".label AS "Что случилось",
    "System".label AS "Система, где произошла неиспр.",
    "Part".label AS "Неисправная деталь(узел)",
    "Cause".label AS "Описание причины неисправности",
    "CarMake".fdds AS "VEHICLE MAKE",
    --Реализация функции VEHICLEMODEL()
    CASE
        WHEN
                ("CarMake".value = 'ford' OR "CarMake".label = 'Ford') AND "CarModel".fdds IS NULL
                THEN
                        '0900'
        WHEN
                ("CarMake".value = 'chevy' OR "CarMake".label = 'Chevrolet') AND "CarModel".fdds IS NULL
                THEN
                        '1000'
        WHEN
                ("CarMake".value = 'opel' OR "CarMake".label = 'Opel' OR "CarMake".label = 'Vauxhall') AND "CarModel".fdds IS NULL
                THEN
                        '2300'
        WHEN
                ("CarMake".value = 'cad' OR "CarMake".label = 'Cadillac') AND "CarModel".fdds IS NULL
                THEN
                        '1115'
        ELSE
                "CarModel".fdds
    END AS "VEHICLE MODEL",




    --ФУНКЦИЯ FAULTCODE (ЕСЛИ УСЛУГА НЕ ЗАПОЛНЕНА, НО ФОЛТКОДА НЕТУ)
    CASE
                WHEN "ServiceType".fdds IS NOT NULL
                THEN
                concat(
                        CASE
                                WHEN
                                        "Part".fdds IS NULL
                                THEN '150'
                                ELSE "Part".fdds
                        END,
                CASE
                        WHEN
                                "Cause".fdds IS NULL
                        THEN '09'
                        ELSE "Cause".fdds
                END,
     "ServiceType".fdds)::text
                ELSE
                NULL::text
        END AS "FAULTCODE",
    casetbl.temperature AS "Погодные условия",
    casetbl.customerComment AS "Комментарий к кейсу",
    casetbl.caseaddress_address AS "Место поломки",
    casetbl.caseaddress_comment AS "Адрес места поломки/примечания",
    servicetbl.contractor_partner AS "Субподрядчик, оказавший услугу",

    servicetbl.contractor_partner AS "Название партнёра при создании",
    contractor_partner_svc.name AS "Tекущее название партнёра",
    servicetbl.contractor_partnerLegacy->>'priority1' AS "ПН на момент создания услуги",
    servicetbl.contractor_partnerLegacy->>'priority2' AS "ПБГ на момент создания услуги",
    servicetbl.contractor_partnerLegacy->>'priority3' AS "ПБЗ на момент создания услуги",
    contractor_partner_svc.priority1 AS "Текущий приоритет ПН",
    contractor_partner_svc.priority2 AS "Текущий приоритет ПБГ",
    contractor_partner_svc.priority3 AS "Текущий приоритет ПБЗ",

    "ServiceStatus".label AS "Результат оказания помощи",
    "Complication".label AS "Сложный случай",
    coalesce("FalseCall".label, '-') AS "Ложный вызов",
    '-' :: text AS "Выставлен счет за ложный вызов",
    'Обработано'::text AS "Статус обращения(: обработано)",

   "ServiceType".label as "Тип обращения",--"Услуга"
   servicetbl.clientcancelreason AS "Причина отказа клиента",

   allservicesview.towdealer_partner AS "Назначение эвакуации-назв. дилера",
   allservicesview.towdealer_partner AS "Название дилера при создании",
   towdealer_partner_new.name AS "Tекущее название дилера",

   allservicesview.whatToSay1 AS "Описание проблемы",
   "ConsultationType".label AS "Тип консультации",
   "ConsultationResult".label AS "Результат консультации",
   u3.realname AS "Консультант",
   p2.code AS "Код дилера",
   casecity.label AS "Город места поломки",
   dealerTOcity.label AS "Город дилера (где было ТО)",
   towdealercity.label AS "Город ДЦ",
   p4.name AS "Дилер ТО(последний)",
   p4.code AS "Код дилера ТО(последн.)",
   initcap(split_part(casetbl.contact_name, ' '::text, 2)) AS "Имя клиента",
   initcap(split_part(casetbl.contact_name, ' '::text, 1)) AS "Фамилия клиента",
   CASE
            WHEN casetbl.contact_contactowner =true THEN '+'::text
            ELSE '-'::text

   END AS "Владелец",
    CASE
            WHEN servicetbl.warrantycase = true THEN '+'::text
            ELSE '-'::text
    END AS "Гарантийный случай",
    allservicesview.repairenddate AS "Дата окончания ремонта",
    allservicesview.suburbanmilage AS "Пробег эвак-ра/техпом. за городом",
      concat_ws(', '::text,
        SUBSTRING(casetbl.contact_phone1, 1, 2)::text || ' ('::text || SUBSTRING(casetbl.contact_phone1, 3, 3)::text || ') '::text ||  SUBSTRING(casetbl.contact_phone1, 6, 3)::text || ' '::text || SUBSTRING(casetbl.contact_phone1, 9, 2)::text || ' '::text || SUBSTRING(casetbl.contact_phone1, 11, 2)::text || ' '::text,
        SUBSTRING(casetbl.contact_phone2, 1, 2)::text || ' ('::text || SUBSTRING(casetbl.contact_phone2, 3, 3)::text || ') '::text ||  SUBSTRING(casetbl.contact_phone2, 6, 3)::text || ' '::text || SUBSTRING(casetbl.contact_phone2, 9, 2)::text || ' '::text || SUBSTRING(casetbl.contact_phone2, 11, 2)::text || ' '::text,
        SUBSTRING(casetbl.contact_phone3, 1, 2)::text || ' ('::text || SUBSTRING(casetbl.contact_phone3, 3, 3)::text || ') '::text ||  SUBSTRING(casetbl.contact_phone3, 6, 3)::text || ' '::text || SUBSTRING(casetbl.contact_phone3, 9, 2)::text || ' '::text || SUBSTRING(casetbl.contact_phone3, 11, 2)::text || ' '::text,
        SUBSTRING(casetbl.contact_phone4, 1, 2)::text || ' ('::text || SUBSTRING(casetbl.contact_phone4, 3, 3)::text || ') '::text ||  SUBSTRING(casetbl.contact_phone4, 6, 3)::text || ' '::text || SUBSTRING(casetbl.contact_phone4, 9, 2)::text || ' '::text || SUBSTRING(casetbl.contact_phone4, 11, 2)::text || ' '::text)
         AS "Телефоны клиента",
    servicetbl.payment_limitedcost AS "Стоимость для заказчика",
    allservicesview.providedfor AS "Дни(Срок предоставления)",
    servicetbl.contractor_partner AS "Субпод-к, оказ.усл.(как по дог-ру)",
    servicetbl.payment_partnercost AS "Стоимость у партнера (число)",
    servicetbl.payment_costtranscript AS "Расшифровка стоимости",
    servicetbl.payment_paidbyruamc AS "Стоимость со слов партнёра", --"Оплата РАМК",
    usermetatbl.realname AS "Сотрудник, принявший звонок", --"Сотрудник РАМК"
    u2.realName AS "Сотрудник, заказавший услугу", --"Ответственный",
    timezone('Europe/Moscow'::text, servicetbl.times_factservicestart) AS "Время погруз.(Факт. нач. ок. усл.)",
    timezone('Europe/Moscow'::text, servicetbl.times_factserviceend) AS "Время разгр.(Факт.оконч. ок. усл.)",
    "Satisfaction".label AS "Комментарий (Клиент доволен/нет)",
    casetbl.claim AS "Претензии / Благодарность",
    p4.name AS "Дилер прохождение ТО",
    "Contract".validsince AS "Дата прохождения ТО",
    "Contract".startmileage AS "Пробег на последнем ТО",
    casetbl.car_makeyear AS "Год выпуска автомобиля",
    "ProgramType".label AS "Тип программы",
    "Program".label AS "Программа",
    "SubProgram".label AS "Подпрограмма",
    casetbl.car_color AS "Цвет",
    "Transmission".label AS "Коробка передач",
    "Engine".label AS "Тип двигателя",
    casetbl.car_liters AS "Объём двигателя",
    "Contract".checkperiod AS "Межсервиный интервал",
    "CarClass".label AS "Класс автомобиля",
    casetbl.dealercause AS "Причина неисправ. со слов дилера",
    "Contract".cardnumber AS "Номер карты",
    array_to_string(commentLists.txt, E'\n') AS "Комментарии аналитиков",
    p3.code AS "Код дилера, продавшего автомобиль",
    allservicesview.towaddress_address AS "Назначение эвакуации-адрес дилера",
    "Contract".validsince AS "Дата начала действия гарантии",
    "Contract".validuntil AS "Дата окончания действия гарантии",
    --ТО, ЧТО НЕ ВХОДИТ В ОБЩИЙ ОТЧЕТ НА МОМЕНТ (12.05.2014)
    --РЕАЛИЗАЦИЯ ФУНКЦИИ DATEDIFF
        round(EXTRACT(EPOCH FROM(servicetbl.times_factServiceStart-servicetbl.createtime))/60) AS "Время прибытия",
        round(EXTRACT(EPOCH FROM(servicetbl.times_factServiceEnd-servicetbl.times_factservicestart))/60) AS "Время выполнения",
    casetbl.contact_owneremail AS "Email владельца",
    casetbl.contact_email AS "Email звонящего",
    servicetbl.contractor_address AS "Адрес выезда эвакуатора",
    "TowType".label AS "Вид эвакуации",
    COALESCE("TowSort".label, "BikeTowType".label) AS "Тип эвакуации",
    "TowerType".label AS "Тип эвакуатора",
    timezone('Europe/Moscow'::text, servicetbl.times_expecteddispatch) AS "Время выезда партнёра",
     timezone('Europe/Moscow'::text, servicetbl.bill_billingdate) AS "Дата выставления счёта",
    casetbl.repair AS "Дата починки",
    concat_ws(', '::text, casetbl.contact_ownerphone1, casetbl.contact_ownerphone2, casetbl.contact_ownerphone3, casetbl.contact_ownerphone4) AS "Контактные телефоны владельца",
    casetbl.id AS "Номер кейса",
    servicetbl.bill_billnumber AS "Номер счёта",
    casetbl.contact_ownername AS "Имя владельца",
    servicetbl.id AS "Номер услуги",
    u1.realName AS "Сотрудник, создавший услугу",
     p1.code AS "Код партнёра",
    casetbl.caseaddress_coords AS "Координаты места поломки",
    servicetbl.contractor_coords AS "Координаты партнёра",
    "TechType".label AS "Мероприятие в услуге",
    servicetbl.payment_expectedcost AS "Ожидаемая стоимость (число)",
    timezone('Europe/Moscow'::text, servicetbl.times_expectedserviceclosure) AS "Ожидаемое время закрытия услуги",
    timezone('Europe/Moscow'::text, servicetbl.times_expectedserviceend) AS "Ожид. время оконч. оказания услуги",
    timezone('Europe/Moscow'::text, servicetbl.times_expecteddealerinfo) AS "Ожид. время получения информации",
    CASE
        WHEN
                servicetbl.paid = true THEN 'Y'
        ELSE 'N'
    END   AS "Оплата",
    servicetbl.payment_paidbyclient AS "Оплата Клиент",
    servicetbl.original AS "Оригинал получен",
    --РЕАЛИЗАЦИЯ ФУНКЦИИ DATEDIFF
        round(EXTRACT(EPOCH FROM(servicetbl.times_factservicestart-servicetbl.times_expecteddispatch))/60) AS "Нач.оказ.усл-время выезда партнера",
    servicetbl.payment_calculatedcost AS "Расчётная стоимость",
    "Suggestion".label AS "Рекомендация",
    servicetbl.scan AS "Скан загружен",
    "CaseSource".label AS "Источник кейса",
    "CaseStatus".label AS "Статус кейса",
    CASE
        WHEN
                servicetbl.payment_overcosted = true THEN 'Y'
        ELSE 'N'
    END   AS "Стоимость превышена?",
    servicetbl.bill_billingcost AS "Сумма по счёту",
    casetbl.files AS "Файлы, прикрепленные к кейсу",
    servicetbl.files AS "Файлы, прикрепленные к услуге",
    timezone('Europe/Moscow'::text, servicetbl.times_factserviceclosure) AS "Фактическое время закрытия услуги",
    timezone('Europe/Moscow'::text, servicetbl.times_factdealerinfo) AS "Факт. время получения информации",
    casetbl.contact_name AS "ФИО звонящего",

    CASE allservicesview.flags->>'Заблокирован электронный ручной т'
      WHEN 'true'::text THEN 'Y' ELSE 'N' END as "Заблокирован электронный ручной т",
    CASE allservicesview.flags->>'Руль заблокирован'
      WHEN 'true'::text THEN 'Y' ELSE 'N' END as "Руль заблокирован",

    CASE allservicesview.flags->>'Капот открывается'
      WHEN 'true' THEN 'Y' ELSE 'N' END as "Капот открывается",
    CASE allservicesview.flags->>'Наличие запасного колеса'
      WHEN 'true' THEN 'Y' ELSE 'N' END as "Наличие запасного колеса",
    CASE allservicesview.flags->>'Наличие секреток'
      WHEN 'true' THEN 'Y' ELSE 'N' END as "Наличие секреток",
    CASE allservicesview.flags->>'Запасной ключ имеется'
      WHEN 'true' THEN 'Y' ELSE 'N' END as "Запасной ключ имеется",
    CASE allservicesview.flags->>'Документы на автомобиль на руках'
      WHEN 'true' THEN 'Y' ELSE 'N' END as "Документы на автомобиль на руках",
    CASE allservicesview.flags->>'Не открывается лючок бензобака'
      WHEN 'true' THEN 'Y' ELSE 'N' END as "Не открывается лючок бензобака",

    cities_regions.regionlist as "Регион",
	c.name1 as "Сложность_1",
	CASE 	
		WHEN t.type = 27 THEN compl27p1
		WHEN t.type = 28 THEN compl28p1
		WHEN t.type = 32 THEN compl32p1
		WHEN t.type = 33 THEN compl33p1
		WHEN t.type = 31 THEN compl31p1
		WHEN t.type = 35 THEN compl35p1
		WHEN t.type = 34 THEN compl34p1
		WHEN t.type = 37 THEN compl37p1
		WHEN t.type = 36 THEN compl36p1
		WHEN t.type = 41 THEN compl41p1
		ELSE null
	END as  "Сложность_значение_1",
	c.name2 as "Сложность_2",
	CASE 	
		WHEN t.type = 27 THEN compl27p2
		WHEN t.type = 28 THEN compl28p2
		WHEN t.type = 32 THEN compl32p2
		WHEN t.type = 33 THEN compl33p2
		WHEN t.type = 31 THEN compl31p2
		WHEN t.type = 35 THEN compl35p2
		WHEN t.type = 34 THEN compl34p2
		WHEN t.type = 37 THEN compl37p2
		WHEN t.type = 36 THEN compl36p2
		WHEN t.type = 41 THEN compl41p2
		ELSE null
	END as  "Сложность_значение_2",	
	c.name3 as "Сложность_3",
	CASE 	
		WHEN t.type = 27 THEN compl27p3
		WHEN t.type = 28 THEN compl28p3
		WHEN t.type = 32 THEN compl32p3
		WHEN t.type = 33 THEN compl33p3
		WHEN t.type = 31 THEN compl31p3
		WHEN t.type = 35 THEN compl35p3
		WHEN t.type = 34 THEN compl34p3
		WHEN t.type = 37 THEN compl37p3
		WHEN t.type = 36 THEN compl36p3
		WHEN t.type = 41 THEN compl41p3
		ELSE null
	END as  "Сложность_значение_3",
	c.name4 as "Сложность_4",
	CASE 	
		WHEN t.type = 27 THEN compl27p4
		WHEN t.type = 28 THEN compl28p4
		WHEN t.type = 32 THEN compl32p4
		WHEN t.type = 33 THEN compl33p4
		WHEN t.type = 31 THEN compl31p4
		WHEN t.type = 35 THEN compl35p4
		WHEN t.type = 34 THEN compl34p4
		WHEN t.type = 37 THEN compl37p4
		WHEN t.type = 36 THEN compl36p4
		WHEN t.type = 41 THEN compl41p4
		ELSE null
	 END as  "Сложность_значение_4",
	c.name5 as "Сложность_5",
	CASE 	
		WHEN t.type = 27 THEN compl27p5
		WHEN t.type = 28 THEN compl28p5
		WHEN t.type = 32 THEN compl32p5
		WHEN t.type = 33 THEN compl33p5
		WHEN t.type = 31 THEN compl31p5
		WHEN t.type = 35 THEN compl35p5
		WHEN t.type = 34 THEN compl34p5
		WHEN t.type = 37 THEN compl37p5
		WHEN t.type = 36 THEN compl36p5
		WHEN t.type = 41 THEN compl41p5
		ELSE null
	 END as  "Сложность_значение_5"
   FROM casetbl
   LEFT JOIN commentLists ON casetbl.id = commentLists.caseId
   LEFT JOIN usermetatbl ON casetbl.callTaker = usermetatbl.id
   LEFT JOIN "Program" ON casetbl.program = "Program".id
   LEFT JOIN "ProgramType" ON "Program".ptype = "ProgramType".id
   LEFT JOIN "SubProgram" ON casetbl.subprogram = "SubProgram".id
   LEFT JOIN "CarClass" ON casetbl.car_class = "CarClass".id
   LEFT JOIN "Engine" ON casetbl.car_engine = "Engine".id
   LEFT JOIN "Transmission" ON casetbl.car_transmission = "Transmission".id
   LEFT JOIN partnertbl p3 ON casetbl.car_seller = p3.id
   LEFT JOIN partnertbl p4 ON casetbl.car_dealerto = p4.id
   LEFT JOIN "City" casecity ON casetbl.city = casecity.id
   LEFT JOIN cities_regions ON casetbl.city = cities_regions.city
   LEFT JOIN "City" dealerTOcity ON p4.city = dealerTOcity.id
   LEFT JOIN "CarMake" ON casetbl.car_make = "CarMake".id
   LEFT JOIN "CarModel" ON casetbl.car_model = "CarModel".id
   LEFT JOIN "Contract" ON casetbl.contract = "Contract".id
   LEFT JOIN "Wazzup" ON casetbl.comment = "Wazzup".id
   LEFT JOIN "System" ON casetbl.diagnosis1 = "System".id
   LEFT JOIN "Part" ON casetbl.diagnosis2 = "Part".id
   LEFT JOIN "Cause" ON casetbl.diagnosis3 = "Cause".id
   LEFT JOIN "Suggestion" ON casetbl.diagnosis4 = "Suggestion".id
   LEFT JOIN "CaseSource" ON casetbl.source = "CaseSource".id
   LEFT JOIN "CaseStatus" ON casetbl.caseStatus = "CaseStatus".id
   LEFT JOIN "ContractCheckStatus" ON casetbl.vinchecked = "ContractCheckStatus".id,
   servicetbl
   LEFT JOIN allservicesview ON allservicesview.id = servicetbl.id AND servicetbl.parentid = allservicesview.parentid
   LEFT JOIN partnertbl p1 ON servicetbl.contractor_partnerid = p1.id
   LEFT JOIN partnertbl p2 ON allservicesview.towdealer_partnerid = p2.id
   LEFT JOIN partnertbl towdealer_partner_new ON allservicesview.towdealer_partnerid = towdealer_partner_new.id
   LEFT JOIN partner_with_priorities contractor_partner_svc
    ON (servicetbl.contractor_partnerid = contractor_partner_svc.id AND servicetbl.type::text = contractor_partner_svc.type)
   LEFT JOIN "City" towdealercity ON p2.city = towdealercity.id
   LEFT JOIN "ConsultationResult" ON allservicesview.consResult = "ConsultationResult".id
   LEFT JOIN "ConsultationType" ON allservicesview.consType = "ConsultationType".id
   LEFT JOIN "TowType" ON allservicesview.towType = "TowType".id

   LEFT JOIN "BikeTowType" ON allservicesview.bikeTowType = "BikeTowType".id
   LEFT JOIN "TowSort" ON allservicesview.towSort = "TowSort".id

   LEFT JOIN "TowerType" ON allservicesview.towerType = "TowerType".id
   LEFT JOIN servicecounts ON servicetbl.parentid = servicecounts.parentid
   LEFT JOIN "Complication" ON servicetbl.complication = "Complication".id
   LEFT JOIN "FalseCall" ON servicetbl.falseCall = "FalseCall".id
   LEFT JOIN "ServiceType" ON servicetbl.type = "ServiceType".id
   LEFT JOIN "PaymentType" ON servicetbl.paytype = "PaymentType".id
   LEFT JOIN "ServiceStatus" ON servicetbl.status = "ServiceStatus".id
   LEFT JOIN "Satisfaction" ON servicetbl.clientsatisfied = "Satisfaction".id
   LEFT JOIN orderActions ON servicetbl.id = orderActions.serviceId
   LEFT JOIN usermetatbl u1 ON u1.id = servicetbl.creator
   LEFT JOIN usermetatbl u2 ON u2.id = orderActions.assignedTo
   LEFT JOIN usermetatbl u3 ON u3.id = allservicesview.consultant
   LEFT JOIN "TechType" ON "TechType".id = allservicesview.techtype
   LEFT JOIN techtbl t ON t.id = servicetbl.id AND t.parentid = servicetbl.parentid
   LEFT JOIN comps c on t.type = c.tech_type_id
WHERE casetbl.id = servicetbl.parentid;

GRANT SELECT ON "Услуги" TO reportgen;

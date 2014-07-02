CREATE VIEW "Услуги" AS
WITH servicecounts AS (
         SELECT servicetbl_1.parentid,
            count(*) AS amount
           FROM servicetbl servicetbl_1
          GROUP BY servicetbl_1.parentid
        )
 SELECT 
    "PaymentType".label AS "Тип оплаты",
        "substring"(servicetbl.parentid, 6) || COALESCE(('/'::text || rank() OVER (PARTITION BY servicetbl.parentid ORDER BY servicetbl.createtime ASC)) ||
        CASE
            WHEN servicecounts.amount < 2 THEN NULL::text
            ELSE ''::text
        END, ''::text) AS "Номер происшествия",
    '1-RAMC'::text AS "Орг-ия, обрабатывающая происшеств.",
    timezone('Europe/Moscow'::text, casetbl.calldate) AS "Дата и время звонка (МСК)", --"Дата звонка"
    timezone('Europe/Moscow'::text, servicetbl.times_expectedservicestart) AS "Ожид. время нач. оказания услуги",
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
		WHEN "ServiceNames".fdds IS NOT NULL
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
     "ServiceNames".fdds)::text 
		ELSE
		NULL::text
	END AS "FAULTCODE", 
    casetbl.temperature AS "Погодные условия",
    casetbl.customerComment AS "Комментарий к кейсу",
    casetbl.caseaddress_address AS "Место поломки",
    casetbl.caseaddress_comment AS "Адрес места поломки/примечания",
    servicetbl.contractor_partner AS "Субподрядчик, оказавший услугу",
    "ServiceStatus".label AS "Результат оказания помощи",
    CASE
            WHEN servicetbl.falsecall = 'bill'::text OR servicetbl.falsecall = 'nobill'::text THEN 'Y'::text
            ELSE 'N'::text
        END AS "Ложный вызов",
        CASE
            WHEN servicetbl.falsecall = 'bill'::text THEN 'Y'::text
            ELSE 'N'::text
        END AS "Выставлен счет за ложный вызов",
        'Обработано'::text AS "Статус обращения(: обработано)",
        
            "ServiceNames".label as "Тип обращения",--"Услуга"
   allservicesview.towdealer_partner AS "Назначение эвакуации-назв. дилера",
   p2.code AS "Код дилера",
   casecity.label AS "Город места поломки",
   dealercity.label AS "Город дилера (куда эвакуируют)",
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
	SUBSTRING(casetbl.contact_phone1, 1, 2)::text || ' ('::text || SUBSTRING(casetbl.contact_phone1, 3, 3)::text || ') '::text ||  SUBSTRING(casetbl.contact_phone1, 6, 3)::text || ' '::text || SUBSTRING(casetbl.contact_phone1, 8, 2)::text || ' '::text || SUBSTRING(casetbl.contact_phone1, 10, 2)::text || ' '::text, 
	SUBSTRING(casetbl.contact_phone2, 1, 2)::text || ' ('::text || SUBSTRING(casetbl.contact_phone2, 3, 3)::text || ') '::text ||  SUBSTRING(casetbl.contact_phone2, 6, 3)::text || ' '::text || SUBSTRING(casetbl.contact_phone2, 8, 2)::text || ' '::text || SUBSTRING(casetbl.contact_phone2, 10, 2)::text || ' '::text, 
	SUBSTRING(casetbl.contact_phone3, 1, 2)::text || ' ('::text || SUBSTRING(casetbl.contact_phone3, 3, 3)::text || ') '::text ||  SUBSTRING(casetbl.contact_phone3, 6, 3)::text || ' '::text || SUBSTRING(casetbl.contact_phone3, 8, 2)::text || ' '::text || SUBSTRING(casetbl.contact_phone3, 10, 2)::text || ' '::text, 
	SUBSTRING(casetbl.contact_phone4, 1, 2)::text || ' ('::text || SUBSTRING(casetbl.contact_phone4, 3, 3)::text || ') '::text ||  SUBSTRING(casetbl.contact_phone4, 6, 3)::text || ' '::text || SUBSTRING(casetbl.contact_phone4, 8, 2)::text || ' '::text || SUBSTRING(casetbl.contact_phone4, 10, 2)::text || ' '::text)
	 AS "Телефоны клиента",
    servicetbl.payment_limitedcost AS "Стоимость для заказчика",
    allservicesview.providedfor AS "Дни(Срок предоставления)",
    servicetbl.contractor_partner AS "Субпод-к, оказ.усл.(как по дог-ру)",
    servicetbl.payment_partnercost AS "Стоимость у партнера (число)",
    servicetbl.payment_costtranscript AS "Расшифровка стоимости",
    servicetbl.payment_paidbyruamc AS "Стоимость со слов партнёра", --"Оплата РАМК",
    casetbl.calltaker AS "Сотрудник, принявший звонок", --"Сотрудник РАМК"
    allservicesview.assignedto AS "Сотрудник, заказавший услугу", --"Ответственный",
    timezone('Europe/Moscow'::text, servicetbl.times_factservicestart) AS "Время погруз.(Факт. нач. ок. усл.)",
    timezone('Europe/Moscow'::text, servicetbl.times_factserviceend) AS "Время разгр.(Факт.оконч. ок. усл.)",
    CASE 
	WHEN 
		servicetbl.clientsatisfied = 'satis' THEN 'Доволен'
	WHEN 
		servicetbl.clientsatisfied = 'notSatis' THEN 'Недоволен'
	ELSE
		servicetbl.clientsatisfied
    END    AS "Комментарий (Клиент доволен/нет)",
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
    casetbl.comments AS "Комментарии аналитиков",
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
    allservicesview.towtype AS "Вид эвакуации",
    timezone('Europe/Moscow'::text, servicetbl.times_expecteddispatch) AS "Время выезда партнёра",
     timezone('Europe/Moscow'::text, servicetbl.bill_billingdate) AS "Дата выставления счёта",
    casetbl.repair AS "Дата починки",
    casetbl.actions AS "Действия",
    concat_ws(', '::text, casetbl.contact_ownerphone1, casetbl.contact_ownerphone2, casetbl.contact_ownerphone3, casetbl.contact_ownerphone4) AS "Контактные телефоны владельца",
    casetbl.id AS "Номер кейса",
    servicetbl.bill_billnumber AS "Номер счёта",
    casetbl.contact_ownername AS "Имя владельца",
    servicetbl.id AS "Номер услуги",
     p1.code AS "Код партнёра",
    casetbl.caseaddress_coords AS "Координаты места поломки",
    servicetbl.contractor_coords AS "Координаты партнёра",
     allservicesview.techtype AS "Мероприятие в услуге",
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
    "CaseStatus".label AS "Статус кейса",
    CASE 
	WHEN 
		servicetbl.payment_overcosted = true THEN 'Y'
	ELSE 'N' 
    END   AS "Стоимость превышена?",  
    servicetbl.bill_billingcost AS "Сумма по счёту", 
    casetbl.services AS "Услуги",    
    casetbl.files AS "Файлы, прикрепленные к кейсу",
    servicetbl.files AS "Файлы, прикрепленные к услуге",
    timezone('Europe/Moscow'::text, servicetbl.times_factserviceclosure) AS "Фактическое время закрытия услуги",
    timezone('Europe/Moscow'::text, servicetbl.times_factdealerinfo) AS "Факт. время получения информации",
    casetbl.contact_name AS "ФИО звонящего"   
    
   FROM casetbl
   LEFT JOIN "Program" ON casetbl.program = "Program".id
   LEFT JOIN "ProgramType" ON "Program".ptype = "ProgramType".id
   LEFT JOIN "SubProgram" ON casetbl.subprogram = "SubProgram".id
   LEFT JOIN "CarClass" ON casetbl.car_class = "CarClass".id
   LEFT JOIN "Engine" ON casetbl.car_engine = "Engine".id
   LEFT JOIN "Transmission" ON casetbl.car_transmission = "Transmission".id
   LEFT JOIN partnertbl p3 ON casetbl.car_seller = p3.id::text
   LEFT JOIN partnertbl p4 ON casetbl.car_dealerto = p4.id::text
   LEFT JOIN "City" casecity ON casetbl.city = casecity.value
   LEFT JOIN "City" dealercity ON p4.city = dealercity.value
   LEFT JOIN "CarMake" ON casetbl.car_make = "CarMake".id
   LEFT JOIN "CarModel" ON casetbl.car_model = "CarModel".id
   LEFT JOIN "Contract" ON casetbl.contract = "Contract".id
   LEFT JOIN "Wazzup" ON casetbl.comment = "Wazzup".id
   LEFT JOIN "System" ON casetbl.diagnosis1 = "System".id
   LEFT JOIN "Part" ON casetbl.diagnosis2 = "Part".id
   LEFT JOIN "Cause" ON casetbl.diagnosis3 = "Cause".id
   LEFT JOIN "Suggestion" ON casetbl.diagnosis4 = "Suggestion".id
   LEFT JOIN "CaseStatus" ON casetbl.caseStatus = "CaseStatus".id
   LEFT JOIN "ContractCheckStatus" ON casetbl.vinchecked = "ContractCheckStatus".id,
   servicetbl
   LEFT JOIN allservicesview ON allservicesview.id = servicetbl.id AND allservicesview.type = servicetbl.type  AND servicetbl.parentid = allservicesview.parentid
   LEFT JOIN partnertbl p1 ON servicetbl.contractor_partnerid = ('partner:'::text || p1.id) 
   LEFT JOIN partnertbl p2 ON allservicesview.towdealer_partnerid = ('partner:'::text || p2.id)
   LEFT JOIN servicecounts ON servicetbl.parentid = servicecounts.parentid
   LEFT JOIN "ServiceNames" ON servicetbl.type = "ServiceNames".value
   LEFT JOIN "PaymentType" ON servicetbl.paytype = "PaymentType".id
   LEFT JOIN "ServiceStatus" ON servicetbl.status = "ServiceStatus".id
WHERE casetbl.id = split_part(servicetbl.parentid, ':'::text, 2)::integer;

GRANT SELECT ON "Услуги" TO reportgen;
GRANT ALL ON "Услуги" TO analyst;

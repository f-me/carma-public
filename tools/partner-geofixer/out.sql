DROP TABLE IF EXISTS geo_partners;
CREATE TABLE geo_partners (id INTEGER PRIMARY KEY, name TEXT, city TEXT, address TEXT);
SELECT AddGeometryColumn ('geo_partners','coords',4326,'POINT',2);
INSERT INTO geo_partners VALUES (12, 'Абсолют-авто', 'Брянск', 'Ленина проспект, 8', ST_PointFromText('POINT(34.356643 53.237041)', 4326));
INSERT INTO geo_partners VALUES (13, 'Лидер-2', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (16, 'Август ООО', 'Самара', '', ST_PointFromText('POINT(50.191184 53.205226)', 4326));
INSERT INTO geo_partners VALUES (18, 'Авиакар', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (19, 'АВИК', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (20, 'Авилон', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (21, 'Авилон АГ', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (23, 'Авто Алеа', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (24, 'АВТО ГАНЗА', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (25, 'АВТО ЗИГЕР', 'Архангельск', '', ST_PointFromText('POINT(40.551776 64.545818)', 4326));
INSERT INTO geo_partners VALUES (27, 'Авто Эвакуация ', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (31, 'Авто-Дина', 'Тюмень', '', ST_PointFromText('POINT(65.558412 57.182631)', 4326));
INSERT INTO geo_partners VALUES (32, 'Авто-друг', 'Казань', 'улица Садовая, 3', ST_PointFromText('POINT(49.156315 55.773611)', 4326));
INSERT INTO geo_partners VALUES (36, 'АВТО-СТИМУЛ', 'Омск', '', ST_PointFromText('POINT(73.365535 54.990215)', 4326));
INSERT INTO geo_partners VALUES (39, 'Авто+', 'Минеральные воды', 'станция Минеральные Воды', ST_PointFromText('POINT(43.138717 44.214233)', 4326));
INSERT INTO geo_partners VALUES (40, 'АВТОАНГЕЛ', 'Липецк', 'улица Фрунзе, 5', ST_PointFromText('POINT(39.594019 52.602772)', 4326));
INSERT INTO geo_partners VALUES (49, 'Автодор', 'Самара', '', ST_PointFromText('POINT(50.191184 53.205226)', 4326));
INSERT INTO geo_partners VALUES (50, 'Автодоставка', 'Томск', '', ST_PointFromText('POINT(84.972128 56.495116)', 4326));
INSERT INTO geo_partners VALUES (52, 'Автокей', 'Сыктывкар', '', ST_PointFromText('POINT(50.836920 61.668865)', 4326));
INSERT INTO geo_partners VALUES (55, 'АВТОКЛУБ', 'Чебоксары', '', ST_PointFromText('POINT(47.235484 56.135459)', 4326));
INSERT INTO geo_partners VALUES (61, 'Автомастер СПБ', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (62, 'АвтоМастер Сыктывкар', 'Сыктывкар', '', ST_PointFromText('POINT(50.836920 61.668865)', 4326));
INSERT INTO geo_partners VALUES (69, 'АВТОПАРТНЕР', 'Москва', 'Привольная улица, 2с4', ST_PointFromText('POINT(37.840489 55.703718)', 4326));
INSERT INTO geo_partners VALUES (70, 'Автопартнер Орск', 'Орск', '', ST_PointFromText('POINT(58.556682 51.232007)', 4326));
INSERT INTO geo_partners VALUES (78, 'Автосос', 'Москва', 'Лужнецкая набережная, 2/4с23б', ST_PointFromText('POINT(37.571120 55.715175)', 4326));
INSERT INTO geo_partners VALUES (80, 'Автоспас ДСС', 'Барнаул', 'улица Малахова, 169', ST_PointFromText('POINT(83.698586 53.330531)', 4326));
INSERT INTO geo_partners VALUES (81, 'Автоспасатель', 'Хабаровск', '', ST_PointFromText('POINT(135.056475 48.460732)', 4326));
INSERT INTO geo_partners VALUES (82, 'Лоджик Сити', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (85, 'АвтоСТОП', 'КРАСНОДАР', 'улица Воровского, 15', ST_PointFromText('POINT(38.951768 45.042801)', 4326));
INSERT INTO geo_partners VALUES (87, 'Автотранссервис 62', 'Рязань', '', ST_PointFromText('POINT(39.744954 54.619886)', 4326));
INSERT INTO geo_partners VALUES (96, 'Автоштадт Нева', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (98, 'Автоюрцентр 45', 'Курган', 'Половинская улица, 10а', ST_PointFromText('POINT(65.378479 55.447183)', 4326));
INSERT INTO geo_partners VALUES (108, 'Акцент-Н', 'Пермь', '', ST_PointFromText('POINT(56.237654 58.004785)', 4326));
INSERT INTO geo_partners VALUES (109, 'Алакс', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (110, 'Аларм-моторс Лахта', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (111, 'Аларм-моторс Озерки', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (112, 'Аларм-моторс Юго-Запад', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (122, 'АМУР ООО', 'Екатеринбург', 'улица Монтажников, 2б', ST_PointFromText('POINT(60.541339 56.881709)', 4326));
INSERT INTO geo_partners VALUES (123, 'Ангел-Сервис', 'Тольятти', '', ST_PointFromText('POINT(49.418084 53.511311)', 4326));
INSERT INTO geo_partners VALUES (124, 'Антонина-Сервис', 'город Курск', 'улица Энгельса, 173', ST_PointFromText('POINT(36.161799 51.702997)', 4326));
INSERT INTO geo_partners VALUES (130, 'Артан', 'Нижний Новгород', '', ST_PointFromText('POINT(44.002672 56.324117)', 4326));
INSERT INTO geo_partners VALUES (136, 'Ассистанская компания "ЛАТ"', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (137, 'АССМС', 'Тольятти', '', ST_PointFromText('POINT(49.418084 53.511311)', 4326));
INSERT INTO geo_partners VALUES (138, 'АСТ Моторс', 'Москва', 'Волоколамское шоссе, 88с4', ST_PointFromText('POINT(37.443955 55.824546)', 4326));
INSERT INTO geo_partners VALUES (139, 'АСТ-54', 'Новосибирск', '', ST_PointFromText('POINT(82.906928 55.028739)', 4326));
INSERT INTO geo_partners VALUES (143, 'Автотранспортное предприятие-1', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (166, 'БуксирОфф', 'Москва', 'Поморская улица, 5', ST_PointFromText('POINT(37.577328 55.869046)', 4326));
INSERT INTO geo_partners VALUES (170, 'Ваш Эвакуатор', 'Новокузнецк', '', ST_PointFromText('POINT(87.155205 53.786502)', 4326));
INSERT INTO geo_partners VALUES (175, 'Веснин', 'Пермь', '', ST_PointFromText('POINT(56.237654 58.004785)', 4326));
INSERT INTO geo_partners VALUES (189, 'ВостокМоторс 35', 'Череповец', '', ST_PointFromText('POINT(37.861923 59.125138)', 4326));
INSERT INTO geo_partners VALUES (191, 'ВТС 24', 'КРАСНОДАР', 'улица Ставропольская, 199', ST_PointFromText('POINT(39.045049 45.016206)', 4326));
INSERT INTO geo_partners VALUES (200, 'Гильдия перевозчиков Санкт-Петербурга', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (204, 'Глобал Транс', 'Тольятти', '', ST_PointFromText('POINT(49.418084 53.511311)', 4326));
INSERT INTO geo_partners VALUES (216, 'Гринкар ООО', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (217, 'Грузовое такси 771-774', 'Ижевск', 'Воткинское шоссе, 162', ST_PointFromText('POINT(53.269225 56.890226)', 4326));
INSERT INTO geo_partners VALUES (234, 'Дженсер-Калуга', 'Калуга', '', ST_PointFromText('POINT(36.237041 54.533832)', 4326));
INSERT INTO geo_partners VALUES (237, 'Дина-Автотрейд', 'Тюмень', '', ST_PointFromText('POINT(65.558412 57.182631)', 4326));
INSERT INTO geo_partners VALUES (239, 'Дорожная служба спасения', 'Самара', '', ST_PointFromText('POINT(50.191184 53.205226)', 4326));
INSERT INTO geo_partners VALUES (241, 'Дорожный Ангел', 'Владимир', 'улица Соколова-Соколенка, 7а', ST_PointFromText('POINT(40.454335 56.161865)', 4326));
INSERT INTO geo_partners VALUES (243, 'ДорСпас', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (244, 'ИП Архипцев К.В.', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (248, 'Еврокар-Челябинск-Плюс', 'Челябинск', '', ST_PointFromText('POINT(61.387103 55.152161)', 4326));
INSERT INTO geo_partners VALUES (249, 'Еврокарс', 'Псков', '', ST_PointFromText('POINT(28.332855 57.819264)', 4326));
INSERT INTO geo_partners VALUES (250, 'ЕвроМобилРус', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (272, 'ИнтерТехЦентр', 'Сургут', '', ST_PointFromText('POINT(73.395952 61.253827)', 4326));
INSERT INTO geo_partners VALUES (275, 'ИП Агаларян Рафаэл Викторович', 'Астрахань', 'улица Татищева, 33', ST_PointFromText('POINT(48.050768 46.366435)', 4326));
INSERT INTO geo_partners VALUES (276, 'ИП Аксенова П.Г.', 'Минеральные Воды', '', ST_PointFromText('POINT(43.137810 44.199539)', 4326));
INSERT INTO geo_partners VALUES (278, 'ИП Артемова Е.Н.', 'Нижний Новгород', '', ST_PointFromText('POINT(44.002672 56.324117)', 4326));
INSERT INTO geo_partners VALUES (280, 'ИП Ахмедов А.К.', 'Набережные Челны', '', ST_PointFromText('POINT(52.407911 55.741678)', 4326));
INSERT INTO geo_partners VALUES (281, 'ИП Ахметшин Р.Р.', 'Стерлитамак', '', ST_PointFromText('POINT(55.962069 53.651622)', 4326));
INSERT INTO geo_partners VALUES (283, 'ИП Булдыгина Е.А.', 'Коломна', 'улица Ленина, 99', ST_PointFromText('POINT(38.769275 55.086471)', 4326));
INSERT INTO geo_partners VALUES (286, 'ИП Великанов Денис Сергеевич', 'Магнитогорск', 'улица Калмыкова, 14', ST_PointFromText('POINT(58.999569 53.342850)', 4326));
INSERT INTO geo_partners VALUES (287, 'ИП Вербицкий Олег Александрович', 'Петрозаводск', '', ST_PointFromText('POINT(34.364961 61.787586)', 4326));
INSERT INTO geo_partners VALUES (290, 'ИП Гаврилова Анна Николаевна', 'Калуга', 'улица Спартака, 9', ST_PointFromText('POINT(36.210838 54.493404)', 4326));
INSERT INTO geo_partners VALUES (291, 'ИП Ганиева Г.И.', 'город Альметьевск', 'улица Ленина, 115', ST_PointFromText('POINT(52.271601 54.897200)', 4326));
INSERT INTO geo_partners VALUES (292, 'ИП Голубков Сергей Михайлович', 'Ярославль', '', ST_PointFromText('POINT(39.887714 57.622506)', 4326));
INSERT INTO geo_partners VALUES (293, 'ИП Горбачева О.А.', 'Смоленск', '', ST_PointFromText('POINT(31.999095 54.779800)', 4326));
INSERT INTO geo_partners VALUES (294, 'ИП Гринкевич Станислав Генрихович', 'Екатеринбург', 'улица Таганская, 77', ST_PointFromText('POINT(60.629715 56.906320)', 4326));
INSERT INTO geo_partners VALUES (295, 'ИП Гритчина М.Н.', 'город Елец', 'улица Королёва, 25', ST_PointFromText('POINT(38.477997 52.616202)', 4326));
INSERT INTO geo_partners VALUES (296, 'ИП Девяткин Андрей Владимирович', 'Великий Новгород', '', ST_PointFromText('POINT(31.270669 58.522580)', 4326));
INSERT INTO geo_partners VALUES (297, 'ИП Дойников С.К.', 'Сочи', '', ST_PointFromText('POINT(39.722271 43.582795)', 4326));
INSERT INTO geo_partners VALUES (298, 'ИП Донских С.И.', 'Воронеж', 'Московский проспект, 149а', ST_PointFromText('POINT(39.178799 51.734165)', 4326));
INSERT INTO geo_partners VALUES (299, 'ИП Донченко А.В.', 'Сургут', '', ST_PointFromText('POINT(73.395952 61.253827)', 4326));
INSERT INTO geo_partners VALUES (300, 'ИП Евдокимов И.В.', 'Саратов', '', ST_PointFromText('POINT(46.004549 51.537652)', 4326));
INSERT INTO geo_partners VALUES (301, 'ИП Жадан О.Л.', 'город Калининград', 'улица Олега Кошевого, 20-24', ST_PointFromText('POINT(20.510999 54.673898)', 4326));
INSERT INTO geo_partners VALUES (303, 'ИП Журавлев Андрей Евгеньевич', 'Новороссийск', '', ST_PointFromText('POINT(37.776493 44.720120)', 4326));
INSERT INTO geo_partners VALUES (304, 'ИП Зиннятуллин', 'город Нижний Новгород', 'улица Володарского, 30а', ST_PointFromText('POINT(44.004504 56.318002)', 4326));
INSERT INTO geo_partners VALUES (305, 'ИП Ильенко В.А.', 'Орел', '', ST_PointFromText('POINT(36.083061 52.968153)', 4326));
INSERT INTO geo_partners VALUES (306, 'ИП Илюхин А.Н.', 'Тула', '', ST_PointFromText('POINT(37.619028 54.193802)', 4326));
INSERT INTO geo_partners VALUES (307, 'ИП Кабатов А.В.', 'город Белгород', 'проспект Б. Хмельницкого, 77а', ST_PointFromText('POINT(36.578796 50.597198)', 4326));
INSERT INTO geo_partners VALUES (308, 'ИП Камнев С.В.', 'Пенза', '', ST_PointFromText('POINT(45.020121 53.199449)', 4326));
INSERT INTO geo_partners VALUES (310, 'ИП Каримов М.М.', 'Нижнекамск', '', ST_PointFromText('POINT(51.861152 55.645532)', 4326));
INSERT INTO geo_partners VALUES (313, 'ИП Колчин Д.С.', 'Таганрог', '', ST_PointFromText('POINT(38.919626 47.211477)', 4326));
INSERT INTO geo_partners VALUES (314, 'ИП Комаров А.А.', 'Оренбург', '', ST_PointFromText('POINT(55.097449 51.768060)', 4326));
INSERT INTO geo_partners VALUES (315, 'ИП Кондратенко Светлана Ивановна', 'Тверь', '', ST_PointFromText('POINT(35.904539 56.860477)', 4326));
INSERT INTO geo_partners VALUES (316, 'ИП Константинов А.А.', 'Великий Новгород', 'Парковая улица, 19', ST_PointFromText('POINT(31.296469 58.545599)', 4326));
INSERT INTO geo_partners VALUES (318, 'ИП Криволапова А.В.', 'Ставрополь', '', ST_PointFromText('POINT(41.965167 45.042935)', 4326));
INSERT INTO geo_partners VALUES (319, 'ИП Куклин В.А.', 'Киров', 'улица Космонавта Владислава Волкова, 5', ST_PointFromText('POINT(49.592834 58.592332)', 4326));
INSERT INTO geo_partners VALUES (320, 'ИП Кулькова Ксения Борисовна', 'Пермь', '', ST_PointFromText('POINT(56.237654 58.004785)', 4326));
INSERT INTO geo_partners VALUES (321, 'ИП Курихина Ю.Л.', 'Котлас', 'Гвардейская улица, 61', ST_PointFromText('POINT(46.677702 61.262199)', 4326));
INSERT INTO geo_partners VALUES (322, 'ИП Кутузов В.В.', 'Ульяновск', '', ST_PointFromText('POINT(48.412160 54.306953)', 4326));
INSERT INTO geo_partners VALUES (323, 'ИП КФХ Мухаметгалиев Ришат Рауфович', 'Уфа', '', ST_PointFromText('POINT(55.983161 54.738437)', 4326));
INSERT INTO geo_partners VALUES (325, 'ИП Лапин А.П.', 'Нижний Новгород', '', ST_PointFromText('POINT(44.002672 56.324117)', 4326));
INSERT INTO geo_partners VALUES (326, 'ИП Логинов О.А.', 'Ульяновск', '', ST_PointFromText('POINT(48.412160 54.306953)', 4326));
INSERT INTO geo_partners VALUES (327, 'ИП Макаров В.М.', 'Смоленск', '', ST_PointFromText('POINT(31.999095 54.779800)', 4326));
INSERT INTO geo_partners VALUES (328, 'ИП Матвеева Е.А.', 'Великий Новгород', 'Парковая улица, 18к4', ST_PointFromText('POINT(31.299613 58.543311)', 4326));
INSERT INTO geo_partners VALUES (330, 'ИП Морковкина С.А.', 'Иваново', 'улица Тимирязева', ST_PointFromText('POINT(41.093801 56.944400)', 4326));
INSERT INTO geo_partners VALUES (331, 'ИП Морозов Р.В.', 'Томск', '', ST_PointFromText('POINT(84.972128 56.495116)', 4326));
INSERT INTO geo_partners VALUES (332, 'ИП Мосин В.Ю.', 'Нижний Новгород', '', ST_PointFromText('POINT(44.002672 56.324117)', 4326));
INSERT INTO geo_partners VALUES (333, 'ИП Москалев В.С.', 'Волгоград', '', ST_PointFromText('POINT(44.515942 48.707793)', 4326));
INSERT INTO geo_partners VALUES (334, 'ИП Москалев П.С.', 'Волгоград', 'Запорожская улица, 4', ST_PointFromText('POINT(44.447958 48.671037)', 4326));
INSERT INTO geo_partners VALUES (335, 'ИП Мусина С.А.', 'Альметьевск', 'улица Рината Галеева', ST_PointFromText('POINT(52.395497 54.883799)', 4326));
INSERT INTO geo_partners VALUES (337, 'ИП Наумова Н.И.', 'Тверь', '', ST_PointFromText('POINT(35.904539 56.860477)', 4326));
INSERT INTO geo_partners VALUES (338, 'ИП Новоселов И.В.', 'Йошкар-Ола', 'Красноармейская улица, 84а', ST_PointFromText('POINT(47.871159 56.645891)', 4326));
INSERT INTO geo_partners VALUES (340, 'ИП Ошкин О.Г.', 'город Белгород', 'улица Полевая', ST_PointFromText('POINT(36.645703 50.603602)', 4326));
INSERT INTO geo_partners VALUES (341, 'ИП Павлюк М.Н.', 'Петрозаводск', 'бульвар Интернационалистов, 15', ST_PointFromText('POINT(34.313425 61.762214)', 4326));
INSERT INTO geo_partners VALUES (342, 'ИП Паличев И.О.', 'Екатеринбург', 'улица Восточная, 230', ST_PointFromText('POINT(60.639237 56.820617)', 4326));
INSERT INTO geo_partners VALUES (345, 'ИП Пономарев Д.С.', 'Саратов', '', ST_PointFromText('POINT(46.004549 51.537652)', 4326));
INSERT INTO geo_partners VALUES (346, 'ИП Прищепов А.А.', 'Красноярск', 'улица Заводская, 6', ST_PointFromText('POINT(92.843660 56.021427)', 4326));
INSERT INTO geo_partners VALUES (347, 'ИП Ревуненков И.А.', 'город Курск', 'улица Бойцов 9 Дивизии', ST_PointFromText('POINT(36.160137 51.732626)', 4326));
INSERT INTO geo_partners VALUES (348, 'ИП Родионова Вера Александровна', 'Нижний Новгород', '', ST_PointFromText('POINT(44.002672 56.324117)', 4326));
INSERT INTO geo_partners VALUES (349, 'ИП Рюмин Ю.М.', 'Рязань', '', ST_PointFromText('POINT(39.744954 54.619886)', 4326));
INSERT INTO geo_partners VALUES (350, 'ИП Савельев Евгений Александрович', 'Самара', '', ST_PointFromText('POINT(50.191184 53.205226)', 4326));
INSERT INTO geo_partners VALUES (351, 'ИП Салыев Альберт Маратович', 'Челябинск', '', ST_PointFromText('POINT(61.387103 55.152161)', 4326));
INSERT INTO geo_partners VALUES (352, 'ИП Сербинович С.В.', 'Вологда', 'улица Добролюбова, 45', ST_PointFromText('POINT(39.907082 59.230631)', 4326));
INSERT INTO geo_partners VALUES (353, 'ИП Слободина Людмила Николаевна', 'Каменск-Шахтинский', 'улица Ворошилова, 21', ST_PointFromText('POINT(40.271896 48.316602)', 4326));
INSERT INTO geo_partners VALUES (354, 'ИП Соболев Д.А.', 'Великий Новгород', '', ST_PointFromText('POINT(31.270669 58.522580)', 4326));
INSERT INTO geo_partners VALUES (355, 'ИП Созонова Т.В.', 'Архангельск', 'улица Прокопия Галушина, 26к1', ST_PointFromText('POINT(40.624620 64.531926)', 4326));
INSERT INTO geo_partners VALUES (356, 'ИП Соцких В.А.', 'Курган', 'Чернореченская улица, 87', ST_PointFromText('POINT(65.319747 55.487509)', 4326));
INSERT INTO geo_partners VALUES (359, 'ИП Статных О.А.', 'город Кемерово', 'Советский проспект, 9б', ST_PointFromText('POINT(86.048902 55.361902)', 4326));
INSERT INTO geo_partners VALUES (360, 'ИП Сулимов Ю.М.', 'Пятигорск', '', ST_PointFromText('POINT(43.025960 44.052845)', 4326));
INSERT INTO geo_partners VALUES (361, 'ИП Суфиянов Д.Т.', 'город Березники', 'улица Свердлова, 146', ST_PointFromText('POINT(56.841699 59.411100)', 4326));
INSERT INTO geo_partners VALUES (363, 'ИП Трифонов Геннадий Александрович', 'Тамбов', '', ST_PointFromText('POINT(41.454241 52.722032)', 4326));
INSERT INTO geo_partners VALUES (365, 'ИП Урюков А.В.', 'Саратов', '', ST_PointFromText('POINT(46.004549 51.537652)', 4326));
INSERT INTO geo_partners VALUES (366, 'ИП Фартдинов Ленер Маратович', 'Томск', '', ST_PointFromText('POINT(84.972128 56.495116)', 4326));
INSERT INTO geo_partners VALUES (368, 'ИП Филимонов Вячеслав Михайлович', 'Киров', 'улица Карла Маркса, 21', ST_PointFromText('POINT(49.666523 58.614672)', 4326));
INSERT INTO geo_partners VALUES (369, 'ИП Харитонов Владимир Николаевич', 'Новороссийск', '', ST_PointFromText('POINT(37.776493 44.720120)', 4326));
INSERT INTO geo_partners VALUES (373, 'ИП Чидалин А.А.', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (374, 'ИП Шанов Илья Александрович', 'Кострома', 'Студенческий проезд, 11', ST_PointFromText('POINT(40.921064 57.733416)', 4326));
INSERT INTO geo_partners VALUES (375, 'ИП Шеин В.А', 'Миасс', 'станция Сыростан', ST_PointFromText('POINT(59.923963 55.072242)', 4326));
INSERT INTO geo_partners VALUES (377, 'ИП Шувалов В.О.', 'Кострома', 'проезд Березовый, 18', ST_PointFromText('POINT(40.915405 57.729288)', 4326));
INSERT INTO geo_partners VALUES (378, 'ИП Щелчков А.Г.', 'Ижевск', 'улица Оружейника Драгунова, 70', ST_PointFromText('POINT(53.137810 56.827335)', 4326));
INSERT INTO geo_partners VALUES (392, 'Классик Групп', 'Тюмень', '', ST_PointFromText('POINT(65.558412 57.182631)', 4326));
INSERT INTO geo_partners VALUES (418, 'Крон', 'Волгоград', 'улица Землянского, 5', ST_PointFromText('POINT(44.534744 48.726666)', 4326));
INSERT INTO geo_partners VALUES (421, 'КузнецкСпецТранс', 'Новокузнецк', '', ST_PointFromText('POINT(87.155205 53.786502)', 4326));
INSERT INTO geo_partners VALUES (433, 'Липецкавтотехобслуживание-1', 'Липецк', 'Московская улица, 79', ST_PointFromText('POINT(39.527292 52.607087)', 4326));
INSERT INTO geo_partners VALUES (448, 'Маяк', 'Тольятти', '', ST_PointFromText('POINT(49.418084 53.511311)', 4326));
INSERT INTO geo_partners VALUES (449, 'МЕДВЕДЬ АвтоЦентр', 'Красноярск', '', ST_PointFromText('POINT(92.870412 56.008711)', 4326));
INSERT INTO geo_partners VALUES (450, 'Медикэл Менеджмент Групп', 'Москва', 'улица Пресненский Вал, 14', ST_PointFromText('POINT(37.566395 55.766754)', 4326));
INSERT INTO geo_partners VALUES (498, 'Пальмира', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (504, 'Транссервис', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (508, 'Питлейн плюс', 'Кемерово', 'улица Терешковой, 51', ST_PointFromText('POINT(86.160060 55.315642)', 4326));
INSERT INTO geo_partners VALUES (525, 'Прокатов', 'Красноярск', '', ST_PointFromText('POINT(92.870412 56.008711)', 4326));
INSERT INTO geo_partners VALUES (532, 'Рейс Авто', 'Москва', 'улица Цюрупы, 28б', ST_PointFromText('POINT(37.566719 55.667477)', 4326));
INSERT INTO geo_partners VALUES (537, 'Ричмонт Транспортные Услуги', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (555, 'Рязоблавтотехобслуживание', 'Рязань', '', ST_PointFromText('POINT(39.744954 54.619886)', 4326));
INSERT INTO geo_partners VALUES (568, 'Центральная Транспортная Компания', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (573, 'Сигма Моторс ГмбХ', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (579, 'Служба помощи на дорогах (СПД)', 'Мурманск', '', ST_PointFromText('POINT(33.077918 68.963254)', 4326));
INSERT INTO geo_partners VALUES (586, 'Современные информационные системы', 'Рязань', '', ST_PointFromText('POINT(39.744954 54.619886)', 4326));
INSERT INTO geo_partners VALUES (588, 'Соколов Артем Сергеевич', 'Казань', '', ST_PointFromText('POINT(49.122853 55.786764)', 4326));
INSERT INTO geo_partners VALUES (596, 'СПАС Казань', 'Казань', 'улица Петербургская, 25', ST_PointFromText('POINT(49.127524 55.784557)', 4326));
INSERT INTO geo_partners VALUES (597, 'СПАС Тольятти', 'Тольятти', '', ST_PointFromText('POINT(49.418084 53.511311)', 4326));
INSERT INTO geo_partners VALUES (598, 'СПАС-Нижний Новгород', 'Нижний Новгород', '', ST_PointFromText('POINT(44.002672 56.324117)', 4326));
INSERT INTO geo_partners VALUES (600, 'Спецтранс', 'Астрахань', 'станция Астрахань-2', ST_PointFromText('POINT(48.073010 46.405511)', 4326));
INSERT INTO geo_partners VALUES (621, 'Темп-Авто', 'Самара', '', ST_PointFromText('POINT(50.191184 53.205226)', 4326));
INSERT INTO geo_partners VALUES (624, 'ТЕСТ-АЗТ', 'Абакан', 'улица Крылова, 17а', ST_PointFromText('POINT(91.459446 53.736675)', 4326));
INSERT INTO geo_partners VALUES (631, 'Транспарк', 'Иркутск', 'Набережная Иркута, 1', ST_PointFromText('POINT(104.255445 52.295278)', 4326));
INSERT INTO geo_partners VALUES (634, 'Транс-сервис', 'Кемерово', 'Кузнецкий проспект, 19', ST_PointFromText('POINT(86.069510 55.360925)', 4326));
INSERT INTO geo_partners VALUES (647, 'Уралтехсервис ООО', 'Екатеринбург', 'улица Мира, 39', ST_PointFromText('POINT(60.656305 56.834240)', 4326));
INSERT INTO geo_partners VALUES (667, 'ФОКС ГРУПП', 'Москва', 'Рижский проезд, 13', ST_PointFromText('POINT(37.663324 55.817279)', 4326));
INSERT INTO geo_partners VALUES (673, 'ФЦ Германика', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (674, 'ФЦ Германика Север', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (683, 'Центр экспертизы Сюрвей', 'Челябинск', '', ST_PointFromText('POINT(61.387103 55.152161)', 4326));
INSERT INTO geo_partners VALUES (686, 'ЦКР Атлас', 'Оренбург', '', ST_PointFromText('POINT(55.097449 51.768060)', 4326));
INSERT INTO geo_partners VALUES (688, 'Час Пик Ассистанс', 'Тула', '', ST_PointFromText('POINT(37.619028 54.193802)', 4326));
INSERT INTO geo_partners VALUES (707, 'Эвакуатор ООО', 'Иваново', 'улица Станко, 15', ST_PointFromText('POINT(40.978457 56.991385)', 4326));
INSERT INTO geo_partners VALUES (714, 'Эналс', 'Мурманск', '', ST_PointFromText('POINT(33.077918 68.963254)', 4326));
INSERT INTO geo_partners VALUES (715, 'Эра', 'Петрозаводск', '', ST_PointFromText('POINT(34.364961 61.787586)', 4326));
INSERT INTO geo_partners VALUES (718, 'ЭТНОС', 'Нальчик', '', ST_PointFromText('POINT(43.625936 43.492649)', 4326));
INSERT INTO geo_partners VALUES (720, 'Юг-Авто', 'КРАСНОДАР', '', ST_PointFromText('POINT(38.976032 45.034942)', 4326));
INSERT INTO geo_partners VALUES (730, 'AvtoMax OOO', 'Волгоград', 'улица Землячки', ST_PointFromText('POINT(44.497805 48.758208)', 4326));
INSERT INTO geo_partners VALUES (731, 'BMGS АО', 'Санкт-Петербург', 'улица Есенина', ST_PointFromText('POINT(30.334652 60.042316)', 4326));
INSERT INTO geo_partners VALUES (732, 'Hertz (ЗАО "Ричмонт Транспортные Услуги")', 'Москва', '1-я Брестская улица', ST_PointFromText('POINT(37.589509 55.772598)', 4326));
INSERT INTO geo_partners VALUES (733, 'River Park Hotel', 'Новосибирск', 'улица Добролюбова', ST_PointFromText('POINT(82.963495 55.018892)', 4326));
INSERT INTO geo_partners VALUES (734, 'VIPЦентр "Такси"', 'Волгоград ', 'Волгоград городской округ', ST_PointFromText('POINT(44.298235 48.602645)', 4326));
INSERT INTO geo_partners VALUES (735, 'Абриколь', 'Санкт-Петербург', 'Воронежская улица', ST_PointFromText('POINT(30.347085 59.914570)', 4326));
INSERT INTO geo_partners VALUES (736, 'Ассоциация', 'город Нефтеюганск', '', ST_PointFromText('POINT(72.575071 61.097780)', 4326));
INSERT INTO geo_partners VALUES (737, 'Ассоциация аварийных комиссаров', 'Череповец', 'Олимпийская улица', ST_PointFromText('POINT(38.011268 59.125065)', 4326));
INSERT INTO geo_partners VALUES (738, 'Блюз Такси OOO', 'Кемерово', 'Институтская улица', ST_PointFromText('POINT(86.114237 55.392301)', 4326));
INSERT INTO geo_partners VALUES (739, 'Братсктранс OOО', 'Братск ', 'Енисейская улица', ST_PointFromText('POINT(101.896001 56.294501)', 4326));
INSERT INTO geo_partners VALUES (741, 'Гостиница Приокская ГП', 'Калуга', 'улица Суворова', ST_PointFromText('POINT(36.258439 54.516453)', 4326));
INSERT INTO geo_partners VALUES (742, 'Группа отелей "Евразия"', 'Санкт-Петербург', 'улица Подрезова', ST_PointFromText('POINT(30.303696 59.964395)', 4326));
INSERT INTO geo_partners VALUES (744, 'Журавли Гостиничный комплекс', 'город Саратов', 'улица 32-й Стрелковой дивизии', ST_PointFromText('POINT(46.051100 51.543002)', 4326));
INSERT INTO geo_partners VALUES (745, 'ЗАО "Аком-Авто"', 'Москва', 'Слободской переулок', ST_PointFromText('POINT(37.683311 55.757926)', 4326));
INSERT INTO geo_partners VALUES (746, 'ЗАО "Гостиница "Ростов"', 'Ростов-на-Дону', 'Буденновский проспект', ST_PointFromText('POINT(39.703847 47.228736)', 4326));
INSERT INTO geo_partners VALUES (748, 'ЗАО "Гостиница"Центральная-Бристоль"', 'Таганрог', 'улица Петровская', ST_PointFromText('POINT(38.930173 47.214611)', 4326));
INSERT INTO geo_partners VALUES (749, 'ЗАО "Домодедово Аэротель"', 'Домодедово-Аэропорт', 'аэропорт Домодедово', ST_PointFromText('POINT(37.900470 55.414327)', 4326));
INSERT INTO geo_partners VALUES (751, 'ЗАО "Отель-Сервис "Белгород"', 'город Белгород', 'площадь Литвинова', ST_PointFromText('POINT(36.607704 50.598902)', 4326));
INSERT INTO geo_partners VALUES (753, 'ЗАО "Страховая Компания Правоохранительных органов"', 'Рыбинск', '', ST_PointFromText('POINT(38.846234 58.045849)', 4326));
INSERT INTO geo_partners VALUES (754, 'ЗАО "ТАКСИ"', 'Мурманск', 'улица Марата', ST_PointFromText('POINT(33.072438 68.949995)', 4326));
INSERT INTO geo_partners VALUES (755, 'ЗАО "Такси"', 'Мурманск', 'улица Марата', ST_PointFromText('POINT(33.072438 68.949995)', 4326));
INSERT INTO geo_partners VALUES (756, 'ЗАО "Туррис"', 'Санкт-Петербург', 'Торжковская улица', ST_PointFromText('POINT(30.312958 59.989657)', 4326));
INSERT INTO geo_partners VALUES (759, 'ЗАО «Авто-Ойл»', 'город Курск', 'улица 2-я Новоселовка', ST_PointFromText('POINT(36.224663 51.733011)', 4326));
INSERT INTO geo_partners VALUES (760, 'ЗАО Ассистанская компания "ЛАТ"', 'Санкт-Петербург', 'улица Моисеенко, 22', ST_PointFromText('POINT(30.389045 59.937689)', 4326));
INSERT INTO geo_partners VALUES (761, 'ЗАО по туризму, отдыху и лечению в г.Иваново гостиница "Турист"', 'Иваново', 'улица Набережная', ST_PointFromText('POINT(40.979158 57.003686)', 4326));
INSERT INTO geo_partners VALUES (762, 'ИП Аббасова Е.А.', 'Владивосток', 'Бородинская улица', ST_PointFromText('POINT(131.929197 43.166452)', 4326));
INSERT INTO geo_partners VALUES (763, 'ИП Абрамов А.М.', 'Братск ', 'Олимпийская улица', ST_PointFromText('POINT(101.731996 56.313798)', 4326));
INSERT INTO geo_partners VALUES (766, 'ИП Аймурадова С.К.', 'Иваново', 'улица Печатная', ST_PointFromText('POINT(41.006907 57.002961)', 4326));
INSERT INTO geo_partners VALUES (767, 'ИП Пичугин В.В.', 'Тамбов', 'Красноармейская улица', ST_PointFromText('POINT(41.467761 52.700400)', 4326));
INSERT INTO geo_partners VALUES (768, 'ИП Александрова С.В.', 'город Чебоксары', 'улица Хузангая, 22к1', ST_PointFromText('POINT(47.274803 56.090799)', 4326));
INSERT INTO geo_partners VALUES (769, 'ИП Артемова Е.Н.', 'Дзержинск ', 'деревня Ямская', ST_PointFromText('POINT(36.022254 55.485474)', 4326));
INSERT INTO geo_partners VALUES (770, 'ИП Арутюнян С.В.', 'Энгельс', 'Степная улица', ST_PointFromText('POINT(46.117126 51.488652)', 4326));
INSERT INTO geo_partners VALUES (771, 'ИП Аршинов Е.А.', 'Полярные Зори (Мурманская область)', 'Полярные Зори', ST_PointFromText('POINT(32.503652 67.374576)', 4326));
INSERT INTO geo_partners VALUES (772, 'ИП Афлятунов Р.Р.', 'Казань', 'улица 2-я Азинская', ST_PointFromText('POINT(49.203118 55.807284)', 4326));
INSERT INTO geo_partners VALUES (773, 'ИП Ахмадьяров А.Р.', 'город Нефтекамск', 'улица Строителей', ST_PointFromText('POINT(54.227700 56.103402)', 4326));
INSERT INTO geo_partners VALUES (774, 'ИП Ахмедов А.К.', 'Набережные Челны', 'Чулман проспект', ST_PointFromText('POINT(52.388714 55.751503)', 4326));
INSERT INTO geo_partners VALUES (776, 'ИП БОЮЛ Донских С.И.', 'Воронеж', 'проезд Монтажный', ST_PointFromText('POINT(39.287621 51.648242)', 4326));
INSERT INTO geo_partners VALUES (778, 'ИП Бакин С.Ю.', 'Благовещенск', 'Студенческая улица', ST_PointFromText('POINT(127.509998 50.295319)', 4326));
INSERT INTO geo_partners VALUES (779, 'ИП Башарова Е.В.', 'Пермь', 'проспект Парковый', ST_PointFromText('POINT(56.155647 57.997531)', 4326));
INSERT INTO geo_partners VALUES (780, 'ИП Божко С.Н.', 'Псков', 'Советская улица', ST_PointFromText('POINT(28.337966 57.808981)', 4326));
INSERT INTO geo_partners VALUES (781, 'ИП Булдыгина Е.А.', 'Коломна', 'улица Ленина', ST_PointFromText('POINT(38.773857 55.085425)', 4326));
INSERT INTO geo_partners VALUES (782, 'ИП Ванькова О.В.', 'Пермь', 'улица Васильева', ST_PointFromText('POINT(56.233908 57.953752)', 4326));
INSERT INTO geo_partners VALUES (783, 'ИП Великанов Д.С.', 'Магнитогорск', 'проспект Карла Маркса', ST_PointFromText('POINT(58.978378 53.391620)', 4326));
INSERT INTO geo_partners VALUES (785, 'ИП Вербицкий О.А.', 'Петрозаводск', 'Первомайский проспект', ST_PointFromText('POINT(34.319228 61.799862)', 4326));
INSERT INTO geo_partners VALUES (786, 'ИП Вертунова О.М.', 'Череповец', 'улица Металлургов', ST_PointFromText('POINT(37.896607 59.131321)', 4326));
INSERT INTO geo_partners VALUES (787, 'ИП Ветошкина Н.А.', 'Ярославль', 'проспект Ленина', ST_PointFromText('POINT(39.865220 57.635970)', 4326));
INSERT INTO geo_partners VALUES (788, 'ИП Волкова Т.Н.', 'город Якутск', 'улица Дзержинского', ST_PointFromText('POINT(129.738996 62.049502)', 4326));
INSERT INTO geo_partners VALUES (789, 'ИП Гаврилова А.Н.', 'Калуга', 'улица Дальняя', ST_PointFromText('POINT(36.260523 54.602132)', 4326));
INSERT INTO geo_partners VALUES (792, 'ИП Гинтер В. Л.', 'Красноярск', 'улица Лесопитомник', ST_PointFromText('POINT(92.739159 56.018197)', 4326));
INSERT INTO geo_partners VALUES (793, 'ИП Голубков С. М.', 'Ярославль', 'Автозаводская улица', ST_PointFromText('POINT(39.830312 57.642603)', 4326));
INSERT INTO geo_partners VALUES (794, 'ИП Голубков С.М.', 'Ярославль', 'улица Ньютона', ST_PointFromText('POINT(39.865624 57.579754)', 4326));
INSERT INTO geo_partners VALUES (796, 'ИП Горбачева О.А.', 'Смоленск', 'улица Фрунзе', ST_PointFromText('POINT(32.041262 54.811342)', 4326));
INSERT INTO geo_partners VALUES (800, 'ИП Дойников С.К', 'Сочи ', 'улица Учительская', ST_PointFromText('POINT(39.737030 43.576238)', 4326));
INSERT INTO geo_partners VALUES (801, 'ИП Дойников С.К.', 'Оренбург', 'Учительская улица', ST_PointFromText('POINT(55.097800 51.722640)', 4326));
INSERT INTO geo_partners VALUES (802, 'ИП Донкин Д.А.', 'Чебоксары', 'проспект Ленина', ST_PointFromText('POINT(47.253351 56.123162)', 4326));
INSERT INTO geo_partners VALUES (803, 'ИП Донченко А.В.', 'Сургут', 'Быстринская улица', ST_PointFromText('POINT(73.433394 61.260023)', 4326));
INSERT INTO geo_partners VALUES (805, 'ИП Ермолаева С.Ю.', 'Киров', 'улица Ленина', ST_PointFromText('POINT(49.681956 58.590676)', 4326));
INSERT INTO geo_partners VALUES (806, 'ИП Жадан О.Л.', 'Калининград', 'пионерский лагерь имени О.Кошевого', ST_PointFromText('POINT(137.222978 50.632583)', 4326));
INSERT INTO geo_partners VALUES (807, 'ИП Жигурова Н. А.', 'Сургут', '', ST_PointFromText('POINT(73.395952 61.253827)', 4326));
INSERT INTO geo_partners VALUES (808, 'ИП Жила А.В.', 'Ангарск', 'улица Карла Маркса', ST_PointFromText('POINT(103.896720 52.531924)', 4326));
INSERT INTO geo_partners VALUES (809, 'ИП Журавлёв А.Е.', 'Новороссийск', 'улица Уварова', ST_PointFromText('POINT(37.765076 44.697116)', 4326));
INSERT INTO geo_partners VALUES (810, 'ИП Зайцев Е.П.', 'Ростов-на-Дону', 'Комарова бульвар', ST_PointFromText('POINT(39.703829 47.289159)', 4326));
INSERT INTO geo_partners VALUES (812, 'ИП Зарипова Э.Р.', 'Нефтекамск', '', ST_PointFromText('POINT(54.274161 56.096463)', 4326));
INSERT INTO geo_partners VALUES (814, 'ИП Зинченко', 'Смоленск', '1-й Краснофлотский переулок', ST_PointFromText('POINT(32.024670 54.787614)', 4326));
INSERT INTO geo_partners VALUES (815, 'ИП Ильенко В.А.', 'Орел', 'улица Маринченко', ST_PointFromText('POINT(36.137014 53.010342)', 4326));
INSERT INTO geo_partners VALUES (816, 'ИП Илюхин А.Н.', 'Тула', 'улица Демонстрации', ST_PointFromText('POINT(37.591539 54.191379)', 4326));
INSERT INTO geo_partners VALUES (817, 'ИП Кабатов А.В.', 'город Белгород', 'проспект Б. Хмельницкого', ST_PointFromText('POINT(36.576802 50.592098)', 4326));
INSERT INTO geo_partners VALUES (818, 'ИП Калугин А.В.', 'Комсомольск-На-Амуре', 'улица Кирова', ST_PointFromText('POINT(137.023004 50.559103)', 4326));
INSERT INTO geo_partners VALUES (819, 'ИП Каминский А.Н.', 'Орск', 'Ялтинская улица', ST_PointFromText('POINT(58.434807 51.247875)', 4326));
INSERT INTO geo_partners VALUES (821, 'ИП Камнев С.В.', 'Надеждино', 'поселок Надеждино', ST_PointFromText('POINT(42.020395 53.804106)', 4326));
INSERT INTO geo_partners VALUES (825, 'ИП Кладов В. А.', 'Астрахань', 'улица Кирова', ST_PointFromText('POINT(48.037545 46.344466)', 4326));
INSERT INTO geo_partners VALUES (826, 'ИП Козлова С.А.', 'Саранск', 'улица Васенко', ST_PointFromText('POINT(45.174820 54.194829)', 4326));
INSERT INTO geo_partners VALUES (827, 'ИП Колчин Д.С.', 'Таганрог', '7-й Новый переулок', ST_PointFromText('POINT(38.909008 47.272011)', 4326));
INSERT INTO geo_partners VALUES (828, 'ИП Комаров А.А.', 'Оренбург', 'Беляевская улица', ST_PointFromText('POINT(55.115658 51.721669)', 4326));
INSERT INTO geo_partners VALUES (829, 'ИП Кондратенко С.И.', 'Тверь', 'улица Мусоргского', ST_PointFromText('POINT(35.902715 56.873077)', 4326));
INSERT INTO geo_partners VALUES (830, 'ИП Константинов А.А. "Служба Помощи Водителям"', 'Великий Новгород', 'Парковая улица', ST_PointFromText('POINT(31.294385 58.542358)', 4326));
INSERT INTO geo_partners VALUES (831, 'ИП Коптенко Е.В.', 'Орел', 'улица Максима Горького', ST_PointFromText('POINT(36.071563 52.976206)', 4326));
INSERT INTO geo_partners VALUES (833, 'ИП Криволапова А.В', 'Ставрополь', 'улица Доваторцев', ST_PointFromText('POINT(41.930240 45.012542)', 4326));
INSERT INTO geo_partners VALUES (834, 'ИП Кудряшов И.Ф.', 'Екатеринбург', 'улица Ветеринарная', ST_PointFromText('POINT(60.563329 56.823794)', 4326));
INSERT INTO geo_partners VALUES (835, 'ИП Кузнецов А.В.', 'Рязань', 'площадь Димитрова', ST_PointFromText('POINT(39.702598 54.630034)', 4326));
INSERT INTO geo_partners VALUES (836, 'ИП Кузьмин А.Г.', 'Таганрог', 'улица Вишневая', ST_PointFromText('POINT(38.874693 47.238679)', 4326));
INSERT INTO geo_partners VALUES (837, 'ИП Куклин В.К.', 'Киров', 'Московская улица', ST_PointFromText('POINT(49.633420 58.602962)', 4326));
INSERT INTO geo_partners VALUES (838, 'ИП Кулькова К.Б.', 'Пермь', 'улица Беляева', ST_PointFromText('POINT(56.163498 57.968004)', 4326));
INSERT INTO geo_partners VALUES (839, 'ИП Курихина Ю.Л.', 'Котлас ', 'Гвардейская улица', ST_PointFromText('POINT(46.678798 61.262000)', 4326));
INSERT INTO geo_partners VALUES (840, 'ИП Лапин А.П.', 'Алматы', 'улица Жансугурова', ST_PointFromText('POINT(76.930526 43.307687)', 4326));
INSERT INTO geo_partners VALUES (841, 'ИП Лебедева Н. И.', 'Петрозаводск', 'Лососинская улица', ST_PointFromText('POINT(34.348971 61.777275)', 4326));
INSERT INTO geo_partners VALUES (842, 'ИП Лобанов С.А.', 'город Нижний Тагил', 'Краснознамённая улица', ST_PointFromText('POINT(59.914899 57.949101)', 4326));
INSERT INTO geo_partners VALUES (843, 'ИП Логинов О.А.', 'Ульяновск', 'проспект Созидателей', ST_PointFromText('POINT(48.592164 54.369670)', 4326));
INSERT INTO geo_partners VALUES (844, 'ИП Львов Ю.А.', 'Псков', 'улица Максима Горького', ST_PointFromText('POINT(28.321914 57.815156)', 4326));
INSERT INTO geo_partners VALUES (845, 'ИП Мадьянкин С.А.', 'город Димитровград', 'улица 50 лет Октября', ST_PointFromText('POINT(49.595601 54.235801)', 4326));
INSERT INTO geo_partners VALUES (847, 'ИП Макарова Л.А.', 'Сызрань', '', ST_PointFromText('POINT(48.456537 53.136895)', 4326));
INSERT INTO geo_partners VALUES (848, 'ИП Максимов А.А', 'Пенза', 'проспект Победы', ST_PointFromText('POINT(44.949837 53.225829)', 4326));
INSERT INTO geo_partners VALUES (849, 'ИП Мамалиева Ю. В.', 'Тамбов', 'бульвар Энтузиастов', ST_PointFromText('POINT(41.430597 52.755988)', 4326));
INSERT INTO geo_partners VALUES (850, 'ИП Манукянц Э.Ю.', 'Кострома ', 'Кострома городской округ', ST_PointFromText('POINT(40.858802 57.790295)', 4326));
INSERT INTO geo_partners VALUES (852, 'ИП Махмудов Р.М.', '', 'улица Ольховка', ST_PointFromText('POINT(57.406003 57.154298)', 4326));
INSERT INTO geo_partners VALUES (854, 'ИП Монахов Д.С.', '', 'Зона', ST_PointFromText('POINT(-81.459670 38.691890)', 4326));
INSERT INTO geo_partners VALUES (855, 'ИП Морковкина С.В.', 'Иваново', 'улица Советская', ST_PointFromText('POINT(40.986398 56.995710)', 4326));
INSERT INTO geo_partners VALUES (856, 'ИП Мосин В.Ю.', 'Нижний Новгород', 'улица Юлиуса Фучика', ST_PointFromText('POINT(43.875264 56.235170)', 4326));
INSERT INTO geo_partners VALUES (857, 'ИП Москалев П.С.', 'Волгоград', 'проезд Аптечный', ST_PointFromText('POINT(44.475698 48.756705)', 4326));
INSERT INTO geo_partners VALUES (858, 'ИП Мунгалова В.В.', 'Чита', 'улица Евгения Гаюсана', ST_PointFromText('POINT(113.476444 52.064718)', 4326));
INSERT INTO geo_partners VALUES (861, 'ИП Назмеев Р.Г.', 'Йошкар-Ола', 'улица Димитрова', ST_PointFromText('POINT(47.846626 56.647024)', 4326));
INSERT INTO geo_partners VALUES (863, 'ИП Нефедов Н.Ю.', 'город Якутск', 'улица Кирова', ST_PointFromText('POINT(129.727004 62.029001)', 4326));
INSERT INTO geo_partners VALUES (864, 'ИП Новикова В.И.', 'Чита', 'улица Ленина', ST_PointFromText('POINT(113.507346 52.030920)', 4326));
INSERT INTO geo_partners VALUES (866, 'ИП Нургалиев А.М.', 'Казань', 'улица Лукина', ST_PointFromText('POINT(49.092346 55.864909)', 4326));
INSERT INTO geo_partners VALUES (867, 'ИП Осичанский Э.П.', 'Владивосток', 'Деревенская улица', ST_PointFromText('POINT(131.935736 43.134795)', 4326));
INSERT INTO geo_partners VALUES (869, 'ИП Салащенко С.Н.', 'Петрозаводск', 'бульвар Интернационалистов, 15', ST_PointFromText('POINT(34.313425 61.762214)', 4326));
INSERT INTO geo_partners VALUES (870, 'ИП Павлюченко В. П.', 'Брянск', 'улица Дуки', ST_PointFromText('POINT(34.364512 53.261033)', 4326));
INSERT INTO geo_partners VALUES (873, 'ИП Першина Л. Е.', 'Тверь', 'улица Громова', ST_PointFromText('POINT(35.777122 56.843944)', 4326));
INSERT INTO geo_partners VALUES (874, 'ИП Писахов Т.', 'Казань', 'улица Хади Такташа', ST_PointFromText('POINT(49.132205 55.769646)', 4326));
INSERT INTO geo_partners VALUES (876, 'ИП Пономарев Д.С.', 'Саратов ', 'Саратов городской округ', ST_PointFromText('POINT(45.856210 51.520781)', 4326));
INSERT INTO geo_partners VALUES (877, 'ИП Попинов П.В. ГК «Порт Тортуга»', 'Нижний Новгород', '', ST_PointFromText('POINT(44.002672 56.324117)', 4326));
INSERT INTO geo_partners VALUES (879, 'ИП Ревуненков И. А.', 'город Курск', 'улица Бойцов 9 Дивизии', ST_PointFromText('POINT(36.160137 51.732626)', 4326));
INSERT INTO geo_partners VALUES (880, 'ИП Рыбков П.Г.', 'Оренбург', 'Волгоградская улица', ST_PointFromText('POINT(55.130804 51.843540)', 4326));
INSERT INTO geo_partners VALUES (881, 'ИП Рыжков С.В.', 'Липецк', 'Московская улица', ST_PointFromText('POINT(39.514814 52.601809)', 4326));
INSERT INTO geo_partners VALUES (882, 'ИП Рюмин Ю.М.', 'Рязань', 'улица Есенина', ST_PointFromText('POINT(39.757826 54.622909)', 4326));
INSERT INTO geo_partners VALUES (884, 'ИП Савенков А.А.', 'Орск', 'проспект Ленина', ST_PointFromText('POINT(58.452289 51.251116)', 4326));
INSERT INTO geo_partners VALUES (885, 'ИП Салыев А.М.', 'Челябинск', 'улица Косарева, 2', ST_PointFromText('POINT(61.369999 55.195267)', 4326));
INSERT INTO geo_partners VALUES (886, 'ИП Седова Г.И.', 'Рязань', 'площадь Димитрова', ST_PointFromText('POINT(39.702598 54.630034)', 4326));
INSERT INTO geo_partners VALUES (887, 'ИП Семенов В.В.', 'Ульяновск', 'улица Радищева', ST_PointFromText('POINT(48.398272 54.335001)', 4326));
INSERT INTO geo_partners VALUES (888, 'ИП Сербинович С.В.', 'Вологда', 'улица Добролюбова', ST_PointFromText('POINT(39.908860 59.231984)', 4326));
INSERT INTO geo_partners VALUES (889, 'ИП Синтяй С.Ф.', 'Ижевск', 'Молодежная улица', ST_PointFromText('POINT(53.291629 56.853001)', 4326));
INSERT INTO geo_partners VALUES (890, 'ИП Скулкин Юрий Витальевич', 'Йошкар-Ола', 'Первомайский переулок', ST_PointFromText('POINT(47.887463 56.642287)', 4326));
INSERT INTO geo_partners VALUES (891, 'ИП Слободина Л.М.', 'Каменск-Шахтинский', 'улица Ворошилова', ST_PointFromText('POINT(40.289198 48.309298)', 4326));
INSERT INTO geo_partners VALUES (893, 'ИП Солдатов И.В.', 'Набережные Челны', 'Романтиков бульвар', ST_PointFromText('POINT(52.426066 55.738409)', 4326));
INSERT INTO geo_partners VALUES (894, 'ИП Соцких В.А.', 'Курган', 'Чернореченская улица', ST_PointFromText('POINT(65.316379 55.485459)', 4326));
INSERT INTO geo_partners VALUES (895, 'ИП Спирин С.И.', 'Оренбург', 'Томилинская улица', ST_PointFromText('POINT(55.120419 51.797328)', 4326));
INSERT INTO geo_partners VALUES (896, 'ИП Статных О.А.', 'КРАСНОДАР', 'улица Красная', ST_PointFromText('POINT(38.975062 45.037069)', 4326));
INSERT INTO geo_partners VALUES (898, 'ИП Суфиянов Д.Т.', 'город Березники', 'улица Свердлова', ST_PointFromText('POINT(56.833596 59.410298)', 4326));
INSERT INTO geo_partners VALUES (899, 'ИП Тарасова Ю.Ю.', 'Тула', 'улица Шухова', ST_PointFromText('POINT(37.665542 54.207164)', 4326));
INSERT INTO geo_partners VALUES (900, 'ИП Терентьев О.С.', 'Сургут', 'Производственная улица', ST_PointFromText('POINT(73.428902 61.275083)', 4326));
INSERT INTO geo_partners VALUES (902, 'ИП Трушин А.М.', 'Нижневартовск', 'проспект Победы', ST_PointFromText('POINT(76.558938 60.937710)', 4326));
INSERT INTO geo_partners VALUES (903, 'ИП Тюрин Е.П.', 'Калуга', 'улица Максима Горького', ST_PointFromText('POINT(36.281122 54.509502)', 4326));
INSERT INTO geo_partners VALUES (906, 'ИП Филимонов В.М.', 'Киров', 'улица Карла Маркса', ST_PointFromText('POINT(49.668149 58.601856)', 4326));
INSERT INTO geo_partners VALUES (908, 'ИП Черных Е.В.', 'Комсомольск-На-Амуре ', 'улица Кирова', ST_PointFromText('POINT(137.023004 50.559103)', 4326));
INSERT INTO geo_partners VALUES (909, 'ИП Чернявская О.А.', 'город Петропавловск-Камчатский', 'проспект Таранца', ST_PointFromText('POINT(158.645003 53.073601)', 4326));
INSERT INTO geo_partners VALUES (910, 'ИП Чидалин А.А.', 'Москва', 'проспект Мира', ST_PointFromText('POINT(37.637542 55.809884)', 4326));
INSERT INTO geo_partners VALUES (912, 'ИП Шацкий С.В.', 'Таганрог', 'улица Дзержинского', ST_PointFromText('POINT(38.917381 47.241187)', 4326));
INSERT INTO geo_partners VALUES (913, 'ИП Шведова А. А.', 'Омск', 'проспект Королева', ST_PointFromText('POINT(73.320844 55.034183)', 4326));
INSERT INTO geo_partners VALUES (917, 'ИП Щеголев Д.А.', 'Чита', 'Бульварная улица', ST_PointFromText('POINT(113.488302 52.020270)', 4326));
INSERT INTO geo_partners VALUES (919, 'ИПБЮЛ Прошин А.С.', 'Сыктывкар', 'улица Ленина', ST_PointFromText('POINT(50.835788 61.668669)', 4326));
INSERT INTO geo_partners VALUES (922, 'Кори', 'Казань', 'Победы проспект', ST_PointFromText('POINT(49.218982 55.771540)', 4326));
INSERT INTO geo_partners VALUES (924, 'МУП "Гостиница "Чернигов" г. Брянска', 'Брянск', 'Карла Маркса площадь', ST_PointFromText('POINT(34.366003 53.242295)', 4326));
INSERT INTO geo_partners VALUES (925, 'МУП "Рума"', 'Комсомольск-На-Амуре', 'Комсомольск-на-Амуре', ST_PointFromText('POINT(137.013652 50.604167)', 4326));
INSERT INTO geo_partners VALUES (926, 'МУП г. Омск гостичный комплекс "Иртыш"', 'Омск', 'улица Красный Путь', ST_PointFromText('POINT(73.337454 55.010984)', 4326));
INSERT INTO geo_partners VALUES (929, 'Новгородский филиал ООО "Центр правового и делового консалтинга" Гостиница "Береста Палас"', 'Великий Новгород', 'Студенческая улица', ST_PointFromText('POINT(31.298895 58.535871)', 4326));
INSERT INTO geo_partners VALUES (930, 'ОАО "ВАО "Волгоград интурист"', 'Волгоград', 'улица Мира', ST_PointFromText('POINT(44.516418 48.709606)', 4326));
INSERT INTO geo_partners VALUES (931, 'ОАО "Военно-страховая компания"', 'Смоленск', '', ST_PointFromText('POINT(31.999095 54.779800)', 4326));
INSERT INTO geo_partners VALUES (932, 'ОАО "Гостиница "Барнаул"', 'Барнаул', 'Красноармейский проспект', ST_PointFromText('POINT(83.774117 53.338102)', 4326));
INSERT INTO geo_partners VALUES (933, 'ОАО "Гостиница "Восток"', 'город Стерлитамак', 'Комсомольская улица', ST_PointFromText('POINT(55.958502 53.612602)', 4326));
INSERT INTO geo_partners VALUES (934, 'ОАО "Гостиница "Новороссийск"', 'Новороссийск', 'улица Исаева', ST_PointFromText('POINT(37.782377 44.705247)', 4326));
INSERT INTO geo_partners VALUES (935, 'ОАО "Гостиница "Новослoбодская"', 'Москва', 'Новослободская улица', ST_PointFromText('POINT(37.595357 55.785280)', 4326));
INSERT INTO geo_partners VALUES (936, 'ОАО "Гостиница "Октябрьская"', 'Пермь', 'улица Плеханова', ST_PointFromText('POINT(56.207542 58.003040)', 4326));
INSERT INTO geo_partners VALUES (937, 'ОАО "Гостиница "Полярные зори"', 'Мурманск', 'улица Воровского', ST_PointFromText('POINT(33.077047 68.969037)', 4326));
INSERT INTO geo_partners VALUES (938, 'ОАО "Гостиница "Саранск"', 'Саранск', 'Коммунистическая улица', ST_PointFromText('POINT(45.172889 54.184730)', 4326));
INSERT INTO geo_partners VALUES (939, 'ОАО "Гостиница "Тюмень"', 'Тюмень', 'улица Орджоникидзе', ST_PointFromText('POINT(65.547246 57.152892)', 4326));
INSERT INTO geo_partners VALUES (940, 'ОАО "Гостиничный комплекс "Россия"', 'Уфа', 'Октября проспект', ST_PointFromText('POINT(56.022822 54.769876)', 4326));
INSERT INTO geo_partners VALUES (942, 'ОАО "Интурист - Краснодар"', 'КРАСНОДАР', 'улица Красная', ST_PointFromText('POINT(38.975062 45.037069)', 4326));
INSERT INTO geo_partners VALUES (945, 'ОАО "Лазурная"', 'Сочи ', 'Курортный проспект', ST_PointFromText('POINT(39.758410 43.562385)', 4326));
INSERT INTO geo_partners VALUES (947, 'ОАО "МРСК"', 'Нефтекамск', '', ST_PointFromText('POINT(54.274161 56.096463)', 4326));
INSERT INTO geo_partners VALUES (948, 'ОАО "НИИАР "Гостиница Радуга"', 'город Димитровград', 'улица Гончарова', ST_PointFromText('POINT(49.552697 54.225801)', 4326));
INSERT INTO geo_partners VALUES (949, 'ОАО "Отель "Интурист-Кострома"', 'Кострома', 'улица Магистральная', ST_PointFromText('POINT(40.907428 57.734040)', 4326));
INSERT INTO geo_partners VALUES (950, 'ОАО "Прокопьевское Транспортное Управление "Парк-отель Аврора"', 'Прокопьевск', 'улица Запарковая', ST_PointFromText('POINT(86.798304 53.797799)', 4326));
INSERT INTO geo_partners VALUES (951, 'ОАО "СК РОСНО"', 'Вологда', 'улица Ленинградская', ST_PointFromText('POINT(39.851251 59.211787)', 4326));
INSERT INTO geo_partners VALUES (952, 'ОАО "Северсталь СБК гостиница "Металлург"', 'Череповец', 'Вологодская улица', ST_PointFromText('POINT(37.917897 59.133528)', 4326));
INSERT INTO geo_partners VALUES (953, 'ОАО «Национальная страховая компания Татарстан»', 'город Набережные Челны', 'улица Шамиля Усманова', ST_PointFromText('POINT(52.371799 55.722897)', 4326));
INSERT INTO geo_partners VALUES (954, 'ОАО ГК Гостиница "Югор"', 'Сыктывкар', 'улица Горького', ST_PointFromText('POINT(50.840216 61.675188)', 4326));
INSERT INTO geo_partners VALUES (956, 'ООО " Гостиница Националь- Самара"', 'Самара', 'улица Фрунзе', ST_PointFromText('POINT(50.092782 53.189744)', 4326));
INSERT INTO geo_partners VALUES (957, 'ООО " ПКК "Медвежий Угол"', 'Сургут', 'улица Крылова', ST_PointFromText('POINT(73.347147 61.286051)', 4326));
INSERT INTO geo_partners VALUES (958, 'ООО " Рейс Авто"', 'Москва', 'улица Цюрупы', ST_PointFromText('POINT(37.573537 55.667599)', 4326));
INSERT INTO geo_partners VALUES (959, 'ООО "Электроник"', 'город Петропавловск-Камчатский', 'проспект 50 лет Октября', ST_PointFromText('POINT(158.633002 53.057600)', 4326));
INSERT INTO geo_partners VALUES (960, 'ООО "007-Сервис"', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (961, 'ООО "A.M.RENTА"', 'Москва', 'Партийный переулок', ST_PointFromText('POINT(37.629843 55.720844)', 4326));
INSERT INTO geo_partners VALUES (962, 'ООО "ACT Моторс"', 'Москва', 'Волоколамское шоссе', ST_PointFromText('POINT(37.439383 55.822999)', 4326));
INSERT INTO geo_partners VALUES (964, 'ООО "АБ Аварком"', 'Москва', '3-я Хорошевская улица', ST_PointFromText('POINT(37.500711 55.782689)', 4326));
INSERT INTO geo_partners VALUES (965, 'ООО "АВАРИЙНЫЙ КОМИССАР"', 'город Белгород', 'Архиерейская улица', ST_PointFromText('POINT(36.559896 50.568601)', 4326));
INSERT INTO geo_partners VALUES (966, 'ООО "АВАРКОМ 51"', 'Мурманск', 'проспект Ленина', ST_PointFromText('POINT(33.073022 68.963957)', 4326));
INSERT INTO geo_partners VALUES (967, 'ООО "АВИК"', 'Санкт-Петербург', 'проспект Металлистов', ST_PointFromText('POINT(30.417279 59.965309)', 4326));
INSERT INTO geo_partners VALUES (968, 'ООО "АВТОАССИСТАНС"', 'Красноярск', '', ST_PointFromText('POINT(92.870412 56.008711)', 4326));
INSERT INTO geo_partners VALUES (970, 'ООО "АЗИМУТ Хотелс Компани"', 'Москва', 'Потаповский переулок', ST_PointFromText('POINT(37.641225 55.760813)', 4326));
INSERT INTO geo_partners VALUES (971, 'ООО "АК АВТО"', 'Уфа', 'улица Академика Королева', ST_PointFromText('POINT(56.070990 54.773797)', 4326));
INSERT INTO geo_partners VALUES (972, 'ООО "АК-Сервис"', 'Саранск', 'Советская улица', ST_PointFromText('POINT(45.171012 54.181837)', 4326));
INSERT INTO geo_partners VALUES (973, 'ООО "АМАНАТ"', 'город Махачкала', 'улица Леваневского', ST_PointFromText('POINT(47.507898 42.976900)', 4326));
INSERT INTO geo_partners VALUES (974, 'ООО "АНДИ Моторс C.О"', 'Старый Оскол ', 'улица Прядченко', ST_PointFromText('POINT(37.845996 51.299702)', 4326));
INSERT INTO geo_partners VALUES (975, 'ООО "АПН"', 'Великий Новгород', 'Октябрьская улица', ST_PointFromText('POINT(31.247349 58.521781)', 4326));
INSERT INTO geo_partners VALUES (976, 'ООО "АПЭКС ГРУП', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (977, 'ООО "АПЭКС ГРУПП"', 'Москва', 'Варшавское шоссе', ST_PointFromText('POINT(37.604331 55.600531)', 4326));
INSERT INTO geo_partners VALUES (978, 'ООО "АСТ-54"', 'Новосибирск', '2-й Степной переулок', ST_PointFromText('POINT(82.821965 54.975744)', 4326));
INSERT INTO geo_partners VALUES (979, 'ООО "Абсолют-авто"', 'Брянск', 'Ленина проспект', ST_PointFromText('POINT(34.365608 53.244656)', 4326));
INSERT INTO geo_partners VALUES (981, 'ООО "АварКом-96"', 'Екатеринбург', 'улица Родонитовая', ST_PointFromText('POINT(60.625960 56.792355)', 4326));
INSERT INTO geo_partners VALUES (982, 'ООО "Аварийный комиссар"', 'Киров', 'улица Чапаева', ST_PointFromText('POINT(49.645322 58.587224)', 4326));
INSERT INTO geo_partners VALUES (983, 'ООО "Аварком"', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (984, 'ООО "Аваркос"', 'Санкт-Петербург', 'Минеральная улица', ST_PointFromText('POINT(30.371330 59.965412)', 4326));
INSERT INTO geo_partners VALUES (986, 'ООО "Авто +"', 'Ставрополь', 'улица Советская', ST_PointFromText('POINT(41.971760 45.047036)', 4326));
INSERT INTO geo_partners VALUES (987, 'ООО "Авто Рента "', 'Нижний Новгород', 'переулок Мотальный', ST_PointFromText('POINT(43.949779 56.295150)', 4326));
INSERT INTO geo_partners VALUES (989, 'ООО "Авто-Алекс"', 'Липецк', 'улица 40 лет Советской Армии', ST_PointFromText('POINT(39.594549 52.599567)', 4326));
INSERT INTO geo_partners VALUES (990, 'ООО "Авто-Друг"', 'Казань', 'улица Садовая', ST_PointFromText('POINT(49.156648 55.773505)', 4326));
INSERT INTO geo_partners VALUES (991, 'ООО "Авто-Стимул"', 'Омск', '', ST_PointFromText('POINT(73.365535 54.990215)', 4326));
INSERT INTO geo_partners VALUES (992, 'ООО "АвтоГарант"', 'Нижний Новгород', 'Большая Печерская улица', ST_PointFromText('POINT(44.027762 56.323912)', 4326));
INSERT INTO geo_partners VALUES (993, 'ООО "АвтоСТОП"', 'КРАСНОДАР', 'улица Карла Маркса', ST_PointFromText('POINT(38.954930 45.045011)', 4326));
INSERT INTO geo_partners VALUES (994, 'ООО "Автоангел"', 'Липецк', 'проспект Победы', ST_PointFromText('POINT(39.556945 52.590185)', 4326));
INSERT INTO geo_partners VALUES (995, 'ООО "Автогарант"', 'Нижний Новгород', '', ST_PointFromText('POINT(44.002672 56.324117)', 4326));
INSERT INTO geo_partners VALUES (996, 'ООО "Автодор"', 'Самара', 'Московская улица', ST_PointFromText('POINT(50.151739 53.196806)', 4326));
INSERT INTO geo_partners VALUES (997, 'ООО "Автодоставка"', 'Томск', 'проспект Мира', ST_PointFromText('POINT(84.974463 56.513547)', 4326));
INSERT INTO geo_partners VALUES (998, 'ООО "Автокей"', 'Сыктывкар', 'Гаражная улица', ST_PointFromText('POINT(50.820912 61.656370)', 4326));
INSERT INTO geo_partners VALUES (999, 'ООО "Автокласс"', 'Томск', 'улица Белинского', ST_PointFromText('POINT(84.957863 56.469244)', 4326));
INSERT INTO geo_partners VALUES (1000, 'ООО "Автоклуб"', 'Чебоксары', 'улица Афанасьева', ST_PointFromText('POINT(47.219853 56.148644)', 4326));
INSERT INTO geo_partners VALUES (1001, 'ООО "Автоколор"', 'Аладьино', 'Марта', ST_PointFromText('POINT(11.926159 42.534947)', 4326));
INSERT INTO geo_partners VALUES (1003, 'ООО "Автомобильная Спасательная Служба-М-Сервис"', 'Тольятти', 'Ленинградская', ST_PointFromText('POINT(49.412120 53.505303)', 4326));
INSERT INTO geo_partners VALUES (1004, 'ООО "Автопартнер"', 'город Нижний Тагил', 'улица Циолковского', ST_PointFromText('POINT(59.977502 57.918099)', 4326));
INSERT INTO geo_partners VALUES (1005, 'ООО "Автопартнер" (Авангард)', 'Москва', 'Привольная улица', ST_PointFromText('POINT(37.849715 55.690466)', 4326));
INSERT INTO geo_partners VALUES (1006, 'ООО "Автополис"', 'Комсомольск-На-Амуре', 'улица Кирова', ST_PointFromText('POINT(137.023004 50.559103)', 4326));
INSERT INTO geo_partners VALUES (1007, 'ООО "Авторост Универсал"', 'Сочи ', 'улица Пластунская', ST_PointFromText('POINT(39.741962 43.619516)', 4326));
INSERT INTO geo_partners VALUES (1008, 'ООО "Автосиндикат"', 'Владивосток', 'Нерчинская улица', ST_PointFromText('POINT(131.896597 43.124459)', 4326));
INSERT INTO geo_partners VALUES (1009, 'ООО "Автоспас"', 'Иваново', 'улица Варенцовой', ST_PointFromText('POINT(40.973606 56.994783)', 4326));
INSERT INTO geo_partners VALUES (1010, 'ООО "Автоспасатель"', 'Хабаровск', '60 лет Октября проспект', ST_PointFromText('POINT(135.126004 48.447968)', 4326));
INSERT INTO geo_partners VALUES (1011, 'ООО "Автоспутник-Саранск"', 'Саранск', 'улица Степана Разина', ST_PointFromText('POINT(45.167167 54.188602)', 4326));
INSERT INTO geo_partners VALUES (1012, 'ООО "Автотранспортное предприятие 1"', 'Москва', 'Волоколамское шоссе', ST_PointFromText('POINT(37.439383 55.822999)', 4326));
INSERT INTO geo_partners VALUES (1013, 'ООО "Автоэвакуация"', 'город Стерлитамак', 'улица Бабушкина', ST_PointFromText('POINT(55.989701 53.659402)', 4326));
INSERT INTO geo_partners VALUES (1014, 'ООО "Автоюрцентр 45"', 'Курган', 'Половинская улица', ST_PointFromText('POINT(65.372505 55.451425)', 4326));
INSERT INTO geo_partners VALUES (1015, 'ООО "Агама"', 'Пермь', 'Комсомольский проспект, 34', ST_PointFromText('POINT(56.242020 58.008939)', 4326));
INSERT INTO geo_partners VALUES (1017, 'ООО "Азия"', 'Екатеринбург', 'улица Уральская', ST_PointFromText('POINT(60.630631 56.857815)', 4326));
INSERT INTO geo_partners VALUES (1018, 'ООО "Акцент-Н"', 'Пермь', 'улица Васильева, 7', ST_PointFromText('POINT(56.225131 57.954335)', 4326));
INSERT INTO geo_partners VALUES (1020, 'ООО "Алмаз-сервис"', 'Челябинск', 'улица Лесопарковая', ST_PointFromText('POINT(61.364788 55.153401)', 4326));
INSERT INTO geo_partners VALUES (1022, 'ООО "АнДи Моторс"', '', 'улица Прядченко', ST_PointFromText('POINT(37.845996 51.299702)', 4326));
INSERT INTO geo_partners VALUES (1024, 'ООО "Англитер"', 'Ставрополь', 'улица Лермонтова', ST_PointFromText('POINT(41.965203 45.034464)', 4326));
INSERT INTO geo_partners VALUES (1025, 'ООО "Артекс"', 'Великий Устюг', '', ST_PointFromText('POINT(46.299870 60.761850)', 4326));
INSERT INTO geo_partners VALUES (1026, 'ООО "Атон"', 'Сочи ', 'улица Транспортная', ST_PointFromText('POINT(39.755230 43.591493)', 4326));
INSERT INTO geo_partners VALUES (1029, 'ООО "Бизнес-Класс"', 'Тюмень', 'улица Щербакова', ST_PointFromText('POINT(65.556777 57.183627)', 4326));
INSERT INTO geo_partners VALUES (1030, 'ООО "Бизнес-Отель"Мираж"', 'Челябинск', 'улица 8 Марта', ST_PointFromText('POINT(61.394253 55.171756)', 4326));
INSERT INTO geo_partners VALUES (1031, 'ООО "Блэк Хорс"', 'Измаил', 'улица Маршала Жукова', ST_PointFromText('POINT(28.838328 45.375362)', 4326));
INSERT INTO geo_partners VALUES (1032, 'ООО "Братсктурист"', 'Братск ', 'улица Наймушина', ST_PointFromText('POINT(101.755999 56.302902)', 4326));
INSERT INTO geo_partners VALUES (1033, 'ООО "БуксирОФФ"', 'Москва', 'Поморская улица', ST_PointFromText('POINT(37.576510 55.869784)', 4326));
INSERT INTO geo_partners VALUES (1034, 'ООО "ВИП-КАРАВАН"', 'Новосибирск', 'улица Толстого', ST_PointFromText('POINT(82.960917 55.013080)', 4326));
INSERT INTO geo_partners VALUES (1035, 'ООО "ВИТ и К"', 'Курган', 'улица Коли Мяготина', ST_PointFromText('POINT(65.313019 55.436407)', 4326));
INSERT INTO geo_partners VALUES (1036, 'ООО "ВТС 24 " ("Такси Плюс ")', 'КРАСНОДАР', 'улица Колхозная', ST_PointFromText('POINT(38.990593 45.051481)', 4326));
INSERT INTO geo_partners VALUES (1037, 'ООО "Вектор"', 'Казань', 'улица Энгельса', ST_PointFromText('POINT(49.074389 55.809343)', 4326));
INSERT INTO geo_partners VALUES (1038, 'ООО "Веснин"', 'Пермь', 'улица Чкалова', ST_PointFromText('POINT(56.266930 57.985887)', 4326));
INSERT INTO geo_partners VALUES (1040, 'ООО "Волга-ГРК"', 'Ярославль', 'площадь Ярославль Главный', ST_PointFromText('POINT(39.834408 57.626159)', 4326));
INSERT INTO geo_partners VALUES (1041, 'ООО "Волжск.Транс.Комп"', 'Волжский', 'улица Мира', ST_PointFromText('POINT(44.808811 48.767003)', 4326));
INSERT INTO geo_partners VALUES (1042, 'ООО "ВостокМоторс 35"', 'Череповец', 'улица Краснодонцев', ST_PointFromText('POINT(37.997596 59.130038)', 4326));
INSERT INTO geo_partners VALUES (1043, 'ООО "ГК" Яхонт-Плюс"', 'Красноярск', 'улица Тельмана', ST_PointFromText('POINT(92.973808 56.052512)', 4326));
INSERT INTO geo_partners VALUES (1044, 'ООО "Гевс- Аварийные комиссары"', 'Москва', '1-я Останкинская улица', ST_PointFromText('POINT(37.619019 55.824304)', 4326));
INSERT INTO geo_partners VALUES (1045, 'ООО "Гелиопарк Кайзерхоф"спас', 'Калининград', 'улица Октябрьская', ST_PointFromText('POINT(20.516650 54.701995)', 4326));
INSERT INTO geo_partners VALUES (1046, 'ООО "Гера-Такси"', 'КРАСНОДАР', 'улица Новороссийская', ST_PointFromText('POINT(39.047681 45.036929)', 4326));
INSERT INTO geo_partners VALUES (1047, 'ООО "Гермес-Плюс"', 'Ростов-на-Дону', '', ST_PointFromText('POINT(39.744918 47.227163)', 4326));
INSERT INTO geo_partners VALUES (1048, 'ООО "Гильдия Перевозчиков"', 'Санкт-Петербург', 'Автовская улица', ST_PointFromText('POINT(30.273117 59.871211)', 4326));
INSERT INTO geo_partners VALUES (1049, 'ООО "Глобал Транс"', 'Ульяновск', 'Красноармейская улица', ST_PointFromText('POINT(48.398146 54.328743)', 4326));
INSERT INTO geo_partners VALUES (1050, 'ООО "Городское такси"', 'Подольск', 'улица Правды', ST_PointFromText('POINT(37.562649 55.407709)', 4326));
INSERT INTO geo_partners VALUES (1051, 'ООО "Гостевой дом "', 'Рыбинск ', 'улица Луначарского', ST_PointFromText('POINT(38.834897 58.047102)', 4326));
INSERT INTO geo_partners VALUES (1052, 'ООО "Гостиница "Салют"', 'Орел', 'улица Ленина', ST_PointFromText('POINT(36.064484 52.966992)', 4326));
INSERT INTO geo_partners VALUES (1053, 'ООО "Гостиница "Словакия"', 'Саратов', 'улица Лермонтова', ST_PointFromText('POINT(46.052483 51.527007)', 4326));
INSERT INTO geo_partners VALUES (1054, 'ООО "Гостиница "Царский дворъ"', 'Челябинск', 'Двинская улица', ST_PointFromText('POINT(61.348394 55.181575)', 4326));
INSERT INTO geo_partners VALUES (1055, 'ООО "Гостиница "Южная"', 'Волгоград', 'Рабоче-Крестьянская улица', ST_PointFromText('POINT(44.488516 48.690144)', 4326));
INSERT INTO geo_partners VALUES (1056, 'ООО "Гостиница Авача "', 'город Петропавловск-Камчатский', 'Ленинградская улица', ST_PointFromText('POINT(158.653996 53.035302)', 4326));
INSERT INTO geo_partners VALUES (1057, 'ООО "Гостиница Динамо - Тамбов"', 'Тамбов', 'Красноармейская улица', ST_PointFromText('POINT(41.467761 52.700400)', 4326));
INSERT INTO geo_partners VALUES (1059, 'ООО "Гостиница Ласточка"', 'Пенза', 'улица Мира', ST_PointFromText('POINT(44.982940 53.190062)', 4326));
INSERT INTO geo_partners VALUES (1060, 'ООО "Гостиница Северная"', 'Петрозаводск', 'проспект Ленина', ST_PointFromText('POINT(34.362302 61.789683)', 4326));
INSERT INTO geo_partners VALUES (1061, 'ООО "Гостиничный комплекс "Татарстан"', 'Набережные Челны', 'улица Гидростроителей', ST_PointFromText('POINT(52.293161 55.686264)', 4326));
INSERT INTO geo_partners VALUES (1063, 'ООО "Грузовое такси 771-774"', 'Ижевск', 'Воткинское шоссе, 162', ST_PointFromText('POINT(53.269225 56.890226)', 4326));
INSERT INTO geo_partners VALUES (1064, 'ООО "Группа содействия "Дельта"', 'Москва', 'улица Земляной Вал', ST_PointFromText('POINT(37.656658 55.754304)', 4326));
INSERT INTO geo_partners VALUES (1065, 'ООО "ДТП-СЕРВИС"', 'Пермь', 'улица Монастырская', ST_PointFromText('POINT(56.224188 58.013740)', 4326));
INSERT INTO geo_partners VALUES (1066, 'ООО "Двор Подзноева"', 'Псков', 'улица Некрасова', ST_PointFromText('POINT(28.341075 57.815822)', 4326));
INSERT INTO geo_partners VALUES (1067, 'ООО "Деловой центр"', 'Вологда', 'улица Герцена', ST_PointFromText('POINT(39.899123 59.208995)', 4326));
INSERT INTO geo_partners VALUES (1068, 'ООО "Диадема"', 'Калининград', 'улица Емельянова', ST_PointFromText('POINT(20.561161 54.682216)', 4326));
INSERT INTO geo_partners VALUES (1070, 'ООО "ДорСпас"', 'Балашиха', 'шоссе Энтузиастов М-7', ST_PointFromText('POINT(38.017502 55.802700)', 4326));
INSERT INTO geo_partners VALUES (1071, 'ООО "Дорожная Служба Спасения"', 'Самара', 'улица Ново-вокзальная, 195', ST_PointFromText('POINT(50.217567 53.237677)', 4326));
INSERT INTO geo_partners VALUES (1072, 'ООО "Дорожно-спасательная служба "Автоспас"', 'Барнаул', 'улица Малахова', ST_PointFromText('POINT(83.701488 53.360248)', 4326));
INSERT INTO geo_partners VALUES (1073, 'ООО "Дорожный патруль"', 'Уфа', 'улица Сельская Богородская', ST_PointFromText('POINT(56.101775 54.789945)', 4326));
INSERT INTO geo_partners VALUES (1074, 'ООО "Доставка машин"', 'Москва', 'Шипиловская улица, 64к1', ST_PointFromText('POINT(37.753667 55.622965)', 4326));
INSERT INTO geo_partners VALUES (1075, 'ООО "ЕВРАЗКАР"', 'Новосибирск', 'улица Челюскинцев', ST_PointFromText('POINT(82.906542 55.039620)', 4326));
INSERT INTO geo_partners VALUES (1078, 'ООО "Евроотель"', 'Ставрополь', 'улица Маршала Жукова', ST_PointFromText('POINT(41.970152 45.039693)', 4326));
INSERT INTO geo_partners VALUES (1079, 'ООО "Европкар"', 'Москва', 'проспект Вернадского', ST_PointFromText('POINT(37.511948 55.679783)', 4326));
INSERT INTO geo_partners VALUES (1080, 'ООО "Единая диспетчерская служба"', 'Йошкар-Ола', 'Вознесенская улица', ST_PointFromText('POINT(47.901423 56.636699)', 4326));
INSERT INTO geo_partners VALUES (1081, 'ООО "Желтое такси"', 'Воронеж', 'Краснодонская улица', ST_PointFromText('POINT(39.144430 51.677097)', 4326));
INSERT INTO geo_partners VALUES (1082, 'ООО "ЗАО Владинстар"', 'город Владивосток', 'проспект 100-летия Владивостока', ST_PointFromText('POINT(131.906002 43.146998)', 4326));
INSERT INTO geo_partners VALUES (1083, 'ООО "Изумрудный берег"', 'Новокузнецк', 'улица Орджоникидзе', ST_PointFromText('POINT(87.114843 53.763355)', 4326));
INSERT INTO geo_partners VALUES (1084, 'ООО "Интеграл"', 'Самара', 'Новоурицкая улица', ST_PointFromText('POINT(50.155287 53.181682)', 4326));
INSERT INTO geo_partners VALUES (1085, 'ООО "Интурист-Новгород"', 'Великий Новгород', 'Юрьевское шоссе', ST_PointFromText('POINT(31.262755 58.491725)', 4326));
INSERT INTO geo_partners VALUES (1087, 'ООО "Кармин"', 'город Белгород', 'улица Князя Трубецкого', ST_PointFromText('POINT(36.597104 50.596501)', 4326));
INSERT INTO geo_partners VALUES (1088, 'ООО "Клининговая компания "ПИРАТ"', 'Сочи', '', ST_PointFromText('POINT(39.722271 43.582795)', 4326));
INSERT INTO geo_partners VALUES (1089, 'ООО "Клип"', 'Ставрополь', 'Рылеева переулок', ST_PointFromText('POINT(41.986951 45.049055)', 4326));
INSERT INTO geo_partners VALUES (1090, 'ООО "Клуб автолюбителей Картель"', 'Сургут', '', ST_PointFromText('POINT(73.395952 61.253827)', 4326));
INSERT INTO geo_partners VALUES (1092, 'ООО "Компания ТурСервис Центр"', 'город Якутск', 'улица Ярославского', ST_PointFromText('POINT(129.728998 62.024900)', 4326));
INSERT INTO geo_partners VALUES (1094, 'ООО "КрасАрти"', 'Красноярск', 'улица Александра Матросова', ST_PointFromText('POINT(92.887804 55.983826)', 4326));
INSERT INTO geo_partners VALUES (1095, 'ООО "Кубань Бизнесс"', 'Сыктывкар', 'Почтовая улица', ST_PointFromText('POINT(50.873113 61.653649)', 4326));
INSERT INTO geo_partners VALUES (1096, 'ООО "КузбассИнком-Авто"', 'Кемерово', 'Инициативная улица', ST_PointFromText('POINT(86.030559 55.400941)', 4326));
INSERT INTO geo_partners VALUES (1098, 'ООО "Максима"', 'Тверь', 'улица Дружинная', ST_PointFromText('POINT(35.861878 56.827148)', 4326));
INSERT INTO geo_partners VALUES (1100, 'ООО "Маска"', 'Липецк', 'улица Ленина', ST_PointFromText('POINT(39.732000 52.657442)', 4326));
INSERT INTO geo_partners VALUES (1102, 'ООО "Меркурий-Констракшн"', 'Кемерово', 'улица Дзержинского', ST_PointFromText('POINT(86.074846 55.350755)', 4326));
INSERT INTO geo_partners VALUES (1104, 'ООО "Мотель Авто Джунгли Плюс"', 'Подольск ', 'городской округ Подольск', ST_PointFromText('POINT(37.540883 55.438879)', 4326));
INSERT INTO geo_partners VALUES (1105, 'ООО "Мустанг"', 'Нижний Новгород', 'Красносельская улица', ST_PointFromText('POINT(43.980007 56.308950)', 4326));
INSERT INTO geo_partners VALUES (1106, 'ООО "НИКА"', 'Кемерово', 'Ленинградский проспект', ST_PointFromText('POINT(86.171972 55.350858)', 4326));
INSERT INTO geo_partners VALUES (1107, 'ООО "Наш Оренбург"', 'Оренбург', 'улица Бурзянцева', ST_PointFromText('POINT(55.093937 51.757714)', 4326));
INSERT INTO geo_partners VALUES (1108, 'ООО "Ника"', 'Санкт-Петербург', 'Воронежская улица', ST_PointFromText('POINT(30.347085 59.914570)', 4326));
INSERT INTO geo_partners VALUES (1109, 'ООО "Новый Плюс"', 'Йошкар-Ола', 'улица Строителей', ST_PointFromText('POINT(47.852456 56.624430)', 4326));
INSERT INTO geo_partners VALUES (1110, 'ООО "Отель "Брно"', 'Воронеж', 'Плехановская улица', ST_PointFromText('POINT(39.195840 51.665311)', 4326));
INSERT INTO geo_partners VALUES (1112, 'ООО "Отель-сервис"', 'город Нефтекамск', 'Комсомольский проспект', ST_PointFromText('POINT(54.236899 56.077498)', 4326));
INSERT INTO geo_partners VALUES (1113, 'ООО "ПКФ Партнерство"', 'Москва', '13-й километр', ST_PointFromText('POINT(37.837597 55.667752)', 4326));
INSERT INTO geo_partners VALUES (1115, 'ООО "Партнер-регион"', 'Воронеж', 'улица Фридриха Энгельса', ST_PointFromText('POINT(39.204608 51.668646)', 4326));
INSERT INTO geo_partners VALUES (1116, 'ООО "Перевозчик ДВ"', 'Хабаровск', 'Дежнева переулок', ST_PointFromText('POINT(135.103555 48.499564)', 4326));
INSERT INTO geo_partners VALUES (1117, 'ООО "Перспектива"', 'Москва', 'Ленинградское шоссе', ST_PointFromText('POINT(37.442383 55.883520)', 4326));
INSERT INTO geo_partners VALUES (1118, 'ООО "Пит-Стоп"', 'город Курск', 'улица Карла Маркса', ST_PointFromText('POINT(36.179900 51.772067)', 4326));
INSERT INTO geo_partners VALUES (1119, 'ООО "Престиж Авто"', 'Киров', 'улица Дзержинского', ST_PointFromText('POINT(49.613306 58.633921)', 4326));
INSERT INTO geo_partners VALUES (1120, 'ООО "ПрестижАвтоЛюкс"', 'Вологда', 'улица Зосимовская', ST_PointFromText('POINT(39.897425 59.212929)', 4326));
INSERT INTO geo_partners VALUES (1122, 'ООО "РентаЛайн" (Sixt)', 'Москва', 'Мещанская улица', ST_PointFromText('POINT(37.627616 55.776011)', 4326));
INSERT INTO geo_partners VALUES (1123, 'ООО "Рентакар"', 'Калининград', 'улица Шевченко', ST_PointFromText('POINT(20.513811 54.710953)', 4326));
INSERT INTO geo_partners VALUES (1124, 'ООО "Русь"', 'Вологда', 'улица Воркутинская', ST_PointFromText('POINT(39.907207 59.187646)', 4326));
INSERT INTO geo_partners VALUES (1125, 'ООО "Рязоблтехобслуживание"', 'Рязань', 'Куйбышевское шоссе', ST_PointFromText('POINT(39.767618 54.596728)', 4326));
INSERT INTO geo_partners VALUES (1126, 'ООО "САК ВОА"', 'Казань', 'Ямашева проспект', ST_PointFromText('POINT(49.133552 55.827004)', 4326));
INSERT INTO geo_partners VALUES (1127, 'ООО "СПАС"', 'Казань', 'улица Петербургская', ST_PointFromText('POINT(49.132950 55.781585)', 4326));
INSERT INTO geo_partners VALUES (1128, 'ООО "СТРАХОВАЯ КОМПАНИЯ АСКО-ЛИПЕЦК"', 'Липецк', 'Октябрьская улица', ST_PointFromText('POINT(39.591512 52.601700)', 4326));
INSERT INTO geo_partners VALUES (1129, 'ООО "Север-Юг"', 'Новороссийск', 'улица Прохорова', ST_PointFromText('POINT(37.770852 44.702146)', 4326));
INSERT INTO geo_partners VALUES (1130, 'ООО "Севеста"', 'Тула', 'Ленина проспект', ST_PointFromText('POINT(37.588709 54.166606)', 4326));
INSERT INTO geo_partners VALUES (1131, 'ООО "Сервис-А"', 'Санкт-Петербург', 'Ижорская улица', ST_PointFromText('POINT(30.591139 59.742271)', 4326));
INSERT INTO geo_partners VALUES (1132, 'ООО "СибАвтоЛайнс"', 'Прокопьевск ', 'Комиссаровская улица', ST_PointFromText('POINT(86.757799 53.880698)', 4326));
INSERT INTO geo_partners VALUES (1133, 'ООО "СибСпас"', 'Прокопьевск ', 'Институтская улица', ST_PointFromText('POINT(86.646102 53.866800)', 4326));
INSERT INTO geo_partners VALUES (1134, 'ООО "Славком"', 'Самара', '', ST_PointFromText('POINT(50.191184 53.205226)', 4326));
INSERT INTO geo_partners VALUES (1135, 'ООО "Служба Аварийных Независимых Комиссаров"', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (1136, 'ООО "Служба эвакуации"', 'Челябинск', 'Городская улица', ST_PointFromText('POINT(61.269010 55.183554)', 4326));
INSERT INTO geo_partners VALUES (1137, 'ООО "Служба аварийных комиссаров"', 'Пятигорск', '', ST_PointFromText('POINT(43.025960 44.052845)', 4326));
INSERT INTO geo_partners VALUES (1139, 'ООО "Служба помощи на дорогах"', 'Мурманск ', 'пионерский лагерь имени О.Кошевого', ST_PointFromText('POINT(137.222978 50.632583)', 4326));
INSERT INTO geo_partners VALUES (1140, 'ООО "Соловьиная роща"', 'город Курск', 'улица Ленина', ST_PointFromText('POINT(36.193725 51.739481)', 4326));
INSERT INTO geo_partners VALUES (1143, 'ООО "Спас"', 'Нижний Новгород', 'улица Новикова-Прибоя', ST_PointFromText('POINT(43.914197 56.268181)', 4326));
INSERT INTO geo_partners VALUES (1144, 'ООО "Средне уральская торгово-транспортная компания"', 'город Нижний Тагил', 'улица Орджоникидзе', ST_PointFromText('POINT(60.108297 57.925202)', 4326));
INSERT INTO geo_partners VALUES (1145, 'ООО "Стойлянка"', 'Старый Оскол', 'Комсомольский проспект', ST_PointFromText('POINT(37.799796 51.273599)', 4326));
INSERT INTO geo_partners VALUES (1146, 'ООО "Сыктывкар такси"', 'Сыктывкар', 'Гаражная улица', ST_PointFromText('POINT(50.820912 61.656370)', 4326));
INSERT INTO geo_partners VALUES (1147, 'ООО "ТАНТО-Сервис"', 'Ульяновск', 'проспект Ленинского Комсомола', ST_PointFromText('POINT(48.592290 54.380196)', 4326));
INSERT INTO geo_partners VALUES (1149, 'ООО "ТТЦ" Автомаркет Плюс"', '', 'улица Шоссейная', ST_PointFromText('POINT(39.860962 43.519523)', 4326));
INSERT INTO geo_partners VALUES (1151, 'ООО "Такси 3"', 'Казань', 'улица Рихарда Зорге', ST_PointFromText('POINT(49.202740 55.753488)', 4326));
INSERT INTO geo_partners VALUES (1152, 'ООО "Такси"', 'Новокузнецк', 'проспект Советской Армии', ST_PointFromText('POINT(87.172003 53.833836)', 4326));
INSERT INTO geo_partners VALUES (1155, 'ООО "Таксопарк"', 'город Пятигорск', 'улица Нежнова', ST_PointFromText('POINT(43.045400 44.030003)', 4326));
INSERT INTO geo_partners VALUES (1156, 'ООО "Танто-Сервис"', 'Ульяновск', 'проспект Созидателей', ST_PointFromText('POINT(48.592164 54.369670)', 4326));
INSERT INTO geo_partners VALUES (1157, 'ООО "Твое Новое Такси"', 'Магнитогорск', 'Профсоюзная улица', ST_PointFromText('POINT(59.030022 53.396657)', 4326));
INSERT INTO geo_partners VALUES (1159, 'ООО "Темп АВТО"', 'Самара', 'улица Аэродромная', ST_PointFromText('POINT(50.191273 53.191713)', 4326));
INSERT INTO geo_partners VALUES (1160, 'ООО "Темп Авто"', 'КРАСНОДАР', 'улица Ростовское шоссе', ST_PointFromText('POINT(38.994510 45.106522)', 4326));
INSERT INTO geo_partners VALUES (1161, 'ООО "Техническая экспертиза и оценка"', 'Челябинск', 'Свердловский проспект', ST_PointFromText('POINT(61.387597 55.172326)', 4326));
INSERT INTO geo_partners VALUES (1162, 'ООО "Техцентр Авто-Родео"', 'Омск', 'улица 10 лет Октября', ST_PointFromText('POINT(73.432639 54.987255)', 4326));
INSERT INTO geo_partners VALUES (1163, 'ООО "Транс-Сервис"', 'Кемерово', 'улица Веры Волошиной', ST_PointFromText('POINT(86.082598 55.313106)', 4326));
INSERT INTO geo_partners VALUES (1165, 'ООО "Транспарк"', 'Иркутск', 'Набережная Иркута', ST_PointFromText('POINT(104.257897 52.294644)', 4326));
INSERT INTO geo_partners VALUES (1166, 'ООО "Транспортная компания"', 'Барнаул', 'улица Гридасова', ST_PointFromText('POINT(83.701407 53.332069)', 4326));
INSERT INTO geo_partners VALUES (1167, 'ООО "Триумф-трейд"', 'город Курск', 'улица Карла Маркса', ST_PointFromText('POINT(36.179900 51.772067)', 4326));
INSERT INTO geo_partners VALUES (1168, 'ООО "Туристский комплекс "Россия"', 'Смоленск', 'улица Дзержинского', ST_PointFromText('POINT(32.035720 54.786716)', 4326));
INSERT INTO geo_partners VALUES (1169, 'ООО "УралТехСервис"', 'Екатеринбург', 'улица Мира', ST_PointFromText('POINT(60.650619 56.842817)', 4326));
INSERT INTO geo_partners VALUES (1170, 'ООО "Фаэтон"', 'Новосибирск', 'Железнодорожная улица', ST_PointFromText('POINT(82.899032 55.044784)', 4326));
INSERT INTO geo_partners VALUES (1172, 'ООО "Фокс Групп"', 'Москва', 'проспект Мира', ST_PointFromText('POINT(37.637542 55.809884)', 4326));
INSERT INTO geo_partners VALUES (1174, 'ООО "Форсаж"', 'Тольятти', 'Автостроителей', ST_PointFromText('POINT(49.329043 53.537028)', 4326));
INSERT INTO geo_partners VALUES (1175, 'ООО "Центр Авто Прокат"', 'Набережные Челны', 'Мира проспект', ST_PointFromText('POINT(52.409259 55.744009)', 4326));
INSERT INTO geo_partners VALUES (1176, 'ООО "Центр Автомобильного и водного туризма"', 'Великий Новгород', 'улица Щусева', ST_PointFromText('POINT(31.283973 58.564144)', 4326));
INSERT INTO geo_partners VALUES (1178, 'ООО "Центр независимой экспертизы и оценки"', 'Томск', 'улица Белинского', ST_PointFromText('POINT(84.957863 56.469244)', 4326));
INSERT INTO geo_partners VALUES (1179, 'ООО "Час Пик"', 'Тула', 'улица Никитская', ST_PointFromText('POINT(37.626780 54.193928)', 4326));
INSERT INTO geo_partners VALUES (1180, 'ООО "Черноречье"', 'Дзержинск', 'улица Урицкого', ST_PointFromText('POINT(43.456936 56.231253)', 4326));
INSERT INTO geo_partners VALUES (1181, 'ООО "Шарк"', 'Самара', 'улица Авроры', ST_PointFromText('POINT(50.190941 53.190532)', 4326));
INSERT INTO geo_partners VALUES (1183, 'ООО "Экспертный Страховой Центр"', 'Ярославль', 'улица Салтыкова-Щедрина', ST_PointFromText('POINT(39.861447 57.623306)', 4326));
INSERT INTO geo_partners VALUES (1184, 'ООО "Экспо-Рост"', 'Тольятти', 'Комзина', ST_PointFromText('POINT(49.360736 53.478239)', 4326));
INSERT INTO geo_partners VALUES (1185, 'ООО "Эналс"', 'Мурманск', 'улица Домостроительная', ST_PointFromText('POINT(33.124927 68.994857)', 4326));
INSERT INTO geo_partners VALUES (1190, 'ООО "Юрий"', 'Омск', 'улица Звездова', ST_PointFromText('POINT(73.404989 54.987213)', 4326));
INSERT INTO geo_partners VALUES (1192, 'ООО «NPN-авто»', 'Псков', 'Индустриальная улица', ST_PointFromText('POINT(28.373396 57.825826)', 4326));
INSERT INTO geo_partners VALUES (1194, 'ООО «АНДИ Моторс»', 'Воронеж', 'улица Бахметьева', ST_PointFromText('POINT(39.176095 51.658065)', 4326));
INSERT INTO geo_partners VALUES (1195, 'ООО «Аварком – ДВ»', 'Хабаровск', 'Спортивный переулок', ST_PointFromText('POINT(135.096387 48.449193)', 4326));
INSERT INTO geo_partners VALUES (1198, 'ООО «Автоград-кузовной ремонт»', 'Тюмень', 'улица Республики', ST_PointFromText('POINT(65.581471 57.131712)', 4326));
INSERT INTO geo_partners VALUES (1200, 'ООО «Автомобильное агентство «Вариант»', 'Иваново', 'улица Станкостроителей', ST_PointFromText('POINT(40.952397 56.965687)', 4326));
INSERT INTO geo_partners VALUES (1201, 'ООО «Автосейл»', 'Саратов ', 'Рахова', ST_PointFromText('POINT(18.390652 51.938420)', 4326));
INSERT INTO geo_partners VALUES (1203, 'ООО «Автошкола Таксопарк плюс»', 'Великий Новгород', 'Студенческая улица', ST_PointFromText('POINT(31.298895 58.535871)', 4326));
INSERT INTO geo_partners VALUES (1204, 'ООО «Антонина-Сервис»', 'город Курск', 'улица Энгельса, 173', ST_PointFromText('POINT(36.161799 51.702997)', 4326));
INSERT INTO geo_partners VALUES (1206, 'ООО «Гамболс-Череповец»', 'Череповец', 'улица Химиков', ST_PointFromText('POINT(37.951332 59.124473)', 4326));
INSERT INTO geo_partners VALUES (1208, 'ООО «ЕВРОТАКСИ Бизнес»', 'Челябинск', 'улица Энтузиастов', ST_PointFromText('POINT(61.375910 55.153596)', 4326));
INSERT INTO geo_partners VALUES (1209, 'ООО «Еврокар-Челябинск-Плюс»,', 'Снежинск ', 'улица Нечая', ST_PointFromText('POINT(60.743801 56.073102)', 4326));
INSERT INTO geo_partners VALUES (1211, 'ООО «Единая Служба Аварийных Комиссаров»', 'Тольятти', 'Коммунальная', ST_PointFromText('POINT(49.317572 53.558711)', 4326));
INSERT INTO geo_partners VALUES (1213, 'ООО «Классик Групп»', 'Тюмень', 'улица Ямская', ST_PointFromText('POINT(65.497164 57.161825)', 4326));
INSERT INTO geo_partners VALUES (1214, 'ООО «Кубань Бизнесс»', 'Ростов-на-Дону', 'улица Социалистическая', ST_PointFromText('POINT(39.721894 47.221888)', 4326));
INSERT INTO geo_partners VALUES (1215, 'ООО «Медикэл Менеджмент Групп»/ПОМНАДОР', 'Москва', 'улица Пресненский Вал', ST_PointFromText('POINT(37.565138 55.768466)', 4326));
INSERT INTO geo_partners VALUES (1216, 'ООО «Модус Пятигорск»', 'город Пятигорск', 'улица Ермолова', ST_PointFromText('POINT(42.985797 44.052398)', 4326));
INSERT INTO geo_partners VALUES (1219, 'ООО «Насонов и К»', 'Пермь', 'Комсомольский проспект', ST_PointFromText('POINT(56.249422 57.999826)', 4326));
INSERT INTO geo_partners VALUES (1220, 'ООО «ПИТЛЕЙН ПЛЮС»', 'Кемерово', 'улица Терешковой', ST_PointFromText('POINT(86.136174 55.344940)', 4326));
INSERT INTO geo_partners VALUES (1221, 'ООО «Промкомплекс»', 'Нижний Тагил', '', ST_PointFromText('POINT(60.003383 57.925403)', 4326));
INSERT INTO geo_partners VALUES (1222, 'ООО «Рентур»', 'Иркутск', 'улица Тимирязева', ST_PointFromText('POINT(104.297037 52.282250)', 4326));
INSERT INTO geo_partners VALUES (1223, 'ООО «СИТРОЕН ЦЕНТР»', 'Череповец', 'улица Стройиндустрии', ST_PointFromText('POINT(37.866585 59.121906)', 4326));
INSERT INTO geo_partners VALUES (1224, 'ООО «Саратовавтоспас»', 'Саратов', 'улица Мичурина', ST_PointFromText('POINT(46.040563 51.526480)', 4326));
INSERT INTO geo_partners VALUES (1226, 'ООО «Служба независимых аварийных комиссаров»', 'Волгоград', 'улица Генерала Штеменко', ST_PointFromText('POINT(44.555531 48.775554)', 4326));
INSERT INTO geo_partners VALUES (1227, 'ООО «СтелС»', 'Оренбург', 'Одесская улица', ST_PointFromText('POINT(55.133705 51.788187)', 4326));
INSERT INTO geo_partners VALUES (1228, 'ООО «СтройАвтоСервис»', 'Нижневартовск', '', ST_PointFromText('POINT(76.559459 60.939419)', 4326));
INSERT INTO geo_partners VALUES (1229, 'ООО «СтройКомплект»', 'Нижневартовск', '', ST_PointFromText('POINT(76.559459 60.939419)', 4326));
INSERT INTO geo_partners VALUES (1230, 'ООО «Транссервис»', 'Уфа', 'улица Кирова', ST_PointFromText('POINT(55.967701 54.728212)', 4326));
INSERT INTO geo_partners VALUES (1231, 'ООО «Центр Независимой Экспертизы и Оценки»', 'Ижевск', 'Пушкинская улица', ST_PointFromText('POINT(53.212254 56.850590)', 4326));
INSERT INTO geo_partners VALUES (1234, 'ООО «Элит-авто»', 'Волгоград', 'бульвар 30-летия Победы', ST_PointFromText('POINT(44.499269 48.748751)', 4326));
INSERT INTO geo_partners VALUES (1235, 'ООО Гостиница"Золотая Долина "', 'Новосибирск', 'улица Ильича', ST_PointFromText('POINT(83.099221 54.837089)', 4326));
INSERT INTO geo_partners VALUES (1239, 'ООО Транспортная компания "АвтоКаприз"', 'Екатеринбург', 'Космонавтов проспект', ST_PointFromText('POINT(60.614345 56.900461)', 4326));
INSERT INTO geo_partners VALUES (1241, 'ООО" Крон"', 'Волгоград', 'улица Землянского, 5', ST_PointFromText('POINT(44.534744 48.726666)', 4326));
INSERT INTO geo_partners VALUES (1244, 'ООО"Лада Инвест"', 'Муром ', 'Московская улица', ST_PointFromText('POINT(42.031696 55.574900)', 4326));
INSERT INTO geo_partners VALUES (1245, 'ООО"Просто такси"', 'Калуга', 'улица Карпова', ST_PointFromText('POINT(36.254577 54.508352)', 4326));
INSERT INTO geo_partners VALUES (1246, 'ООО"Союз"', 'Санкт-Петербург', 'Белоостровская улица', ST_PointFromText('POINT(30.320036 59.986628)', 4326));
INSERT INTO geo_partners VALUES (1247, 'ОСАО "Россия"', 'Сыктывкар', 'Первомайская улица', ST_PointFromText('POINT(50.828987 61.666923)', 4326));
INSERT INTO geo_partners VALUES (1248, 'Олимп Сервис ООО', 'Владимир ', 'село Московская', ST_PointFromText('POINT(53.124174 58.825359)', 4326));
INSERT INTO geo_partners VALUES (1249, 'Прищепов А.А. ИП', 'Красноярск', 'улица Заводская', ST_PointFromText('POINT(92.843166 56.021030)', 4326));
INSERT INTO geo_partners VALUES (1251, 'РГУ "Гостиница "Атал"', 'Чебоксары', 'Президентский бульвар', ST_PointFromText('POINT(47.243416 56.128721)', 4326));
INSERT INTO geo_partners VALUES (1252, 'Региональная общественная организация "Городской автоклуб А24"', 'Санкт-Петербург', '', ST_PointFromText('POINT(30.314278 59.938806)', 4326));
INSERT INTO geo_partners VALUES (1253, 'Рослидер 2 OOO', 'Воронеж', 'улица Свободы', ST_PointFromText('POINT(39.185375 51.662909)', 4326));
INSERT INTO geo_partners VALUES (1254, 'Россия', 'Воронеж', 'улица Театральная', ST_PointFromText('POINT(39.209809 51.665697)', 4326));
INSERT INTO geo_partners VALUES (1256, 'Служба аварийных комиссаров НКО ФБД ООО', 'Челябинск', 'улица Кирова', ST_PointFromText('POINT(61.399877 55.171812)', 4326));
INSERT INTO geo_partners VALUES (1257, 'Спас OOO', 'Тольятти', 'Матросова', ST_PointFromText('POINT(49.474400 53.476299)', 4326));
INSERT INTO geo_partners VALUES (1258, 'Спутник', 'Воронеж', 'Московский проспект', ST_PointFromText('POINT(39.181988 51.704097)', 4326));
INSERT INTO geo_partners VALUES (1259, 'Столица', 'город Махачкала', 'Агачаульская улица', ST_PointFromText('POINT(47.470896 42.971200)', 4326));
INSERT INTO geo_partners VALUES (1260, 'ТНЦ СО РАН', 'Томск', 'Академический проспект', ST_PointFromText('POINT(85.042367 56.476334)', 4326));
INSERT INTO geo_partners VALUES (1261, 'Такси "Авангард"', 'Благовещенск', 'Пионерская улица', ST_PointFromText('POINT(127.535851 50.279940)', 4326));
INSERT INTO geo_partners VALUES (1263, 'Томский научный центр Сибирского отделения РАН. Гостиница "Рубин"', 'Томск', 'Академический проспект', ST_PointFromText('POINT(85.042367 56.476334)', 4326));
INSERT INTO geo_partners VALUES (1264, 'Тюменская Областная Организация Общественной Организации "Всероссийское Общество Автомобилистов"', 'Тюмень', 'улица Пермякова', ST_PointFromText('POINT(65.583151 57.121457)', 4326));
INSERT INTO geo_partners VALUES (1265, 'У двух львов', 'Киров', 'Московская улица', ST_PointFromText('POINT(49.633420 58.602962)', 4326));
INSERT INTO geo_partners VALUES (1266, 'Уют', 'Новороссийск', 'улица Корницкого', ST_PointFromText('POINT(37.775631 44.700345)', 4326));
INSERT INTO geo_partners VALUES (1267, 'ООО "Интернет тревел"(Зеленски)', 'Москва', 'проспект Мира', ST_PointFromText('POINT(37.637542 55.809884)', 4326));
INSERT INTO geo_partners VALUES (1283, 'ИнформСити', 'Москва', 'Дербеневская улица, 20с9', ST_PointFromText('POINT(37.647828 55.721392)', 4326));
INSERT INTO geo_partners VALUES (1285, 'ИП Бакин С.Ю.', 'Благовещенск', 'Студенческая улица, 26', ST_PointFromText('POINT(127.515899 50.302178)', 4326));
INSERT INTO geo_partners VALUES (1286, 'Атосиндикат', 'Владивосток', 'Шилкинская улица, 11а', ST_PointFromText('POINT(131.925208 43.121090)', 4326));
INSERT INTO geo_partners VALUES (1288, 'ИП Муст С.Е.', 'Выборг', 'Приморская улица, 11', ST_PointFromText('POINT(28.766786 60.695601)', 4326));
INSERT INTO geo_partners VALUES (1289, 'ИП Гумаров Р.Р.', 'Казань', 'улица Чапаева, 53б', ST_PointFromText('POINT(49.083193 55.871309)', 4326));
INSERT INTO geo_partners VALUES (1290, 'ИП Соколов А.В.', 'Казань', 'улица Чистопольская, 4', ST_PointFromText('POINT(49.102138 55.818331)', 4326));
INSERT INTO geo_partners VALUES (1291, 'ИП Аксенова П.Г.', 'Минеральные воды', 'Красногвардейская улица', ST_PointFromText('POINT(43.097700 44.234497)', 4326));
INSERT INTO geo_partners VALUES (1292, 'Трансхимресурс', 'Кемерово', 'улица Тухачевского, 58в', ST_PointFromText('POINT(86.140405 55.309653)', 4326));
INSERT INTO geo_partners VALUES (1293, 'ИП Капылов Д.А.', 'Брянск', 'улица Тельмана, 66/4', ST_PointFromText('POINT(34.441695 53.251762)', 4326));
INSERT INTO geo_partners VALUES (1294, 'Автогарантсервис', 'Сыктывкар', 'Заводская улица, 84', ST_PointFromText('POINT(50.869313 61.664736)', 4326));
INSERT INTO geo_partners VALUES (1296, 'ИП Миронов А.А.', 'Сургут', 'улица Лермонтова, 11', ST_PointFromText('POINT(73.390086 61.265192)', 4326));
INSERT INTO geo_partners VALUES (1297, 'Торгово-промышленная компания ОРС', 'Саратов', 'Сокурский тракт, 1', ST_PointFromText('POINT(45.970755 51.614861)', 4326));
INSERT INTO geo_partners VALUES (1298, 'Авторешение', 'город Москва', 'Нагорный проезд, 10а', ST_PointFromText('POINT(37.612003 55.686497)', 4326));
INSERT INTO geo_partners VALUES (1299, 'ООО "Экстрим"', 'Коряжма (Котлас)', 'город Коряжма', ST_PointFromText('POINT(47.143496 61.304602)', 4326));
INSERT INTO geo_partners VALUES (1300, 'ИП Фролов С.В.', 'Кострома', 'Кострома городской округ', ST_PointFromText('POINT(40.858802 57.790295)', 4326));
INSERT INTO geo_partners VALUES (1301, 'ИП Тарасов Е.А.', 'Красноярск', 'улица Урванцева, 34', ST_PointFromText('POINT(92.916047 56.065630)', 4326));
INSERT INTO geo_partners VALUES (1302, 'Авто-Доктор', 'Курган', 'улица Ястржембского, 1', ST_PointFromText('POINT(65.329323 55.448694)', 4326));
INSERT INTO geo_partners VALUES (1304, 'ИП Аксёнова П.Г.', 'Минеральные Воды', '', ST_PointFromText('POINT(43.137810 44.199539)', 4326));
INSERT INTO geo_partners VALUES (1305, 'АНО "Безопасность дорожного движения"', 'Казань', 'Оренбургский тракт, 5', ST_PointFromText('POINT(49.154546 55.760408)', 4326));
INSERT INTO geo_partners VALUES (1306, 'партнёр', 'Москва', '', ST_PointFromText('POINT(37.617761 55.755773)', 4326));
INSERT INTO geo_partners VALUES (1307, 'ООО "АвтоТранс"', 'Самара', 'улица Авиационная, 1а', ST_PointFromText('POINT(50.155628 53.184510)', 4326));
INSERT INTO geo_partners VALUES (1308, 'ИП Ахмедов В.Т.О.', 'Владимир', 'улица Безыменского, 6а', ST_PointFromText('POINT(40.449700 56.167348)', 4326));
INSERT INTO geo_partners VALUES (1309, 'ДРК-Альянс', 'Смоленск', '', ST_PointFromText('POINT(31.999095 54.779800)', 4326));
INSERT INTO geo_partners VALUES (1310, 'ИП Хазин А.С.', 'Набережные Челны', '', ST_PointFromText('POINT(52.407911 55.741678)', 4326));
INSERT INTO geo_partners VALUES (1311, 'ИП Шунин А.Г', 'Мурманск', '', ST_PointFromText('POINT(33.077918 68.963254)', 4326));
INSERT INTO geo_partners VALUES (1313, 'ИП Овчаренко А.С.', 'Оренбург', '', ST_PointFromText('POINT(55.097449 51.768060)', 4326));
INSERT INTO geo_partners VALUES (1314, 'Авторейнджер', 'Пенза', '', ST_PointFromText('POINT(45.020121 53.199449)', 4326));

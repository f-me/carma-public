CREATE TABLE "Colors"
  (
    id       SERIAL PRIMARY KEY
  , label    text NOT NULL DEFAULT ''
  , value    text UNIQUE NOT NULL
  );

GRANT ALL ON "Colors" TO carma_db_sync;
GRANT ALL ON "Colors" TO carma_search;

INSERT INTO "Colors" (value, label) VALUES
    ('white', 'Белый')
  , ('black', 'Чёрный')
  , ('grey', 'Серый')
  , ('red', 'Красный')
  , ('orange', 'Оранжевый')
  , ('yellow', 'Желтый')
  , ('green', 'Зелёный')
  , ('blue', 'Голубой')
  , ('darkBlue', 'Синий')
  , ('purple', 'Фиолетовый')
  , ('brown', 'Коричневый')
  , ('golden', 'Золотой')
  , ('silveryMetallic', 'Серебристый металик')
  , ('candy', 'Кэнди')
  , ('deep', 'Дип')
  , ('toffeeBrown', 'Тоффи раун')
  , ('reflex', 'Рефлекс')
  , ('silverLeaf', 'Силвер лиф')
  , ('graphit', 'Графит')
  , ('acapulcoBlue', 'Акапулько блю')
  , ('slate', 'Слайт')
  , ('wildCherry', 'Вайлд черри')
  , ('urano', 'Урано')
  , ('iceland', 'Айслэнд')
  , ('cashmere', 'Кашмир')
  , ('pepperGray', 'Пеппер грэй')
  , ('nightBlue', 'Найт блю')
  , ('shadow', 'Шэдоу')
  , ('moccaAnthracite', 'Мокка антрацит')
  , ('lightBrown', 'Лайт браун')
  , ('hotOrange', 'Хот орандж')
  , ('mocca', 'Мокка')
  , ('iron', 'Айрон')
  , ('tornado', 'Торнадо')
  , ('savanna', 'Саванна')
  , ('united', 'Юнитед')
  , ('moonlight', 'Мунлайт')
  , ('flash', 'Флэш')
  , ('granite', 'Гранит')
  , ('arctic', 'Арктик')
  , ('cobalt', 'Кобальт')
  , ('amaryllis', 'Амариллис')
  , ('shark', 'Шарк')
  , ('seaBlue', 'Си блю')
  , ('carbonSteel', 'Карбон стил')
  , ('acapulco', 'Акапулько')
  , ('uranus', 'Уранус')
  , ('salsa', 'Сальса')
  , ('venetian', 'Венециан')
  , ('india', 'Индия')
  , ('blackOak', 'Блэк оэк')
;

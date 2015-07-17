CREATE TABLE "Colors"
  (
    id       SERIAL PRIMARY KEY
  , label    text UNIQUE NOT NULL CHECK (label <> '')
  );

GRANT ALL ON "Colors" TO carma_db_sync;

INSERT INTO "Colors" (label) VALUES
    ('Белый')
  , ('Чёрный')
  , ('Серый')
  , ('Красный')
  , ('Оранжевый')
  , ('Желтый')
  , ('Зелёный')
  , ('Голубой')
  , ('Синий')
  , ('Фиолетовый')
  , ('Коричневый')
  , ('Золотой')
  , ('Серебристый металик')
  , ('Кэнди')
  , ('Дип')
  , ('Тоффи раун')
  , ('Рефлекс')
  , ('Силвер лиф')
  , ('Графит')
  , ('Акапулько блю')
  , ('Слайт')
  , ('Вайлд черри')
  , ('Урано')
  , ('Айслэнд')
  , ('Кашмир')
  , ('Пеппер грэй')
  , ('Найт блю')
  , ('Шэдоу')
  , ('Мокка антрацит')
  , ('Лайт браун')
  , ('Хот орандж')
  , ('Мокка')
  , ('Айрон')
  , ('Торнадо')
  , ('Саванна')
  , ('Юнитед')
  , ('Мунлайт')
  , ('Флэш')
  , ('Гранит')
  , ('Арктик')
  , ('Кобальт')
  , ('Амариллис')
  , ('Шарк')
  , ('Си блю')
  , ('Карбон стил')
  , ('Акапулько')
  , ('Уранус')
  , ('Сальса')
  , ('Венециан')
  , ('Индия')
  , ('Блэк оэк')
;

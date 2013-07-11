CREATE TABLE "CarModel"
  (id     SERIAL PRIMARY KEY
  ,value  text
  ,label  text NOT NULL
  ,parent int4 REFERENCES "CarMaker" ON DELETE SET NULL
  ,UNIQUE (label, parent)
  );
CREATE UNIQUE INDEX ON "CarModel" (label) WHERE parent IS NULL;
INSERT INTO Dictionary (name, parent)
  SELECT 'CarModel', id
    FROM Dictionary WHERE name = 'CarMaker';

INSERT INTO "CarModel" (value, label, parent)
  SELECT 'alfa147',
         '147',
         id
    FROM "CarMaker" WHERE value = 'alfa';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'alfa166',
         '166',
         id
    FROM "CarMaker" WHERE value = 'alfa';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a2',
         'A2',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a3',
         'A3',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a4',
         'A4',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a6',
         'A6',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a8',
         'A8',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'arnage',
         'Arnage',
         id
    FROM "CarMaker" WHERE value = 'bentley';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'continental',
         'Continental Flying Spur',
         id
    FROM "CarMaker" WHERE value = 'bentley';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'chrysler300',
         '300',
         id
    FROM "CarMaker" WHERE value = 'chrysler';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sebring',
         'Sebring',
         id
    FROM "CarMaker" WHERE value = 'chrysler';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c1',
         'C1',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c2',
         'C2',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'berlingo',
         'Berlingo',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c15',
         'C15',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c3Picasso',
         'C3 Picasso',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c3Pluriel',
         'C3 Pluriel',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c4Aircross',
         'C4 Aircross',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c4Picasso',
         'C4 Picasso',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c8',
         'C8',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cCrosser',
         'C-Crosser',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cZero',
         'C-Zero',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ds3',
         'DS3',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ds4',
         'DS4',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ds5',
         'DS5',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'jumper',
         'Jumper',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'nemo',
         'Nemo',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'saxo',
         'Saxo',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xsaraPicasso',
         'Xsara Picasso',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'zx',
         'ZX',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'evasionJumpy',
         'Evasion/Jumpy',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c3',
         'C3',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c4',
         'C4',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c5',
         'C5',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c6',
         'C6',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xsara',
         'Xsara',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'alero',
         'Alero',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aveo',
         'Aveo',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'beretta',
         'Beretta',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'blazer',
         'Blazer',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'camaro',
         'Camaro',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'capriceCh',
         'Caprice',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'captiva',
         'Captiva',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cavalier',
         'Cavalier',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'celta',
         'Celta',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cheyenne',
         'Cheyenne',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cobaltCh',
         'Cobalt',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'colorado',
         'Colorado',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'wind',
         'Corsa Wind',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'corsica',
         'Corsica',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'corvette',
         'Corvette',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cruze',
         'Cruze',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'epicaCh',
         'Epica',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'evanda',
         'Evanda',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'express',
         'Express',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'orlando',
         'Orlando',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'hhr',
         'HHR',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'impalaSS',
         'Impala SS',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ipanemaGL',
         'Ipanema GL',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'jimmy',
         'Jimmy',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lacetti',
         'Lacetti',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lanosCh',
         'Lanos',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'impala',
         'Impala',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lumina',
         'Lumina',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'malibu',
         'Malibu',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'metro',
         'Metro',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'monteCarlo',
         'Monte Carlo',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'monza',
         'Monza',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'niva',
         'NIVA',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'prism',
         'Prism',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 's10',
         'S-10',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'spark',
         'Spark',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ss',
         'SS',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'suburban',
         'Suburban',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tacuma',
         'Tacuma',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tahoe',
         'Tahoe',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'trackerConv',
         'Tracker Convertible',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'trackerHard',
         'Tracker Hardtop',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'trailBlazer',
         'Trail Blazer',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'Trans',
         'Trans Sport',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'venture',
         'Venture',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'viva',
         'Viva',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'chairman',
         'Chairman',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'espero',
         'Espero',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'matiz',
         'Matiz',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'nexia',
         'Nexia',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tosca',
         'Tosca',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'caliber',
         'Caliber',
         id
    FROM "CarMaker" WHERE value = 'dodge';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'albea',
         'Albea',
         id
    FROM "CarMaker" WHERE value = 'fiat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bravo',
         'Bravo',
         id
    FROM "CarMaker" WHERE value = 'fiat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'panda',
         'Panda',
         id
    FROM "CarMaker" WHERE value = 'fiat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'punto',
         'Punto',
         id
    FROM "CarMaker" WHERE value = 'fiat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'epica',
         'Epica',
         id
    FROM "CarMaker" WHERE value = 'holden';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'statesman',
         'Statesman',
         id
    FROM "CarMaker" WHERE value = 'holden';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'caprice',
         'Caprice',
         id
    FROM "CarMaker" WHERE value = 'holden';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'accord',
         'Accord',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'civic',
         'Civic',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fit',
         'Fit',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'jazz',
         'Jazz',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'legend',
         'Legend',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'logo',
         'Logo',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'i10',
         'i10',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'i20',
         'i20',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'i30',
         'i30',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'centennial',
         'Centennial',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'equus',
         'Equus',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cheryQQ',
         'Chery QQ (Sweet)',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'accent',
         'Accent',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'atos',
         'Atos',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'elantra',
         'Elantra',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'genesis',
         'Genesis',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'getz',
         'Getz',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'santaFe',
         'Santa Fe new',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'solaris',
         'Solaris',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sonata',
         'Sonata',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xg',
         'XG',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sType',
         'S-Type',
         id
    FROM "CarMaker" WHERE value = 'jaguar';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xf',
         'XF',
         id
    FROM "CarMaker" WHERE value = 'jaguar';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xj',
         'XJ',
         id
    FROM "CarMaker" WHERE value = 'jaguar';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xType',
         'X-Type',
         id
    FROM "CarMaker" WHERE value = 'jaguar';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ceed',
         'Cee''d',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cerato',
         'Cerato',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'magentis',
         'Magentis',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'opirus',
         'Opirus',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'picanto',
         'Picanto',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rio',
         'Rio',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'spectra',
         'Spectra',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2104',
         '2104',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2107',
         '2107',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2113Samara',
         '2113 (Samara)',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2114Samara',
         '2114 (Samara)',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2115Samara',
         '2115 (Samara)',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2121Niva',
         '2121 (Niva)',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'granta',
         'Granta',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'kalina',
         'Kalina',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'priora',
         'Priora',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'largus',
         'Largus',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'thesis',
         'Thesis',
         id
    FROM "CarMaker" WHERE value = 'lancia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'defender',
         'Defender',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'discavery',
         'Discavery',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'freelander',
         'Freelander',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'range',
         'Range Rover',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rangeEvoque',
         'Range Rover Evoque',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rangeSport',
         'Range Rover Sport',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'gs',
         'GS',
         id
    FROM "CarMaker" WHERE value = 'lexus';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'is',
         'IS',
         id
    FROM "CarMaker" WHERE value = 'lexus';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ls',
         'LS',
         id
    FROM "CarMaker" WHERE value = 'lexus';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'quattroporte',
         'Quattroporte',
         id
    FROM "CarMaker" WHERE value = 'maserati';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mazda2',
         '2',
         id
    FROM "CarMaker" WHERE value = 'mazda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mazda3',
         '3',
         id
    FROM "CarMaker" WHERE value = 'mazda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mazda6',
         '6',
         id
    FROM "CarMaker" WHERE value = 'mazda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'demio',
         'Demio',
         id
    FROM "CarMaker" WHERE value = 'mazda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aClass',
         'A',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bClass',
         'B',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cClass',
         'C',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'eClass',
         'E',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sClass',
         'S',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cls',
         'CLS',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'colt',
         'Colt',
         id
    FROM "CarMaker" WHERE value = 'mitsubishi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'galant',
         'Galant',
         id
    FROM "CarMaker" WHERE value = 'mitsubishi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lancer',
         'Lancer',
         id
    FROM "CarMaker" WHERE value = 'mitsubishi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'proudia',
         'Proudia',
         id
    FROM "CarMaker" WHERE value = 'mitsubishi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'almera',
         'Almera',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cedric',
         'Cedric',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cima',
         'Cima',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'laurel',
         'Laurel',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'maxima',
         'Maxima',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'micra',
         'Micra',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'murano',
         'Murano',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'note',
         'Note',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'president',
         'President',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'primera',
         'Primera',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'pathfinder',
         'Pathfinder',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'teana',
         'Teana',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xTrail',
         'X-Trail',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tiida',
         'Tiida',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ssangYong',
         'Ssang Yong',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'qashqai',
         'Qashqai',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'agila',
         'Agila',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'antara',
         'Antara',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'astra',
         'Astra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'astragtc',
         'Astra GTC',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'calibra',
         'Calibra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'combo',
         'Combo',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'corsa',
         'Corsa',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'frontera',
         'Frontera',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'insignia',
         'Insignia',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'kadett',
         'Kadett',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'meriva',
         'Meriva',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mokka',
         'Mokka',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'monterey',
         'Monterey',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'movano',
         'Movano',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'omega',
         'Omega',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'signum',
         'Signum',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sintra',
         'Sintra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tigra',
         'Tigra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vectra',
         'Vectra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vita',
         'Vita',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vivaro',
         'Vivaro',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'zafira',
         'Zafira',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'allante',
         'Allante',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bls',
         'BLS',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'brougham',
         'Brougham',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'catera',
         'Catera',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cts',
         'CTS',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ville',
         'DE Ville',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'dts',
         'DTS',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'eldorado',
         'Eldorado',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escalade',
         'Escalade',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fleetwood',
         'Fleetwood',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lse',
         'LSE',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'seville',
         'Seville',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'srx',
         'SRX',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sts',
         'STS',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xlr',
         'XLR',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'caddy',
         'Caddy',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'caravelle',
         'Caravelle',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'amarok',
         'Amarok',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'crafter',
         'Crafter',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 't5',
         'T5',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tiguan',
         'Tiguan',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'polo',
         'Polo',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'touareg',
         'Touareg',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'passat',
         'Passat',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'passatCC',
         'Passat CC',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'jetta',
         'Jetta',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'multivan',
         'Multivan',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'golf',
         'Golf',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'golfPlus',
         'Golf Plus',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sharan',
         'Sharan',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'touran',
         'Touran',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'phaeton',
         'Phaeton',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'eos',
         'Eos',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lupo',
         'Lupo',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'pointer',
         'Pointer',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'transporter',
         'Transporter',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'scirocco',
         'Scirocco',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ford427',
         '427',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aerostar',
         'Aerostar',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cMax',
         'Focus C-Max',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aspire',
         'Aspire',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bronco',
         'Bronco',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sMax',
         'S-Max',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cMaxII',
         'C-Max II',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'contour',
         'Contour',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cougar',
         'Cougar',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'kuga',
         'Kuga',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'crownVictoria',
         'Crown Victoria',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'econoline',
         'Econoline',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escape',
         'Escape',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escort',
         'Escort',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortCabrio',
         'Escort Cabrio',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortClassic',
         'Escort Classic',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortEstate',
         'Escort Estate',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortHatchback',
         'Escort Hatchback',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortTurnier',
         'Escort Turnier',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortZX2',
         'Escort ZX2',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'excursion',
         'Excursion',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'expedition',
         'Expedition',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'explorer',
         'Explorer',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'faction',
         'Faction',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fairlane',
         'Fairlane',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'falconGT',
         'Falcon GT',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fiesta',
         'Fiesta',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'focus',
         'Focus',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fusion',
         'Fusion',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'galaxy',
         'Galaxy',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fordGT',
         'GT',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ikon',
         'Ikon',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ka',
         'Ka',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ltd',
         'LTD',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'maverick',
         'Maverick',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'modelU',
         'Model U',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mondeo',
         'Mondeo',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mustang',
         'Mustang',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'probe',
         'Probe',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'puma',
         'Puma',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ranger',
         'Ranger',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'scorpio',
         'Scorpio',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sierra',
         'Sierra',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'shelbyGR',
         'Shelby GR',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sportKa',
         'SportKa',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'streetKa',
         'StreetKa',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'taurus',
         'Taurus',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'thunderbird',
         'Thunderbird',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tourenoConnect',
         'Tourneo Connect',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'transit',
         'Transit',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'windstar',
         'Windstar',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '1s',
         '1 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '3s',
         '3 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '5s',
         '5 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '6s',
         '6 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '7s',
         '7 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '8s',
         '8 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'm3',
         'M3',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'm5',
         'M5',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'x1',
         'X1',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'x3',
         'X3',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'x5',
         'X5',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'x6',
         'X6',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xActivity',
         'xActivity',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'z1',
         'Z1',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'z3',
         'Z3',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'z4',
         'Z4',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'z8',
         'Z8',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '107',
         '107',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '206',
         '206',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '106',
         '106',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '208',
         '208',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '309',
         '309',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '508',
         '508',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '807',
         '807',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '1007',
         '1007',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '3008',
         '3008',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '4007',
         '4007',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '4008',
         '4008',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '5008',
         '5008',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '206+',
         '206+',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bipper',
         'Bipper',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'boxer',
         'Boxer',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'expert',
         'Expert',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ion',
         'Ion',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'partner',
         'Partner',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rcz',
         'RCZ',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '207',
         '207',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '307',
         '307',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '308',
         '308',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '407',
         '407',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '408',
         '408',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '607',
         '607',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'clio',
         'Clio',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'laguna',
         'Laguna',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'logan',
         'Logan',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'megane',
         'Megane',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'symbol',
         'Symbol',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'twingo',
         'Twingo',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'velSatis',
         'Vel Satis',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'phantom',
         'Phantom',
         id
    FROM "CarMaker" WHERE value = 'rolls';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '111',
         '111',
         id
    FROM "CarMaker" WHERE value = 'rover';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '75',
         '75',
         id
    FROM "CarMaker" WHERE value = 'rover';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'saab93',
         '9-3',
         id
    FROM "CarMaker" WHERE value = 'saab';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'leon',
         'Leon',
         id
    FROM "CarMaker" WHERE value = 'seat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fabia',
         'Fabia',
         id
    FROM "CarMaker" WHERE value = 'skoda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'octavia',
         'Octavia',
         id
    FROM "CarMaker" WHERE value = 'skoda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'superb',
         'Superb',
         id
    FROM "CarMaker" WHERE value = 'skoda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fortwo',
         'Fortwo',
         id
    FROM "CarMaker" WHERE value = 'smart';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'impreza',
         'Impreza',
         id
    FROM "CarMaker" WHERE value = 'subaru';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'legacy',
         'Legacy',
         id
    FROM "CarMaker" WHERE value = 'subaru';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ignis',
         'Ignis',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'liana',
         'Liana',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'swift',
         'Swift',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sx4',
         'SX4',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vitara',
         'Vitara',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'allion',
         'Allion',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'auris',
         'Auris',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'avensis',
         'Avensis',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aygo',
         'Aygo',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'camry',
         'Camry',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'corolla',
         'Corolla',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ist',
         'Ist',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'landCruiser1',
         'Land Cruiser 1',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'landCruiser105',
         'Land Cruiser 105',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mark',
         'Mark II',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'chaser',
         'Chaser',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cresta',
         'Cresta',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rav4',
         'RAV-4',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vitz',
         'Vitz',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'yaris',
         'Yaris',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c30',
         'C30',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 's40',
         'S40',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 's60',
         'S60',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 's80',
         'S80',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'v50',
         'V50',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'h1',
         'H1',
         id
    FROM "CarMaker" WHERE value = 'hum';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'h2',
         'H2',
         id
    FROM "CarMaker" WHERE value = 'hum';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'h3',
         'H3',
         id
    FROM "CarMaker" WHERE value = 'hum';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'shalanda',
         'Shalanda',
         id
    FROM "CarMaker" WHERE value = 'maz';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '5516',
         '5516',
         id
    FROM "CarMaker" WHERE value = 'maz';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '6430A9',
         '6430A9',
         id
    FROM "CarMaker" WHERE value = 'maz';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '4370',
         '4370',
         id
    FROM "CarMaker" WHERE value = 'maz';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bMax',
         'B-Max',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'f250',
         'F250',
         id
    FROM "CarMaker" WHERE value = 'ford';


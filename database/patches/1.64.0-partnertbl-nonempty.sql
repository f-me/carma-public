UPDATE partnertbl
SET name = 'Партнёр №' || id
WHERE name = '';

ALTER TABLE "partnertbl"
ADD CONSTRAINT "partnertbl_name_nonempty"
CHECK (name <> '');

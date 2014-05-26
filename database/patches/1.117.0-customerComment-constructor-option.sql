INSERT INTO "ConstructorFieldOption"
(model, screen, program, ord, field, label, info, required, r, w)
SELECT o.model, o.screen, o.program, o.ord, 'customerComment', 'Неисправность со слов клиента', '', o.required, o.r, o.w
FROM "ConstructorFieldOption" o
WHERE field = 'comment';

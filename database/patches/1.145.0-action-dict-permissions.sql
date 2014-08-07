INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('lovAdmin', 'ActionType', 'label', 'true', 'true');
INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('lovAdmin', 'ActionType', 'desc', 'true', 'true');
INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('lovAdmin', 'ActionType', 'priority', 'true', 'true');

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('lovViewer', 'ActionType', 'label', 'true', 'false');
INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('lovViewer', 'ActionType', 'desc', 'true', 'false');
INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('lovViewer', 'ActionType', 'priority', 'true', 'false');


INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('lovAdmin', 'ActionResult', 'label', 'true', 'true');

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('lovViewer', 'ActionResult', 'label', 'true', 'false');

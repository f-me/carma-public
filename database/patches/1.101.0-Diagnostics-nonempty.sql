ALTER TABLE "Wazzup"
ADD CONSTRAINT "Wazzup_label_nonempty"
CHECK (label <> '');

ALTER TABLE "System"
ADD CONSTRAINT "System_label_nonempty"
CHECK (label <> '');

ALTER TABLE "Part"
ADD CONSTRAINT "Part_label_nonempty"
CHECK (label <> '');

ALTER TABLE "Cause"
ADD CONSTRAINT "Cause_label_nonempty"
CHECK (label <> '');

ALTER TABLE "Suggestion"
ADD CONSTRAINT "Suggestion_label_nonempty"
CHECK (label <> '');

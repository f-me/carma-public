ALTER TABLE "Program"
ADD CONSTRAINT "Program_label_nonempty"
CHECK (label <> '');

ALTER TABLE "SubProgram"
ADD CONSTRAINT "SubProgram_label_nonempty"
CHECK (label <> '');

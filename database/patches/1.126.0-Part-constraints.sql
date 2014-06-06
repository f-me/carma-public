ALTER TABLE "Part" DROP CONSTRAINT "Part_label_key";
ALTER TABLE "Part" ADD UNIQUE (label, parent);

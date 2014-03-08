ALTER TABLE "SubProgramService" DROP COLUMN parent;
ALTER TABLE "SubProgramService" ADD COLUMN parent int4 REFERENCES "SubProgram" NOT NULL;

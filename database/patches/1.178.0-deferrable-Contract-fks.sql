ALTER TABLE "Contract"
DROP CONSTRAINT "Contract_carclass_fkey",
DROP CONSTRAINT "Contract_checktype_fkey",
DROP CONSTRAINT "Contract_committer_fkey",
DROP CONSTRAINT "Contract_enginetype_fkey",
DROP CONSTRAINT "Contract_lastcheckdealer_fkey",
DROP CONSTRAINT "Contract_legalform_fkey",
DROP CONSTRAINT "Contract_make_fkey",
DROP CONSTRAINT "Contract_model_fkey",
DROP CONSTRAINT "Contract_seller_fkey",
DROP CONSTRAINT "Contract_subprogram_fkey",
DROP CONSTRAINT "Contract_transmission_fkey";


ALTER TABLE "Contract"
ADD CONSTRAINT "Contract_carclass_fkey" FOREIGN KEY (carclass) REFERENCES "CarClass"(id) DEFERRABLE,
ADD CONSTRAINT "Contract_checktype_fkey" FOREIGN KEY (checktype) REFERENCES "CheckType"(id) DEFERRABLE,
ADD CONSTRAINT "Contract_committer_fkey" FOREIGN KEY (committer) REFERENCES usermetatbl(id) DEFERRABLE,
ADD CONSTRAINT "Contract_enginetype_fkey" FOREIGN KEY (enginetype) REFERENCES "Engine"(id) DEFERRABLE,
ADD CONSTRAINT "Contract_lastcheckdealer_fkey" FOREIGN KEY (lastcheckdealer) REFERENCES partnertbl(id) DEFERRABLE,
ADD CONSTRAINT "Contract_legalform_fkey" FOREIGN KEY (legalform) REFERENCES "LegalForm"(id) DEFERRABLE,
ADD CONSTRAINT "Contract_make_fkey" FOREIGN KEY (make) REFERENCES "CarMake"(id) DEFERRABLE,
ADD CONSTRAINT "Contract_model_fkey" FOREIGN KEY (model) REFERENCES "CarModel"(id) DEFERRABLE,
ADD CONSTRAINT "Contract_seller_fkey" FOREIGN KEY (seller) REFERENCES partnertbl(id) DEFERRABLE,
ADD CONSTRAINT "Contract_subprogram_fkey" FOREIGN KEY (subprogram) REFERENCES "SubProgram"(id) DEFERRABLE,
ADD CONSTRAINT "Contract_transmission_fkey" FOREIGN KEY (transmission) REFERENCES "Transmission"(id) DEFERRABLE;

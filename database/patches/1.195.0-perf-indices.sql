-- Speed up /actionsFor and /backoffice/caseActions queries
CREATE INDEX ON "actiontbl" (caseid);
CREATE INDEX ON "actiontbl" (result);

-- Speed up #rkc screen
CREATE INDEX ON actiontbl (duetime);
CREATE INDEX ON towagetbl (createTime);
CREATE INDEX ON techtbl (createTime);
CREATE INDEX ON renttbl (createTime);

-- Speed up <>LoggedOut user selection in assignQ
CREATE INDEX ON "UserState" (userid, id DESC);

DROP INDEX "Contract_buydate_idx1";
DROP INDEX "Contract_carclass_idx1";
DROP INDEX "Contract_cardnumber_idx2";
DROP INDEX "Contract_checkperiod_idx1";
DROP INDEX "Contract_checktype_idx1";
DROP INDEX "Contract_codeword_idx2";
DROP INDEX "Contract_color_idx1";
DROP INDEX "Contract_comment_idx1";
DROP INDEX "Contract_email_idx2";
DROP INDEX "Contract_enginetype_idx1";
DROP INDEX "Contract_enginevolume_idx1";
DROP INDEX "Contract_lastcheckdealer_idx1";
DROP INDEX "Contract_legalform_idx1";
DROP INDEX "Contract_make_idx1";
DROP INDEX "Contract_makeyear_idx1";
DROP INDEX "Contract_managername_idx1";
DROP INDEX "Contract_model_idx1";
DROP INDEX "Contract_name_idx2";
DROP INDEX "Contract_ordernumber_idx1";
DROP INDEX "Contract_phone_idx2";
DROP INDEX "Contract_platenum_idx2";
DROP INDEX "Contract_seller_idx1";
DROP INDEX "Contract_startmileage_idx1";
DROP INDEX "Contract_subprogram_idx1";
DROP INDEX "Contract_transmission_idx1";
DROP INDEX "Contract_validsince_idx1";
DROP INDEX "Contract_validuntil_idx1";
DROP INDEX "Contract_vin_idx2";

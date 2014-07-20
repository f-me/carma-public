DROP INDEX partnertbl_isactive_idx;
CREATE INDEX partnertbl_isactive_idx ON partnertbl USING btree (isactive);

DROP INDEX actiontbl_caseid_idx;
CREATE INDEX actiontbl_caseid_idx ON actiontbl USING btree (caseid);

DROP INDEX actiontbl_targetgroup_idx;
CREATE INDEX actiontbl_targetgroup_idx ON actiontbl USING btree (targetgroup);

DROP INDEX calltbl_callername_phone1_idx;
CREATE INDEX calltbl_callername_phone1_idx ON calltbl USING btree (callername_phone1);

DROP INDEX partnertbl_city_idx;
CREATE INDEX partnertbl_city_idx ON partnertbl USING btree (city) WHERE (isactive = true);

DROP INDEX partnertbl_isdealer_idx;
CREATE INDEX partnertbl_isdealer_idx ON partnertbl USING btree (isdealer) WHERE (isactive = true);

DROP INDEX servicetbl_urgentservice_idx;
CREATE INDEX servicetbl_urgentservice_idx ON servicetbl USING btree (urgentservice);

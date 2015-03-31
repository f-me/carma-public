CREATE TABLE "CaseComment"
  ( id    SERIAL PRIMARY KEY
  , caseId int4 REFERENCES casetbl NOT NULL
  , ctime timestamptz NOT NULL DEFAULT now()
  , author int4 REFERENCES usermetatbl NOT NULL
  , comment text NOT NULL CHECK (comment <> '')
  );

GRANT ALL ON "CaseComment" TO carma_db_sync;
GRANT ALL ON "CaseComment" TO carma_search;
GRANT ALL ON "CaseComment_id_seq" TO carma_db_sync;
GRANT ALL ON "CaseComment_id_seq" TO carma_search;

CREATE INDEX ON "CaseComment" (caseid);
CREATE INDEX ON "CaseComment" (author);

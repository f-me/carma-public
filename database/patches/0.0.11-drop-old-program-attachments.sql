UPDATE programtbl SET contracts=NULL WHERE
(
contracts IS NOT NULL AND
NOT contracts like 'attachment:%' AND
contracts <> ''
);
UPDATE programtbl SET logo=NULL WHERE
(
logo IS NOT NULL AND
NOT logo like 'attachment:%' AND
logo <> ''
);
UPDATE servicetbl SET files=NULL WHERE
(
files IS NOT NULL AND
NOT files like 'attachment:%' AND
files <> ''
);

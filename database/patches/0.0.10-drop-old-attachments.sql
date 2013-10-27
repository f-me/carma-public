UPDATE casetbl SET files=NULL WHERE
(
files IS NOT NULL AND
NOT files like 'attachment:%' AND
files <> ''
)

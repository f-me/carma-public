#!/bin/bash

DB_NAME=${1:-carma}
MESSAGE=`
psql --html $DB_NAME << EOF
SELECT
  replace(caseid, 'case:', 'http://carma:8000/#case/') as "URL",
  split_part(caseid, ':', 2) as "Номер кейса",
  assignedto as "Оператор",
  date_trunc('seconds', localtimestamp - ('5 minutes'::interval + ctime)) as "Просрочено",
  "ActionName".label as "Тип действия",
  duetime as "Запланировано",
  "City".label as "Город",
  programtbl.label as "Программа"
FROM
  actiontbl
LEFT JOIN "ActionName" ON actiontbl.name = "ActionName".value
INNER JOIN casetbl ON split_part(actiontbl.caseid, ':', 2)::int = casetbl.id
LEFT JOIN "City" ON casetbl.city = "City".value
LEFT JOIN programtbl ON casetbl.program = programtbl.value

WHERE
  (localtimestamp > ('5 minutes'::interval + ctime) AND assigntime IS NULL)
  OR
  (name = ANY ('{orderService, callMeMaybe, orderServiceAnalyst}')
   AND (assigntime IS NOT NULL AND closetime IS NULL)
   AND localtimestamp > ('15 minutes'::interval + assigntime))
EOF
`

SUBJECT="[Действия] - Сообщение от CaRMa"
EMAIL_TO="supervisor@ruamc.ru"
EMAIL_FROM="carma@ruamc.ru"

/usr/sbin/sendmail "${EMAIL_FROM}" << EOF
From: $EMAIL_FROM
To: $EMAIL_TO
Subject: $SUBJECT
MIME-Version: 1.0
Content-Type: text/html
Content-Disposition: inline

$MESSAGE

EOF


#!/bin/bash

DB_NAME=${1:-carma}

SUBJECT="[Действия] - Сообщение от CaRMa"
EMAIL_TO="supervisor@ruamc.ru, robots@formalmethods.ru"
EMAIL_FROM="carma@carma.ruamc.ru"
EMAIL_SENDER="psa@carma.ruamc.ru"

run_query () {
  psql \
    -t \
    --no-align \
    --field-separator ';' \
    --quiet \
    $DB_NAME | while IFS=';' read -ra Record; do
      echo "<tr>"
      for i in "${Record[@]}"; do
        echo "<td>$i</td>"
      done
      echo "</tr>"
    done
}

build_table () {
  cat << EOF
    <table width=100% border="1" style="border-collapse:collapse;"
      cellpadding="2" cellspacing="2">
      <thead>
        <tr>
          <th> URL </th>
          <th> Номер кейса </th>
          <th> Оператор </th>
          <th> Просрочено </th>
          <th> Тип действия </th>
          <th> Запланировано </th>
          <th> Город </th>
          <th> Программа </th>
        </tr>
      </thead>
      <tbody>
        $1
      </tbody>
    </table>
EOF
}

send_message () {
/usr/sbin/sendmail -t -r $EMAIL_SENDER << EOF
From: $EMAIL_FROM
To: $EMAIL_TO
Subject: $SUBJECT
MIME-Version: 1.0
Content-Type: text/html
Content-Disposition: inline

$1

EOF
}

unassigned () {
run_query <<EOF
  SELECT
    'http://carma:8000/#case/' || caseid,
    caseid,
    u.realName,
    date_trunc('seconds', now() - ctime),
    "ActionType".label,
    duetime,
    "City".label,
    "Program".label
  FROM
    actiontbl
  LEFT JOIN "ActionType" ON actiontbl.type = "ActionType".id
  INNER JOIN casetbl ON actiontbl.caseid = casetbl.id
  LEFT JOIN "City" ON casetbl.city = "City".id
  LEFT JOIN "Program" ON casetbl.program = "Program".id
  LEFT JOIN usermetatbl u ON assignedto = u.id
  WHERE TRUE
    AND result IS NULL
    AND ctime > now()::date - 7
    AND now() > duetime
    AND type = ANY ('{1, 20, 17, 19}')
    AND assigntime IS NULL
    AND now() > ('5 minutes'::interval + ctime);
EOF
}


outstanding () {
run_query <<EOF
  SELECT
    'http://carma:8000/#case/' || caseid,
    caseid,
    u.realName,
    date_trunc('seconds', now() - assigntime),
    "ActionType".label,
    duetime,
    "City".label,
    "Program".label
  FROM
    actiontbl
  LEFT JOIN "ActionType" ON actiontbl.type = "ActionType".id
  INNER JOIN casetbl ON actiontbl.caseid = casetbl.id
  LEFT JOIN "City" ON casetbl.city = "City".id
  LEFT JOIN "Program" ON casetbl.program = "Program".id
  LEFT JOIN usermetatbl u ON assignedto = u.id
  WHERE TRUE
    AND result IS NULL
    AND ctime > now()::date - 7
    AND now() > ('10 minutes'::interval + duetime)
    AND type = ANY ('{1, 20, 17, 19}')
    AND (assigntime IS NOT NULL AND closetime IS NULL)
    AND now() > ('15 minutes'::interval + assigntime);
EOF
}

UNASSIGNED_RESULT=$(unassigned)
OUTSTANDING_RESULT=$(outstanding)
MESSAGE=""

if [[ ! -z "$UNASSIGNED_RESULT" ]]
then
  UNASSIGNED_TABLE=$(build_table "$UNASSIGNED_RESULT")
  MESSAGE="$MESSAGE <h3>Нераспределенные действия</h3> $UNASSIGNED_TABLE"
fi

if [[ ! -z "$OUTSTANDING_RESULT" ]]
then
  OUTSTANDING_TABLE=$(build_table "$OUTSTANDING_RESULT")
  MESSAGE="$MESSAGE <h3>Невыполненные действия</h3> $OUTSTANDING_TABLE"
fi

if [[ ! -z "$MESSAGE" ]]
then
  send_message "$MESSAGE"
fi

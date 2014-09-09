#!/usr/bin/env bash

EMAIL_TO="robots@formalmethods.ru"
EMAIL_FROM="carma@carma.ruamc.ru"
EMAIL_SENDER="carma@carma.ruamc.ru"


send_message () {
/usr/sbin/sendmail -t -r $EMAIL_SENDER << EOF
From: $EMAIL_FROM
To: $EMAIL_TO
Subject: $1
MIME-Version: 1.0
Content-Type: text/plain
Content-Disposition: inline

$2

EOF
}

delete_duplicates () {
  psql carma << EOF
    with
      svcs as
        (select id as case_id, regexp_matches(services,'(\w+):(\d+)', 'g') as svc
          from casetbl)
      delete
        from servicetbl s
        where not exists
          (select 1 from svcs p
            where p.case_id = s.parentId
              and p.svc[2] :: int = s.id)
       returning s.id, s.type, s.parentId;
EOF
}

RESULT=`delete_duplicates`
COUNT=`echo "$RESULT" | tail -n 1`
if [[ "$COUNT" == 'DELETE 1' ]] ; then
  send_message "$COUNT service duplicate" "$RESULT"
elif [[ "$COUNT" != 'DELETE 0' ]] ; then
  send_message "$COUNT service duplicates" "$RESULT"
fi

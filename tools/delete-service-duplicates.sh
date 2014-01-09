#!/usr/bin/env bash

EMAIL_TO="support@formalmethods.ru"
EMAIL_FROM="carma@ruamc.ru"
EMAIL_SENDER="psa@ruamc.ru"


send_message () {
/usr/sbin/sendmail -t -r $EMAIL_SENDER << EOF
From: $EMAIL_FROM
To: $EMAIL_TO
Subject: $1
MIME-Version: 1.0
Content-Type: text/html
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
            where 'case:' || p.case_id = s.parentId
              and p.svc[2] :: int = s.id
              and p.svc[1] = s.type)
       returning s.id, s.type, s.parentId;
EOF
}

RESULT=`delete_duplicates`
COUNT=`echo "$RESULT" | tail -n 1`
if [[ "$COUNT" != 'DELETE 0' ]] ; then
  send_message "$COUNT service duplicates" "$RESULT"
fi


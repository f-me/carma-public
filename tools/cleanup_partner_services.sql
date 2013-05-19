UPDATE partnertbl set services = NULL
WHERE services !~ '^partner_service'
AND services is not null
AND services != '';

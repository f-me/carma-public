UPDATE actiontbl SET targetGroup = r.id FROM "Role" r
WHERE targetGroup = r.value;

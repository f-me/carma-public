drop index if exists "casetbl_contractidentifier_idx";
create
  index "casetbl_contractidentifier_idx"
  on casetbl(lower(contractidentifier))
  where length(contractidentifier) > 4;


local fields = {
  'comment', 'diagnosis1', 'diagnosis2',
  'diagnosis3', 'diagnosis4'}
local res = {}

for _,k in pairs(redis.call('keys', 'case:*')) do
  local case = redis.call('hmget', k,
      'comment', 'diagnosis1', 'diagnosis2',
      'diagnosis3', 'diagnosis4')

  local json = {id = k}
  for i,f in pairs(fields) do
    if case[i] then
      json[f] = case[i]
    end
  end
  table.insert(res, cjson.encode(json))
end

return res


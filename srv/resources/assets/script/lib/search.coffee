simpleFuzzySearch = (query, entry) ->
  query = query.toLowerCase()
  entry = entry.toLowerCase()
  (word.trim() for word in query.split /\s+/g)
    .every (word) -> ~ entry.indexOf word

module.exports = {simpleFuzzySearch}

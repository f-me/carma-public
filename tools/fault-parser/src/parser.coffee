path = require 'path'
fs = require 'fs'
csv = do require 'csv'

start = (input, prefix = 'fp', tabSize = 4) ->
  inCsv = fs.createReadStream input
  basename = path.basename input, '.csv'

  outTypesName = "#{path.dirname input}/#{basename}Type.json"
  outTypes = fs.createWriteStream outTypesName

  outDetailsName = "#{path.dirname input}/#{basename}Detail.json"
  outDetails = fs.createWriteStream outDetailsName

  csv.from inCsv

  # accumulators and counters
  # ..for types
  types =
    entries: []
  typeCounter = 0

  # ..for details
  details =
    entries: {}
  detailCounter = 0

  csv.on 'record', (row, index) ->
    # skip first row, wich contains columns names
    if index isnt 0
      # if row contains types definition
      if row[0]
        typeCounter++
        type =
          value: "#{prefix}#{typeCounter}"
          label: row[0]
        types.entries.push type

        # add type to details entries
        # will be a parent for next details
        details.entries[type.value] = []
        # reset childs counter
        detailCounter = 0

      # add details
      detailCounter++
      detail =
        value: "#{prefix}#{typeCounter}.#{detailCounter}"
        label: row[1]
      parentType = types.entries.slice(-1)[0].value
      details.entries[parentType].push detail

  inCsv.on 'end', ->
    outTypes.end "#{JSON.stringify types, null, tabSize}"
    outDetails.end "#{JSON.stringify details, null, tabSize}"

  csv.on 'end', (count) ->
    console.info "# parsed lines: #{count}"

  csv.on 'error', (error) ->
    console.error error

exports.start = start

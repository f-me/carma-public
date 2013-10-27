parser = require './parser'

do ->
  csv = process.argv[2]
  prefix = process.argv[3] if process.argv[3]
  tabSize = parseInt process.argv[4] if process.argv[4]

  if csv
    parser.start csv, prefix, tabSize


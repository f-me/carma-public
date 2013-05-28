// Generated by CoffeeScript 1.6.2
(function() {
  var csv, fs, path, start;

  path = require('path');

  fs = require('fs');

  csv = require('csv')();

  start = function(input, prefix, tabSize) {
    var basename, detailCounter, details, inCsv, outDetails, outDetailsName, outTypes, outTypesName, typeCounter, types;

    if (prefix == null) {
      prefix = 'fp';
    }
    if (tabSize == null) {
      tabSize = 4;
    }
    inCsv = fs.createReadStream(input);
    basename = path.basename(input, '.csv');
    outTypesName = "" + (path.dirname(input)) + "/" + basename + "Type.json";
    outTypes = fs.createWriteStream(outTypesName);
    outDetailsName = "" + (path.dirname(input)) + "/" + basename + "Detail.json";
    outDetails = fs.createWriteStream(outDetailsName);
    csv.from(inCsv);
    types = {
      entries: []
    };
    typeCounter = 0;
    details = {
      entries: {}
    };
    detailCounter = 0;
    csv.on('record', function(row, index) {
      var detail, parentType, type;

      if (index !== 0) {
        if (row[0]) {
          typeCounter++;
          type = {
            value: "" + prefix + typeCounter,
            label: row[0]
          };
          types.entries.push(type);
          details.entries[type.value] = [];
          detailCounter = 0;
        }
        detailCounter++;
        detail = {
          value: "" + prefix + typeCounter + "." + detailCounter,
          label: row[1]
        };
        parentType = types.entries.slice(-1)[0].value;
        return details.entries[parentType].push(detail);
      }
    });
    inCsv.on('end', function() {
      outTypes.end("" + (JSON.stringify(types, null, tabSize)));
      return outDetails.end("" + (JSON.stringify(details, null, tabSize)));
    });
    csv.on('end', function(count) {
      return console.info("# parsed lines: " + count);
    });
    return csv.on('error', function(error) {
      return console.error(error);
    });
  };

  exports.start = start;

}).call(this);

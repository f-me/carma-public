
fs = require('fs')

var config =
  [{file: "ActionNames"
   ,name: "ActionName"
   }
  ,{file: "ActionResults"
   ,name: "ActionResult"
   ,parent: "ActionName"
   }
  ,{file: "CallerTypes"
   ,name: "CallerType"
   }
  ,{file: "CallTypes"
   ,name: "CallType"
   ,parent: "CallerType"
   }
  ,{file: "CarMakers"
   ,name: "CarMaker"
   }
  ,{file: "CarModels"
   ,name: "CarModel"
   ,parent: "CarMaker"
   }
  ,{file: "DealerCities"
   ,name: "City"
   }
  ]

function main() {
  var srcDir = process.argv[3] || '.'
  // var files  = fs.readdirSync(srcDir)
  console.log(
    'CREATE TABLE Dictionary'
    + '\n  (id          SERIAL PRIMARY KEY'
    + '\n  ,name        text UNIQUE NOT NULL'
    + '\n  ,description text'
    + '\n  ,parent      int4 REFERENCES Dictionary'
    + '\n  );\n')
  config.forEach(function (d){
    fs.readFile(srcDir + '/' + d.file + '.json', 'utf8', function(err,data) {
      var sql = json2sql(d, JSON.parse(data))
      if (process.argv[2] === 'scheme') {
        console.log(sql.tbl)
      }
      else if (process.argv[2] === 'data')
        console.log(sql.dat.join('\n'))
    })
  })
}


function json2sql(d, data){
  var rows = d.parent ? flatten(data.entries) : data.entries
  if (d.parent) return {
    tbl: '\nCREATE TABLE "' + d.name + '"'
      +  '\n  (id     SERIAL PRIMARY KEY'
      +  '\n  ,value  text'
      +  '\n  ,label  text NOT NULL'
      +  '\n  ,parent int4 REFERENCES "' + d.parent + '" ON DELETE SET NULL'
      +  '\n  ,UNIQUE (label, parent)'
      +  '\n  );'
      +  '\nCREATE UNIQUE INDEX ON "' + d.name + '" (label) WHERE parent IS NULL;'
      +  '\nINSERT INTO Dictionary (name, parent)'
      +  '\n  SELECT ' + q(d.name) + ', id'
      +  '\n    FROM Dictionary WHERE name = ' + q(d.parent) + ';',
    dat: rows.map(function(r){
      return 'INSERT INTO "' + d.name + '" (value, label, parent)'
           + '\n  SELECT ' + q(r.value) + ','
           + '\n         ' + q(r.label) + ','
           + '\n         id'
           + '\n    FROM "' + d.parent + '" WHERE value = ' + q(r.parent) + ';'
      })
  }
  else return {
    tbl: '\nCREATE TABLE "' + d.name + '"'
      +  '\n  (id    SERIAL PRIMARY KEY'
      +  '\n  ,value text'
      +  '\n  ,label text UNIQUE NOT NULL'
      +  '\n  );'
      +  '\nINSERT INTO Dictionary (name) VALUES (' + q(d.name) + ');',
    dat: rows.map(function(r){
      return 'INSERT INTO "' + d.name + '"' + ' (value, label) VALUES'
           + '\n (' + q(r.value)
           + '\n ,' + q(r.label) + ');'
      })
  }
}

function q(x) {
  return "'" + x.replace(/'/g, "''") + "'"
}

function flatten(data) {
  var res = [];
  for(var parent in data) {
    data[parent].forEach(function(v) {
      res.push({
        value: v.value,
        label: v.label,
        parent: parent})
    })
  }
  return res;
}


main()

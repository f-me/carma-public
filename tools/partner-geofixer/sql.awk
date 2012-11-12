BEGIN {
    FS="|"
    print "DROP TABLE IF EXISTS geo_partners;"
    print "CREATE TABLE geo_partners (id INTEGER PRIMARY KEY, name TEXT, city TEXT, address TEXT);"
    print "SELECT AddGeometryColumn ('geo_partners','coords',4326,'POINT',2);"
}

{
    # Skip ungeocoded columns
    if (($5 != "") && ($6 != "")) {
        print "INSERT INTO geo_partners VALUES ("$1", '"$2"', '"$3"', '"$4"', ST_PointFromText('POINT("$5" "$6")', 4326));"
    }
}

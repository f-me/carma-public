BEGIN {
    FS = "|"
    CURL = "curl -s -X PUT "
    EC = ENVIRON["CARMA_PORT"]
    CARMA_PORT = EC ? EC : "8000"
    CARMA_CITY_PUT = "http://localhost:"CARMA_PORT"/_/city/"
}

{
    # Skip ungeocoded columns
    if (($3 != "") && ($4 != "")) {
        print CURL CARMA_CITY_PUT $1 "/ --data '{\"coords\":\""$3","$4"\", \"timezone\":\""$5"\"}'"
    }
}

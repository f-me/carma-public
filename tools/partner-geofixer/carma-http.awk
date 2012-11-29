BEGIN {
    FS="|"
    CURL="curl -X PUT "
    CARMA_PARTNER_PUT="http://localhost:8000/_/partner/"
}

{
    # Skip ungeocoded columns
    if (($5 != "") && ($6 != "")) {
        if ($4 != "") {
            addr = "\"addrDeFacto\":\""$4"\","
        }
        else
        {
            addr = ""
        }
        print CURL CARMA_PARTNER_PUT $1 "/ --data '{\"city\":\""$3"\","addr"\"coords\":\""$5","$6"\"}'"
    }
}

// Search main view
function setupSearchTable(viewName, args) {
    $el(viewName).html($el("search-table-template").html());
    global.viewsWare[viewName] = {};
    
    // Show table controls with form-inline
    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline";
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline";

    $el("searchtable").dataTable({
        aoColumnDefs: [{
            // Render case id as link to case page
            fnRender: function (o, val) {
                return "<a href=\"/#case/" + val + "\">" + val + "</a>";
            },
            aTargets: [0]
        }],
        oLanguage: {
            sSearch: "Фильтр",
            oPaginate: {
                sNext: "Вперёд",
                sPrevious: "Назад"
            },
            sInfo: "Показаны записи с _START_ по _END_ (всего _TOTAL_)",
            sInfoEmpty: "",
            sLengthMenu: "Показывать по _MENU_ записей",
            sZeroRecords: "Ничего не найдено"
        }});

    $("#search-datepicker").data('date', getFormatDate());
}

// Manually load JSON data from server and add it to table
//
// TODO Allow adjusting of search fields etc.
function doSearch() {
    var searchFields = ["id", "caller_ownerName", "callDate",
                        "caller_phone", "car_plateNum", "program"];
    var serveFields = searchFields;
    var sType = "or"
    var limit = "100";

    var t = $el("searchtable").dataTable();
    var term = $el("table-query").val();
    var method = "search/?";
    for (f in searchFields) {
        method += (searchFields[f] + "=" + term + "&");
    };
    method += "_matchType=s&_searchType=" + sType 
        + "&_limit=" + limit
        + "&_fields=" + serveFields.join(",");
    $.getJSON(modelMethod("case", method),
              function(results) {
                  t.fnClearTable();
                  t.fnAddData(results);
              });
}

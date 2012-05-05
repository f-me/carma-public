// Search main view
function setupSearchTable(viewName, args) {
    $el(viewName).html($el("search-table-template").html());
    global.viewsWare[viewName] = {};
    
    // Show table controls with form-inline
    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline";
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline";

    $el("searchtable").dataTable({
        sScrollY: "400px",
        iDisplayLength: 50,
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
}


// Manually load JSON data from server and add it to table
//
// TODO Allow adjusting of search fields etc.
function doSearch() {
    var serveFields = ["id", "caller_name", "callDate",
                       "caller_phone1", "car_plateNum", "program"];
    var sType = "or"
    var limit = "100";

    var t = $el("searchtable").dataTable();
    var method = "search/?q=" + $el("table-query").val();
    method += "&_matchType=s&_searchType=" + sType 
        + "&_limit=" + limit
        + "&_fields=" + serveFields.join(",");
    $.getJSON(modelMethod("case", method),
              function(results) {
                  t.fnClearTable();
                  t.fnAddData(results);
              });
}

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
}

// Manually load JSON data from server and add it to table
//
// TODO Allow adjusting of search fields etc.
function doSearch() {
    var fields = ["id", "ownerName", "callDate", "phone", "plateNum", "program"];
    var sType = "or"
    var limit = "100";

    var t = $el("searchtable").dataTable();
    var term = $el("table-query").val();
    var method = "search/?";
    for (f in fields) {
        method += (fields[f] + "=" + term + "&");
    };
    method += "_matchType=s&_searchType=" + sType + "&_limit=" + limit;
    $.getJSON(modelMethod("case", method),
              function(results) {
                  var data = []
                  for (i = 0; i < results.length; i++) {
                      var o = results[i];
                      var row = [];
                      // Extract fields data from JSON, show dash in
                      // case of undefined field
                      for (j = 0; j < fields.length; j++) {
                          if (_.isUndefined(o[fields[j]]))
                              row[j] = "—";
                          else
                              row[j] = o[fields[j]];
                      }
                      data[i] = row;
                  }
                  t.fnClearTable();
                  t.fnAddData(data);
              });
}

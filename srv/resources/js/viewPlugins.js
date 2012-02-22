function viewPlugins() {
  return {
    searchTable: function (field,fieldMeta) {
        var aoColumns = _.map(
            fieldMeta.searchTable.columns,
            function(name) { return {sTitle:name}; });

        field.dataTable({
          bInfo: false,
          bPaginate: false,
          sScrollY: "350px",
          bDeferRender: true,
          bSortClasses: false,
          oLanguage: {
            sSearch: "",
            sZeroRecords: "Ничего не найдено"
          },
          aoColumns: aoColumns,
          sAjaxSource: fieldMeta.searchTable.source,
          bServerSide: true,
          sDom: "frtiS"
        });
    }
  };
}
/*
      _.each(fieldMeta.searchTable.query, function(colId,fId) {
        var fModel = _.reduce(fId.split("."), function(res,p) {
          return res[p];
        },
        global.viewModel);

        fModel.subscribe(function(val) {
          field.fnFilter(val,colId);
        });
      });
*/
//   searchcase.$("td").hover(function() {
//     $(this.parentNode).addClass("highlighted");
//   }, function() {
//     searchcase.$("tr.highlighted").removeClass('highlighted');
//   } );
// 
//   $("#searchcase tbody tr").click( function(e) {
//     if ($(this).hasClass('row_selected')) {
//       $(this).removeClass('row_selected');
//     }
//     else {
//       searchcase.$('tr.row_selected').removeClass('row_selected');
//       $(this).addClass('row_selected');
//       window.location.href="newcase.html";
//     }
//   });
// 

function viewPlugins() {
  return {
    searchTable: function (field,fieldMeta) {
        var aoColumns = _.map(
            fieldMeta.searchTable.columns,
            function(name) { return {sTitle:name}; });

        field.dataTable({
          bInfo: false,
          bPaginate: false,
          sScrollY: "220px",
          bScrollInfinite: true,
          bDeferRender: true,
          bSortClasses: false,
          iTabIndex: -1,
          oLanguage: {
            sSearch: "",
            sZeroRecords: "Ничего не найдено"
          },
          aoColumns: aoColumns,
          sAjaxSource: fieldMeta.searchTable.source,
          bServerSide: true
        });

        _.each(fieldMeta.searchTable.query, function(colId,fId) {
          var fModel = _.reduce(
            fId.split("."),
            function(res,p) { return res[p]; },
            global.viewModel);

          fModel.subscribe(_.throttle(
            function(val) {
              field.fnFilter(val,colId === "*" ? undefined : colId);
            },
            2000));
        });

        field.on("click", "tr", function() {
            field.find("tr").removeClass("row_selected");
            $(this).addClass("row_selected");
        });
    }
  };
}


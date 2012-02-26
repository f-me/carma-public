function viewPlugins() {
  return {
    data: function(field,meta) {
      if (meta.type === "text") {
        createCatComplete(
            field,
            meta.levels === 2
              ? mk2LevelList(meta.data)
              : transformForCatComplete(meta.data));
      }
    },
    dependsOn: function(field,meta) {
      var srcPath = meta.dependsOn.split(":");
      var srcElemPath = srcPath[0];
      var dataTag = srcPath[1];

      var srcVM = _.reduce(srcElemPath.split("."),
          function(res, p) { return res[p]; },
          global.viewModel);

      srcVM.subscribe(function(newVal) {
        var data = $elem(srcElemPath).data("data");
        var tgtElem = $elem(meta.id);

        if (meta.type === "form") {
          var tgtVM = _.reduce(
              meta.id.split("."),
              function(res, p) { return res[p]; },
              global.viewModel);
          tgtVM = tgtVM || {};
          tgtElem.children().detach(); // FIXME: нужно их не только отцеплять, но и освобождать память.
          _.each(data[dataTag], function(f) {
            var field = createField(meta.id, tgtVM, f);
            tgtElem.append(field);
          });
          ko.applyBindings(global.viewModel);
        }
        else if (meta.type === "text") {
          createCatComplete(tgtElem, mk2LevelList(data[dataTag]));
        }
      });
      
    },
    searchTable: function (field,fieldMeta) {
        var aoColumns = _.map(
            fieldMeta.searchTable.columns,
            function(name) { return {sTitle:name}; });

        field.dataTable({
          bInfo: false,
          bPaginate: false,
          sScrollY: "220px",
          bScrollInfinite: true,
          iScrollLoadGap: 100,
          iDisplayLength: 100,
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
              field.fnFilter(val.toUpperCase(),colId === "*" ? undefined : colId);
            },
            500));
        });

        field.on("click", "tr", function() {
            field.find("tr").removeClass("row_selected");
            $(this).addClass("row_selected");
        });
    }
  };
}


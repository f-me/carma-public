<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>CaRMa</title>
    <link rel="stylesheet" href="/s/css/bootstrap.min.css" />
    <link rel="stylesheet" href="/s/css/jquery.dataTables.css" />
    <link rel="stylesheet" href="/s/css/local.css" />
    <!-- DOM manipulation -->
    <script src="/s/js/3p/jquery-1.7.1.min.js" />

    <!-- Rich UI -->
    <script src="/s/js/3p/bootstrap.min.js" />
    
    <!-- Tabular display -->
    <script src="/s/js/3p/jquery.dataTables.min.js" />

    <!-- Responsive UI javascript library -->
    <script src="/s/js/3p/knockout-2.0.0.js" />

    <!-- Utility library, Backbone dependency -->
    <script src="/s/js/3p/underscore-1.3.1.min.js" />

    <!-- Loose MVC -->
    <script src="/s/js/3p/backbone-0.9.1.min.js" />

    <!-- Knockback is a Knockout + Backbone glue -->
    <script src="/s/js/3p/knockback-0.13.min.js" />

    <!-- Simple templates -->
    <script src="/s/js/3p/mustache.js" />

    <!-- Model processing -->
    <script src="/s/js/metamodel.js" />
    <script src="/s/js/main.js" />
  </head>
  <body>
    
    <!-- Navigation bar on top -->
    <div class="navbar navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <ul class="nav">
            <a class="brand" href="/">
              CaRMa
            </a>
            <li class="divider-vertical" />
            <li id="main-screen-nav">
              <a href="/">Диспетчер</a>
            </li>
            <li id="call-screen-nav">
              <a href="/call/">Приём звонка</a>
            </li>
            <li id="case-screen-nav">
              <a href="#" onclick="renderScreen('case', {});">Кейс</a>
            </li>
            <li id="search-screen-nav">
              <a href="#" onclick="renderScreen('search');">Поиск</a>
            </li>
            <li id="help-screen-nav">
              <a href="/help/">Справка</a>
            </li>
          </ul>
            <ifLoggedIn>
              <ul class="nav pull-right">
                <li class="divider-vertical" />
                <li class="dropdown">
                  <a href="#"
                     class="dropdown-toggle"
                     data-toggle="dropdown">
                    <i class="icon-user icon-white" />&nbsp;<loggedInUser />
                    <b class="caret"></b>
                  </a>
                  <ul class="dropdown-menu">
                    <li>
                      <a href="/logout/">
                        <i class="icon-off icon-black" />&nbsp;Выход
                      </a>
                  </ul>
                </li>
              </ul>
            </ifLoggedIn>
        </div>
      </div>
    </div>

    <!-- Main container for dynamically rendered layouts -->
    <div class="container-fluid">
      <div class="row-fluid" id="layout" />
    </div>

    <!-- Case screen layout -->
    <script type="text/template" 
            class="screen-template"
            id="case-screen-template">
      <!-- Main case form -->
      <div class="box span6" id="left">
          <fieldset>
            <legend>
              <span id="form-title" />
            </legend>
            <form id="form" class="form-horizontal" />
          </fieldset>
      </div>
      <!-- Subform -->
      <div class="box span5" id="right">
          <fieldset>
            <legend>
              <span id="subform-title" />
            </legend>
            <form id="subform" class="form-horizontal" />
          </fieldset>
      </div>
    </script>

    <!-- Search screen -->
    <script type="text/template" 
            id="search-screen-template" 
            class="screen-template">
      <div class="box span12" id="tableView" />
    </script>

    <script type="text/template" 
            id="search-table-template" 
            class="view-template">
      <div style="text-align:center;">
      <fieldset>
        <legend>Поиск</legend>
        <form class="form-inline">
          <!-- ID magically cannot being with "searchtable" -->
          <input type="text" class="span6" id="table-query" />
          <button class="btn btn-success" onclick="doSearch();" type="button">
            Поиск
          </button>
        </form>
      </fieldset>
      </div>
      <table id="searchtable">
        <thead>
          <tr>
            <th>ID</th>
            <th>ФИО</th>
            <th>Дата звонка</th>
            <th>Телефон</th>
            <th>Номер машины</th>
            <th>Программа</th>
          </tr>
        </thead>
        <tbody/>
      </table>
    </script>


    <!-- 
         Form field templates.
         
         Field template must have id in form of <type>-field-template,
         where <type> is field type to be rendered using this
         template, or <name>-<type>-field-template, where <name> is
         the name of field of given type which will be rendered with
         this template. Client code must prefer named templates to
         only-typed ones.
         
      -->
    
    <script type="text/template" 
            class="field-template"
            id="textarea-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          <textarea class="span5"
                    name="{{ name }}" 
                    {{# readonly }}disabled{{/ readonly }}
                    rows="7"
                    data-bind="value: {{ name }},
                               valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="text-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          <input type="text"
                 class="span5"
                 name="{{ name }}"
                 {{# readonly }}disabled{{/ readonly }}
                 data-bind="value: {{ name }}, 
                            valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="select-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          <select name="{{ name }}"
                  {{# readonly }}disabled{{/ readonly }}
                  data-bind="value: {{ name }},
                             valueUpdate: 'change'">
            {{# choice }}
            <option value="{{.}}">{{.}}</option>
            {{/ choice }}
          </select>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="checkbox-field-template">
      <div class="control-group">
        <div class="controls">
          <label class="checkbox inline"><input type="checkbox" 
                                                name="{{ name }}"
                                                {{# readonly }}disabled{{/ readonly }}
                                                data-bind="checked: {{ name }},
                                                           valueUpdate: 'change'" />
            {{ label }}
          </label>
        </div>
      </div>
    </script>

    <!-- Template for fields with unknown type -->
    <script type="text/template"
            class="field-template"
            id="unknown-field-template">
      <div class="control-group">
        <div class="controls">
          <span class="label label-important">
            (Ошибка — поле {{ name }} неизвестного типа)
          </span>
        </div>
      </div>
    </script>
    
    <!-- Form controls wrt user permissions -->
    <script type="text/template"
            id="permission-template">
      <div class="form-actions">
        {{# readonly }}
        <button class="btn disabled" type="button">
          <i class="icon-ban-circle" /> Только для чтения</button>
        {{/ readonly }}
        {{# canUpdate }}
        <button class="btn btn-success" type="button"
                onClick="saveInstance('{{ viewName }}');">
          <i class="icon-pencil icon-white" /> Сохранить</button>
        {{/ canUpdate }}
        {{# canCreate }}
        <button class="btn" type="button"
                onClick="createInstance('{{ viewName }}');">
          <i class="icon-file" /> Начать новую</button>
        {{/ canCreate }}
        {{# canDelete }}
        <button class="btn btn-danger" type="button"
                style="float:right;"
                onClick="removeInstance('{{ viewName }}');">
          <i class="icon-trash icon-white" /> Удалить</button>
        {{/ canDelete }}
        <div style="clear: both;" />
      </div>
    </script>
  </body>
</html>

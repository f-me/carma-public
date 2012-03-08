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
    <script src="/s/js/search.js" />
    <script src="/s/js/main.js" />
    <script src="/s/js/local.js" />
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
              <a href="#call">Приём звонка</a>
            </li>
            <li id="case-screen-nav">
              <a href="#case">Кейс</a>
            </li>
            <li id="search-screen-nav">
              <a href="#search">Поиск</a>
            </li>
            <li id="help-screen-nav">
              <a href="#help">Справка</a>
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

    <!-- Call handling screen layout -->
    <script type="text/template"
            class="screen-template"
            id="call-screen-template">
      <div class="box" id="left">
        <div id="call-form"/>
      </div>
      <div class="box" id="right"/>
    </script>

    <!-- Case screen layout -->
    <script type="text/template"
            class="screen-template"
            id="case-screen-template">
      <!-- Main case form -->
      <div class="box" id="left">
        <form class="form-horizontal">
          <div id="case-form" />
          
          <div class="control-group">
            <div class="control-label">
              <label>Услуги</label>
            </div>
            <div class="controls">
              <div class="accordion" id="case-service-references" />
              
              <!-- This list will be generated automatically based
                   on programs data.
              -->
              <select multiple="true" size="6" name="service-picker">
                <option value="tech">Техпомощь</option>
                <option value="towage">Буксировка</option>
                <option value="taxi">Такси</option>
                <option value="hotel">Гостиница</option>
                <option value="rent">Подменный автомобиль</option>
                <option value="sober">Трезвый водитель</option>
              </select><br />
              <button type="button" class="btn"
                      onclick="addService();">
                <i class="icon icon-plus" />Добавить услугу</button>
            </div>
          </div>
          
          <div id="case-permissions" />
        </form>
      </div>

      <!-- Right pane with subform -->
      <!--
      TODO Should be span6 when fluid containers are fixed in
           Bootstrap upstream. -->
      <div class="box" id="right">
          <fieldset>
            <legend>
              <span id="case-subform-title" />
            </legend>
            <form id="case-subform" class="form-horizontal" />
          </fieldset>
      </div>
    </script>

    <!-- Search screen -->
    <script type="text/template"
            id="search-screen-template"
            class="screen-template">
      <!-- Can't use offsetN class here due to fluid layout. -->
      <div class="box" id="tableView" />
    </script>

    <script type="text/template"
            id="search-table-template"
            class="view-template">
      <div style="text-align:center;">
      <fieldset>
        <legend>Поиск</legend>
        <form class="form-inline">
          <!-- ID magically cannot start with "searchtable" -->
          <input type="text" class="span6" id="table-query" />
          <button class="btn btn-success" onclick="doSearch();" type="button">
            Поиск
          </button>
        </form>
      </fieldset>
      </div>
      <table id="searchtable" class="table table-striped table-bordered">
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
          <textarea class="pane-span"
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
                 class="pane-span"
                 name="{{ name }}"
                 {{# readonly }}disabled{{/ readonly }}
                 data-bind="value: {{ name }},
                            valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="dictionary-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          <input type="text"
                 class="pane-span"
                 data-source="global.dictionaries['{{dictionaryName}}']"
                 data-bind="value: {{ name }}"
                 data-provide="typeahead" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="callerType-dictionary-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          {{# dictionary }}
            <label>
              <!-- Mustache.js contexts support bubbling -->
              <input type="radio"
                     name="{{ name }}"
                     value="{{ value }}"
                     data-bind="checked: {{ name }}"></input>
              {{ label }}
            </label><br/>
          {{/ dictionary }}
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
          <label class="checkbox inline">
            <input type="checkbox"
                   name="{{ name }}"
                   {{# readonly }}disabled{{/ readonly }}
                   data-bind="checked: {{ name }},
                              valueUpdate: 'change'" />
            {{ label }}
          </label>
        </div>
      </div>
    </script>

    <!-- NOP here — references are rendered after model has loaded -->
    <script type="text/template" 
            class="field-template"
            id="reference-field-template" />

    <!-- 
    
         Template for one of references.
    
         Must generate id="{{refField}}-view-{{refN}}" element which
         will hold contents of referenced model. Its class is
         {{refField}}-view.

         Also "{{refField}}-view-{{refN}}-link" element may be used
         which may contain link to model loading,
         "{{refField}}-view-{{refN}}-head" which is top-level
         container of referenced instance, and
         "{{refField}}-view-{{refN}}-perms" for instance permissions.

         May setup on-demand loading function.

    -->
    <script type="text/template"
            class="reference-template"
            id="-reference-template">
      <div class="accordion-group">
        <div class="accordion-heading">
          <a class="accordion-toggle"
             id="{{refField}}-view-{{refN}}-link"
             data-target="#{{refField}}-view-{{refN}}-head"
             data-toggle="collapse">Услуга…</a>
        </div>

        <div id="{{refField}}-view-{{refN}}-head"
             class="accordion-body collapse {{^refId}}in{{/refId}}">
          <div class="accordion-inner {{ refField }}-view" 
               id="{{refField}}-view-{{ refN }}">
            <!-- Instance contents are rendered here -->

          </div>
          <div id="{{refField}}-view-{{ refN }}-perms" />
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
        <div style="clear: both;" />
      </div>
    </script>
  </body>
</html>

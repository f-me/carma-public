<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>CaRMa</title>
    <link rel="stylesheet" href="/s/css/bootstrap.min.css" />
    <link rel="stylesheet" href="/s/css/datepicker.css" />
    <link rel="stylesheet" href="/s/css/jquery.dataTables.css" />
    <link rel="stylesheet" href="/s/css/local.css" />
    <!-- DOM manipulation -->
    <script src="/s/js/3p/jquery-1.7.1.min.js" />

    <!-- Rich UI -->
    <script src="/s/js/3p/bootstrap.min.js" />
    <script src="/s/js/3p/bootstrap-datepicker.js" />

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
        <form class="form-vertical">
          <div class="control-group">
            <div class="control-label">
              <label>Номер</label>
            </div>
            <div class="controls">
              <input type="text" disabled id="case-number"
                     data-bind="value: maybeId"/>
            </div>
          </div>
          <div id="case-form" />
          
          <div class="control-group">
            <div class="control-label">
              <label>Услуги</label>
            </div>
            <div class="controls">
              <span class="accordion" id="case-service-references" />
              
              <span id="service-picker-container" />
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
      <fieldset style="width:50%; margin-left:25%;">
        <legend>Поиск</legend>
        <form class="form-inline">
          По дате<br />
          <div data-date-format="mm-dd-yyyy" 
               id="datepicker" 
               class="input-append date">
	    <input type="text"
                   id="table-query"
                   size="16" class="span2" id="acpro_inp2">
	    <span class="add-on"><i class="icon-calendar"></i></span>
          </div>
          <br />
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
      <div class="control-group"
           {{# required }}data-bind="css: { error: {{name}}Not }"{{/ required}}
           >
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">             
          <textarea class="pane-span focusable"
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
      <div class="control-group"
           {{# required }}data-bind="css: { error: {{name}}Not }"{{/ required}}
           >
        <div class="control-label">
          <label>{{ label }}</label>
        </div>
        <div class="controls">
          <input type="text"
                 class="pane-span focusable" 
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
          <div class="input-append">
            <input type="text"
                   class="pane-span focusable"
                   name="{{ name }}"
                   data-source="global.dictionaries['{{dictionaryName}}']"
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'"
                   data-provide="typeahead" />
            <span class="add-on"><i class="icon icon-chevron-down" /></span>
          </div>
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
            <label class="radio">
              <!-- Mustache.js contexts support bubbling -->
              <input type="radio"
                     name="{{ name }}"
                     value="{{ value }}"
                     data-bind="checked: {{ name }}"></input>
              {{ label }}
            </label>
          {{/ dictionary }}
        </div>
      </div>
    </script>

    <!-- May be used for plain rendering of dictionaries as well -->
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
            {{# dictionary }}
            <option value="{{value}}">{{label}}</option>
            {{/ dictionary }}
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

   <script type="text/template" 
           class="field-template"
           id="group-field-template">
     <div class="control-group">
       <div class="control-label">
         {{label}}
       </div>
       <div class="controls">
         <div class="input-append">
           <input type="text"
                  class="pane-span"
                  onfocus="showComplex('{{ viewName }}', '{{ name }}');"
                  {{# readonly }}disabled{{/ readonly }}
                  data-bind="value: {{ name }}Ref" />
           <span class="add-on">
             <i onclick="showComplex('{{ viewName }}', '{{ name }}');"
                class="icon icon-share" />
           </span>
         </div>
       </div>
     </div>
   </script>

    <!-- 
    
         Template for one of references.
    
         Must generate id="{{ refView }}" element which
         will hold contents of referenced model. Its class must be is
         {{ refClass }}.

         "{{ refView }}-perms" will be used for instance permissions.

         May setup on-demand loading function.
    -->
    <script type="text/template"
            class="reference-template"
            id="services-reference-template">
      <div class="accordion-group">
        <div class="accordion-heading">
          <a class="accordion-toggle"
             id="{{ refView }}-link"
             data-target="#{{ refView }}-head"
             data-toggle="collapse">Услуга…</a>
        </div>

        <div id="{{ refView }}-head"
             class="accordion-body collapse {{^refId}}in{{/refId}}">
          <div class="accordion-inner {{ refClass }}" 
               id="{{ refView }}">
            <!-- Instance contents are rendered here -->

          </div>
          <div id="{{ refView }}-perms" />
        </div>
      </div>
    </script>

    <!-- A simpler template for single-instance references -->
    <script type="text/template"
            class="reference-template"
            id="-reference-template">
      <!-- Link is unused -->
      <span style="display:none;" id="{{refView}}-link"/>
      <fieldset>
        <form id="{{ refView }}"
              class="{{ refClass }} complex-field form-horizontal"
              style="display: none;" />
      </fieldset>
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

    <!-- Render service picker with services dictionary -->
    <script type="text/template"
            id="service-picker-template">
      <ul class="nav nav-pills">
        <li class="dropdown">
          <button class="dropdown-toggle btn btn-action"
                  type="button"
                  data-toggle="dropdown">
            <i class="icon icon-plus" />Добавить услугу
          </button>
          <ul class="dropdown-menu">
            {{# dictionary }}
            <li>
              <a href="#" onclick="addService('{{value}}');">
                <i class="icon-{{icon}} icon-black" />
                {{label}}
              </a>
            </li>
            {{/ dictionary }}
          </ul>
        </li>
      </ul>
    </script>

    <!-- Fallback template for pickTemplate failures -->
    <script type="text/template"
            id="unknown-template">
      <span class="label label-important">
        Не удалось найти ни один из шаблонов:
        {{#names}}{{.}}&nbsp;{{/names}}
      </span>
    </script>
  </body>
</html>

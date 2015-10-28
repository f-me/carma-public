module.exports = (grunt) ->

  require('load-grunt-tasks')(grunt)

  content = "resources/assets"
  pub     = "resources/static"
  scripts = "#{content}/script"
  built   = "#{pub}/js/gen"
  js      = "#{pub}/js"
  tpl     = "#{pub}/tpl"
  css     = "#{pub}/css"

  config =
    pkg: grunt.file.readJSON('package.json')
    coffee:
      all:
        options: { bare:  true }
        expand:  true
        flatten: false
        cwd:     scripts
        src:     ['**/*.coffee']
        dest:    built
        ext:     '.js'

    copy:
      js:
        expand: true
        cwd: scripts
        src: ["**/*.js"]
        dest: js
        filter: 'isFile'
      template:
        expand: true
        cwd: "#{content}/template"
        src: ["**/*.html"]
        dest: tpl
        filter: 'isFile'

    less:
      production:
        options:
          paths: ["#{content}/style", "bower_components"]
          cleancss: true
        files:
          "resources/static/css/style.css": ["#{content}/style/style.less","#{content}/style/*.css"]


    jade:
      compile:
        options:
          pretty: true
          basedir: "#{content}/template"
        files: [
          { cwd: "#{content}/template"
          , src: "**/*.jade"
          , filter: (f) ->
              # ignore index.jade and files, that begin with "_"
              (f != "index.jade") and (not /.*?\/?_[^\/]+\.jade/.test(f))
          , dest: tpl
          , expand: true
          , ext: ".html"
          },
          { src: "#{content}/template/index.jade"
          , dest: "#{tpl}/index.tpl"
          }
          ]

    shell:
      bower:
        command: "bower install"

    clean:
      all:
        src: ["#{js}/**/*", tpl, css]

    watch:
      coffee:
        files: "#{scripts}/**/*.coffee"
        tasks: "newer:coffee"
      js:
        files: "#{scripts}/**/*.js"
        tasks: "newer:copy:js"
      jade:
        files: ["#{content}/template/**/*.jade"]
        tasks: "jade"
      html:
        files: ["#{content}/template/**/*.html"]
        tasks: "newer:copy:template"
      style:
        files: ["#{content}/style/**/*"]
        tasks: "newer:less"


  thirdParty =
    md5:        {src: 'js-md5/js',       file: 'md5.min.js'}
    d3:         {src: 'd3',              file: 'd3.min.js'}
    moment:     {src: 'moment/min',      file: 'moment-with-locales.min.js'}
    mousetrap:  {src: 'mousetrap',       file: 'mousetrap.min.js'}
    mustache:   {src: 'mustache',        file: 'mustache.js'}
    notify:     {src: 'notifyjs/dist',   file: 'notify-combined.min.js'}
    spin:       {src: 'spin.js',         file: ['spin.js', 'jquery.spin.js']}
    'bootstrap-daterangepicker':
      src: 'bootstrap-daterangepicker'
      dest: 'daterangepicker'
      file: ['daterangepicker.js', 'daterangepicker-bs3.css']
    'js-base64':{src: 'js-base64',       file: 'base64.min.js'}
    'jquery.browser':
      src: 'jquery.browser/dist'
      file: 'jquery.browser.min.js'
    'jquery-knob':
      src: 'jquery-knob/dist'
      file: 'jquery.knob.min.js'
    datatables:
      src: 'datatables/media'
      dest: 'datatables'
      file: ['css/jquery.dataTables.min.css'
             'js/jquery.dataTables.min.js'
             'images/*'
            ]
    bootstrap:
      src: 'bootstrap/dist'
      dest: 'bootstrap'
      file: ['fonts/**', 'js/**']
    openLayers:
      src: 'OpenLayers'
      dest: 'OpenLayers'
      file: ['OpenLayers.js', 'img/**', 'theme/**']
    'jasny-bootstrap':
      src:  'jasny-bootstrap/dist'
      dest: 'jasny-bootstrap'
      file: [ 'js/jasny-bootstrap.min.js'
            , 'css/jasny-bootstrap.min.css'
            ]
    'bootstrap3-wysihtml5':
      src:  'bootstrap3-wysihtml5-bower/dist'
      dest: 'wysihtml5'
      file: [ 'bootstrap3-wysihtml5.all.min.js'
            , 'bootstrap3-wysihtml5.css'
            , 'locales/bootstrap-wysihtml5.ru-RU.js'
            ]
    wysihtml5x:
      src: 'wysihtml5x/dist'
      dest: 'wysihtml5x'
      file: [ 'wysihtml5x.min.js', 'wysihtml5x.min.map'
            , 'wysihtml5x-toolbar.min.js', 'wysihtml5x-toolbar.min.map']
    'bootstrap-datepicker':
      src:  'bootstrap-datepicker'
      dest: 'bootstrap-datepicker'
      file: [ 'css/datepicker3.css'
            , 'js/bootstrap-datepicker.js'
            , 'js/locales/bootstrap-datepicker.ru.js'
            ]
    'normalize-css':
      src: 'normalize-css'
      dest: 'normalize-css'
      file: 'normalize.css'

  mkCopyAndClean = (libs, cfg) ->
    for lib, libCfg of libs
      cfg.copy[lib] =
        expand: true
        cwd:  "bower_components/#{libCfg.src}"
        src:  libCfg.file
        dest: "#{pub}/3p/#{libCfg.dest ? ''}"
      cfg.clean[lib] =
        expand: true
        cwd: "#{pub}/3p/#{libCfg.dest ? ''}"
        src: libCfg.file

  mkCopyAndClean thirdParty, config
  grunt.initConfig config

  newerify = (ts) -> "newer:#{t}" for t in ts

  grunt.registerTask("build", ['copy', 'coffee', 'less', 'jade'])
  grunt.registerTask("rebuild", ['shell:bower', 'clean', 'build'])
  grunt.registerTask("bwatch", ['rebuild', 'watch'])
  grunt.registerTask("default", "rebuild")

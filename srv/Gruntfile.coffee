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
      css:
        expand: true
        cwd: "#{content}/style"
        src: ["**/*.css"]
        dest: css
        filter: 'isFile'
      template:
        expand: true
        cwd: "#{content}/template"
        src: ["**/*.html"]
        dest: tpl
        filter: 'isFile'

    jade:
      compile:
        options:
          pretty: true
          basedir: "#{content}/template"
        files: [
          cwd: "#{content}/template"
          src: "**/*.jade"
          filter: (f) ->
            # ignore lib dir and files, that begin with "_"
            (not /\/lib/.test(f)) and (not /.*?\/?_[^\/]+\.jade/.test(f))
          dest: tpl
          expand: true
          ext: ".html"
          ]

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
        tasks: "newer:copy:css"


  thirdParty =
    md5:        {src: 'js-md5/js',       file: 'md5.min.js'}
    d3:         {src: 'd3',              file: 'd3.min.js'}
    mustache:   {src: 'mustache',        file: 'mustache.js'}
    underscore: {src: 'underscore',      file: 'underscore.js'}
    finch:      {src: 'finchjs',         file: 'finch.min.js'}
    jquery:     {src: 'jquery',          file: 'jquery.js'}
    knockout:   {src: 'knockoutjs/dist', file: 'knockout.js'}
    notify:     {src: 'notifyjs/dist',   file: 'notify-combined.min.js'}
    spin:       {src: 'spin.js',         file: ['spin.js', 'jquery.spin.js']}
    'jquery-maskedinput':
      src: 'jquery-maskedinput/src'
      file: 'jquery.maskedinput.js'
    datatables:
      src: 'datatables/media'
      dest: 'datatables'
      file: ['css/jquery.dataTables.min.css'
             'js/jquery.dataTables.min.js'
             'images/*'
            ]
    bootstrap:
      src: 'bootstrap'
      dest: 'bootstrap'
      file: ['css/**', 'img/**', 'js/**']
    openLayers:
      src: 'OpenLayers'
      dest: 'OpenLayers'
      file: ['OpenLayers.js', 'img/**', 'theme/**']
    wysihtml5:
      src:  'wysihtml5/dist'
      dest: 'wysihtml5'
      file: 'wysihtml5-0.3.0.min.js'
    'wysihtml5-boot':
      src:  'bootstrap-wysihtml5/src'
      dest: 'wysihtml5'
      file: [
        'bootstrap-wysihtml5.js',
        'bootstrap-wysihtml5.css',
        'locales/bootstrap-wysihtml5.ru-RU.js']


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

  grunt.registerTask("build", newerify ['coffee', 'copy', 'jade'])
  grunt.registerTask("rebuild", ['clean', 'build'])
  grunt.registerTask("bwatch", ['build', 'watch'])
  grunt.registerTask("default", "rebuild")

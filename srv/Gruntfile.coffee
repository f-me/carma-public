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
        filter: (f) -> not /js\/3p/.test(f)

    watch:
      coffee:
        files: "#{scripts}/**/*.coffee"
        tasks: "newer:coffee"
      js:
        files: "#{scripts}/**/*.js"
        tasks: "newer:copy:js"
      jade:
        files: ["#{content}/template/**/*.jade"]
        tasks: "newer:jade"
      html:
        files: ["#{content}/template/**/*.html"]
        tasks: "newer:copy:template"
      style:
        files: ["#{content}/style/**/*"]
        tasks: "newer:copy:css"


  thirdParty =
    md5:        {dir: 'js-md5/js',       file: 'md5.min.js'}
    d3:         {dir: 'd3',              file: 'd3.min.js'}
    mustache:   {dir: 'mustache',        file: 'mustache.js'}
    underscore: {dir: 'underscore',      file: 'underscore.js'}
    finch:      {dir: 'finchjs',         file: 'finch.min.js'}
    jquery:     {dir: 'jquery',          file: 'jquery.js'}
    knockout:   {dir: 'knockoutjs/dist', file: 'knockout.js'}
    notify:     {dir: 'notifyjs/dist',   file: 'notify-combined.min.js'}
    spin:       {dir: 'spin.js',         file: ['spin.js', 'jquery.spin.js']}


  mkCopyAndClean = (libs, cfg) ->
    for lib, libCfg of libs
      cfg.copy[lib] =
        expand: true
        cwd:  "bower_components/#{libCfg.dir}"
        src:  libCfg.file
        dest: "#{pub}/js/3p"
      cfg.clean[lib] =
        expand: true
        cwd: "#{pub}/js/3p"
        src: libCfg.file

  mkCopyAndClean thirdParty, config
  grunt.initConfig config

  newerify = (ts) -> "newer:#{t}" for t in ts

  grunt.registerTask("build", newerify ['coffee', 'copy', 'jade'])
  grunt.registerTask("rebuild", ['clean', 'build'])
  grunt.registerTask("bwatch", ['build', 'watch'])
  grunt.registerTask("default", "rebuild")

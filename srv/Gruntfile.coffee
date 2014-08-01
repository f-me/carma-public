module.exports = (grunt) ->

  require('load-grunt-tasks')(grunt)

  content = "resources/assets"
  pub     = "resources/static"
  scripts = "#{content}/script"
  built   = "#{pub}/js/gen"
  js      = "#{pub}/js"
  tpl     = "#{pub}/tpl"
  css     = "#{pub}/css"

  bowerCopy = (dir, file) ->
    expand: true
    cwd:  "bower_components/#{dir}"
    src:  file
    dest: "#{pub}/js/3p"


  grunt.initConfig
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
      md5:        bowerCopy 'js-md5/js', 'md5.min.js'
      base64:     bowerCopy 'js-base64', 'base64.min.js'
      d3:         bowerCopy 'd3',        'd3.min.js'
      mustache:   bowerCopy 'mustache',  'mustache.js'
      underscore: bowerCopy 'underscore','underscore.js'
      notify:     bowerCopy 'notifyjs/dist', 'notify-combined.min.js'
      spin:       bowerCopy('spin.js', ['spin.js', 'jquery.spin.js'])

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

  newerify = (ts) -> "newer:#{t}" for t in ts

  grunt.registerTask("build", newerify ['coffee', 'copy', 'jade'])
  grunt.registerTask("rebuild", ['clean', 'build'])

  grunt.registerTask("bwatch", ['build', 'watch'])

  grunt.registerTask("default", "rebuild");

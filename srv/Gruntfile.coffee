module.exports = (grunt) ->

  require('load-grunt-tasks')(grunt)

  content = "resources/assets"
  pub     = "resources/static"
  scripts = "#{content}/script"
  built   = "#{pub}/js/gen"
  js      = "#{pub}/js"
  tpl     = "#{pub}/tpl"
  css     = "#{pub}/css"

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
      all:
        files: "#{content}/**/*"
        tasks: "build"

  newerify = (ts) -> "newer:#{t}" for t in ts

  grunt.registerTask("build", newerify ['coffee', 'copy', 'jade'])
  grunt.registerTask("rebuild", ['clean', 'build'])

  grunt.registerTask("bwatch", ['build', 'watch'])

  grunt.registerTask("default", "rebuild");

module.exports = (grunt) ->

  grunt.loadNpmTasks('grunt-contrib-coffee')
  grunt.loadNpmTasks('grunt-contrib-watch')
  grunt.loadNpmTasks('grunt-contrib-copy')
  grunt.loadNpmTasks('grunt-contrib-clean')

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

    clean:
      all:
        src: ["#{js}/**/*", tpl, css]
        filter: (f) -> not /js\/3p/.test(f)

    watch:
      scripts:
        files: "#{scripts}/**/*.*"
        tasks: ['coffee', 'copy'],

  grunt.registerTask("build",   ['coffee', 'copy'])
  grunt.registerTask("rebuild", ['clean', 'build'])

  grunt.registerTask("default", "rebuild");

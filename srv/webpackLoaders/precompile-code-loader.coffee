fs = require "fs"
path = require "path"
{Script} = require "vm"
{getOptions} = require "loader-utils"
cs = require "coffeescript"

extensions = [".coffee", ".js", ".json"]

execInSandbox = (requirableLibs, dir, source) ->
  new Script(source).runInNewContext
    require: customRequire requirableLibs, dir
    module: exports: {}

# WARNING! Keep in mind that modules that required by precompiled module is't
# handled by webpack's watcher.
customRequire = (requirableLibs, moduleDir) -> (moduleName) -> switch
  when moduleName in requirableLibs
    require moduleName
  when /^\.?\.\//.test moduleName # starts with ./ or ../
    modulePath = path.resolve moduleDir, moduleName

    [filePath, ext] = do ->
      for ext in extensions when modulePath.substr(-ext.length) is ext
        return [modulePath, ext]

      for ext in extensions
        file = "#{modulePath}#{ext}"
        return [file, ext] if fs.existsSync file

      throw new Error "Module '#{modulePath}' not found"

    source = do ->
      x = fs.readFileSync(filePath, "utf8").toString()

      switch ext
        when ".coffee" then "#{cs.compile x}\n\nmodule.exports"
        when ".json"   then "\n\nmodule.exports = #{x};\n\nmodule.exports"
        else "#{x}\n\nmodule.exports"

    execInSandbox requirableLibs, path.dirname(modulePath), source
  else
    throw new Error """
      You can't require '#{moduleName}' module.
      #{
        if requirableLibs.length is 0 then "" else
          "Available libraries: \
            #{("'#{x}'" for x in requirableLibs).join ", "}.\n"
      }\
      You able to require project-relative modules \
      which will be evaluated at compile-time.
    """

module.exports = (source) ->
  requirableLibs = getOptions(this)?.requirableLibs ? []
  json = execInSandbox requirableLibs, @context, "#{source}\n\nmodule.exports"
  "module.exports = #{JSON.stringify json};"

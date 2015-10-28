module.exports = function(source) {
  return eval(source + '; module.exports();');
};

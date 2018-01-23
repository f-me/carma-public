module.exports = {
  extends: 'clock',
  disallowIdLiterals: null, // we need them often
  requireLowerCaseTags: null, // `addLocalName` is interpolated by heist
  validateAttributeQuoteMarks: null, // inconsistent / hard to fix
  validateAttributeSeparator: null,  // inconsistent / hard to fix
  validateIndentation: null,         // inconsistent / hard to fix
  requireClassLiteralsBeforeIdLiterals: null, // too lazy to fix
  requireSpecificAttributes: null, // Our forms don't need `action`
  disallowClassAttributeWithStaticValue: null, // some classes are interpolated by mustache
};

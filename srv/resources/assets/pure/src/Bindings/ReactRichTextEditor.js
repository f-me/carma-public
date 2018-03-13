/* global exports */
"use strict";

var RichTextEditor = require('react-rte').default;

exports.richTextEditor = RichTextEditor;
exports.createEmptyValue = RichTextEditor.createEmptyValue;

function createValueFromStringRaw(markup) {
  return function (format) {
    return function () {
      return RichTextEditor.createValueFromString(markup, format);
    };
  };
}

exports.createValueFromStringRaw = createValueFromStringRaw;

function valueToStringRaw(value) {
  return function (format) {
    return value.toString(format);
  };
}

exports.valueToStringRaw = valueToStringRaw;

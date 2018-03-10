/* global exports */
"use strict";

var Dropzone = require('react-dropzone').default;

exports.dropzone = Dropzone;
exports.bytesInfinity = Infinity;


function handle2(f) {
  return function (a, b) {
    return f(a)(b)();
  };
}

exports.handle2 = handle2;


function handle3(f) {
  return function (a, b, c) {
    return f(a)(b)(c)();
  };
}

exports.handle3 = handle3;

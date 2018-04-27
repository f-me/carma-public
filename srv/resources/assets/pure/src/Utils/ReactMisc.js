/* global exports */
"use strict";

function callEventHandler(evHandler) {
  return function (a) {
    return function () {
      evHandler(a);
    };
  };
}

exports.callEventHandler = callEventHandler;

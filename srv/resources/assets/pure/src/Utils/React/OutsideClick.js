/* global exports */
"use strict";

function subscribeOutsideClick(handler) {
  return function (ref) {
    return function () {
      var outsideClickListener = function (event) {
        if (!ref.contains(event.target)) {
          handler();
        }
      };

      document.addEventListener('click', outsideClickListener);

      return function () {
        document.removeEventListener('click', outsideClickListener);
      };
    };
  };
}

function unsubscribeOutsideClick(unsubscribe) {
  return function () {
    unsubscribe();
  };
}

exports.subscribeOutsideClick = subscribeOutsideClick;
exports.unsubscribeOutsideClick = unsubscribeOutsideClick;

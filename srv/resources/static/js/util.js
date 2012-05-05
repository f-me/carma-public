/// Common helpers

// Return current DD.MM.YYYY
function getFormatDate() {
    var d = new Date;
    var sd = d.getDate() + '.' + (d.getMonth() + 1) + '.' + d.getFullYear();
    return sd;
}

// Return current HH:MM
function getFormatTime() {
    var d = new Date;
    var sd = d.getHours() + ':' + d.getMinutes();
    return sd;
}

// Scroll to the bottom of the page
function scrollDown() {
    window.scrollTo(0, document.body.scrollHeight - 300);
}

// jquery -> html(as strung) conversion, with selected element
jQuery.fn.outerHTML = function() {
    return jQuery("<div>").append(this.clone()).html();
}

// like _.has but for list
function hasL(lst, e) {
    return _.find(lst, function(x) { return x == e });
}

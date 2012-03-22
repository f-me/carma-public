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

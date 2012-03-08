(function($) {
    // Wrapper around Typeahead to provide dictionary from
    // global.dictionaries
    $.fn.dictionary = function(option) {
        return this.each(function () {
            var $this = $(this)
            , data = $this.data('dictionary')
            , options = typeof option == 'object' && option;
            
            options.source = global.dictionaries[options.source];
            // To support categories, we chould either use catcomplete
            // or provide custom rendering to typeahead
            if (!data) 
                $this.typeahead(options);
        })};


    // Data-api hook
    $(function () {
        $('body').on('focus.dictionary.data-api', 
                     '[data-provide="dictionary"]', 
                     function (e) {
                         var $this = $(this);
                         if ($this.data('dictionary'))
                             return;
                         e.preventDefault();
                         $this.dictionary($this.data());
                     });
    });
})(jQuery);

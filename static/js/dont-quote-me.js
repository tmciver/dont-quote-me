window.onload = function () {
  $('#quotee-text').autocomplete({
    source: function(req, callback) {
      $.get('/people', {'nameHas': req.term})
        .done(function(data) {
          const acVals = data.map(d => ({
            label: d.name,
            value: d.uri
          }));
          callback(acVals);
        });
    },
    minLength: 3
  });
};

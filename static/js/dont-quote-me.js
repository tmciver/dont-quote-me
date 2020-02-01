window.onload = function () {
  $('#quotee-text').autocomplete({
    source: [
      {
        label: 'Malcolm Reynolds',
        value: ' http://dbpedia.org/resource/Malcolm_Reynolds'
      },
      {
        label: 'Abraham Lincoln',
        value: 'http://dbpedia.org/resource/Abraham_Lincoln'
      },
      {
        label: 'Walt Disney',
        value: ' http://dbpedia.org/resource/Walt_Disney'
      },
      {
        label: 'Ayn Rand',
        value: 'http://dbpedia.org/resource/Ayn_Rand'
      },
      {
        label: 'Milton Friedman',
        value: 'http://dbpedia.org/resource/Milton_Friedman'
      },
      {
        label: 'John Milton',
        value: 'http://dbpedia.org/resource/John_Milton'
      },
    ]
  });
};

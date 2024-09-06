// custom.js

Shiny.addCustomMessageHandler('getImageDimensions', function(message) {
  var img = document.querySelector('#current_photo img');
  if (img) {
    Shiny.setInputValue('image_width', img.naturalWidth);
    Shiny.setInputValue('image_height', img.naturalHeight);
  }
});


$(document).keyup(function(event) {
  if ($("#password").is(":focus") && (event.key == "Enter")) {
    $("#submit").click();
  }
});

$(document).keyup(function(event) {
  if ($("#stopCromwell").is(":focus") && (event.key == "Enter")) {
    $("#deleteCromwell").click();
  }
});

$(document).keyup(function(event) {
  if (event.key == "Enter") {
    $("#beginCromwell").click();
  }
});



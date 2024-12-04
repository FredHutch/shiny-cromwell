$(document).keyup(function(event) {
  if ($("#password").is(":focus") && (event.key == "Enter")) {
    $("#submit").click();
  }
});

$(document).keyup(function(event) {
  if ($("#ownCromwellURL").is(":focus") && (event.key == "Enter")) {
    $("#submitOwnCromwell").click();
  }
});

$(document).keyup(function(event) {
  if (event.key == "Enter") {
    $("#deleteCromwell").click();
  }
});

$(document).keyup(function(event) {
  if (event.key == "Enter") {
    $("#beginCromwell").click();
  }
});



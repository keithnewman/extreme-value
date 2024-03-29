/**
 * If the inputted data throws an error, then display a prominent error
 * @param message An array containing three entries in the following order:
 *                1) Boolean of whether an error was thrown. true indicates an
 *                   error and false indicates everything was OK.
 *                2) A short description of the issue
 *                3) Any internal message that have been thrown.
 * */
Shiny.addCustomMessageHandler("dataErrorMessage", function(message) {
  if (message != "") {
    $("#data-danger-message").removeClass("hidden");
    $("#data-danger-summary").text(message);
    $("#manualDataInput").parent().removeClass("has-success");
    $("#manualDataInput").parent().addClass("has-danger");
  } else {
    $("#data-danger-message").addClass("hidden");
    $("#manualDataInput").parent().removeClass("has-danger");
    $("#manualDataInput").parent().addClass("has-success");
    $("#data-danger-summary").text("");
  }
});

/**
 * If the manually inputted data contains some invalid numeric values, show a
 * warning that these values had to be ignored.
 * @param isOK Boolean value indicating whether all values were valid (true), or
 *             one or more values had to be removed (false)
 * */
Shiny.addCustomMessageHandler("dataInputError", function(isOK) {
  if (isOK) {
    $("#input-error").addClass("hidden");
    $("#manualDataInput").parent().removeClass("has-warning");
    $("#manualDataInput").parent().addClass("has-success");
  } else {
    $("#input-error").removeClass("hidden");
    $("#manualDataInput").parent().removeClass("has-success");
    $("#manualDataInput").parent().addClass("has-warning");
  }
});

/**
 * If a demo dataset is selected, update the description and map's iframe src
 * for the current demo dataset.
 * @param details Array containing the [description, mapUrl] for the demo data.
 * */
Shiny.addCustomMessageHandler("updateDemoDescription", function(details) {
  $("#demoDatasetDescription").html(details[0]);
  $("#demoDatasetMap").attr("src", details[1]);
});
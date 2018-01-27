#' Print updates to console
#'
#' @param message_number The index of the message to print
#' @param vm_variables
#' @param vm_variables
#' @param is_Message print update as a message?
#'
#' @return
#' @keywords internal
message_update <-
  function(message_number,
    vm_variables,
    duration,
    is_message = FALSE) {

  note <-
    switch(
      message_number,
      paste("\nProcessing", file, "..."),
      paste(
        "\n     Getting VM for variables searched on the following criteri(a/on):",
        vm_variables,
        "\n"
      ),
      "Failed to detect start time in file header",
      paste("\nFile processed. Processing took", round(duration/60, 2), "minutes.\n"),
      "5"
    )
  if (is_message) {
    message(note)
  } else{
    cat(note)
  }
}

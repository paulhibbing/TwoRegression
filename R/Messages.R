#' Print updates to console
#'
#' @param message_number the index of the message to print
#' @param file the name of the file being processed
#' @param vm_variables variables being used to calculate vector magnitude
#' @param duration the duration of processing
#' @param is_Message print update as a message?
#'
#' @keywords internal
message_update <-
  function(message_number,
    file,
    vm_variables,
    duration,
    cvs,
    window_secs,
    is_message = FALSE) {

  note <-
    switch(
      message_number,
      paste("\nProcessing", basename(file), "..."),
      paste(
        "\n     Getting VM for variables searched on the following criteri(a/on):",
        vm_variables,
        "\n"
      ),
      "Failed to detect start time in file header",
      paste("\nFile processed. Processing took", round(duration/60, 2), "minutes.\n"),
      "\n\n-- Filtering Gyroscope...",
      " Done.\n",
      "\n-- Calculating Vector Magnitudes...",
      "\n     Vector magnitude calculation complete.\n",
      "Number of rows not divisible by samp_rate*output_window\nTruncating data.",
      "\n-- Collapsing data. This could take awhile...",
      paste("\nCalculating CV per 10s for:", paste(cvs, collapse = " and ")),
      "\nThis could take awhile. Be patient...",
      paste("\n... Getting ", window_secs, "s CVs", sep = ""),
      "14"
    )
  if (is_message) {
    message(note)
  } else{
    cat(note)
  }
}

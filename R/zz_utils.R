epoch_length_sec <- function (AG, time_var = "Timestamp") {

  stopifnot(
    inherits(AG, "data.frame"),
    exists(time_var, AG)
  )

  nrow(AG) %>%
  {. * 0.1} %>%
  ceiling(.) %>%
  pmax(2) %>%
  seq(.) %>%
  AG[., time_var] %>%
  diff(.) %>%
  as.numeric(units = "secs") %>%
  unique(.) %T>%
  {if (length(.) != 1) stop(
    "Detected multiple epoch lengths",
    call. = FALSE
  )}

}

reintegrate <- function(AG, time_var, to) {

  AG %>%
  dplyr::group_by(
    !!as.name(time_var) := lubridate::floor_date(
      x = !!as.name(time_var), unit = to
    )
  ) %>%
  dplyr::summarise(
    dplyr::across(
      where(is.numeric),
      sum
    ),
    dplyr::across(
      where(function(x) ! is.numeric(x)),
      dplyr::first
    )
  ) %>%
  dplyr::select(dplyr::all_of(names(AG))) %>%
  as.data.frame(.)

}

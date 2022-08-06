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

#' Add a cut-point to a plot
#'
#' @param fig A partially-assembled figure
#' @param cutPoint The threshold value to insert into the figure
#' @param x numeric scalar giving x coordinate for label placement
#' @param y numeric scalar giving y coordinate for label placement
#'
#' @keywords internal
#'
plotCP <- function(fig, cutPoint, x = NULL, y = NULL){
  # cutPoint <- object$sed_cutpoint

  if (is.null(x)) x <- cutPoint + 3
  if (is.null(y)) y <- 6

  label <-
    paste('Threshold =', format(cutPoint, digits = 1, nsmall = 1))

  return(
    fig +
      geom_vline(xintercept = cutPoint, size = 1.3) +
      geom_label(x = x, y = y, label = label, colour = 'black', size = 4) +
      expand_limits(y = y, x = x))
}

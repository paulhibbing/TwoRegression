# Data and functions ------------------------------------------------------

utils::data(raw_for_cv, package = "TwoRegression")

cv <- function(signal) {

  if (any(is.na(signal))) return(NA)
  if (mean(signal) == 0) {
    0
  } else {
    sd(signal)/mean(signal) * 100
  }

}

get_cvPER_old <- function(
    big_data, window_secs = 10, Algorithm, verbose = FALSE
) {

  inds <- sapply(
    seq(big_data),
    function(x) {
      sapply(
        window_secs:1,
        function(y) {
          ((x - y):(x - y + window_secs - 1)) + 1
        })
    },
    simplify = FALSE
  )

  CVS <- do.call(
    rbind,
    lapply(
      inds,
      function(x) {
        values <- sapply(
          data.frame(x),
          function(y) {
            Y <- y[y > 0 & y <= length(big_data)]
            if (length(y) != length(Y)) {
              data.frame(CV = NA)
            } else {
              data.frame(CV = cv(big_data[Y]))
            }
          },
          simplify = FALSE
        )

        CV <- sapply(
          do.call(rbind, values),
          min,
          na.rm = TRUE
        )

        return(CV)
      }
    )
  )

  stopifnot(ncol(CVS)==1 | is.vector(CVS))
  CVS <- as.vector(CVS)

  CVS

}

get_cv_static_old <- function(x, window_size = 10, verbose = FALSE) {

  if (verbose) message_update(31, window_size = window_size)
  block <-
    {length(x) / window_size} %>%
    ceiling(.) %>%
    seq(.) %>%
    rep(each = window_size) %>%
    {.[seq(length(x))]}

  cvs <- tapply(x, block, cv)

  ifelse(table(block) == window_size, cvs, NA) %>%
  as.vector(.)

}


# Tests -------------------------------------------------------------------

test_that("TwoRegression sliding CV (RcppRoll) lines up with original", {

  testthat::expect_equal(
    get_cvPER_old(raw_for_cv$ENMO),
    cv_2rm(raw_for_cv$ENMO)
  )

  testthat::expect_equal(
    get_cvPER_old(raw_for_cv$ENMO, 6),
    cv_2rm(raw_for_cv$ENMO, 6)
  )

})

test_that("TwoRegression static CV (RcppRoll) lines up with original", {

  testthat::expect_equal(
    get_cv_static_old(raw_for_cv$ENMO),
    cv_2rm(raw_for_cv$ENMO, approach = "static"),
    ignore_attr = TRUE
  )

  testthat::expect_equal(
    get_cv_static_old(raw_for_cv$ENMO, 6),
    cv_2rm(raw_for_cv$ENMO, 6, "static"),
    ignore_attr = TRUE
  )

})

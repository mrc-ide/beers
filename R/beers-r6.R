library(R6)

Beers_Interpolator <- R6Class("Beers_Interpolator",

  public = list(
    b1 = NULL,
    b2 = NULL,
    bm = NULL,
    bp = NULL,
    bf = NULL,

    initialize = function(b1, b2, bm, bp, bf) {
      self$b1 <- b1
      self$b2 <- b2
      self$bm <- bm
      self$bp <- bp
      self$bf <- bf
    },

    interpolate = function(points) {
      if (length(points) < 6) {
        stop("Beers interpolation requires at least 6 points")

      } else if (!is.numeric(points)) {
        stop("Beers interpolation requires numeric argument")
      }

      res <- rep(0, ((length(points) * 5) - 4))
      for (p in 1:(length(points) - 1)) {

        if (p == 1) {
          for (i in 1:5) res[i] <-
            sum(points[1:6] * self$b1[i, 1:6])

        } else if (p == 2) {
          for (i in 1:5) res[i + 5] <-
            sum(points[1:6] * self$b2[i, 1:6])

        } else if (p == length(points) - 2) {
          for (i in 1:5) res[(((p - 1) * 5) + i)] <-
            sum(points[((p - 4) + 1):((p - 4) + 6)] * self$bp[i, 1:6])

        } else if (p == length(points)-1) {
          for (i in 1:6) res[(((p - 1) * 5) + i)] <-
            sum(points[((p - 5) + 1):((p - 5) + 6)] * self$bf[i, 1:6])

        } else {
          for (i in 1:5) res[(((p - 1) * 5) + i)] <-
            sum(points[((p - 3) + 1):((p - 3) + 6)] * self$bm[i, 1:6])
        }
      }
      res
    }
  )
)

beers_modified_r6 <- Beers_Interpolator$new(
  b1 = matrix(nrow = 5, ncol = 6, byrow = TRUE, data = c(
    1.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
    0.6668,  0.5270, -0.2640,  0.0820, -0.0140,  0.0022,
    0.4099,  0.8592, -0.3598,  0.1052, -0.0173,  0.0028,
    0.2196,  1.0279, -0.3236,  0.0874, -0.0136,  0.0023,
    0.0862,  1.0644, -0.1916,  0.0464, -0.0066,  0.0012)),

  b2 = matrix(nrow = 5, ncol = 6, byrow = TRUE, data = c(
     0.0000,  1.0000,  0.0000,  0.0000,  0.0000,  0.0000,
    -0.0486,  0.8655,  0.2160, -0.0350,  0.0030, -0.0009,
    -0.0689,  0.6903,  0.4238, -0.0442,  0.0003, -0.0013,
    -0.0697,  0.5018,  0.5938, -0.0152, -0.0097, -0.0010,
    -0.0589,  0.3233,  0.7038,  0.0578, -0.0257, -0.0003)),

  bm = matrix(nrow = 5, ncol = 6, byrow = TRUE, data = c(
    -0.0430,  0.1720,  0.7420,  0.1720, -0.0430,  0.0000,
    -0.0270,  0.0587,  0.7072,  0.3162, -0.0538, -0.0013,
    -0.0141, -0.0132,  0.6098,  0.4708, -0.0477, -0.0056,
    -0.0056, -0.0477,  0.4708,  0.6098, -0.0132, -0.0141,
    -0.0013, -0.0538,  0.3162,  0.7072,  0.0587, -0.0270)),

  bp = matrix(nrow = 5, ncol = 6, byrow = TRUE, data = c(
    0.0000, -0.0430,  0.1720,  0.7420,  0.1720, -0.0430,
   -0.0003, -0.0257,  0.0578,  0.7038,  0.3233, -0.0589,
   -0.0010, -0.0097, -0.0152,  0.5938,  0.5018, -0.0697,
   -0.0013,  0.0003, -0.0442,  0.4238,  0.6903, -0.0689,
   -0.0009,  0.0030, -0.0350,  0.2160,  0.8655, -0.0486)),

  bf = matrix(nrow = 6, ncol = 6, byrow = TRUE, data = c(
    0.0000,  0.0000,  0.0000,  0.0000,  1.0000,  0.0000,
    0.0012, -0.0066,  0.0464, -0.1916,  1.0644,  0.0862,
    0.0023, -0.0136,  0.0874, -0.3236,  1.0279,  0.2196,
    0.0028, -0.0173,  0.1052, -0.3598,  0.8529,  0.4099,
    0.0022, -0.0140,  0.0820, -0.2640,  0.5270,  0.6668,
    0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  1.0000))
)

beers_ordinary_r6 <- Beers_Interpolator$new(
  b1 = matrix(nrow = 5, ncol = 6, byrow = TRUE, data = c(
    1.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
    0.6667,  0.4969, -0.1426, -0.1006,  0.1079, -0.0283,
    0.4072,  0.8344, -0.2336, -0.0976,  0.1224, -0.0328,
    0.2148,  1.0204, -0.2456, -0.0536,  0.0884, -0.0244,
    0.0819,  1.0689, -0.1666, -0.0126,  0.0399, -0.0115)),

  b2 = matrix(nrow = 5, ncol = 6, byrow = TRUE, data = c(
     0.0000,  1.0000,  0.0000,  0.0000,  0.0000,  0.0000,
    -0.0404,  0.8404,  0.2344, -0.0216, -0.0196,  0.0068,
    -0.0497,  0.6229,  0.5014, -0.0646, -0.0181,  0.0081,
    -0.0389,  0.3849,  0.7534, -0.1006, -0.0041,  0.0053,
    -0.0191,  0.1659,  0.9354, -0.0906,  0.0069,  0.0015)),

  bm = matrix(nrow = 5, ncol = 6, byrow = TRUE, data = c(
    0.0000,  0.0000,  1.0000,  0.0000,  0.0000,  0.0000,
    0.0117, -0.0921,  0.9234,  0.1854, -0.0311,  0.0027,
    0.0137, -0.1101,  0.7194,  0.4454, -0.0771,  0.0087,
    0.0087, -0.0771,  0.4454,  0.7194, -0.1101,  0.0137,
    0.0027, -0.0311,  0.1854,  0.9234, -0.0921,  0.0117)),

  bp = matrix(nrow = 5, ncol = 6, byrow = TRUE, data = c(
    0.0000,  0.0000,  0.0000,  1.0000,  0.0000,  0.0000,
    0.0015,  0.0069, -0.0906,  0.9354,  0.1659, -0.0191,
    0.0053, -0.0041, -0.1006,  0.7534,  0.3849, -0.0389,
    0.0081, -0.0181, -0.0646,  0.5014,  0.6229, -0.0497,
    0.0068, -0.0196, -0.0216,  0.2344,  0.8404, -0.0404)),

  bf = matrix(nrow = 6, ncol = 6, byrow = TRUE, data = c(
    0.0000,  0.0000,  0.0000,  0.0000,  1.0000,  0.0000,
   -0.0115,  0.0399, -0.0126, -0.1666,  1.0689,  0.0819,
   -0.0244,  0.0884, -0.0536, -0.2456,  1.0204,  0.2148,
   -0.0328,  0.1224, -0.0976, -0.2336,  0.8344,  0.4072,
   -0.0283,  0.1079, -0.1006, -0.1426,  0.4969,  0.6667,
    0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  1.0000))
)

#' Beers modified algorithm for interpolation.
#'
#' @param points A list of at least 6 numbers.
#' @return A list where each original number exists with four interpolated points between each.
#' @export
#' @examples
#' beers_modified(c(1:6)) returns 1.0 1.2 1.4 1.6 1.8 2.0 2.2 ... 5.8 6.0

beers_modified <- function(points) {
  beers_modified_r6$interpolate(points)
}

#' Beers ordinary algorithm for interpolation.
#'
#' @param points A list of at least 6 numbers.
#' @return A list where each original number exists with four interpolated points between each.
#' @export
#' @examples
#' beers_ordinary(c(1:6)) returns 1.0 1.2 1.4 1.6 1.8 2.0 2.2 ... 5.8 6.0
#'
beers_ordinary <- function(points) {
  beers_ordinary_r6$interpolate(points)
}

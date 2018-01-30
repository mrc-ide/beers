load(file = "R/sysdata.rda")

interpolate <- function(B1, B2, BM, BP, BF) {
  function(points) {
    if (length(points) < 6) {
      stop("Beers interpolation requires at least 6 points")
    }  

    if (!is.numeric(points)) {
      stop("Beers interpolation requires numeric input")
    }

    n <- length(points)
    n_new <- (n * 5) - 4
    res <- rep(NA, n_new)

    # The algorithm uses a sliding-window style algorithm,
    # where interpolating  points inserted between x(i) and x(i+1) will
    # be a function of x(i-2, i-1, i, i+1, i+2). 

    # Hence, special cases are needed at the margins
    # for the first, second, penultimate, and final interpolation.

    # FIRST, SECOND, PENULTIMATE, FINAL

    for (i in 1:5) {

      res[i] <-   sum(points[1:6] * B1[i, 1:6])

      res[i+5] <- sum(points[1:6] * B2[i, 1:6])

      res[((n_new - 11) + i)] <-
        sum(points[((n - 6) + 1):((n - 6) + 6)] * BP[i, 1:6])

      res[((n_new - 6) +i)] <-
        sum(points[((n - 6) + 1):((n - 6) + 6)] * BF[i, 1:6])

    }

    # Middle frames. We know n >=6, so 3:(n-3) is always valid

    for (p in 3:(n - 3)) {
      for (i in 1:5) res[(((p - 1) * 5) + i)] <-
        sum(points[((p - 3) + 1):((p - 3) + 6)] * BM[i, 1:6])  
    }

    # Copy final point.

    res[n_new] <- points[n]

    res
  }
}

subdivide <- function(B1, B2, BM, BP, BF) {
  function(points) {
    if (length(points) < 5) {
      stop("Beers subdivision requires at least 5 bins")
    }  
    
    if (!is.numeric(points)) {
      stop("Beers subdivision requires numeric input")
    }
    
    n <- length(points)
    n_new <- (n * 5)
    res <- rep(NA, n_new)
    
    # The algorithm uses a sliding-window style algorithm,
    # where subdividing bin x(i) will be a function of
    # bins x(i-2, i-1, i, i+1, i+2). 
    
    # Hence, special cases are needed at the margins
    # for the first, second, penultimate, and final interpolation.
    
    # FIRST, SECOND, PENULTIMATE, FINAL
    
    for (i in 1:5) {
      
      res[i] <-   sum(points[1:5] * B1[i, 1:5])
      
      res[i+5] <- sum(points[1:5] * B2[i, 1:5])
      
      res[((n_new - 10) + i)] <-
        sum(points[((n - 5) + 1):((n - 5) + 5)] * BP[i, 1:5])
      
      res[((n_new - 5) + i)] <-
        sum(points[((n - 5) + 1):((n - 5) + 5)] * BF[i, 1:5])
      
    }
    
    # Middle frames. We know n >=5, so 3:(n-2) is always valid
    
    for (p in 3:(n - 2)) {
      for (i in 1:5) res[(((p - 1) * 5) + i)] <-
          sum(points[((p - 3) + 1):((p - 3) + 5)] * BM[i, 1:5])  
    }
    
    # Copy final point.
    
    res[n_new] <- points[n]
    
    res
  }
}

#' Beers ordinary algorithm for interpolation.
#'
#' @param points A list of at least 6 numbers.
#' @return A list where each original number exists with four interpolated points between each.
#' @export
#' @examples
#' beers_int_ordinary(1:6)
beers_int_ordinary <- interpolate(BEERS_INT_ORD_FIRST, BEERS_INT_ORD_SECOND,
                              BEERS_INT_ORD_MID, BEERS_INT_ORD_PENULT,
                              BEERS_INT_ORD_FINAL)

#' Beers modified algorithm for interpolation.
#'
#' @param points A list of at least 6 numbers.
#' @return A list where each original number exists with four interpolated points between each.
#' @export
#' @examples
#' beers_int_modified(1:6)
beers_int_modified <- interpolate(BEERS_INT_MOD_FIRST, BEERS_INT_MOD_SECOND,
                              BEERS_INT_MOD_MID, BEERS_INT_MOD_PENULT,
                              BEERS_INT_MOD_FINAL)

#' Beers ordinary algorithm for subdivision.
#'
#' @param points A list of at least 5 numbers.
#' @return A list 5 times as long as the original, with each original entry subdivided into 5.
#' @export
#' @examples
#' beers_sub_ordinary(1:6)
beers_sub_ordinary <- subdivide(BEERS_SUB_ORD_FIRST, BEERS_SUB_ORD_SECOND,
                                BEERS_SUB_ORD_MID, BEERS_SUB_ORD_PENULT,
                                BEERS_SUB_ORD_FINAL)

#' Beers modified algorithm for subdivision.
#'
#' @param points A list of at least 5 numbers.
#' @return A list 5 times as long as the original, with each original entry subdivided into 5.
#' @export
#' @examples
#' beers_sub_modified(1:6)
beers_sub_modified <- subdivide(BEERS_SUB_MOD_FIRST, BEERS_SUB_MOD_SECOND,
                                BEERS_SUB_MOD_MID, BEERS_SUB_MOD_PENULT,
                                BEERS_SUB_MOD_FINAL)
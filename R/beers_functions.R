load(file = "R/sysdata.rda")

interpolate <- function(points, B1, B2, BM, BP, BF) {
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

subdivide <- function(points, B1, B2, BM, BP, BF) {
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
    
  res
}

#' Beers interpolation, using ordinary or modified method.
#' 
#' Create 4 interpolated points between each pair of given values.
#' With the ordinary method, the given data points given will be included unchanged in the interpolated list.
#' With the modified method, some smoothing occurs, and only the first and last points given are guaranteed to appear unchanged in the interpolated list.
#'
#' @param points A list of at least 6 numbers, eg, populations over time.
#' @return A list with four extra interpolated points between each pair of given points. 
#' @export
#' @rdname beers_int
#' @examples
#' # Interpolate population of UK (1950, 1955, 1960, 1965, 1970, 1975) to yearly points
#' 
#' beers_int_ordinary(c(50616014, 51123707, 52433157, 54303107, 55634935, 56211947))
beers_int_ordinary <- function(points) {
  interpolate(points, BEERS_INT_ORD_FIRST, BEERS_INT_ORD_SECOND,
                      BEERS_INT_ORD_MID, BEERS_INT_ORD_PENULT,
                      BEERS_INT_ORD_FINAL)
}

#' @export
#' @rdname beers_int
#' @examples
#' beers_int_modified(c(50616014, 51123707, 52433157, 54303107, 55634935, 56211947))
beers_int_modified <- function(points) {
  interpolate(points, BEERS_INT_MOD_FIRST, BEERS_INT_MOD_SECOND,
                      BEERS_INT_MOD_MID, BEERS_INT_MOD_PENULT,
                      BEERS_INT_MOD_FINAL)
}

#' Beers subdivision, using ordinary or modified method.
#' 
#' With the ordinary method, each set of 5 subdivided values will always sum to the original data point given.
#' With the modified method, some smoothing occurs, and this property is only true for the first and last given data point.
#' @param points A list of at least 5 numbers, eg, populations by 5-year age-band.
#' @return For subdivision: a list 5 times as long as the original, with each point subdivided into 5.
#' @export
#' @rdname beers_sub
#' @examples
#' 
#' # Subdivide population of UK (2015), ages 0-4, 5-9, 10-14, 15-19, 20-24 into single years.
#' 
#' beers_sub_ordinary(c(4042918, 3927745, 3529200, 3779712, 4174572))
beers_sub_ordinary <- function(points) {
  subdivide(points, BEERS_SUB_ORD_FIRST, BEERS_SUB_ORD_SECOND,
                    BEERS_SUB_ORD_MID,   BEERS_SUB_ORD_PENULT,
                    BEERS_SUB_ORD_FINAL)
}


#' @export
#' @rdname beers_sub
#' @examples
#' beers_sub_modified(c(4042918, 3927745, 3529200, 3779712, 4174572))
beers_sub_modified <- function(points) {
  subdivide(points, BEERS_SUB_MOD_FIRST, BEERS_SUB_MOD_SECOND,
                    BEERS_SUB_MOD_MID,   BEERS_SUB_MOD_PENULT,
                    BEERS_SUB_MOD_FINAL)
}

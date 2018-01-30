library(beers)
context("Beers Interpolation and Subdivision")

test_that("Beers ordinary interpolation simple test", {
  expect_equal(beers_int_ordinary(1:6), seq(1.0, 6.0, 0.2))
})

test_that("Beers interpolation modified simple test", {
  expect_equal(beers_int_modified(1:6), seq(1.0, 6.0, 0.2))
})

test_that("Beers subdivision ordinary simple test", {
  expect_equal(beers_sub_ordinary(seq(10, 50, 10)),seq(1.2, 10.8, 0.4))
})

test_that("Beers subdivision modified simple test", {
  expect_equal(beers_sub_ordinary(seq(10, 50, 10)),seq(1.2, 10.8, 0.4))
})

test_that("Beers ordinary interpolation preserves original points", {
  x <- sample(10000)[1:10]
  bx <- beers_int_ordinary(x)
  expect_equal(x, bx[c(seq(1, length(bx), 5))])
})

test_that("Beers modified interpolation preserves original first/last point", {
  x <- sample(10000)[1:10]
  bx <- beers_int_modified(x)
  expect_equal(x[c(1,10)], bx[c(1, length(bx))])
})

test_that("Beers ordinary subdivision sums back to original", {
  x <- sample(10000)[1:10]
  sx <- colSums(matrix(ncol = 10, nrow = 5, byrow = FALSE, 
                       data = beers_sub_ordinary(x)))
  expect_equal(x, sx)
})

test_that("Beers modified subdivision sums back to original in end panels", {
  x <- sample(10000)[1:10]
  sx <- colSums(matrix(ncol = 10, nrow = 5, byrow = FALSE, 
                       data = beers_sub_modified(x)))
  expect_equal(x[c(1, 10)], sx[c(1, 10)])
})

test_that("Input validation", {
  expect_error(beers_sub_ordinary(NA))
  expect_error(beers_int_ordinary(NA))
  expect_error(beers_sub_modified(NA))
  expect_error(beers_int_modified(NA))
  expect_error(beers_sub_ordinary(c(1, 2, 3, 4)))
  expect_error(beers_int_ordinary(c(1, 2, 3, 4, 5)))
  expect_error(beers_sub_modified(c(1, 2, 3, 4)))
  expect_error(beers_int_modified(c(1, 2, 3, 4, 5)))
  expect_error(beers_sub_ordinary(c(1, 2, 3, 4, 5, 6, 'a')))
  expect_error(beers_int_ordinary(c(1, 2, 3, 4, 5, 6, 'a')))
  expect_error(beers_sub_modified(c(1, 2, 3, 4, 5, 6, 'a')))
  expect_error(beers_int_modified(c(1, 2, 3, 4, 5, 6, 'a')))
})
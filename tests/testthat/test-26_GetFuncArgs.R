test_that("It works", {
  f <- function(a, b, c, d = 1) {
    GetFuncArgs()
  }

  l <- f(-1, -2, -3, -4)

  expect_equal(l, list(a = -1, b = -2, c = -3, d = -4))
})


test_that("It works", {
  g <- function(a, b, c, d = 1) {
    GetFuncArgs(exclude = "d")
  }

  l2 <- g(-1, -2, -3, -4)

  g2 <- function(a, b, c, d = 1) {
    GetFuncArgs(exclude = 4)
  }

  l3 <- g(-1, -2, -3, -4)

  expect_equal(l2, list(a = -1, b = -2, c = -3))
  expect_equal(l3, list(a = -1, b = -2, c = -3))
})

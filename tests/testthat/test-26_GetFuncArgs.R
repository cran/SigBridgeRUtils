test_that("It works", {
  f <- function(a, b, c, d = 1L) {
    GetFuncArgs()
  }

  l <- f(-1L, -2L, -3L, -4L)

  expect_equal(l, list(a = -1L, b = -2L, c = -3L, d = -4L))
})


test_that("It works2", {
  g <- function(a, b, c, d = 1L) {
    GetFuncArgs(exclude = "d")
  }

  l2 <- g(-1L, -2L, -3L, -4L)

  g2 <- function(a, b, c, d = 1L) {
    GetFuncArgs(exclude = 4L)
  }

  l3 <- g(-1L, -2L, -3L, -4L)

  g3 <- function(a = 1L, b = 2L, c = 3L, d = 4L) {
    GetFuncArgs(exclude = 4L)
  }

  l4 <- g3()

  expect_equal(l2, list(a = -1L, b = -2L, c = -3L))
  expect_equal(l3, list(a = -1L, b = -2L, c = -3L))
  expect_equal(l4, list(a = 1L, b = 2L, c = 3L))
})

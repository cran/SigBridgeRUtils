test_that("multiplication works", {
  f <- function(
    a = 1,
    b = 2,
    c = 3,
    ...
  ) {
    a * b * c
    message(a, b, c, ...)
  }

  l <- list(a = 10, b = 20, x = 30, y = 40)

  l2 <- FilterArgs4Func(l, f)
  expect_equal(l2, list(a = 10, b = 20))
})

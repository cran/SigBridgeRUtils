test_that("multiplication works", {
  f <- function(
    a = 1L,
    b = 2L,
    c = 3L,
    ...
  ) {
    a * b * c
    message(a, b, c, ...)
  }

  l <- list(a = 10L, b = 20L, x = 30L, y = 40L)

  l2 <- FilterArgs4Func(l, f)
  expect_equal(l2, list(a = 10L, b = 20L))
})

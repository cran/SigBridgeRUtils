test_that("multiplication works", {
  func1 <- function(a, b, c = 10) a + b + c
  func2 <- function(x, y, ...) x * y
  func3 <- function(p, q) p - q
  # Argument list to match
  args <- list(a = 1, b = 2)
  res1 <- MatchFunc2Args(args, func1, func2, func3, name_only = TRUE)
  # Returns c("func1")
  # Anonymous function example
  res2 <- MatchFunc2Args(
    list(x = 5, y = 3),
    function(x, y) x + y,
    mean,
    name_only = TRUE
  )
  # Find compatible functions (returns function objects)
  res3 <- MatchFunc2Args(args, func1, func2, func3)
  # Returns list containing func1 (func3 lacks 'a' and 'b')
  # Get only function names/identifiers

  expect_equal(res1, "func1")
  expect_equal(res2, "function(x, y) x + y")
  expect_equal(res3, list(func1 = func1))
})

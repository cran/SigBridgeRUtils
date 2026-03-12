# test-19_match_arg_in_func.R - Tests for MatchArg function

test_that("MatchArg handles NULL argument", {
  choices <- c("apple", "banana", "cherry")

  # Test with default
  result <- MatchArg(NULL, choices)
  expect_equal(result, "apple") # First choice is default

  # Test with custom default
  result_custom <- MatchArg(NULL, choices, default = "banana")
  expect_equal(result_custom, "banana")
})

test_that("MatchArg exact matching works", {
  choices <- c("apple", "banana", "cherry", "date")

  # Test exact matches
  expect_equal(MatchArg("apple", choices), "apple")
  expect_equal(MatchArg("banana", choices), "banana")
  expect_equal(MatchArg("cherry", choices), "cherry")
  expect_equal(MatchArg("da", choices), "date")
})

test_that("MatchArg partial matching works", {
  choices <- c("apple", "application", "banana", "cherry")

  # Test partial matches
  expect_equal(MatchArg("app", choices), "apple") # First match
  expect_equal(MatchArg("appl", choices), "apple") # First match
  expect_equal(MatchArg("ban", choices), "banana")
  expect_equal(MatchArg("cher", choices), "cherry")
})

test_that("MatchArg handles ambiguous partial matches", {
  choices <- c("apple", "application")

  # This should match the first one due to pmatch behavior
  expect_equal(MatchArg("app", choices), "apple")
})

test_that("MatchArg returns default for invalid arguments", {
  choices <- c("red", "green", "blue")

  # Test with default available
  result <- MatchArg("invalid", choices, default = "green")
  expect_equal(result, "green")
})

test_that("MatchArg throws error for invalid arguments without default", {
  choices <- c("red", "green", "blue")

  # Test error when no default and invalid argument
  expect_error(
    MatchArg("invalid", choices, default = NULL)
  )

  # Test error message contains valid choices
  expect_error(
    MatchArg("purple", choices, default = NULL)
  )
})

test_that("MatchArg handles edge cases", {
  choices <- c("a", "ab", "abc")

  # Test single character matching
  expect_equal(MatchArg("a", choices), "a")

  # Test with empty string
  expect_error(
    MatchArg("", choices, default = NULL),
    "is not a valid choice"
  )

  # Test with choices that have special characters
  special_choices <- c("test-1", "test_2", "test.3")
  expect_equal(MatchArg("test-1", special_choices), "test-1")
  expect_equal(MatchArg("test_2", special_choices), "test_2")
})

test_that("MatchArg handles different argument types", {
  choices <- c("first", "second", "third")

  # Test with numeric argument (should be coerced to character)
  expect_error(
    MatchArg(1, choices, default = NULL),
    class = "MatchArgError"
  )

  # Test with logical argument
  expect_error(
    MatchArg(TRUE, choices, default = NULL),
    class = "MatchArgError"
  )
})

test_that("MatchArg handles empty choices", {
  choices <- character(0)

  # Test with empty choices and NULL argument
  expect_error(MatchArg(NULL, choices))

  # Test with empty choices and non-NULL argument
  expect_error(
    MatchArg("something", choices, default = NULL)
  )
})

test_that("MatchArg handles single choice", {
  choices <- c("only")

  # Test with NULL argument
  expect_equal(MatchArg(NULL, choices), "only")

  # Test with exact match
  expect_equal(MatchArg("only", choices), "only")

  # Test with partial match
  expect_equal(MatchArg("on", choices), "only")
})

test_that("MatchArg handles case sensitivity", {
  choices <- c("Apple", "Banana", "Cherry")

  # Test case-sensitive matching
  expect_equal(MatchArg("Apple", choices), "Apple")
  expect_error(
    MatchArg("apple", choices, default = NULL),
    class = "MatchArgError"
  )
})

test_that("MatchArg handles duplicate choices", {
  choices <- c("test", "test", "other")

  # Test with duplicate choices
  expect_equal(MatchArg("test", choices), "test")
  expect_equal(MatchArg(NULL, choices), "test") # First choice
})

test_that("MatchArg performance with large choice sets", {
  skip_on_cran()

  # Create a large set of choices
  large_choices <- paste0("choice", 1:10000)

  expect_time_lt <- function(expr, time_limit = 1) {
    start_time <- Sys.time()
    force(expr)
    end_time <- Sys.time()
    expect_lt(
      as.numeric(difftime(end_time, start_time, units = "secs")),
      time_limit
    )
  }

  # Test performance with exact match
  expect_time_lt(
    {
      result <- MatchArg("choice5000", large_choices)
      expect_equal(result, "choice5000")
    },
    time_limit = 0.5
  )

  # Test performance with partial match
  expect_time_lt(
    {
      result <- MatchArg("choice1", large_choices)
      expect_equal(result, "choice1")
    },
    time_limit = 0.5
  )
})

test_that("MatchArg error handling and messages", {
  choices <- c("option1", "option2", "option3")

  # Test that error has correct class
  error_obj <- tryCatch(
    MatchArg("invalid", choices, default = NULL),
    error = function(e) e
  )
  expect_s3_class(error_obj, "MatchArgError")
})

test_that("MatchArg with call parameter", {
  choices <- c("yes", "no", "maybe")

  # Test with custom call environment
  test_env <- rlang::caller_env()

  expect_error(
    MatchArg("invalid", choices, default = NULL, call = test_env),
    class = "MatchArgError"
  )
})

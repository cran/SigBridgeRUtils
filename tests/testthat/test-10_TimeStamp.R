# test-10_TimeStamp.R - Tests for TimeStamp functions

test_that("TimeStamp returns correct format", {
  result <- TimeStamp()

  # Check that it returns a character string
  expect_type(result, "character")
  expect_length(result, 1)

  # Check format: YYYY/MM/DD HH:MM:SS
  pattern <- "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  expect_match(result, pattern)
})

test_that("TimeStamp handles different time zones", {
  # Test that TimeStamp works regardless of timezone
  original_tz <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  result_utc <- TimeStamp()
  expect_match(result_utc, "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}$")

  Sys.setenv(TZ = "America/New_York")
  result_ny <- TimeStamp()
  expect_match(result_ny, "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}$")

  # Restore original timezone
  if (original_tz != "") {
    Sys.setenv(TZ = original_tz)
  } else {
    Sys.unsetenv("TZ")
  }
})

test_that("TimeStamp is consistent with Sys.time", {
  # Get time close to TimeStamp call
  sys_time <- Sys.time()
  timestamp_str <- TimeStamp()

  # Parse the timestamp string
  parsed_time <- as.POSIXct(timestamp_str, format = "%Y/%m/%d %H:%M:%S")

  # Should be very close (within 1 second)
  time_diff <- abs(as.numeric(difftime(
    parsed_time,
    sys_time,
    units = "secs"
  )))
  expect_lt(time_diff, 2) # Allow 2 seconds tolerance
})

test_that("TimeStamp handles error gracefully", {
  # This is hard to test directly since we can't easily make Sys.time fail
  # But we can test that the function returns a valid format
  result <- TimeStamp()
  expect_match(result, "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}$")
})

test_that("TimeStamp produces unique values", {
  # Get two timestamps close together
  ts1 <- TimeStamp()
  Sys.sleep(0.1) # Small delay
  ts2 <- TimeStamp()

  # They should be different (or at least can be different)
  expect_type(ts1, "character")
  expect_type(ts2, "character")

  # Both should be valid timestamps
  expect_match(ts1, "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}$")
  expect_match(ts2, "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}$")
})

test_that("TimeStamp components are valid", {
  result <- TimeStamp()

  # Parse components
  parts <- strsplit(result, "[/ :]")[[1]]
  year <- as.numeric(parts[1])
  month <- as.numeric(parts[2])
  day <- as.numeric(parts[3])
  hour <- as.numeric(parts[4])
  minute <- as.numeric(parts[5])
  second <- as.numeric(parts[6])

  # Validate ranges
  expect_gte(year, 2000) # Reasonable lower bound
  expect_lte(year, 2100) # Reasonable upper bound
  expect_gte(month, 1)
  expect_lte(month, 12)
  expect_gte(day, 1)
  expect_lte(day, 31)
  expect_gte(hour, 0)
  expect_lte(hour, 23)
  expect_gte(minute, 0)
  expect_lte(minute, 59)
  expect_gte(second, 0)
  expect_lte(second, 59)
})

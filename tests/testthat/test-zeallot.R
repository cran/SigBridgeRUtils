# Tests for unpacking assignment

test_that("unpacks list and atomic vector elements", {
  c(lat, lng) %<-% list(38.061944, -122.643889)
  expect_equal(lat, 38.061944)
  expect_equal(lng, -122.643889)

  c(first, last) %<-% c("Ai", "Genly")
  expect_identical(first, "Ai")
  expect_identical(last, "Genly")
})

test_that("unpacks values returned by functions", {
  coords_list <- function() list(38.061944, -122.643889)
  to_polar <- function(x, y) c(sqrt(x^2 + y^2), atan(y / x))

  c(lat, lng) %<-% coords_list()
  expect_equal(c(lat, lng), c(38.061944, -122.643889))

  c(radius, angle) %<-% to_polar(12, 5)
  expect_equal(radius, 13)
  expect_equal(angle, atan(5 / 12))
})

test_that("unpacks data-frame columns, named elements, and list elements", {
  data <- data.frame(mpg = c(21, 22), cyl = c(6, 4), wt = c(2.62, 3.215))
  c(mpg, cyl) %<-% data
  expect_equal(mpg, data$mpg)
  expect_equal(cyl, data$cyl)

  c(cyl = , wt = ) %<-% data
  expect_equal(cyl, data$cyl)
  expect_equal(wt, data$wt)

  groups <- split(mtcars, mtcars$gear)
  c(gear3, gear4, gear5) %<-% groups
  expect_equal(gear3, groups[["3"]])
  expect_equal(gear4, groups[["4"]])
  expect_equal(gear5, groups[["5"]])
})

test_that("supports leading, middle, and trailing collectors", {
  c(first, ..rest) %<-% letters
  expect_identical(first, "a")
  expect_identical(rest, letters[-1L])

  c(..skip, penultimate, last) %<-% 1:5
  expect_identical(skip, 1:3)
  expect_identical(penultimate, 4L)
  expect_identical(last, 5L)

  c(begin, ..middle, end) %<-% list("first", 2, 3, "last")
  expect_identical(begin, "first")
  expect_identical(middle, list(2, 3))
  expect_identical(end, "last")

  c(begin, .., end) %<-% 1:5
  expect_identical(begin, 1L)
  expect_identical(end, 5L)
})

test_that("supports nested targets and ignored elements", {
  c(c(x, y), z) %<-% list(list(1, 2), 3)
  expect_identical(x, 1)
  expect_identical(y, 2)
  expect_identical(z, 3)

  c(name, c(latitude, longitude), .) %<-%
    list("station", list(38.061944, -122.643889), "ignored")
  expect_identical(name, "station")
  expect_equal(latitude, 38.061944)
  expect_equal(longitude, -122.643889)
  expect_false(exists(".", inherits = FALSE))
})

test_that("returns the right-hand side invisibly", {
  values <- list("Ai", "Genly")
  result <- c(first, last) %<-% values

  expect_identical(result, values)
  expect_identical(first, "Ai")
  expect_identical(last, "Genly")
})

test_that("reports invalid targets and unpacking failures", {
  expect_error(
    list(first, second) %<-% list("Moe", "Donald"),
    "Invalid destructuring target"
  )
  expect_error(
    c(first, second) %<-% quote(Moe),
    "Unsupported rhs type"
  )
  expect_error(
    c(first, second) %<-% list("Moe"),
    "Cannot unpack element 2"
  )
  expect_error(
    c(missing_column = ) %<-% mtcars,
    "Named element 'missing_column' was not found"
  )
  expect_error(
    c(..first, ..second) %<-% letters,
    "Only one collector"
  )
})

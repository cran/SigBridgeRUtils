skip_on_cran()
skip_if_not_installed("zeallot")
skip_if_not_installed("microbenchmark")

`%<-raw%` <- zeallot::`%<-%`


test_that("2d data", {
  microbenchmark::microbenchmark(
    base = {
      mpg <- mtcars$mpg
      cyl <- mtcars$cyl
    },
    zeallot = c(mpg = , cyl = ) %<-raw% mtcars,
    SigBridgeRUtils = c(mpg = , cyl = ) %<-% mtcars
  )
})

test_that("vector", {
  v <- runif(100)
  microbenchmark::microbenchmark(
    base = {
      first <- v[1L]
      cyl <- v[100L]
    },
    zeallot = c(first, ..middle, last) %<-raw% v,
    SigBridgeRUtils = c(first, ..middle, last) %<-% v
  )
})


test_that("nested list", {
  l <- list(a = list(b = list(c = 1, d = 2), e = 3), f = 4)
  microbenchmark::microbenchmark(
    base = {
      c <- l$a$b$c
      d <- l$a$b$d
      e <- l$a$e
      f <- l$f
    },
    zeallot = c(c(c(c, d), e), f) %<-raw% l,
    SigBridgeRUtils = c(c(c(c, d), e), f) %<-% l
  )
})

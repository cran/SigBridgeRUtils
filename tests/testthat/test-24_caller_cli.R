test_that("multiplication works", {
  caller_cli <- CreateCallerCliEnv()

  f <- function(x) {
    message("f:hello world")
    caller_cli$cli_alert_warning("g:hello world")
    g(x)
    return(1)
  }

  g <- function(x) {
    message("g:hello world")
    caller_cli$cli_alert_info("g:hello world")
    return(x^2)
  }

  caller_cli2 <- CreateCallerCliEnv(c("cli_abort", "cli_warn"))
  expect_error(caller_cli2$cli_abort("abort"))
  expect_warning(caller_cli2$cli_warn("warn"))
})

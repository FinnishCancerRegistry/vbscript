
context("calling vbscript lines and files")

testthat::test_that("calling lines as character vector works", {

  msg <- call_vbscript_lines(vbscript_lines_echo("hello world!"))
  testthat::expect_equal(tail(msg, 1L), "hello world!")

})









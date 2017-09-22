library(artsupport)
context("load_module_timing")

test_that("load_module_timing returns a correct tbl_df without label", {
  tmp <- load_module_timing("timeTracker.db")
  expect_is(tmp, "tbl_df")
  expect_false("lbl" %in% names(tmp))
  })

test_that("load_module_testing returns a correct tbl_df with label", {
  tmp <- load_module_timing("timeTracker.db", "foo")
  expect_is(tmp, "tbl_df")
  expect_true("lbl" %in% names(tmp))
  expect_true(all(tmp$lbl == "foo"))
})

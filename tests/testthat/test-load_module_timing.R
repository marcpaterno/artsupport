context("load_module_timing")

test_that("load_module_timing returns an empty tbl_df when called on a file with no samples", {
  tmp <- load_module_timing("timing-empty.db")
  expect_is(tmp, "tbl_df")
  expect_equal(nrow(tmp), 0L)
})

test_that("load_module_timing returns a correct tbl_df without label", {
  tmp <- load_module_timing("timeTracker.db")
  expect_is(tmp, "tbl_df")
  expect_false("lbl" %in% names(tmp))
})

test_that("load_module_timing returns a correct tbl_df with label", {
  tmp <- load_module_timing("timeTracker.db", "foo")
  expect_is(tmp, "tbl_df")
  expect_true("lbl" %in% names(tmp))
  expect_true(all(tmp$lbl == "foo"))
})

test_that("load_module_timing handles output module correctly", {
  tmp <- load_module_timing("timing-with-output-module.db")
  expect_is(tmp, "tbl_df")
  expect_equal(nrow(tmp), 224L)
})

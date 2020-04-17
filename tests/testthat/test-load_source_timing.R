context("load_source_timing")

test_that("load_source_timing returns an empty tbl_df when called on a file with no samples", {
  tmp <- load_source_timing("timing-empty.db")
  expect_is(tmp, "tbl_df")
  expect_equal(nrow(tmp), 0)
})

test_that("load_source_timing returns a correct tbl_df without label", {
  tmp <- load_source_timing("timeTracker.db")
  expect_is(tmp, "tbl_df")
  expect_false("lbl" %in% names(tmp))
  nr <- nrow(tmp)
  expect_equal(tmp$sample, 1:nr)
  })

test_that("load_source_testing returns a correct tbl_df with label", {
  tmp <- load_source_timing("timeTracker.db", "foo")
  expect_is(tmp, "tbl_df")
  expect_true("lbl" %in% names(tmp))
  expect_true(all(tmp$lbl == "foo"))
  nr <- nrow(tmp)
  expect_equal(tmp$sample, 1:nr)
})

library(artsupport)
context("load_event_memory_use")

test_that("load_event_memory_use returns a correct tbl_df without label", {
  tmp <- load_event_memory_use("memoryTracker.db")
  expect_is(tmp, "tbl_df")
  nr <- nrow(tmp)
  expect_equal(tmp$sample, 1:nr)
  expect_named(tmp,
               c("Run", "SubRun", "Event", "PostRSS", "PreRSS", "DeltaRSS",
                 "PostVsize","PreVsize","DeltaVsize", "sample"),
               ignore.order = TRUE)
})

test_that("load_event_memory_use returns a correct tbl_df with label", {
  tmp <- load_event_memory_use("memoryTracker.db", "foo")
  expect_is(tmp, "tbl_df")
  nr <- nrow(tmp)
  expect_equal(tmp$sample, 1:nr)
  expect_named(tmp,
               c("Run", "SubRun", "Event", "PostRSS", "PreRSS", "DeltaRSS",
                 "PostVsize","PreVsize","DeltaVsize", "sample", "lbl"),
               ignore.order = TRUE)
  expect_true(all(tmp$lbl == "foo"))
})

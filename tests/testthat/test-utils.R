

test_that("load_table loads EventInfo from MemoryTracker", {
  filename <- "memoryTracker.db"
  checkmate::assert_file_exists(filename)
  x <- load_table(filename, "EventInfo")
  expect_is(x, "tbl_df")
  expect_named(x, expected = c("Step", "Run", "SubRun", "Event", "Vsize", "RSS"))
  expect_equal(nrow(x), 20L)
})

test_that("load_table loads TimeSource from TimeTracker", {
  filename <- "timeTracker.db"
  x <- load_table(filename, "TimeSource")
  expect_is(x, "tbl_df")
  expect_named(x, expected = c("Run", "SubRun", "Event", "Source", "Time"))
  expect_equal(nrow(x),10L)
})

test_that("load_table adds label when supplied", {
  filename <- "timeTracker.db"
  x <- load_table(filename, "TimeSource", "foo")
  expect_equal(dplyr::last(names(x)), "lbl")
  labels <- x %>% dplyr::pull()
  expect_setequal(labels, "foo")
})

test_that("load_table handles empty table", {
  filename <- "timing-empty.db"
  x <- load_table(filename, "TimeEvent", "label")
  expect_is(x, "tbl_df")
  expect_equal(nrow(x), 0L)
  expect_named(x, c("Run", "SubRun", "Event", "Time", "lbl"))
})

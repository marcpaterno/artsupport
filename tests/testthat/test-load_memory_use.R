test_that("load_memory_use can load EventInfo", {
  filename <- "memoryTracker.db"
  checkmate::assert_file_exists(filename)
  x <- load_memory_use(filename, "EventInfo")
  expect_is(x, "tbl_df")
  expect_equal(nrow(x), 20L)
  expect_named(x, c("Step", "Run", "SubRun", "Event", "Vsize", "RSS"))
})

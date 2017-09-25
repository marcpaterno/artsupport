context("count_events")

test_that("counting (distinct) events works for unlabeled data", {
  a <- load_module_timing("timing-a.db")
  expect_equal(count_events(a), 120)
  b <- load_module_timing("timing-b.db")
  expect_equal(count_events(b), 240)
})

test_that("counting (distinct) events works for labeled data", {
  a <- load_module_timing("timing-a.db", "a")
  b <- load_module_timing("timing-b.db", "b")
  x <- rbind(a, b)
  expect_equal(count_events(x), 360)
})

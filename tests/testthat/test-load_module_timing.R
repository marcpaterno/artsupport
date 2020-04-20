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

test_that("load_module_timing excludes source by default", {
  tmp <- load_module_timing("timing-with-output-module.db")
  art_path_only <- dplyr::filter(tmp, .data$Path == "[art]")
  expect_equal(nrow(art_path_only), 32L)
  source_label_only <- dplyr::filter(tmp, .data$ModuleLabel == "source")
  expect_equal(nrow(source_label_only), 0L)
})

test_that("load_module_timing includes source when it should", {
  tmp <- load_module_timing("timing-with-output-module.db", include_source = TRUE)
  art_path_only <- dplyr::filter(tmp, .data$Path == "[art]")
  expect_equal(nrow(art_path_only), 64L)
  source_label_only <- dplyr::filter(tmp, .data$ModuleLabel == "source")
  expect_equal(nrow(source_label_only), 32L)
  modules_per_sample <- dplyr::group_by(tmp, sample) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::pull(n)
  expect_equal(modules_per_sample, rep(8, 32))
})

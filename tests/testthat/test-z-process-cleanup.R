test_that("forked test children are collected before the suite exits", {
  skip_on_os("windows")
  bb_cleanup_all_children()
  bb_reap_test_children()
  expect_length(parallel:::children(), 0L)
})

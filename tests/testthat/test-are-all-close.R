test_that("test all all close function", {
  expect_true(are_all_close(1, 1.001, abs_tol = 1e-2, rel_tol = 1e-2)) # should return true
  expect_true(!are_all_close(0.01, 0.0102, abs_tol = 1e-2, rel_tol = 1e-2)) # rel error exceeded, should return false
  expect_true(!are_all_close(1, 1.02, abs_tol = 1e-2, rel_tol = 1e-2)) # abs error exceeded, should return false
})

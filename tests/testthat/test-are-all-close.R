test_that('returns TRUE since no tolerance is exceeded', {
  expect_true(are_all_close(1, 1.001, abs_tol = 1e-2, rel_tol = 1e-2))
})
test_that('returns TRUE since no tolerance is exceeded for all vector input', {
  expect_true(are_all_close(c(1, 1.001, 1), c(1.001, 1, 1.001), abs_tol = 1e-2, rel_tol = 1e-2))
})
test_that('returns FALSE since rel error is exceeded', {
  expect_true(!are_all_close(0.01, 0.0102, abs_tol = 1e-2, rel_tol = 1e-2))
})
test_that('returns FALSE since rel error is exceeded in one vector entry position', {
  expect_true(!are_all_close(c(0.01, 0.01, 0.01), c(0.0102, 0.01, 0.01), abs_tol = 1e-2, rel_tol = 1e-2))
})
test_that('returns FALSE since rel error is exceeded in one vector entry position', {
  expect_true(!are_all_close(c(0.0098, 0.01, 0.01), c(0.01, 0.01, 0.01), abs_tol = 1e-2, rel_tol = 1e-2))
})
test_that('returns FALSE since abs error is exceeded', {
  expect_true(!are_all_close(1, 1.02, abs_tol = 1e-2, rel_tol = 1e-2))
})
test_that('returns FALSE since abs error is exceeded in one vector entry position', {
  expect_true(!are_all_close(c(1, 1, 1), c(1, 1.02, 1), abs_tol = 1e-2, rel_tol = 1e-2))
})
test_that('returns FALSE since abs error is exceeded in one vector entry position', {
  expect_true(!are_all_close(c(1, 0.98, 1), c(1, 1, 1), abs_tol = 1e-2, rel_tol = 1e-2))
})

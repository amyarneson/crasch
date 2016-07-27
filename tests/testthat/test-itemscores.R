library(crasch)
context("item.scores")

test_that("item.scores produces correct frequencies/proportions", {
  expect_equal(matrix(item.scores(SUP)$SUP$counts, nrow = 15, ncol = 5),
               matrix(c(1, NA,  3, 17, 17,
                        3,  3,  1, 12, 18,
                        1,  4, 11,  9, 13,
                        2,  0,  9, 20,  7,
                        4, 13,  4,  6,  3,
                        5, NA,  1, 13, 12,
                        1,  2,  7, 18,  2,
                        4,  2, NA, 16, 10,
                        2,  3,  7,  8, 10,
                        1,  3,  8,  6,  3,
                        1,  0,  2,  6, 12,
                        1, NA,  8, 11, NA,
                        5, NA, 11,  5, NA,
                        2,  1,  0,  0, 17,
                        3,  4,  6,  5,  2
                        ), nrow = 15, ncol = 5, byrow = TRUE))

  expect_equal(round(matrix(item.scores(SUP)$SUP$proportions, nrow = 15,
                            ncol = 5), 2),
               matrix(c(0.03,   NA, 0.08, 0.45, 0.45,
                        0.08, 0.08, 0.03, 0.32, 0.49,
                        0.03, 0.11, 0.29, 0.24, 0.34,
                        0.05, 0   , 0.24, 0.53, 0.18,
                        0.13, 0.43, 0.13, 0.20, 0.10,
                        0.16,   NA, 0.03, 0.42, 0.39,
                        0.03, 0.07, 0.23, 0.60, 0.07,
                        0.12, 0.06,   NA, 0.50, 0.31,
                        0.07, 0.10, 0.23, 0.27, 0.33,
                        0.05, 0.14, 0.38, 0.29, 0.14,
                        0.05, 0   , 0.10, 0.29, 0.57,
                        0.05,   NA, 0.40, 0.55,   NA,
                        0.24,   NA, 0.52, 0.24,   NA,
                        0.10, 0.05, 0   , 0   , 0.85,
                        0.15, 0.20, 0.30, 0.25, 0.10
               ), nrow = 15, ncol = 5, byrow = TRUE))
})

test_that("item.scores produces expected errors", {
  # input is not craschR results
  expect_error(item.scores(AMYwide))
  # palette issues
  expect_error(item.scores(SUP, palette = "pink"), 'Invalid palette argument')
  expect_error(item.scores(SUP, palette = c("pink", "green")),
               'Invalid palette argument.')
})

test_that("item.scores works w/different combinations of arguments", {
  # uniD
  expect_equal(length(item.scores(SUP)), 1)
  expect_equal(length(item.scores(SUP)$SUP), 2)
  # multiD
  expect_equal(length(item.scores(AMY)), 2)
  expect_equal(length(item.scores(AMY, dim = 2)), 1)
  expect_equal(length(item.scores(AMY, dim = 2, freqs = FALSE)), 1)
})

test_that("item.scores produces expected errors",{
  # uniD with invalid dim specified
  expect_error(item.scores(SUP, dim = 3)
               )
})

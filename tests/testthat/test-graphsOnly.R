library(crasch)
context("pers.hist, KIDMAP, infit.MNSQ")

test_that("pers.hist produces expected warnings", {
  expect_warning(pers.hist(SUP, writeout = FALSE))
})

test_that("pers.hist produces expected errors", {
  # invalid dimension specified
  expect_error(pers.hist(SUP, dim = 2))
  expect_error(pers.hist(AMY, dim = 0))
})

test_that("KIDMAP produces expected warnings", {
  expect_warning(KIDMAP(SUP, personID = "112", writeout = FALSE))
})

test_that("KIDMAP produces expected errors", {
  # invalid dimension specified
  expect_error(KIDMAP(SUP, personID = 112, dim = 2))
  expect_error(KIDMAP(AMY, personID = 112, dim = 0))
  expect_error(KIDMAP(SUP, personID = "1"), "Invalid personID argument.")
})

test_that("KIDMAP w/different argument combinations", {
  KIDMAP(SUP, personID = "110")
  KIDMAP(SUP, personID = 110)
  KIDMAP(AMY, personID = "112")
  KIDMAP(AMY, personID = 112)
})

test_that("infit.MNSQ produces expected warnings", {
  expect_warning(infit.MNSQ(SUP, writeout = FALSE))
})

test_that("infit.MNSQ produces expected errors", {
  # dimension instead of item vector specified
  expect_error(infit.MNSQ(SUP, dim = 2))
  # invalid items specified
  expect_error(infit.MNSQ(AMY, itemOrder = c(1:32)),
               "Invalid itemOrder argument.")
})

test_that("infit.MNSQ w/different argument combinations", {
  infit.MNSQ(AMY, params = "steps")
  infit.MNSQ(SUP, params = "steps")
  infit.MNSQ(AMY, itemOrder = c(1:4, 10:12))
  infit.MNSQ(AMY, itemOrder = c(1:4, 10:12), params = "steps")
})

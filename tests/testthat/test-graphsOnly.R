library(crasch)
context("pers.hist, KIDMAP, infit.MNSQ")

test_that("pers.hist produces expected errors", {
  # invalid dimension specified
  expect_error(pers.hist(SUP, dim = 2))
  expect_error(pers.hist(AMY, dim = 0))
  expect_error(pers.hist(SUP, palette = c("red")), "Invalid palette argument.")
})

test_that("pers.hist with different argument combinations", {
  pers.hist(AMY, dim = 1)
  pers.hist(SUP, palette = "PuBu")
  pers.hist(SUP, palette = c("pink", "green"))
})

test_that("KIDMAP produces expected errors", {
  # invalid dimension specified
  expect_error(KIDMAP(SUP, personID = 112, dim = 2))
  expect_error(KIDMAP(AMY, personID = 112, dim = 0))
  expect_error(KIDMAP(SUP, personID = "1"), "Invalid personID argument.")
  expect_error(KIDMAP(SUP, personID = 112, palette = c("yellow", "grey")),
               "Invalid palette argument.")
  expect_error(KIDMAP(SUP, personID = 112, probBounds = c(.8, .2)),
               "Invalid probBounds argument.")
})

test_that("KIDMAP w/different argument combinations", {
  KIDMAP(SUP, personID = "110")
  KIDMAP(SUP, personID = 110)
  KIDMAP(AMY, personID = "112")
  KIDMAP(AMY, personID = 112)
  KIDMAP(SUP, personID = 112, palette = c("lightgreen", "lightpink", "black"))
})

test_that("infit.MNSQ produces expected errors", {
  # dimension instead of item vector specified
  expect_error(infit.MNSQ(SUP, dim = 2))
  # invalid items specified
  expect_error(infit.MNSQ(AMY, itemOrder = c(1:32)),
               "Invalid itemOrder argument.")
  expect_error(infit.MNSQ(AMY, palette = c("lightyellow", "grey", "lightpink")),
               "Invalid palette argument.")
  # error for params = "steps" when all items are dichotomous
  # add this test when I have an all dichotomous data set!
})

test_that("infit.MNSQ w/different argument combinations", {
  infit.MNSQ(AMY, params = "steps")
  infit.MNSQ(SUP, params = "steps")
  infit.MNSQ(AMY, itemOrder = c(1:4, 10:12))
  infit.MNSQ(AMY, itemOrder = c(1:4, 10:12), params = "steps")
  infit.MNSQ(SUP, palette = "PuBu")
  infit.MNSQ(SUP, palette = c("lightpink", "black"))
})

test_that("CPC.graph produces expected errors", {

})

test_that("CPC.graph works w/different argument combinations.", {

})

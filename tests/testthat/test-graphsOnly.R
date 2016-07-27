library(crasch)
context("pers.hist, KIDMAP, infit.MNSQ, ICC.graph, CPC.graph, info.graph, wm")

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
  expect_error(KIDMAP(SUP, personID = 112, palette = "red"))
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
  expect_error(infit.MNSQ(AMY, palette = c("nope")),
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
  expect_error(CPC.graph(AMY, itemOrder = c(30:32)),
               'Invalid itemOrder argument')
  expect_error(CPC.graph(SUP, palette = "red"), 'Invalid palette argument.')
  expect_error(CPC.graph(SUP, palette = c("red", "blue")),
               'Invalid palette argument.')
})

test_that("CPC.graph produces expected warnings", {
  expect_warning(CPC.graph(SUP, observed = TRUE))
})

test_that("CPC.graph works w/different argument combinations.", {
  CPC.graph(AMY, itemOrder = c(4,9,2))
  CPC.graph(SUP, palette = "PuBu")
  CPC.graph(SUP, observed = TRUE, minCell = 12)
  CPC.graph(SUP, observed = FALSE, focusTheta = 1.5)
})

test_that("ICC.graph produces expected errors", {
  expect_error(ICC.graph(AMY, itemOrder = c(30:32)),
               'Invalid itemOrder argument')
  expect_error(ICC.graph(SUP, palette = "gray"), 'Invalid palette argument.')
  expect_error(ICC.graph(SUP, palette = c("red", "blue", "no")),
               'Invalid palette argument.')
})

test_that("ICC.graph works w/different argument combinations.", {
  ICC.graph(AMY, itemOrder = c(4,9,2))
  ICC.graph(SUP, palette = "PuBu")
})

test_that("info.graph produces expected errors", {
  expect_error(info.graph(SUP, palette = "sup"), 'Invalid palette argument.')
  expect_error(info.graph(SUP, palette = c("red", "yellow", "blue")),
               'Invalid palette argument.')
  expect_error(info.graph(AMY, dim = 6), 'Invalid dim argument.')
  expect_error(info.graph(AMY, dim = 6, type = "IIC"), 'Invalid dim argument.')
  expect_error(info.graph(AMY, dim = 6, type = "TIC"), 'Invalid dim argument.')
  expect_error(info.graph(SUP, thetaGrid = NULL), 'Invalid thetaGrid argument.')
  expect_error(info.graph(SUP, type = "item"), 'Invalid type argument.')
})

test_that("info.graph produces expected warnings", {
  expect_warning(info.graph(SUP, thetaGrid = c(-1, 0, 1)))
})

test_that("info.graph w/different argument combinations", {
  info.graph(SUP, completeOnly = FALSE)
  info.graph(SUP, palette = "PuBu")
  info.graph(SUP, palette = c("red", "blue"))
  info.graph(AMY, dim = 2)
  info.graph(SUP, type = "TIC", completeOnly = FALSE)
  info.graph(SUP, type = "TIC", palette = c("red", "pink"))
  info.graph(AMY, type = "TIC", dim = 2)
  info.graph(SUP, type = "IIC")
  info.graph(SUP, type = "IIC", completeOnly = FALSE)
  info.graph(AMY, type = "IIC", dim = 2)
})

test_that("wm produces expected errors", {
  expect_error(wm(AMY, dim = 4), 'Invalid dim argument.')
  expect_error(wm(SUP, byCat = "TRUE"), 'Invalid byCat argument.')
  expect_error(wm(SUP, palette = "red"), 'Invalid palette argument.')
})

test_that("wm w/different argument combinations", {
  wm(AMY)
  wm(AMY, dim = 2)
  wm(AMY, byCat = TRUE)
  wm(SUP, byCat = TRUE)
  wm(AMY, dim = 2, byCat = TRUE)
})

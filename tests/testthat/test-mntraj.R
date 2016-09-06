library(crasch)

context("mn.traj()")

test_that("mn.traj produces expected errors", {
  # invalid dimension specified
  expect_error(mn.traj(SUP, itemOrder = seq(2,20,2)),
               "Invalid itemOrder argument.")
  expect_error(mn.traj(SUP, palette = c("ned", "blue")), "Invalid palette argument.")
})

test_that("mn.traj with different argument combinations", {
  mn.traj(AMY, itemOrder = c(1,5,9))
  mn.traj(SUP, palette = "PuBu")
  mn.traj(SUP, palette = c("pink", "green"))
})

test_that("mn.traj and item.analysis calculate the same mean locations", {
  test1 <- t(mn.traj(SUP)$SUP$meanLocations)
  test2 <- item.analysis(SUP)[[2]][,10]
  expect_equal(test1[!is.na(test1)], test2[!is.na(test2)], tolerance = 0.001)

  test1 <- t(mn.traj(AMY)$SUP$meanLocations)
  test2 <- item.analysis(AMY)[[2]]$MeanPersLoc[item.analysis(AMY)[[2]]$Construct == "SUP"]
  expect_equal(test1[!is.na(test1)], test2[!is.na(test2)], tolerance = 0.001)
})

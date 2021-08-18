context("Combined Gradient Forest")
## Combined Gradient Forest
data(CoMLsimulation)
preds <- colnames(Xsimulation)
specs <- colnames(Ysimulation)
set.seed(201808)
f1c <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:6], ntree=10)
set.seed(201808)
f2c <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:6+6], ntree=10)
set.seed(201808)
f12 <- combinedGradientForest(west=f1c,east=f2c)
test_that("combinedGradientForest fits", {
  expect_snapshot_value(f12, "serialize")
  expect_snapshot_output(print(f12))
})

X <- Xsimulation
Y <- Ysimulation
XY <- data.frame(Y,X)
names(XY)[c(13, 1)] <- c("invalid pred", "invalid resp")
preds_inv <- colnames(XY)[13:22]
specs_inv <- colnames(XY)[1:12]
set.seed(201808)
f1c_inv <- gradientForest(XY, preds_inv, specs_inv[1:6], ntree=10, check.names=FALSE)
set.seed(201808)
f2c_inv <- gradientForest(XY, preds_inv, specs_inv[1:6+6], ntree=10, check.names=FALSE)
set.seed(201808)
gfc <- combinedGradientForest(west = f1c_inv, east = f2c_inv)
test_that("combinedGradientForest fits even with invalid col names", {
  expect_snapshot_value(gfc, "serialize")
  expect_snapshot_output(print(gfc))
})

test_that("invalid col names do not change data output", {

  expect_true(all(predict(f12) == predict(gfc)))
  })

  ## the various plots call the following functions:
  ## importance
  ## cumimp.*
  ## density.*
  ## whiten
if (FALSE) {
  root_dir <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
  plot_dir <- file.path(root_dir, "tests", "testthat", "_plots")
  png(file.path(plot_dir, "gfcombined_overall_valid.png"))
  plot(f12, "O")
  dev.off()
  png(file.path(plot_dir, "gfcombined_ranges_valid.png"))
  plot(f12, "Predictor.Ranges")
  dev.off()
  png(file.path(plot_dir, "gfcombined_density_valid.png"))
  plot(f12, "Predictor.Density")
  dev.off()
  png(file.path(plot_dir, "gfcombined_cumulative_valid.png"))
  plot(f12, "C")
  dev.off()
  png(file.path(plot_dir, "gfcombined_performance_valid.png"))
  plot(f12, "Per")
  dev.off()
  png(file.path(plot_dir, "gfcombined_overall_invalid.png"))
  plot(gfc, "O")
  dev.off()
  png(file.path(plot_dir, "gfcombined_ranges_invalid.png"))
  plot(gfc, "Predictor.Ranges")
  dev.off()
  png(file.path(plot_dir, "gfcombined_density_invalid.png"))
  plot(gfc, "Predictor.Density")
  dev.off()
  png(file.path(plot_dir, "gfcombined_cumulative_invalid.png"))
  plot(gfc, "C")
  dev.off()
  png(file.path(plot_dir, "gfcombined_performance_invalid.png"))
  plot(gfc, "Per")
  dev.off()
}

data(CoMLsimulation)
preds <- colnames(Xsimulation)
specs <- colnames(Ysimulation)

set.seed(202108)
f1 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs, ntree=10)
test_that("gradientForest fits", {
  expect_snapshot_value(f1, "serialize")
  expect_output(print(f1), "A forest of 10 regression trees for each of 10 species")
  expect_output(print(f1), "Call:\\n\\ngradientForest\\(data = data\\.frame\\(Ysimulation, Xsimulation\\), predictor\\.vars = preds, \\n    response\\.vars = specs, ntree = 10\\)")
  expect_output(print(f1), "Important variables:\\n\\[1\\] B A J D C\\n")
})

## testing robustness to invalid cols

X <- Xsimulation
Y <- Ysimulation
XY <- data.frame(Y,X)
names(XY)[c(13, 1)] <- c("invalid pred", "invalid resp")
preds_inv <- colnames(XY)[13:22]
specs_inv <- colnames(XY)[1:12]
set.seed(202108)
test_that("invalid names are rejected", {
          expect_error(gradientForest(XY, preds_inv, specs_inv, ntree=10))
})


f1_invalid <- gradientForest(XY, preds_inv, specs_inv, ntree=10, check.names=FALSE)
test_that("invalid col names can be handled", {
  expect_snapshot_value(f1_invalid, "serialize")
  expect_output(print(f1_invalid), "A forest of 10 regression trees for each of 10 species")
  expect_output(print(f1_invalid), "Call:\\n\\ngradientForest\\(data = XY, predictor\\.vars = preds_inv, response\\.vars = specs_inv, \\n    ntree = 10, check\\.names = FALSE\\)")
  expect_output(print(f1_invalid), "Important variables:\\n\\[1\\] B            invalid pred J            D            C           \\n")
})

  ## test predict

  ## Both GF models were fitted with the same data and same seed
  ## So the predictions should not change, despite the name difference
test_that("invalid col names do not change data and output", {
  expect_true(all(predict(f1) == predict(f1_invalid)))
})

  ## Compare all operations we might want to do on a GF object
  ## Do with f1, and with f1_invalid, check for consistency
  ##

  ## First test, plotting
  ## the various plots call the following functions:
  ## importance
  ## cumimp.*
  ## density.*
  ## whiten

if (FALSE) {
  root_dir <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
  plot_dir <- file.path(root_dir, "tests", "testthat", "_plots")
  png(file.path(plot_dir, "gf_overall_valid.png"))
  plot(f1, "O")
  dev.off()
  png(file.path(plot_dir, "gf_split_valid.png"))
  plot(f1, "S")
  dev.off()
  png(file.path(plot_dir, "gf_cumulative_valid.png"))
  plot(f1, "C")
  dev.off()
  png(file.path(plot_dir, "gf_performance_valid.png"))
  plot(f1, "P")
  dev.off()
  png(file.path(plot_dir, "gf_overall_invalid.png"))
  plot(f1_invalid, "O")
  dev.off()
  png(file.path(plot_dir, "gf_split_invalid.png"))
  plot(f1_invalid, "S")
  dev.off()
  png(file.path(plot_dir, "gf_cumulative_invalid.png"))
  plot(f1_invalid, "C")
  dev.off()
  png(file.path(plot_dir, "gf_performance_invalid.png"))
  plot(f1_invalid, "P")
  dev.off()
}

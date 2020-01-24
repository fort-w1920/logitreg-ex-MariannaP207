testthat::context("logitreg")



test_that("implementation is correct", {

  data_sim <- sim_data(seed = 123, numerics = 1, factors = 2)
  design <- data_sim[["design"]]
  response <- data_sim[["response"]]
  data_df <- data.frame(response, design[, -1])

  expect_equivalent(
    signif(
      logitreg(
      data = design,
      object = response,
      coefs = NULL)[["coefficients"]],
      2),
    signif(glm(response ~ ., family = binomial(link = "logit"),
        data = data_df)[["coefficients"]],
      2)
  )
})


test_that("detects incorrect inputs", {
  testdata_path <- system.file("testdata", "logitreg-data-trouble.Rdata",
                               package = "logitreg")
  load(testdata_path)

  expect_error(
    logitreg(
      data = trouble1[["x"]],
      object = trouble1[["y"]]))

  expect_list(
    logitreg(
      data = trouble2[["x"]],
      object = trouble2[["y"]],
      coefs = NULL),
    len = 3)
})


test_that("method dispatch is correct", {

  data_sim <- sim_data(seed = 123, numerics = 1, factors = 2)
  design <- data_sim[["design"]]
  response <- data_sim[["response"]]
  data_df <- data.frame(response, design[, -1])

  expect_list(
    logitreg(response ~ ., data_df),
    len = 4
  )

  expect_list(
    logitreg(response, design),
    len = 3)
})


test_that("correct class is created", {

  data_sim <- sim_data(seed = 123, numerics = 1, factors = 2)
  design <- data_sim[["design"]]
  response <- data_sim[["response"]]
  data_df <- data.frame(response, design[, -1])

  expect_class(
    logitreg(response ~ ., data_df),
    "logitreg"
  )
})


test_that("new S3 methods for class 'logitreg' are working correctly", {

  data_sim <- sim_data(seed = 123, numerics = 1, factors = 2)
  design <- data_sim[["design"]]
  response <- data_sim[["response"]]
  data_df <- data.frame(response, design[, -1])
  model <- logitreg(response ~ ., data_df)

  expect_numeric(fitted(model))

  expect_list(summary(model), len = 3)

  expect_numeric(coef(model), len = ncol(design))

  expect_numeric(predict(model), len = nrow(design))

  plot_model <- plot(model)
  expect_is(plot_model, "ggplot")

  model2 <- logitreg(response, design)
  expect_class(model2, "list")
  expect_identical(as.vector(model2$fitted), fitted(model))
  expect_matrix(fitted(model2))
  expect_error(predict(model2))
  expect_numeric(coef(model2))

})

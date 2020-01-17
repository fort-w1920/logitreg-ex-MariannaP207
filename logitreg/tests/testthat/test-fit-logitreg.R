testthat::context("fit_logitreg")



test_that("implementation is correct", {

  data_sim <- sim_data(seed = 123, numerics = 1, factors = 2)
  design <- data_sim[["design"]]
  response <- data_sim[["response"]]
  data_df <- data.frame(response, design[, -1])

  expect_equivalent(
    signif(
      fit_logitreg(
      design = design,
      response = response,
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
    fit_logitreg(
      design = trouble1[["x"]],
      response = trouble1[["y"]]))

  expect_list(
    fit_logitreg(
      design = trouble2[["x"]],
      response = trouble2[["y"]],
      coefs = NULL),
    len = 3)
})

testthat::test_that("generate_ui returns valid ui class for continuous vpc", {
  
  model <- Certara.RsNLME::pkmodel(parameterization = "Clearance",
                   absorption = "Intravenous",
                   numCompartments = 2,
                   data = Certara.RsNLME::pkData,
                   ID = "Subject",
                   A1 = "Amount",
                   Time = "Act_Time",
                   CObs = "Conc",
                   modelName = "pk_model")
  
  sort_cols <- unique(colnames(model@inputData))
  
  ui <- generate_ui(hosts = c(), sort_cols, model@covariateList, model@isPopulation)
  
  testthat::expect_s3_class(ui, "shiny.tag.list")
  
})

testthat::test_that("theme_certara() returns valid ui class", {
  
  theme <- suppressWarnings(theme_certara())
  testthat::expect_s3_class(theme, "theme")
})


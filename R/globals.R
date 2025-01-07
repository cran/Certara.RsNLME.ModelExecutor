#' @importFrom utils read.csv
#' @importFrom stats as.formula
NULL

model_executor_env <- new.env()

utils::globalVariables(
  c(
    ".",
    "Iter",
    "Value"
  )
)

library(testthat)
library(rmcorrShiny)

test_check("rmcorrShiny", env = shiny::loadSupport(), reporter = ProgressReporter)

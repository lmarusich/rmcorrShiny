#' A shiny application for repeated measures correlation
#'
#' rmcorrShiny provides an intuitive interface with options to compute and plot the repeated measures correlation.
#' @import shiny
#' @seealso Web version: \url{https://lmarusich.shinyapps.io/shiny_rmcorr/}
#' @seealso [rmcorr::rmcorr]
#' @examples
#' if (interactive()) {
#'   rmcorrShiny::rmcorrShiny()
#' }
#' @references Bakdash, J.Z., & Marusich, L.R. (2017).
#' Repeated Measures Correlation. \emph{Frontiers in Psychology, 8}, 256.
#' \doi{10.3389/fpsyg.2017.00456}
rmcorrShiny <- function() {
  app <- system.file("shiny",
                     package = 'rmcorrShiny')
  shiny::runApp(app)

}

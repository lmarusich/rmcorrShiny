formatCode <- function(input, dataLoadingCode, dataManipulationCode
                       , plotCode) {
  
  librariesCode <- getLibrariesCode(input)
  
  prePlotCode <- '## And plot the data\nplotData <- inputData'

  allCode <- glue(paste(
                        librariesCode,
                        dataLoadingCode, 
                        dataManipulationCode,
                        prePlotCode, 
                        plotCode,
                        sep = "\n"))
  ## We don't want all "+" to be followed by a linebreak, so only the ones 
  ## have a space before them will do.
  allCode <- str_replace_all(allCode, " \\+ ", " +\n  ")
  ## Add a space before the rest now.
  allCode <- str_replace_all(allCode, "\\)\\+ ", ") + ")

  return(allCode)
}

getLibrariesCode <- function(input) {
  librariesUsed <- c('rmcorr','ggplot2')
  if(input$plotTheme == 'theme_cowplot()') {
    librariesUsed <- c(librariesUsed, 'cowplot')
  }
  ggsciPalettes <- c("NPG", "AAAS", "NEJM", "Lancet", "JAMA", "JCO", "UCSCGB",
                     "LocusZoom", "IGV", "UChicago", "UChicago Light",
                     "UChicago Dark", "Star Trek", "Tron Legacy", "Futurama",
                     "Rick and Morty", "The Simpsons")
  if(input$plotPalette %in% ggsciPalettes) {
    librariesUsed <- c(librariesUsed, 'ggsci')
  } else {
    librariesUsed <- c(librariesUsed, 'RColorBrewer')
  }

  librariesCode <- paste(paste0('library("',librariesUsed, '") \n'),
                         collapse = "")

  
  librariesCode <- paste0('## Load the required libraries\n', librariesCode)
  
  return(librariesCode)
}
# For gather
library("tidyr")
# For the code (already loaded from server.R)
# library("glue")

dataUpload <- function(input, output, session) {
  
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    # validate(need(input$excelFile, message = FALSE))
    # return(input$excelFile)
    if (!is.null(input$excelFile)){
      return(input$excelFile)
    } else {
      if(input$sampleData) {
        return(list('name' = 'sampleData',
                    'datapath' = 'bland1995.csv'
        ))
      }
      req(input$excelFile)
    }
  })
  # Alt. req(input$excelFile)
  datapath <- reactive({
    userFile()$datapath
  })
  
  inputData <- reactive({
    read.delim2(
      userFile()$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote,
      check.names = FALSE,
      dec = input$decimalPoint
    )
  })
  
  conditions <- reactive({
    colnames(inputData())
  })
  
  name <- reactive({userFile()$name})
  
  code <- reactive({
    quoteCode <- ifelse(input$quote == '\'', '"{input$quote}"', '\'{input$quote}\'')
    sepCode <- ifelse(input$sep == '\t', '\\t', '{input$sep}')
    glue('## Load the data
  inputData <- read.delim2("{name()}",
  header = {input$header},
  sep = \'', sepCode, '\',
  quote = ', quoteCode, ',
  check.names = FALSE,
  dec = \'{input$decimalPoint}\')\n\n')})
  
  return(list(
    inputData = inputData,
    conditions = conditions,
    name = name,
    code = code,
    datapath = datapath
  ))
}

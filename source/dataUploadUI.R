dataUploadUI <- function(id, label = "File input") {
  
  ns <- NS(id)
  
  tagList(
    # column(12, #offset = 0, 
    h4("Data Upload"),
    p("Upload a .txt or .csv file with the different 
                             conditions and values separated by columns."),
    p("or"),
    checkboxInput(ns("sampleData"), "Use sample data", FALSE),
    conditionalPanel(
      condition = 'input.sampleData == true',
      ns = ns,
      selectInput(ns("whichsampleData"),
                  label = "Sample dataset:",
                  choices = list(
                    "bland1995",
                    "gilden2010",
                    "raz2005",
                    "marusich2016"
                  ),
                  multiple = FALSE)),
    fileInput(ns("excelFile"), 
              label = h5(label),
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"),
              buttonLabel = "Browse...", #Unnecessary line, it's the default
              placeholder = "No file selected"), #Ditto 
    p("If output does not directly appear on the right, edit the file details here.")
    # )
    ,
    
    checkboxInput(ns("header"), "Header", TRUE),
    
    
    radioButtons(ns("sep"), "Column Delimiter",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    radioButtons(ns("decimalPoint"), "Decimal separator",
                 choices = c(Point = '.',
                             Comma = ','),
                 selected = '.'),
    radioButtons(ns("quote"), "Quote",
                 choices = c(None = "",
                             "Double Quote" = "\"",
                             "Single Quote" = "'"),
                 selected = "")
    
  )
}

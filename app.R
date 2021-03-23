library(shiny)
library(shinythemes)
library(shinycssloaders)
library(rmcorr)
library(glue)
library(stringr)
library(tidyr)
library(dplyr)
library(bslib) #https://rstudio.github.io/bslib/index.html

source("source/dataUploadUI.R", local = TRUE)
source("source/dataUpload.R", local = TRUE)
source("source/formatCode.R", local = TRUE)
source("source/createPlot.R", local = TRUE)
source("source/paletteColours.R", local = TRUE)

#Items to add:
#2) Need warnings for: missing data, non-numeric input in X and Y? 
#4) Palettes and plots
#c) Cut off the first color in sequential by default? The first color is usually too faint to see 
#5) How to handle p-values that round to zero? Round to p < 0.001 instead? Add sci notation elsewhere for exact value in Output at the top?
#6) Overall fit using data averaged by participant? Output OLS regression and option to add to plots?

#Jon: I can record a video tutorial and we can embed in Shiny- maybe in about? An interactive tutorial in Shiny would be really challenging to code  

#Pie in the sky items
#1) Power calculation: Could be an additional panel?
#2) Additional plot options: Print stats on plot and specify the location where it's printed
#3) Interactive Tutorial

light <- bs_theme()
dark  <- bs_theme(bg = "black", fg = "white", primary = "lightblue")

ui <- fluidPage(
  theme = light,
  div(
    class = "custom-control custom-switch", 
    tags$input(
      id = "dark_mode", type = "checkbox", class = "custom-control-input",
      onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")
    ),
    tags$label("Dark mode", `for` = "dark_mode", class = "custom-control-label")),
  # CSS, fixes palette picker
  tags$style(".bootstrap-select .dropdown-menu li a span.text {width: 100%;}
             #downloadPlot, #downloadZip {margin-top: 25px}"),
  
  #Title 
  titlePanel("Shiny Repeated Measures Correlation"),
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        # type = "pills",
        tabPanel("Data",
                 hr(),
                 tabsetPanel(
                   tabPanel("Input",
                            column(12,hr()),
                            dataUploadUI("rmcorr", label = "File input")),
                   tabPanel("Variables",
                            column(12,
                                   uiOutput('DataFilterColumnsUI')) ),
                   tabPanel("Data options",
                            column(12,
                                   sliderInput("CIlevel", 
                                               label = h5("Confidence Intervals"),
                                               min = 0.5,
                                               max = 0.99,
                                               value = 0.95,
                                               step = 0.01)),
                            column(12,
                                   checkboxInput("bootstrap", "Bootstrap CIs?", FALSE)),
                            conditionalPanel(
                              condition = 'input.bootstrap == true',
                              column(12,
                                     numericInput("bootstrapnreps",
                                                  label = h5("Number of resamples"),
                                                  value = 100,
                                                  min = 10)),
                              column(12,
                                     checkboxInput("bootstrapout", "Show resamples?", FALSE))
                            )
                   )
                 )
        )
        ,
        tabPanel("Plot Options", 
                 hr()
                 ,
                 tabsetPanel(
                   # type = "pills",
                   tabPanel("General Options",
                            column(12,
                                   h4("Size")),
                            column(6,
                                   numericInput("height",
                                                label = h5("Plot Height"),
                                                value = 600)
                            ),
                            column(6,
                                   numericInput("width",
                                                label = h5("Plot Width"),
                                                value = 600)
                            ),
                            column(6,
                                   checkboxInput("plotLegend",
                                                 "Show Legend",
                                                 FALSE)
                            ),
                            column(6,
                                   checkboxInput("plotMajorGrid",
                                                 "Plot Major Grid",
                                                 FALSE)
                            ),
                            column(6,
                                   conditionalPanel(
                                     condition = 'input.plotMajorGrid == true',
                                     checkboxInput("plotMinorGrid",
                                                   "Plot Minor Grid",
                                                   FALSE))
                            ),
                            column(12,
                                   hr())),
                   tabPanel("Theme and colors",
                            
                            column(12,
                                   selectInput("plotTheme",
                                               label = h5("Theme"),
                                               choices = list(
                                                 "Default (Grey)" = "theme_grey()",
                                                 "Black & White" = "theme_bw()",
                                                 "Linedraw" = "theme_linedraw()",
                                                 "Light" = "theme_light()",
                                                 "Dark" = "theme_dark()",
                                                 "Minimal" = "theme_minimal()",
                                                 "Classic" = "theme_classic()",
                                                 "Void" = "theme_void()",
                                                 "Cowplot" = "theme_cowplot()"
                                               ),
                                               selected = "theme_cowplot()")),
                            column(12,
                                   pickerInput(
                                     inputId = "plotPalette",
                                     label = h5("Palette"),
                                     choices = colors_pal,
                                     selected = "Set1",
                                     width = "100%",
                                     choicesOpt = list(
                                       content = sprintf(
                                         "<div style='width:100%%;padding:2px;border-radius:4px;background:%s;color:%s'>%s</div>",
                                         unname(background_pals),
                                         colortext_pals,
                                         names(background_pals)
                                       )
                                     )
                                   )),
                            column(12,
                                   
                                   hr())),
                   tabPanel("Titles",
                            column(12,
                                   textInput("plotTitle",
                                             label = h5("Main Plot Title"),
                                             value = "Main Plot Title")),
                            column(6,
                                   textInput("xAxisTitle",
                                             label = h5("x Axis Title"),
                                             value = "x Axis Title")
                            ),
                            column(6,
                                   textInput("yAxisTitle",
                                             label = h5("y Axis Title"),
                                             value = "y Axis Title")
                            ),
                            column(6,
                                   numericInput("titleFontSize",
                                                label = h5("Main Title Font Size"),
                                                value = 20,
                                                min = 0)),
                            column(6,
                                   numericInput("axisLabelFontSize",
                                                label = h5("Axis Title Font Size"),
                                                value = 15,
                                                min = 0)),
                            column(12,
                                   hr()))
                   ,
                   tabPanel("Scale",
                            column(6,
                                   numericInput("scaleFontSize",
                                                label = h5("Scale Text Font Size"),
                                                value = 15,
                                                min = 0)),
                            column(6,
                                   numericInput("xAxisAngle",
                                                label = h5("X Axis Scale Angle"),
                                                value = 0,
                                                min = 0,
                                                max = 360)),
                            column(6,
                                   selectInput("xAxishjust",
                                               label = h5("Horizontal Scale Justification"),
                                               choices = list(
                                                 "Left" = 0,
                                                 "Middle" = 0.5,
                                                 "Right" = 1),
                                               selected = 0)),
                            column(6,
                                   selectInput("xAxisvjust",
                                               label = h5("Vertical Scale Justification"),
                                               choices = list(
                                                 "Top" = 0,
                                                 "Middle" = 0.5,
                                                 "Bottom" = 1),
                                               selected = 0)),
                            column(12,
                                   checkboxInput("autoScale",
                                                 "Automatic Scale Limits",
                                                 TRUE)),
                            conditionalPanel(
                              condition = "input.autoScale == false",
                              uiOutput("scaleLimitsUI")
                            ),
                            column(12,
                                   hr()))
                 )
        )
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Results",
                           column(8,
                                  h4("Output from rmcorr"),
                                  withSpinner(
                                    htmlOutput("rmcorrResults"))
                                  ,
                                  h4("Reportable results"),
                                  withSpinner(
                                    htmlOutput("rmcorrReportable"))
                                  ,
                                  conditionalPanel(
                                    condition = 'input.bootstrapout == true',
                                    br(),
                                    h4("Bootstrapped resamples")
                                    ,
                                    withSpinner(
                                      tableOutput("bootstrapResamples"))
                                  ))),
                  tabPanel("Plot",
                           br(),
                           withSpinner(
                             plotOutput("rmcorrPlot", 
                                        height = "auto"))
                           
                  ),
                  tabPanel("R Code",
                           column(8,
                                  h4("R Code"),
                                  withSpinner(
                                    verbatimTextOutput("rmcorrCode")))),
                  tabPanel("Processed Data",
                           # verbatimTextOutput("rmcorrDataSummary"),
                           htmlOutput("rmcorrDataSummary"),
                           tableOutput("rmcorrData")),
                  tabPanel("Download",
                           htmlOutput("rmcorrDownload")),
                  tabPanel("About",
                           includeHTML("www/about.html"))
                  
      )
    )
  )
)


server <- function(input, output, session) {
  
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark else light
    )
  })
  
  # Read the input data.
  inputData <- callModule(dataUpload,"rmcorr")
  
  
  # Process the data. This is a reactive depending on the inputData!
  processedData <- reactive({
    req(input$CIlevel)
    dataManipulation(input, inputData)
  })
  
  dataManipulation <- function(input, inputData) {
    
    subColumn <- input$subColumn
    m1Column <- input$m1Column
    m2Column <- input$m2Column
    
    cleanedData <- select(inputData$inputData(),all_of(c(subColumn, m1Column, m2Column)))
    df <- reactive({cleanedData})
    
    my.rmc = reactive({
      req(input$subColumn)
      rmcorr(participant = subColumn,
             measure1 = m1Column,
             measure2 = m2Column,
             dataset = cleanedData,
             CI.level = input$CIlevel,
             CIs = ifelse(input$bootstrap, "bootstrap", "analytic"),
             nreps = input$bootstrapnreps,
             bstrap.out = input$bootstrapout)
    })
    
    n = reactive({
      length(unique(cleanedData[[subColumn]]))
    })
    
    ##Add code for calculating rmcorr
    code <- reactive({
      glue('## Get sample size
n <- length(unique(inputData${subColumn}))\n
## Calculate rmcorr using selected columns
my.rmc <- rmcorr(participant = {subColumn},
                 measure1 = {m1Column},
                 measure2 = {m2Column},
                 dataset = inputData,
                 CI.level = {input$CIlevel},
                 CIs = "{ifelse(input$bootstrap, "bootstrap", "analytic")}",
                 nreps = {input$bootstrapnreps},
                 bstrap.out = {input$bootstrapout})\n\n')
    })
    
    return(list(
      rmc = my.rmc,
      df = df,
      n = n,
      code = code
    ))
  }
  
  output$bootstrapResamples <- renderTable({
    # We don't render the table without inputData.
    req(inputData$name())
    processedData()$rmc()$resamples
  })
  
  # UI - Data - Filter the data.
  output$DataFilterColumnsUI <- renderUI({
    req(inputData$conditions()) #conditions are the column names
    tagList(
      selectInput('subColumn',
                  label = HTML("<h5>Detected columns</h5>
                             Subject column:"), 
                  choices = inputData$conditions(),
                  selected = inputData$conditions()[1],
                  multiple = FALSE),
      selectInput('m1Column',
                  label = HTML("Measure 1 column"), 
                  choices = inputData$conditions(),
                  selected = inputData$conditions()[2],
                  multiple = FALSE),
      selectInput('m2Column',
                  label = HTML("Measure 2 column:"), 
                  choices = inputData$conditions(),
                  selected = inputData$conditions()[3],
                  multiple = FALSE)
    )
  })
  outputOptions(output, "DataFilterColumnsUI", suspendWhenHidden = FALSE)
  
  # update plot titles:
  observeEvent(input$m1Column, {
    updateTextInput(session, "xAxisTitle", value = input$m1Column)}
  )
  observeEvent(input$m2Column, {
    updateTextInput(session, "yAxisTitle", value = input$m2Column)}
  )
  
  
  #Need warnings for missing data, non-numeric input in X and Y? 
  
  # Generate the plot code based on input options but do not evaluate yet.
  plotCode <- reactive({createPlot(input)})
  
  # Evaluate the code based on the processed data.
  plotFigure <- reactive({
    plotData <- inputData$inputData()
    my.rmc <- processedData()$rmc()
    n <- processedData()$n()
    eval(parse(text = glue(plotCode())))
  })
  
  # Render the plot.
  output$rmcorrPlot <- renderPlot({
    # We don't render the plot without inputData.
    req(inputData$name())
    plotFigure()},
    height = function(x) input$height,
    width = function(x) input$width
    
  )
  
  
  # ScriptCode
  scriptCode <- reactive({
    # cat(file=stderr(), processedData()$code())
    formatCode(input, inputData$code(), processedData()$code()
               , plotCode()
    )
  })
  
  
  output$rmcorrCode <- renderText({
    # We don't render the code without inputData.
    req(inputData$name())
    # inputData$code()
    scriptCode()
    
  })
  
  # Print the data
  output$rmcorrResults <- renderUI({
    req(inputData$name())
    CIlevel <- processedData()$rmc()$CI.level * 100
    str_rrm <- paste("Repeated measures correlation: ", round(processedData()$rmc()$r, digits = 3))
    str_df  <- paste("Degrees of freedom: ", processedData()$rmc()$df)
    str_p   <- paste("p-value:", round(processedData()$rmc()$p, digits = 3))
    str_CI  <- paste(CIlevel,"% Confidence Interval: ", 
                     paste0(round(processedData()$rmc()$CI[1], digits = 3), sep = ", ", round(processedData()$rmc()$CI[2], digits = 3)), sep = "")
    HTML(paste(str_rrm, str_df, str_p, str_CI, sep = '</br>'))
    
  })
  
  output$rmcorrReportable <- renderUI({
    req(inputData$name())
    CIlevel <- processedData()$rmc()$CI.level * 100
    HTML(glue("r<sub>rm</sub>({processedData()$rmc()$df}) = 
              {round(processedData()$rmc()$r, digits = 2)}, 
              {CIlevel}% CI [{round(processedData()$rmc()$CI[1], digits = 3)},
              {round(processedData()$rmc()$CI[2], digits = 3)}], 
              {ifelse(processedData()$rmc()$p < .001, 'p < 0.001', paste('p = ',round(processedData()$rmc()$p, digits = 3),sep = ''))}"))
  })
  
  output$rmcorrDataSummary <- renderPrint({
    # We don't render the table without inputData.
    req(inputData$name())
    # summary(processedData()$df())
    HTML(glue("<h5>Sample size:</h5>
    N = {length(unique(inputData$inputData()[[input$subColumn]]))}
              <h5>Number of repeated measures (k):</h5>
              k<sub>avg</sub> = {mean(table(inputData$inputData()[[input$subColumn]]))}<br>
              k<sub>min</sub> = {min(table(inputData$inputData()[[input$subColumn]]))}<br>
              k<sub>max</sub> = {max(table(inputData$inputData()[[input$subColumn]]))}
              <h5>Dataset:</h5>"))
  })
  
  output$rmcorrData <- renderTable({
    # We don't render the table without inputData.
    req(inputData$name())
    inputData$inputData()
  })
  
  output$rmcorrDownload <- renderUI({
    req(inputData$name())
    tagList(
      h4("Download the plot"),
      p("Select the image format or download a zip file with all the 
               images, the script and data used to generate the plot."),
      selectInput("downloadFormat",
                  label = "Image format",
                  choices = list(
                    "Vectorial" = list(
                      "pdf" = "pdf",
                      "svg" = "svg",
                      "eps" = "eps"
                    ),
                    "Non-vectorial" = list(
                      "tiff" = "tiff",
                      "png" = "png")
                  ),
                  selected = "pdf"),
      downloadButton("downloadPlot", 
                     label = "Download Image"),
      br(),
      downloadButton('downloadZip',
                     label = 'Download Zip')
    )
  })
  
  # Download button
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(paste('rmcorrPlot-',inputData$name(), sep = ""), 
            input$downloadFormat, sep = ".")
    },
    content = function(file) {
      if(input$downloadFormat == 'tiff') {
        ggsave(file,
               plot = plotFigure(),
               device = input$downloadFormat,
               # Width and height are in inches. We increase the dpi to 300, so we
               # have to divide by 72 (original default pixels per inch) 
               width = input$width / 72,
               height = input$height / 72,
               compression = "lzw",
               units = "in",
               dpi = 300)
      } else {
        ggsave(file,
               plot = plotFigure(),
               device = input$downloadFormat,
               width = input$width / 72,
               height = input$height / 72,
               units = "in",
               dpi = 300)
      }
    }
  )
  
  # Download zip file with script, data, and plots.
  output$downloadZip <- downloadHandler(
    filename = function() {
      paste0("rmcorrPlot-", inputData$name(), ".zip")
    },
    content = function(fname) {
      fileList <- c()
      tmpdir <- tempdir()
      # Copy inputData to tmpDir
      file.copy(from = c(inputData$datapath()),
                to = tmpdir)
      
      # Move to the tmpDir to work with the tmpFiles
      setwd(tmpdir)
      
      # Change the name of the uploaded file so that the code still works.
      tmpInputFile <- basename(inputData$datapath())
      file.rename(from = tmpInputFile,
                  to = inputData$name())
      
      # Code
      write(scriptCode(), "rmcorrPlot.R")
      
      fileList <- c(fileList, inputData$name(), "rmcorrPlot.R")
      
      # Create all images (except tiff that is compressed).
      for (format in c('pdf','svg','eps','png')) {
        file <- paste(paste0('rmcorrPlot-',inputData$name()),
                      format, sep = ".")
        ggsave(file,
               plot = plotFigure(),
               device = format,
               width = input$width / 72,
               height = input$height / 72,
               units = "in",
               dpi = 300)
        fileList <- c(fileList, file)
      }
      
      # Add compressed .tiff
      tiffFile <- paste(paste0('rmcorrPlot-',inputData$name()),
                        'tiff', sep = ".")
      ggsave(tiffFile,
             plot = plotFigure(),
             device = 'tiff',
             compression = "lzw",
             width = input$width / 72,
             height = input$height / 72,
             units = "in",
             dpi = 300)
      fileList <- c(fileList, tiffFile)
      
      # And create the zip
      zip(zipfile=fname, files=fileList)
    },
    contentType = "application/zip"
  )
  
}
shinyApp(ui = ui, server = server)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(rmcorr)
library(glue)
library(stringr)
library(tidyr)
library(dplyr)
library(ggsci)
library(bslib) #https://rstudio.github.io/bslib/index.html

source("source/dataUploadUI.R",   local = TRUE)
source("source/dataUpload.R",     local = TRUE)
source("source/formatCode.R",     local = TRUE)
source("source/createPlot.R",     local = TRUE)
source("source/paletteColours.R", local = TRUE)
source("source/paletteColours.R", local = TRUE)
source("source/downloadPlot.R",   local = TRUE)

#Items to add:
#1) Add: Sample size (N) and mean repeated measures (k) with range (min and max)?
#2) Need warnings for: missing data, non-numeric input in X and Y, and too few palette colors? 
#3) For reproducibility:  set.seed for bootstrapping?
#4) Overall fit using data averaged by participant? Output OLS regression  
#5) Palettes and plots
#- Set default with enough colors to graph all participants. Sequential only?
#- Cut off the first color in sequential by default? it's usually too faint to see 
#- Add download plot button: DONE!
#6) How to handle p-values that round to zero? Round to p < 0.0001 instead? Add sci notation elsewhere for exact value- Probably not?
  
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
  #theme = shinytheme("paper"),
  # CSS, fixes palette picker (//github.com/gabrifc/raincloud-shiny/issues/12)
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
                            dataUploadUI("rainCloud", label = "File input")),
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
                          column(12, hr()),
                          column(12,
                                 h4("Download the plot")),
                          # downloadPlotUI(id = 'rainCloudDownload',
                          #                label = "Image format",
                          #                buttonLabel = "Download"),
                          column(12,
                                 p("Select the image format or download a zip file with all the 
                                    images, the script and data used to generate the plot.")),
                          column(4,
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
                                             selected = "pdf")),
                          column(4,
                                 downloadButton("downloadPlot", 
                                                label = "Download Image")),
                          column(12, hr()), 
                          column(4,
                                 downloadButton('downloadZip',
                                                label = 'Download Zip')),
                          
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
        # column(12,
        #        hr())))),
      )))
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
                  tabPanel("Plot",withSpinner(
                    plotOutput("rainCloudPlot", 
                               height = "auto"))
                    
                  ),
                  tabPanel("R Code",
                           column(8,
                                  h4("R Code"),
                                  withSpinner(
                                    verbatimTextOutput("rmcorrCode")))),
                  tabPanel("Processed Data",
                           verbatimTextOutput("rainCloudDataSummary"),
                           
                           tableOutput("rainCloudData"))
                  ,
                  tabPanel("About",
                           includeHTML("www/about.html"))
                  
      )
    )
  )
)

#   tabsetPanel(
#     tabPanel("Explore", fluid = T,
#              sidebarLayout(
#                sidebarPanel(
#                  fileInput("file1", "Choose CSV File", accept = ".csv"),
#                  checkboxInput("header", "Header", TRUE),
#                  #also allow them to paste in data?
#                  
#                  selectInput("subvar", label = "Subject Variable", choices = NULL),
#                  selectInput("xvar", label = "x Variable", choices = NULL),
#                  selectInput("yvar", label = "y Variable", choices = NULL),
#                  
#                  #only enable this if three columns are selected?
#                  actionButton("compute", label = "Compute repeated measures correlation"),
#                  actionButton("plot", label = "Plot"),
#                  
#                  #Move to a better place
#                  br(),
#                  br(), 
#                  br(),
#                  h4("Plot Options"), 
#                  checkboxInput("legend", label = "Legend", value = FALSE),
#                  selectInput("palette",  label = "Color Palette", choices = c("viridis", "parula", "coolwarm", "stepped")),
#                  
#                  br(),
#                  br(),
#                  helpText("This Shiny app computes and visualizes repeated measures correlation (rmcorr). Rmcorr is the common within-individual linear association for paired, 
#                     repeated measures data.  Rmcorr is conceptually similar to a null multilevel model: A fixed slope and varying intercept by participant. For details 
#                     about rmcorr and analyzing repeated measures data see below."),
#                  helpText(a(href = "http://journal.frontiersin.org/article/10.3389/fpsyg.2017.00456/full", "Journal Article: Bakdash and Marusich (2017)")),
#                  helpText(a(href = "https://doi.org/10.3389/fpsyg.2019.01201", "Corrigendum to Journal Article (equations): Bakdash and Marusich (2019)")),
#                  helpText(a(href = "https://cran.r-project.org/web/packages/rmcorr/", "rmcorr R package")), 
#                  helpText(a(href = "https://doi.org/10.1038/nn.3648", "Journal Article: Aarts et al. (2014), Excellent overview on why repeated measures should be modeled and overview of multilevel modeling")),
#                  helpText(a(href = "https://github.com/lmarusich/shiny_rmcorr", target="_blank", "Shiny code for this app")),
#                  
#                ), 
#                mainPanel(
#                  h2("Dataset"),
#                  DT::dataTableOutput("df"),
#                  h1("Model Output"),
#                  uiOutput(outputId = "rmc"),
#                  h1("Plot"),
#                  plotOutput("rmcplot"),
#                  downloadButton('downloadPlot'),
#                ) 
#              )
#     ),
#     tabPanel("Tutorial", fluid = T,
#              sidebarLayout(
#                sidebarPanel(
#                  selectInput("exdata", label = "Choose example dataset", 
#                              choices = c("Bland1995" = "bland1995", 
#                                          "Raz2005" = "raz2005", 
#                                          "Gilden2010" = "gilden2010")),
#                  sliderInput("bslider", label = "Between correlation", min = -1, max = 1, step = 0.1,
#                              value = 0, animate = T),
#                  sliderInput("wslider", label = "Within correlation", min = -1, max = 1, step = 0.1,
#                              value = 0, animate = T)
#                ),
#                mainPanel(
#                  h2("Example Dataset"),
#                  uiOutput("documentation"),
#                  br(),
#                  DT::dataTableOutput("exdf")
#                )
#              )
#     )
#   )
# )
server <- function(input, output, session) {
  
  
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark else light
    )
    })
  
  # Read the input data.
  # inputData <- moduleServer("rainCloud", dataUpload)
  inputData <- callModule(dataUpload,"rainCloud")
  
  # Process the data. This is a reactive depending on the inputData!
  processedData <- reactive({
    
    req(input$CIlevel)
    # callModule(dataManipulation,"rainCloud", 
    #           inputData,
    #           # input$subColumn,
    #           input$m1Column,
    #           input$m2Column,
    #           input$CIlevel
    # }
    
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
    
    ##Add code for calculating rmcorr
    code <- reactive({
      glue('## Calculate rmcorr using selected columns
         my.rmc <- rmcorr(participant = {subColumn},
           measure1 = {m1Column},
           measure2 = {m2Column},
           dataset = inputData)\n\n')
    })
    
    return(list(
      rmc = my.rmc,
      df = df,
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


  # Generate the plot code based on input options but do not evaluate yet.
  plotCode <- reactive({createPlot(input)})
  
  # Evaluate the code based on the processed data.
  plotFigure <- reactive({
    plotData <- processedData()$df()
    my.rmc <- processedData()$rmc()
    eval(parse(text = glue(plotCode())))
  })
  
  # Render the plot.
  output$rainCloudPlot <- renderPlot({
    # We don't render the plot without inputData.
    req(inputData$name())
    plotFigure()},
    # height = function(x) input$height,
    # width = function(x) input$width
    height = 600,
    width = 600
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
  
  # Download button
  output$downloadPlot <- downloadHandler(
    filename = function() {
      # rainCloudPlot-inputdata.txt.pdf
      paste(paste('rainCloudPlot-',inputData$name(), sep = ""), 
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
               # Width and height are in inches. We increase the dpi to 300, so we
               # have to divide by 72 (original default pixels per inch) 
               width = input$width / 72,
               height = input$height / 72,
               units = "in",
               dpi = 300)
      }
    }
  )
  
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
  
  output$rainCloudDataSummary <- renderPrint({
    # We don't render the table without inputData.
    req(inputData$name())
    summary(processedData()$df())
  })
  
  output$rainCloudData <- renderTable({
    # We don't render the table without inputData.
    req(inputData$name())
    inputData$inputData()
  })
}
shinyApp(ui = ui, server = server)
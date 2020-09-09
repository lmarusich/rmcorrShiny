#library(shiny)
#library(rmcorr)
#library(rlang)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, rmcorr, rlang, tidyverse, shinythemes, RColorBrewer, pals) #thematic

#Intruiging but doesn't look stable and hasn't been updated in a while 
#https://rstudio.github.io/shinymeta/index.html
#remotes::install_github("rstudio/shinymeta")
#require(shinymeta)

ui <- fluidPage(
  shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      
      
      selectInput("subvar", label = "Subject Variable", choices = NULL),
      selectInput("xvar", label = "x Variable", choices = NULL),
      selectInput("yvar", label = "y Variable", choices = NULL),
      
      #only enable this if three columns are selected?
      actionButton("compute", label = "Compute repeated measures correlation"),
      actionButton("plot", label = "Plot")
      
      #also allow them to paste in data?
    ), 
    mainPanel(
      h1("Dataset"),
      DT::dataTableOutput("df"),
      h1("Repeated Measures Correlation"),
      uiOutput(outputId = "rmc"),
      h1("Plot"),
      plotOutput("rmcplot"),
      downloadButton('downloadPlot'),
    ) 
  )
)
server <- function(input, output, session) {
  
  df <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = input$header)
  })
  
  output$df <- DT::renderDataTable(df())
  
  observe({
    choices1 = colnames(df())
    updateSelectInput(session,"subvar", choices =  choices1[1])
    updateSelectInput(session,"xvar", choices =  choices1[2])
    updateSelectInput(session,"yvar", choices =  choices1[3])
  })
  
  data <- eventReactive(input$compute, {
    rmcorr(input$subvar, input$xvar, input$yvar, dataset = df())
  })
  
  output$rmc <- renderUI({
    str_rrm <- paste("Repeated measures correlation: ", round(data()$r, digits = 3))
    str_df  <- paste("Degrees of freedom: ", data()$df) 
    str_p   <- paste("p-value:", data()$p)
    str_CI  <- paste("95% Confidence Interval: ", paste0(round(data()$CI[1], digits = 3), sep = ", ", round(data()$CI[2], digits = 3)))
    #N.subs = length(unique(input$subvar))
    HTML(paste(str_rrm, str_df, str_p, str_CI, sep = '</br>'))
    #Add diag info: Sample size (N) and mean repeated measures (k) with range?
    #Need warning for missing data, empty cells? 
    #Jon: Worthwhile for me to add formatted output with paper-ready stats??? r_rm(df) = 0.ZZZ 95% CI [ , ], p = 0.XYZ
  })

  plotdata <- eventReactive(input$plot, {
    ggplot2::ggplot(df(), ggplot2::aes(x = !!(sym(input$xvar)), y = !!(sym(input$yvar)), group = factor(!!(sym(input$subvar))),
                                            color = factor(!!(sym(input$subvar))))) +
      ggplot2::geom_point(ggplot2::aes(colour = factor(!!(sym(input$subvar))))) +
      ggplot2::geom_line(ggplot2::aes(y = data()$model$fitted.values), linetype = 1) #+
      #ggplot2::scale_colour_gradientn(colours=tol(sym(length(unique(input$subvar)))))
  })

  output$rmcplot <- renderPlot({plotdata()})
  plotInput = plotdata
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
}
shinyApp(ui = ui, server = server)
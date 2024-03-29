library(shiny)
library(rmcorr)
library(rlang)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Explore", fluid = T,
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
                 h2("Dataset"),
                 DT::dataTableOutput("df"),
                 h1("Repeated Measures Correlation"),
                 uiOutput(outputId = "rmc"),
                 h1("Plot"),
                 plotOutput("rmcplot")
               ) 
             )
             
    ),
    tabPanel("Tutorial", fluid = T,
             sidebarLayout(
               sidebarPanel(
                 selectInput("exdata", label = "Choose dataset", 
                             choices = c("Bland1995" = "bland1995", "Raz2005" = "raz2005", "Gilden2010" = "gilden2010")),
                 sliderInput("bslider", label = "Between correlation", min = -1, max = 1, step = 0.1,
                             value = 0, animate = T),
                 sliderInput("wslider", label = "Within correlation", min = -1, max = 1, step = 0.1,
                             value = 0, animate = T)
               ),
               mainPanel(
                 DT::dataTableOutput("exdf")
               )
             )
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
    updateSelectInput(session,"subvar", choices =  choices1)
    updateSelectInput(session,"xvar", choices =  choices1)
    updateSelectInput(session,"yvar", choices =  choices1)
  })
  
  data <- eventReactive(input$compute, {
    rmcorr(input$subvar, input$xvar, input$yvar, dataset = df())
  })
  
  output$rmc <- renderUI({
    HTML(paste("Repeated measures correlation: ", data()$r, "<br>"))
  })
  
  plotdata <- eventReactive(input$plot, {
    
    ggplot2::ggplot(df(), ggplot2::aes(x = !!(sym(input$xvar)), y = !!(sym(input$yvar)), group = factor(!!(sym(input$subvar))),
                                       color = factor(!!(sym(input$subvar))))) +
      ggplot2::geom_point(ggplot2::aes(colour = factor(!!(sym(input$subvar))))) +
      ggplot2::geom_line(ggplot2::aes(y = data()$model$fitted.values), linetype = 1)
  })
  
  output$rmcplot <- renderPlot({
    plotdata()
  })
  
  output$exdf <- DT::renderDataTable({
    get(input$exdata)
    })
  
}
shinyApp(ui = ui, server = server)
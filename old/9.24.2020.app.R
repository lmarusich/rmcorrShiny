

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, rmcorr, rlang, tidyverse, shinythemes, RColorBrewer, pals, gbRd, rvest) #thematic

#Intriguing but doesn't look stable and hasn't been updated in a while 
#https://rstudio.github.io/shinymeta/index.html
#remotes::install_github("rstudio/shinymeta")
#require(shinymeta)

ui <- fluidPage(
  shinythemes::themeSelector(),
  #Title 
  headerPanel("Repeated Measures Correlation"),
  
  tabsetPanel(
    tabPanel("Explore", fluid = T,
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV File", accept = ".csv"),
                 checkboxInput("header", "Header", TRUE),
                 #also allow them to paste in data?
                 
                 selectInput("subvar", label = "Subject Variable", choices = NULL),
                 selectInput("xvar", label = "x Variable", choices = NULL),
                 selectInput("yvar", label = "y Variable", choices = NULL),
                 
                 #only enable this if three columns are selected?
                 actionButton("compute", label = "Compute repeated measures correlation"),
                 actionButton("plot", label = "Plot"),
                 
                 #Move to a better place
                 br(),
                 h4("Plot Options"), 
                 checkboxInput("legend", label = "Legend", value = FALSE),
                 
                 br(),
                 br(),
                 helpText("This Shiny app computes and visualizes repeated measures correlation (rmcorr). Rmcorr is the common within-individual linear association for paired, 
                          repeated measures data.  Rmcorr is conceptually similar to a null multilevel model: A fixed slope and varying intercept by participant. For details 
                          about rmcorr and analyzing repeated measures data see below."),
                        helpText(a(href = "http://journal.frontiersin.org/article/10.3389/fpsyg.2017.00456/full", "Journal Article: Bakdash and Marusich (2017)")),
                        helpText(a(href = "https://doi.org/10.3389/fpsyg.2019.01201", "Corrigendum to Journal Article (equations): Bakdash and Marusich (2019)")),
                        helpText(a(href = "https://cran.r-project.org/web/packages/rmcorr/", "rmcorr R package")), 
                        helpText(a(href = "https://doi.org/10.1038/nn.3648", "Journal Article: Aarts et al. (2014), Excellent overview on why repeated measures should be modeled and overview of multilevel modeling")),
                        helpText(a(href = "https://github.com/lmarusich/shiny_rmcorr", target="_blank", "Shiny code for this app")),
                       
               ), 
               mainPanel(
                 h2("Dataset"),
                 DT::dataTableOutput("df"),
                 h1("Model Output"),
                 uiOutput(outputId = "rmc"),
                 h1("Plot"),
                 plotOutput("rmcplot"),
                 downloadButton('downloadPlot'),
               ) 
             )
    ),
    tabPanel("Tutorial", fluid = T,
             sidebarLayout(
               sidebarPanel(
                 selectInput("exdata", label = "Choose example dataset", 
                             choices = c("Bland1995" = "bland1995", 
                                         "Raz2005" = "raz2005", 
                                         "Gilden2010" = "gilden2010")),
                 sliderInput("bslider", label = "Between correlation", min = -1, max = 1, step = 0.1,
                             value = 0, animate = T),
                 sliderInput("wslider", label = "Within correlation", min = -1, max = 1, step = 0.1,
                             value = 0, animate = T)
               ),
               mainPanel(
                 h2("Example Dataset"),
                 uiOutput("documentation"),
                 br(),
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
    updateSelectInput(session,"subvar", choices =  choices1, selected = choices1[1]) #selected to set default column
    updateSelectInput(session,"xvar",   choices =  choices1, selected = choices1[2])
    updateSelectInput(session,"yvar",   choices =  choices1, selected = choices1[3])
  })
  
  data <- eventReactive(input$compute, {
    rmcorr(input$subvar, input$xvar, input$yvar, dataset = df())
    
  })

  # observeEvent(input$legend == FALSE, { returnValue <- "No Legend" }) $tableOut <- if(input$legend == F) Yes
  # # observe({updateSelectInput(session, "legend")})
  # # output$legend <- reactive(input$legend)
  # # renderPrint({output$legend})
  
  output$rmc <- renderUI({
    str_rrm <- paste("Repeated measures correlation: ", round(data()$r, digits = 3))
    str_df  <- paste("Degrees of freedom: ", data()$df) 
    str_p   <- paste("p-value:", data()$p)
    str_CI  <- paste("95% Confidence Interval: ", paste0(round(data()$CI[1], digits = 3), sep = ", ", round(data()$CI[2], digits = 3)))
    HTML(paste(str_rrm, str_df, str_p, str_CI, sep = '</br>'))
    
    #Add diag info: Sample size (N) and mean repeated measures (k) with range?
    
    #Need warnings for missing data, non-numeric input in X and Y? 
    
    #Jon: Worthwhile for me to add formatted output with reporting-ready stats??? r_rm(df) = 0.ZZZ 95% CI [ , ], p = 0.XYZ
  })
  
  plotdata <- eventReactive(input$plot, {
    ggplot2::ggplot(df(), ggplot2::aes(x = !!(sym(input$xvar)), y = !!(sym(input$yvar)), group = (!!(sym(input$subvar))),
                                       color = (!!(sym(input$subvar))))) +
      ggplot2::geom_point(ggplot2::aes(colour = (!!(sym(input$subvar))))) +
      ggplot2::geom_line(ggplot2::aes(y = data()$model$fitted.values), linetype = 1) +
      ggplot2::scale_colour_gradientn(colours=viridis(length(unique(data()$model$model$Participant)))) #Add button to select palette next? 
      
    })
  
  
  #(+ (legend.position = "none"))
  output$rmcplot = renderPlot({plotdata()})                         
  plotInput = plotdata
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$dataset, '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "pdf")
    }
  )
  
  output$exdf <- DT::renderDataTable({
    get(input$exdata)
  })
  
  tmp <- tempfile()
  onSessionEnded(function(){ unlink(tmp) })
  output$documentation <- renderUI({
    Rd2HTML(Rd_fun(input$exdata, keep_section = "\\source"),tmp)
    HTML(read_html(tmp) %>% html_nodes("p")%>% html_text())
  })
}
shinyApp(ui = ui, server = server)
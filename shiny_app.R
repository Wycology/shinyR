# Load the necessary packages for building the shinyApp

library(shiny) # This is the main package that builds the Shiny App.
library(shinythemes) # For changing the themes of the web application created.

# Load the necessary data for building the application.

data(iris) # iris flower data in the base R package
data(mtcars) # cars data which also comes with the base R package

# Define user interface. This is page the users will be interacting with.

ui <- fluidPage(
  titlePanel("Kenyan Tea Farmers"),
  theme = shinytheme("united"), # "cosmo", "cyborg", "journal", "paper", etc
  navbarPage("KALRO - TRI Information ShinyApp",
             tabPanel("Farmers' Details",
                      sidebarPanel(
                        tags$h3("About the farmer:"),
                        textInput("txt4", "Farmer Registration Number:"),
                        textInput("txt1", "Given Name(s):", ""),
                        textInput("txt2", "Surname(s):", ""),
                        numericInput("txt3", "Age:", ""),
                        selectInput("slctInput", "Select your country",
                                    choices = c("Kenya", "Uganda", 
                                                "Tanzania", "Other"))
                             
                           ), # sidebarPanel 1
                           mainPanel(
                             h1("Contact Details"),
                             h4("Full Names"),
                             verbatimTextOutput("txtout"),
                             textOutput("countryOutput"),
                             
                           ) # mainPanel
                           
                  ), # navbar 1 tabpanel 1
                  tabPanel("Tea Yield Trends",
                           sidebarPanel(
                             sliderInput("obs",
                                         "Coffee Yield (Kgs):",
                                         min = 1,
                                         max = 50,
                                         value = 25,
                                         step = 5)
                           ),
                           mainPanel(
                             h1("Distribution"),
                             h3("In Kgs"),
                           )
                           ),
                  tabPanel("Tea data histogram",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("var", "1. Select the variable from the iris 
                                  datset",
                                  choices = c("Sepal.Length" = 1,
                                              "Sepal.Width" = 2,
                                              "Petal.Length" = 3,
                                              "Petal.Width" = 4),
                                  selected = 1),
                      br(),
                      sliderInput("bins", "2. Select the number of BINs for 
                                  histogram",
                                  min = 5, max = 25, value = 15),
                      br(),
                      radioButtons("color", "3. Select the color of histogram",
                                   choices = c("Purple", "Cyan", "Magenta"), 
                                   selected = "Purple")
                    ),
                    mainPanel(
                      plotOutput("myhist")
                    )
                  )
                ),# navbar page
                tabPanel("Farmer data",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("loc", "What is your location?", 
                                 choices = c("On-site", "Off-site"),
                                 selected = "Off-site"),
                    sliderInput("ndaysspent", "No. of days spent", 0, 100, 
                                value = c(10, 20),
                                step = 5),
                    selectInput("dept", "What is your department?",
                                choices = c("Marketing", "Finance", "Sales", "IT"),
                                multiple = TRUE)
                  ),
                  mainPanel(
                    DT::dataTableOutput("iris"),
                    textOutput("location"),
                    textOutput("no_of_days_spent"),
                    textOutput("department")
                  )
                )),
                tabPanel(title = "Shiny Tabset Example",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("ngear", "Select the gear number", 
                                c("Cylinders" = "cyl", "Transmission" = "am",
                                  "Gears" = "gear"))
                  ),
                  mainPanel(
                    tabsetPanel(type = "tab",
                                tabPanel("Help", 
                                         tags$img(src = "shiny.png"),
                                         HTML('<iframe width="560" height="315" 
                                              src="https://www.youtube.com/embed/HVa42mJYppE" 
                                              frameborder="0" allow="accelerometer; 
                                              autoplay; clipboard-write; 
                                              encrypted-media; gyroscope; 
                                              picture-in-picture" 
                                              allowfullscreen></iframe>')),
                                tabPanel("Data", tableOutput("mtcars"), downloadButton("downloadData", "Download Data")),
                                tabPanel("Summary", verbatimTextOutput("summ")),
                                tabPanel("Plot", plotOutput("plot"), downloadButton("downloadPlot", "Download Plot"))
                                )
                    )
                  
                ))
)) # fluidpage


# Define server function

server <- function(input, output){
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, input$txt3, input$txt4, sep = " ")
    
  })
  output$countryOutput <- renderText({
    paste(input$slctInput)
  })
  output$myhist <- renderPlot({
    colm <- as.numeric(input$var)
    hist(iris[,colm], breaks = seq(0, max(iris[, colm], l = input$bins + 1)),
                                    col = input$color)
  })
  output$iris <- DT::renderDataTable({
    iris
  })
  
  output$location <- {(
    renderText(input$loc)
  )}
  
  output$no_of_days_spent <- {(
   renderText(input$ndaysspent)
  )}
  
  output$department <- {(
    renderText(input$dept)
  )}
  
  mtreact <- reactive({
    
    mtcars[,c("mpg", input$ngear)]
  })
  
  output$mtcars <- renderTable({
    mtreact()
  })
  output$summ <- renderPrint({
    summary(mtreact())
  })
  
  output$plot <- renderPlot({
    with(mtreact(), boxplot(mpg ~ mtreact()[, 2]))
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("mtcars", "csv", sep = ".")
    },
    content = function(file){
      write.csv(mtreact(), file)
    }
  )
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste("mtcars-plot", "png", sep = ".")
    },
    content = function(file){
     png(file)
      with(mtreact(), boxplot(mpg ~ mtreact()[, 2]))
      dev.off()
    }
  )
} # server

# create shiny object
shinyApp(ui = ui, server = server)



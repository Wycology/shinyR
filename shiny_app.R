# shinyApp in R, Created on 2nd Mar 2021 by Oluoch Agumba

# shinyApp in R, last edited 29th Sept 2021 by Oluoch Agumba

# Load the packages for building the app
#----------------------------------------------------
library(shiny)
library(shinythemes) 
library(tidyverse)

data(iris) # iris data having five variables and 150 observations
data(mtcars) # mtcars data with 32 observations and 11 variables

# Defining user interface, the so called ui. 

ui <- fluidPage(
  titlePanel("Kenyan Tea Farmers Details"),
  theme = shinytheme("journal"), # "united", "cosmo", "cyborg", "paper", etc
  navbarPage("TRF Information ShinyApp",
             tabPanel("Farmers' Information",
                      sidebarPanel(
                        tags$h3("About farmer:"),
                        textInput("txt4", "Registration No.:"),
                        textInput("txt1", "Given Name(s):", ""),
                        textInput("txt2", "Surname(s):", ""),
                        numericInput("txt3", "Age:", ""),
                        selectInput("slctInput", "Select your home country",
                                    choices = c("Kenya", "Uganda", 
                                                "Tanzania", "Other"))
                             
                           ), # sidebarPanel 1
                           mainPanel(
                             h1("Contacts"),
                             h4("Full Name(s)"),
                             verbatimTextOutput("txtout"),
                             textOutput("countryOutput"),
                             
                           ) # mainPanel definiti
                           
                  ), # navigation bar 1 tabpa
                  tabPanel("Tea Yield Trend",
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
                  tabPanel("Tea histogram",
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
                tabPanel("Farmer general data",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("loc", "State your location", 
                                 choices = c("On-site", "Off-site"),
                                 selected = "Off-site"),
                    sliderInput("ndaysspent", "Number of days spent", 0, 100, 
                                value = c(10, 20),
                                step = 5),
                    selectInput("dept", "State your department",
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
                tabPanel(title = "Shiny Tabset",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("ngear", "Select gear", 
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
)) # fluidpage ends here, server starts


# Defining server functions for the ui.

serveri <- function(input, output){
  
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
}

# creating shiny object using the shinyApp function. Done for the day
shinyApp(ui = ui, server = serveri)

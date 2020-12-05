# define user interface
data(iris)


library(shiny)
library(shinythemes)

user_interface <- fluidPage(titlePanel("Kenyan Farmer"),
                            theme = shinytheme("united"),
                navbarPage(
                  "KALRO - TRI Records",
                  tabPanel("Farmer Details",
                           sidebarPanel(
                             tags$h3("Name:"),
                             textInput("txt4", "Registration Number:"),
                             textInput("txt1", "Given Name(s):", ""),
                             textInput("txt2", "Surname:", ""),
                             textInput("txt3", "Age:", ""),
                             selectInput("slctInput", "Select country you live in",
                                         choices = c("Kenya", "Uganda", "Tanzania"))
                             
                           ), # sidebarPanel 1
                           mainPanel(
                             h1("Contact Details"),
                             h4("Full Names"),
                             verbatimTextOutput("txtout"),
                             textOutput("countryOutput"),
                             
                           ) # mainPanel
                           
                  ), # navbar 1 tabpanel 1
                  tabPanel("Coffee Yield Trend",
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
                  tabPanel("Iris data by Species"),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("var", "1. Select the variable from the iris datset",
                                  choices = c("Sepal.Length" = 1,
                                              "Sepal.Width" = 2,
                                              "Petal.Length" = 3,
                                              "Petal.Width" = 4),
                                  selected = 1),
                      br(),
                      sliderInput("bins", "2. Select the number of BINs for histogram",
                                  min = 5, max = 25, value = 15),
                      br(),
                      radioButtons("color", "3. Select the color of histogram",
                                   choices = c("Purple", "Cyan", "Magenta"), selected = "Purple")
                    ),
                    mainPanel(
                      plotOutput("myhist")
                    )
                  )
                ) # navbar page
) # fluidpage


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
} # server

# create shiny object
shinyApp(ui = user_interface, server = server)



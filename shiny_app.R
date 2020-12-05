# define user interface

ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  # theme = "cerulean",
                  "My first app",
                  tabPanel("Navbar1",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             
                           ), # sidebarPanel 1
                           mainPanel(
                             h1("Header 1"),
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # navbar 1 tabpanel 1
                  tabPanel("Navbar 2", "I want to leave this empty"),
                  tabPanel("Navbar 3", "This panel is supposed to be blank")
                ) # navbar page
) # fluidpage


# Define server function

server <- function(input, output){
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " ")
  })
} # server

# create shiny object
shinyApp(ui = ui, server = server)

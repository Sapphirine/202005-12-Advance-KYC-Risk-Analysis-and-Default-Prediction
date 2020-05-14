#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reticulate)
data<-read.csv("financial_data.csv")
use_python("C:/ProgramData/Anaconda3/python.exe")
source_python('python_ref.py')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Realtime Sentiment Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("name",
                        label="Please select a company",
                        choices = data$name)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("balabala"),
            verbatimTextOutput("sentiment"),
            textOutput("headline")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$sentiment<-renderPrint({
        testMethod(input$name)
    })
    output$balabala<-renderText({
        foo<-balabala(input$name)
        paste("Currently working on",foo)
    })
    output$headline<-renderText({
        topline(input$name)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

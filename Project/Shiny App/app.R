#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#Importing required libraries
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(arules)
library(stats)
library(factoextra)
library(C50)

#Importing training data
train.shiny <- read.csv("passenger_survey_data_1.csv", stringsAsFactors = T)

#Removing unnessary columns
train.shiny <- train.shiny[,-(1:2)]

#Replacing NA values with mean
train.shiny$Arrival.Delay.in.Minutes <- train.shiny$Arrival.Delay.in.Minutes %>% replace_na(mean(train.shiny$Arrival.Delay.in.Minutes, na.rm = T))

#Log transforming right skewed data
for (i in c("Flight.Distance", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes"))
{
    train.shiny[,i] <- log(train.shiny[, i] + 1)
}

#Creating a Decesion Tree model using C5.0
c50.shiny <- C5.0(x = train.shiny[,-23], y = train.shiny$satisfaction)

#Subsetting mandatory fields
fieldsMandatory <- c("Gender", "Customer.Type", "Age", "Type.of.Travel", "Class", "Flight.Distance", "Inflight.wifi.service", 
                     "Departure.Arrival.time.convenient", "Ease.of.Online.booking", "Gate.location", "Food.and.drink", 
                     "Online.boarding", "Seat.comfort", "Inflight.entertainment", "On.board.service", "Leg.room.service",
                     "Baggage.handling", "Checkin.service", "Inflight.service", "Cleanliness", 
                     "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")

#Creating label function for mandatory fields
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

#Add a red asterisk to an input label
appCSS <- ".mandatory_star { color: red; }"


# Define UI for application
ui <- fluidPage(theme = shinytheme("paper"),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("Passenger Survey Form:"),
    br(),
    h4("Enter your name:"),
    fluidRow(column(1, textInput("fname", "First Name", "")),
             column(1, textInput("lname", "Last Name", ""))),
    br(),
    fluidRow(column(2, radioButtons("Gender", labelMandatory("Gender"), c("Male", "Female"), selected = character(0)))),
    br(),
    fluidRow(column(1, dateInput("dob", "Date of Birth", value = Sys.Date(), min = "1920-01-01", max = Sys.Date())),
             column(1, numericInput("Age", labelMandatory("Age"), value = NULL, min = 7, max = 85))),
    br(),
    fluidRow(column(2, radioButtons("Customer.Type", labelMandatory("Customer Type"), c("Loyal Customer", "disloyal Customer"), selected = character(0))),
             column(2, radioButtons("Type.of.Travel", labelMandatory("Type of Travel"), c("Personal Travel", "Business travel"), selected = character(0))),
             column(2, radioButtons("Class", labelMandatory("Class"), c("Eco", "Eco Plus", "Business"), selected = character(0)))),
    br(),
    fluidRow(column(4, sliderInput("Inflight.wifi.service", labelMandatory("Inflight Wi-Fi Service"), 0, 5, 3),
                       sliderInput("Departure.Arrival.time.convenient", labelMandatory("Departure Arrival time convenient"), 0, 5, 3),
                       sliderInput("Ease.of.Online.booking", labelMandatory("Ease of Online booking"), 0, 5, 3),
                       sliderInput("Gate.location", labelMandatory("Gate location"), 0, 5, 3)), 
             column(4, sliderInput("Food.and.drink", labelMandatory("Food and drink"), 0, 5, 3),
                       sliderInput("Online.boarding", labelMandatory("Online boarding"), 0, 5, 3),
                       sliderInput("Seat.comfort", labelMandatory("Seat comfort"), 0, 5, 4),
                       sliderInput("Inflight.entertainment", labelMandatory("Inflight entertainment"), 0, 5, 4)), 
             column(4, sliderInput("On.board.service", labelMandatory("On board service"), 0, 5, 4),
                       sliderInput("Leg.room.service", labelMandatory("Leg room service"), 0, 5, 4),
                       sliderInput("Baggage.handling", labelMandatory("Baggage handling"), 0, 5, 4),
                       sliderInput("Checkin.service", labelMandatory("Checkin service"), 0, 5, 3))),
    fluidRow(column(4, sliderInput("Inflight.service", labelMandatory("Inflight.service"), 0, 5, 4)),
             column(4, sliderInput("Cleanliness", labelMandatory("Cleanliness"), 0, 5, 3))),
    br(),
    fluidRow(column(2, numericInput("Flight.Distance", labelMandatory("Flight Distance (Km)"), value = 30, min = 30, max = 5000)),
             column(2, numericInput("Departure.Delay.in.Minutes", labelMandatory("Departure Delay in Minutes"), value = 0, min = 0, max = 1800)),
             column(2, numericInput("Arrival.Delay.in.Minutes", labelMandatory("Arrival Delay in Minutes"), value = 0, min = 0, max = 1800))),
    br(),
    fluidRow(column(1, actionButton("submitbutton", "Submit", class = "btn btn-primary")),
             column(2, textOutput("val")),
             column(2, verbatimTextOutput('contents'))),
    br()
)

# Define server logic
server <- function(input, output, session) {
    
    # Enable the Submit button when all mandatory fields are filled out
    observe({
        mandatoryFilled <-
            vapply(fieldsMandatory,
                   function(x) {
                       !is.null(input[[x]]) && input[[x]] != ""
                   },
                   logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        shinyjs::toggleState(id = "submitbutton", condition = mandatoryFilled)
    })
    
    # Input Data
    datasetInput <- reactive({  
        
        # Retreving input data from the user
        df <- data.frame(
            Name = c("Gender", "Customer.Type", "Age", "Type.of.Travel", "Class", "Flight.Distance", "Inflight.wifi.service", 
                     "Departure.Arrival.time.convenient", "Ease.of.Online.booking", "Gate.location", "Food.and.drink", 
                     "Online.boarding", "Seat.comfort", "Inflight.entertainment", "On.board.service", "Leg.room.service",
                     "Baggage.handling", "Checkin.service", "Inflight.service", "Cleanliness", 
                     "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes", "satisfaction"),
            Value = as.character(c(input$Gender, 
                                   input$Customer.Type, 
                                   input$Age, 
                                   input$Type.of.Travel, 
                                   input$Class, 
                                   input$Flight.Distance, 
                                   input$Inflight.wifi.service, 
                                   input$Departure.Arrival.time.convenient,
                                   input$Ease.of.Online.booking,
                                   input$Gate.location,
                                   input$Food.and.drink,
                                   input$Online.boarding,
                                   input$Seat.comfort,
                                   input$Inflight.entertainment,
                                   input$On.board.service,
                                   input$Leg.room.service,
                                   input$Baggage.handling,
                                   input$Checkin.service,
                                   input$Inflight.service,
                                   input$Cleanliness,
                                   input$Departure.Delay.in.Minutes,
                                   input$Arrival.Delay.in.Minutes,
                                   "satisfied"
            )),
            stringsAsFactors = FALSE)
        
        # Taking transpose of the input data and adding column names to it
        input <- as.data.frame(t(df[,2]))
        names(input) <- df[,1]
        
        # Creating a cpot of input data for testing
        test <- input
        
        # Setting character columns to factor
        test$Gender <- factor(test$Gender, levels = c("Female", "Male"))
        test$Customer.Type <- factor(test$Customer.Type, levels = c("Loyal Customer", "disloyal Customer"))
        test$Class <- factor(test$Class, levels = c("Eco", "Eco Plus", "Business"))
        test$Type.of.Travel <- factor(test$Type.of.Travel, levels = c("Personal Travel", "Business travel"))
        test$satisfaction <- factor(test$satisfaction, levels = c("neutral or dissatisfied", "satisfied"))
        
        # Setting integer value for numeric columns
        for (i in c(3,6:22))
        {
            test[,i] <- as.integer(test[,i])
        }
        
        # Taking log transform for continious variable
        for (i in c("Flight.Distance", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes"))
        {
            test[,i] <- log(test[,i] + 1)
        }
        
        # Predicting the value for the test data
        val <- predict(c50.shiny, test)
        
        # setting the output value to character
        val <- as.character(val)
        
        # printing a output statement 
        val2 <- sprintf("The Passenger is %s", val)
        
        #Printing the output value
        output <- val2
        output
    })
    
    # Status/Output Text Box
    output$contents <- renderPrint({
        if (input$submitbutton>0) {
            isolate("Calculation complete.")
        } else {
            return("Server is ready for calculation")
        }
    })
    
    # Prediction results table
    output$val <- renderText({
        if (input$submitbutton>0) {
            isolate(datasetInput())
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

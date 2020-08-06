#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
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
        val <- predict(c50.shiny, test[,-23])
        
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
    
})

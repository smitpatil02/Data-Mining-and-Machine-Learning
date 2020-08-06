#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("paper"),
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
)

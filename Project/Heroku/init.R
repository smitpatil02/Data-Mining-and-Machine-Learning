# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "shinyjs", "shinythemes", "tidyverse", "arules", "stats", "factoextra", "C50")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

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
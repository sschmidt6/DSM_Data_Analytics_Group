
#install.packages("shiny")
#install.packages("broom", type="binary")
library(shiny)
library(lubridate)
library(readr)
library(dplyr)
library(readxl)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(tabulizer)


#NOTE: Dashboard can be published to RShiny Cloud Server at this link: https://www.rstudio.com/products/shinyapps/

input_data_path <- "C:/Users/steph/OneDrive/Desktop/University of Iowa/MSBA/FALL 2021/Analytics Experience/Shiny App/Input"
#setwd(input_data_path)

# members <- read.csv("Members.csv")
# attendance <- read.csv("Attendance.csv")
# events <- read.csv("Events.csv")

members <- read_xlsx("dashboard_data_final.xlsx", "Members")
attendance <- read_xlsx("dashboard_data_final.xlsx", "Attendance")
events <- read_xlsx("dashboard_data_final.xlsx", "Events")
members_joined <- merge(x = attendance, y = members, by.x = 'Member ID', by.y = 'MemberID', all.x = TRUE)

# Descriptive Analysis Plots --------------------------------------------------------
#TEST PLOT: Monthly Attendance by Company -------------------------------------------
  # companies <- as.list(unique(members_joined$Company))
  # plot1_selection<- sidebarPanel(selectInput("Company", label = "Choose Company", choices = companies))
  # plot1_content <- mainPanel(plotOutput("plot1"))
  # plot1_panel <- sidebarLayout(plot1_selection, plot1_content)
#PLOT 1: Average Attendance by Event Factor Filter-----------------------------------
  plot1_options <- c("Event Type", "Time of Day", "Location")
  plot1_selection <- sidebarPanel(selectInput("plot1_input", label = "Choose Attendence Factor", choices = plot1_options))
  plot1_content <- mainPanel(plotOutput("plot1"))
  plot1_sidebar <- sidebarLayout(plot1_selection, " ")
  plot1_panel <- sidebarLayout(plot1_sidebar, plot1_content)
#PLOT 2: Member Treemap--------------------------------------------------------------
  plot2_options <- c("Company", "Categorization")
  plot2_selection <- sidebarPanel(selectInput("plot2_input", label = "Choose Demographic Factor", choices = plot2_options))
  plot2_content <- mainPanel(plotOutput("plot2"))
  plot2_sidebar <- sidebarLayout(plot2_selection, " ")
  plot2_panel <- sidebarLayout(plot2_sidebar, plot2_content)
#PLOT 3: Monthly Metrics (Attendance or Membership)
  plot3_options <- c("Attendance", "New Members")
  plot3_year_options <- c(2017:year(Sys.Date()))
  plot3_selection <- sidebarPanel(selectInput("plot3_input", label = "Choose Time Series Factor", choices = plot3_options))
  plot3_year_selection <- sidebarPanel(selectInput("plot3_year_input", label = "Choose Year", choices = plot3_year_options))
  plot3_content <- mainPanel(plotOutput("plot3"))
  plot3_sidebar <- sidebarLayout(plot3_selection, plot3_year_selection)
  plot3_panel <- sidebarLayout(plot3_sidebar, plot3_content)
#PLOT 4: Parallel Coordinates
  loop_list <- as.list(unique(members$Company))
  companies_df <- data.frame()
  for(company in loop_list){
    total <- nrow(subset(members, members$Company == company))
    row <- cbind(company, total)
    companies_df <- rbind(companies_df, row)
  }
  companies_df$company[companies_df$company == "????"] <- "Unknown"
  top_companies <- as.list(companies_df$company[companies_df$total >= 30])
  top_companies[1] <- "All Companies"
  
  plot4_options <- c("All Events", "Events Hosted by the Company")
  plot4_company_options <- top_companies
  plot4_selection <- sidebarPanel(selectInput("plot4_input", label = "Choose Type", choices = plot4_options))
  plot4_company_selection <- sidebarPanel(selectInput("plot4_company_input", label = "Choose Company", choices = plot4_company_options))
  plot4_content <- mainPanel(plotOutput("plot4"))
  plot4_sidebar <- sidebarLayout(plot4_selection, plot4_company_selection)
  plot4_panel <- sidebarLayout(plot4_sidebar, plot4_content)
#PLOT 5: Active Job Categorization
  

# User Interface Layout -------------------------------------------------------------

# ui <- fluidPage(
#   title = "DSM Data & Analytics Group Dashboard", 
#   hr(),
#   fluidRow(
#     column(4, plot1_panel),
#     column(4, plot2_panel),
#     column(4)
#   )
#   plot1_panel, plot2_panel, plot3_panel)

tab_1 <- tabPanel("Attendance Data", plot3_panel, plot1_panel) #create parallel coordinates plot panel
tab_2 <- tabPanel("Membership Data", plot2_panel)
# tab1 <- tabPanel("Dashboard", plot3_panel, plot1_panel, plot1_panel )  
  
ui <- navbarPage("DSM Data & Analytics Group", tab_1, tab_2)

  
  
  
  
  
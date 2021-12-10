
library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(sqldf)
library(readxl)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(tabulizer)
library(treemap)
library(GGally)

#install.packages('GGally')
#install.packages("treemap")

input_data_path <- "C:/Users/steph/OneDrive/Desktop/University of Iowa/MSBA/FALL 2021/Analytics Experience/Shiny App/Input"
#setwd(input_data_path)

# members <- read.csv("Members.csv")
# attendance <- read.csv("Attendance.csv")
# events <- read.csv("Events.csv")
# members_joined <- merge(x = attendance, y = members, by.x = 'MemberKey.Lookup', by.y = 'MemberKey', all.x = TRUE)a

members <- read_xlsx("dashboard_data_final.xlsx", "Members")
attendance <- read_xlsx("dashboard_data_final.xlsx", "Attendance")
events <- read_xlsx("dashboard_data_final.xlsx", "Events")
members_joined <- merge(x = attendance, y = members, by.x = 'Member ID', by.y = 'MemberID', all.x = TRUE)
#members_joined$Event.Date.Lookup <- as.Date(members_joined$Event.Date.Lookup, format = "%m/%d/%Y")

# Dashboard Server -------------------------------------------------------------------------------------------------
server <- function(input, output, session){
  #TEST PLOT: Monthly Attendance by Company-------------------------------
    # plot1_df <- reactive({
    #   plot_filter <- as.character(input$Company)
    #   df <- subset(members_joined, members_joined$Company == plot_filter & (members_joined$Status == "Went" | members_joined$Status == "yes"))
    #   df$event_month_year <- format(df$Event.Date.Lookup, format = "%B-%Y")
    #   df <- sqldf('select "event_month_year", count("MemberKey") as "total" from df group by "event_month_year"')
    #   return(df)
    # })
    # output$plot1 <- renderPlot({
    #   plot_data <- plot1_df()
    #   plot <- ggplot(data = plot_data, aes_string(x = 'event_month_year', y = 'total')) + 
    #     geom_bar(stat = "identity", width = .08) + labs(x = "Month - Year", y = "Total")
    #   return(plot)
    # })
  #PLOT 1: Average Attendance by Event Factor Filter----------------------
    plot1_df <- reactive({
      plot1_factor <- as.character(input$plot1_input)
      if(plot1_factor == "Event Type"){
        factor_col <- "Event type"
        variables <- as.list(unique(events$`Event type`))
      } else if(plot1_factor == "Time of Day"){
        factor_col <- "Time of Day"
        variables <- as.list(unique(events$`Time of Day`))
      } else if(plot1_factor == "Location"){
        factor_col <- "City Grouping"
        variables <- as.list(unique(events$`City Grouping`))
      }
      factor_df <- select(events, c(factor_col, "Total Attendance"))
      mat <- names(factor_df) == factor_col
      names(factor_df)[mat] <- "factor_col"
      
      df <- data.frame()
      for(i in variables){
        variable_subset <- subset(factor_df, factor_df$factor_col == i)
        total_events <- nrow(variable_subset)
        attendance <- sum(variable_subset$`Total Attendance`)
        average <- round(attendance/total_events,2)
        row <- cbind(i, average)
        df <- rbind(df, row)
      }
      
      mat <- names(df) == "i"
      names(df)[mat] <- "x_axis"
      mat <- names(df) == "Total Attendance"
      names(df)[mat] <- "average_attendance"
      return(df)
    })
    output$plot1 <- renderPlot({
      plot_data <- plot1_df()
      plot <- ggplot(data = plot_data, aes_string(x = 'x_axis', y = 'average')) +
        geom_bar(stat = "identity", width = .5) + labs(x = as.character(input$plot1_input), y = "Average Attendance") +
        ggtitle(paste("Average Event Attendance by", as.character(input$plot1_input), sep = " ")) +
        theme(axis.text.x = element_text(size = 12))
      return(plot)
    })
  #PLOT 2: Member Treemap--------------------------------------------------
    plot2_df <- reactive({
      plot2_selection <- as.character(input$plot2_input)
      if(plot2_selection == "Company"){
        loop_list <- as.list(unique(members$Company))
      } else {
        loop_list <- as.list(unique(members$Categorization))
      }
      df <- data.frame()
      for(i in loop_list){
        total <- nrow(subset(members, members[,plot2_selection] == i))
        row <- cbind(i, total)
        df <- rbind(df, row)
      }
      mat <- names(df) == "i"
      names(df)[mat] <- "variable"
      df$total <- as.integer(df$total)
      df$categorization_variable <- ifelse(df$total >= 30, df$variable, "Other")
      df$categorization_variable[df$categorization_variable == "????"] <- "Unknown"
      return(df)
    })
    output$plot2 <- renderPlot({
      plot_data <- df
      plot_data <- plot2_df()
      plot <- treemap(plot_data, index = "categorization_variable", vSize = "total",
                type = "index", palette = "Reds", title = paste(as.character(input$plot2_input), "Treemap", sep = " "),
                fontsize.title = 14)
      return(plot)
    })
  #PLOT 3: Monthly Attendance-----------------------------------------------
    plot3_df <- reactive({
      plot3_selection <- as.character(input$plot3_input)
      plot3_year <- input$plot3_year_input
      #plot3_year <- 2018
      
      month_abbrev <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      if(plot3_selection == "Attendance"){
        events_df <- events
        events_df$month <- month(events_df$EventDate)
        events_df$year <- year(events_df$EventDate)
        events_df <- subset(events_df, events_df$year == plot3_year)
        
        df <- data.frame()
        for(month in 1:12){
          event_list <- as.list(unique(events_df[events_df$month == month,]$EventID))
          total <- 0
          for(event in event_list){
            event_attendance <- nrow(subset(attendance, attendance$`Event ID` == event & attendance$AttendStatus == "Went"))
            #event_attendance <- nrow(attendance[attendance$`Event ID` == event & attendance$AttendStatus == "Went"])
            total <- total + event_attendance
          } #close event loop
          month_row <- cbind(month, total)
          df <- rbind(df, month_row)
        } #close month loop
      } #close attendance condition
      
      else if(plot3_selection == "New Members"){
        members_df <- members
        members_df$month <- month(members_df$JoinedGroupOn)
        members_df$year <- year(members_df$JoinedGroupOn)
        members_df <- subset(members_df, members_df$year == plot3_year)
        
        df <- data.frame()
        for(month in 1:12){
          month <- as.numeric(month)
          total <- nrow(members_df[members_df$month == month,])
          month_row <- cbind(month, total)
          df <- rbind(df, month_row)
        }
      } #close new members condition
      df$month <- month_abbrev[df$month]
      df$month <- factor(df$month, levels = month.abb)
      return(df)
    }) #close reactive data frame function
    output$plot3 <- renderPlot({
      plot_data <- plot3_df()
      plot <- ggplot(plot_data, aes_string(x = 'month', y = 'total')) +
        geom_line(aes(y = total, group = 1), color = "darkred") + 
        ggtitle(paste(as.character(input$plot3_input), "by Month in", as.character(input$plot3_year_input), sep = " ")) +
        theme(axis.text.x = element_text(size = 12))
      return(plot)
    })
  #PLOT 4: Parallel Coordinates-----------------------------------------------
    plot4_df <- reactive({
      plot4_selection <- as.character(input$plot4_input)
      plot4_company_selection <- as.character(input$plot4_company_input)
      
      loop_list <- as.list(unique(members$Company))
      companies_df <- data.frame()
      for(company in loop_list){
        total <- nrow(subset(members, members$Company == company))
        row <- cbind(company, total)
        companies_df <- rbind(companies_df, row)
      }
      companies_df$company[companies_df$company == "????"] <- "Unknown"
      companies_df$total <- as.numeric(companies_df$total)
      companies_df$bin <- cut(companies_df$total, breaks = c(-Inf, 10, 50, 100, Inf), labels = c("0-10", "10-50", "50-100", "100+"))
      #companies_df <- companies_df[order(-as.numeric(companies_df$total)),]
      top_companies <- as.list(companies_df$company)
      #top_companies <- as.list(companies_df$company[as.numeric(companies_df$total) >= 10])
      
      df <- data.frame()
      if(plot4_selection == "All Events"){
        for(i in 1:length(top_companies)){
          #print(i)
          company <- as.character(top_companies[[i]])
          company_subset <- subset(members_joined, members_joined$Company == company)
          company_bin <- as.character(companies_df[companies_df$company == company,][1,3])
          #company_bin_df <- subset(companies_df, companies_df$company == company)
          #company_bin <- as.character(company_bin_df[1,]$bin)
          
          rsvp_total <- nrow(subset(company_subset, company_subset$rsvp.response == "yes"))
          rsvp_attend_total <- nrow(subset(company_subset, company_subset$rsvp.response == "yes" & company_subset$AttendStatus == "Went"))
          no_rsvp_attend_total <- nrow(subset(company_subset, company_subset$rsvp.response == "no" & company_subset$AttendStatus == "Went"))
          company_row <- cbind(company, company_bin, rsvp_total, rsvp_attend_total, no_rsvp_attend_total)
          df <- rbind(df, company_row)
        } #close company loop
      } #close all events condition
      else if(plot4_selection == "Events Hosted by the Company"){
        for(company in top_companies){
          company_subset <- subset(members_joined, members_joined$Company == company)
          events_subset <- subset(events, events$Company == company)
          events_list <- as.list(unique(events_subset$EventID))
          company_subset <- subset(company_subset, company_subset$`Event ID` %in% events_list)
          company_bin <- as.character(companies_df[companies_df$company == company,][1,3])
          
          rsvp_total <- nrow(subset(company_subset, company_subset$rsvp.response == "yes"))
          rsvp_attend_total <- nrow(subset(company_subset, company_subset$rsvp.response == "yes" & company_subset$AttendStatus == "Went"))
          no_rsvp_attend_total <- nrow(subset(company_subset, company_subset$rsvp.response == "no" & company_subset$AttendStatus == "Went"))
          company_row <- cbind(company, company_bin, rsvp_total, rsvp_attend_total, no_rsvp_attend_total)
          df <- rbind(df, company_row)
        } #close company loop
        df <- subset(df, df$rsvp_total != 0 & df$rsvp_attend_total != 0 & df$no_rsvp_attend_total != 0)
      } #close events hosted by the company condition
      
      if(plot4_company_selection != "All Companies"){
        df <- subset(df, df$company == plot4_company_selection)
      }
      
      df <- df[!is.na(df$company_bin),]
      return(df)
    }) #close reactive data frame function
    output$plot4 <- renderPlot({
      plot_data <- plot4_df()
      #plot <- ggparcoord(plot_data, columns = 3:ncol(plot_data), groupColumn = 2, title = paste("Company Attendance:", as.character(input$plot4_input), sep = " "))
      
      return(plot)
    })
  #PLOT 5: Most Active Companies
    
}


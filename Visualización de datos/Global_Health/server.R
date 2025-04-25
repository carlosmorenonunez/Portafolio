#Library
library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)

# Data
data = read.csv("Global Health Statistics.csv")
data = data %>% filter(Country == "Italy")
data_year = data %>% 
  group_by(Year, Disease.Name) %>% 
  summarise(avg_population = mean(Population.Affected, na.rm = TRUE),
            avg_Prevalence = mean(Prevalence.Rate...., na.rm = TRUE),
            avg_incidence = mean(Incidence.Rate...., na.rm = TRUE),
            avg_mortality = mean(Mortality.Rate...., na.rm = TRUE),
            avg_recovery = mean(Recovery.Rate...., na.rm = TRUE),
            common_Treatmen = names(sort(table(Treatment.Type), decreasing = TRUE)[1]),
            .groups = "drop"
  )

#Shiny
shinyServer(function(input, output) {
  #Affected
  output$plot1 = renderPlot({
    filtered_data = data_year %>%
      filter(Disease.Name == input$y1)
    
    p = ggplot(filtered_data, 
               aes(x = Year, y = avg_population)) + 
                 geom_point(alpha = 0.7, size = 2, color = "darkgreen") +
      labs(title = paste("Population Affected of", input$y1, "in Italy"),
           x = "Date", 
           y = "Population Affected")
    
    if (input$lm)
      p = p + geom_smooth(method = "lm", se = TRUE, color ="Black")
    
    print(p)
})
  #Prevalence
  output$plot2 = renderPlot({
    filtered_data = data_year %>%
      filter(Disease.Name == input$y1)
    
    p = ggplot(filtered_data, 
               aes(x = Year, y = avg_Prevalence)) + 
      geom_col(alpha = 0.7, color = "darkgreen") +
      labs(title = paste("Prevalence Rate of", input$y1, "in Italy"),
           x = "Date", 
           y = "Prevalence Rate")
    
    if (input$lm)
      p = p + geom_smooth(method = "lm", se = TRUE, color ="Black")
    
    print(p)
  })
  #Incidence
  output$plot3 = renderPlot({
    filtered_data = data_year %>%
      filter(Disease.Name == input$y1)
    
    p = ggplot(filtered_data, 
               aes(x = Year, y = avg_incidence)) + 
      geom_line(alpha = 0.7, color = "darkgreen") +
      labs(title = paste("Incidence Rate of", input$y1, "in Italy"),
           x = "Date", 
           y = "Incidence Rate")
    
    if (input$lm)
      p = p + geom_smooth(method = "lm", se = TRUE, color ="Black")
    
    print(p)
  })
  #Recovery
  output$plot4 = renderPlot({
    filtered_data = data_year %>%
      filter(Disease.Name == input$y1)
    
    p = ggplot(filtered_data, 
               aes(x = Year, y = avg_recovery)) + 
      geom_smooth(method = "loess", color = "darkgreen") +
      labs(title = paste("Recovery Rate of", input$y1, "in Italy"),
           x = "Date", 
           y = "Recovery Rate")
    
    if (input$lm)
      p = p + geom_smooth(method = "lm", se = TRUE, color ="Black")
    
    print(p)
  })
  #Mortality
  output$plot5 = renderPlot({
    filtered_data = data_year %>%
      filter(Disease.Name == input$y1)
    
    p = ggplot(filtered_data, 
               aes(x = Year, y = avg_mortality)) + 
      geom_area(alpha = 0.7, color = "darkgreen") +
      labs(title = paste("Mortality Rate of", input$y1, "in Italy"),
           x = "Date", 
           y = "Mortality Rate")
    
    if (input$lm)
      p = p + geom_smooth(method = "lm", se = TRUE, color ="Black")
    
    print(p)
  })
})
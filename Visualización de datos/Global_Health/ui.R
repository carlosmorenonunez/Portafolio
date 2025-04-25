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
                                           

# Shiny UI
shinyUI(
  navbarPage("Global Health Statistics",
             
             # Cover Page
             tabPanel("Cover",
                      h1("Global Health Statistics", align = "center"),
                      h2("Carlos Moreno Nuñez", align = "center"),
                      h3("Data Analyst", align = "center"),
                      p("Este conjunto de datos proporciona estadísticas completas sobre la salud global, centradas en diversas enfermedades, tratamientos y resultados. Los datos abarcan múltiples países y años, ofreciendo valiosos conocimientos para la investigación en salud, estudios epidemiológicos y aplicaciones de aprendizaje automático. El conjunto de datos incluye información sobre la prevalencia, incidencia y tasas de mortalidad de las principales enfermedades, así como sobre la eficacia de los tratamientos y la infraestructura sanitaria."),
                      p("En esta visualización realizaremos un estudio de varias de estas enfermedades en Italia"),
                      p("This dataset provides comprehensive statistics on global health, focusing on various diseases, treatments, and outcomes. The data spans multiple countries and years, offering valuable insights for health research, epidemiology studies, and machine learning applications. The dataset includes information on the prevalence, incidence, and mortality rates of major diseases, as well as the effectiveness of treatments and healthcare infrastructure."),
                      p("In this visualization, we will conduct a study of several of these diseases in Italy."),
                      p(""),
                      p("Descargar Dataset   -   Download Dataset", align = "center"),
                    
                      p("https://www.kaggle.com/datasets/malaiarasugraj/global-health-statistics", align = "center")
             ),
             
             # Population Affected
             tabPanel("Visualization",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("y1", "Select Disease", 
                                      choices = c("COVID-19",
                                                  "Hepatitis",
                                                  "Parkinson's Disease",
                                                  "Asthma",
                                                  "Diabetes",
                                                  "Alzheimer's Disease",
                                                  "Cancer"),
                                      selected = "COVID-19"),
                          
                          checkboxInput(inputId = "lm",
                                        label = "Regression line",
                                        value = TRUE)
                        ),
                        mainPanel(
                          fluidRow(
                            column(width = 6,
                                   plotOutput("plot1", height = "350px")
                            ),
                            column(width = 6,
                                   plotOutput("plot2", height = "350px")
                            )
                          ),
                          fluidRow(
                            column(width = 6,
                                   plotOutput("plot3", height = "350px")
                            ),
                            column(width = 6,
                                   plotOutput("plot4", height = "350px")
                            ),
                            column(width = 6,
                                   plotOutput("plot5", height = "350px")
                            )
                          )
                        )
                      )
             )
  )
)
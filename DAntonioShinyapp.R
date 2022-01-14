# DAntonio Lab Shiny App; Zoe Fung

library(shiny) # load packages
library(tidyverse) 
library(readr)
library(janitor)
library(lubridate)
library(shinythemes)
library(ggplot2)


leaf_traits <- read_csv("leaf_traits.csv") # load Midland leaf traits data

species <- unique(leaf_traits$species) # list the different species in the dataset

ui <- fluidPage(theme=shinytheme("darkly"), # set theme of ui
                titlePanel("Midland Leaf Traits"), # set title of page
                sidebarLayout( # create a side bar
                  sidebarPanel("Data Visualization", # title the side bar 
                               radioButtons(inputId="transect",
                                            label="Select a transect to view:",
                                            choices=c(unique(leaf_traits$transect)))), # create a radio buttons widget that allows users to filter data by species
                  mainPanel("Graphical output",
                            plotOutput(outputId="species_plot")), # create a graph in the main panel of transect vs. dependent variable of user's choice
                ))

server <- function(input,output){
  leaf_traits_reactive <- reactive({ # create a reactive data frame for leaf traits data
    leaf_traits %>%
      filter(transect==input$transect) # filter the data set by the species radio buttons input
  })
  output$species_plot <- renderPlot({ # create a reactive plot 
    ggplot(data=leaf_traits_reactive(),aes(x=species,y=mass_g)) + # scatterplot species vs. mass for each species
      geom_point(color="blue")
  })
} 

shinyApp(ui=ui,server=server) # combine ui and server to create Shiny app
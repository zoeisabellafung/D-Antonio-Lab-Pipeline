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
species_types <- unique(leaf_traits$species)

ui <- fluidPage(theme=shinytheme("darkly"), # set theme of ui
                titlePanel("Midland Leaf Traits"), # set title of page
                sidebarLayout( # create a side bar
                  sidebarPanel("Data Visualization", # title the side bar 
                               radioButtons(inputId="transect", # create a radio buttons widget that allows users to filter data by species
                                            label="Select a transect to view:",
                                            choices=c(unique(leaf_traits$transect))),
                               checkboxGroupInput("species", label="Filter by species:", 
                                                  choices = c(unique(leaf_traits$species)),
                                                  selected = unique(leaf_traits$species))),
                  mainPanel(
                    h4("Summary"),
                    verbatimTextOutput("table"),
                    h4("Graphical output"),
                            plotOutput(outputId="species_thickness_plot"), #create a scatterplot in main panel of species vs. thickness
                            plotOutput(outputId="species_curling_plot") # create a scatterplot in main panel of species vs. curling
                            ), 
                ))

server <- function(input,output){
  leaf_traits_reactive <- reactive({ # create a reactive data frame for leaf traits data
    leaf_traits %>%
      filter(transect==input$transect) %>% # filter the data set by the species radio buttons input
      filter(species==input$species)
  })
  output$table <- renderTable({
    head(data=leaf_traits_reactive(), n = isolate(input$transect))
  })
  output$species_thickness_plot <- renderPlot({ # create a reactive plot
    ggplot(data=leaf_traits_reactive(),aes(x=species,y=thickness_mm)) + # scatterplot of species vs. thickness for each transect
      geom_point(color="purple") 
  })
  output$species_curling_plot <- renderPlot({ # create a reactive plot 
    ggplot(data=leaf_traits_reactive(),aes(x=species,y=curling_mm)) + # scatterplot species vs. curling for each transect
      geom_point(color="green")
  })
} 

shinyApp(ui=ui,server=server) # combine ui and server to create Shiny app
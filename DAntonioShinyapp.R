# DAntonio Lab Shiny App; Zoe Fung

library(shiny) # load packages
library(tidyverse) 
library(readr)
library(janitor)
library(lubridate)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(forcats)

leaf_traits <- read_csv("leaf_traits_shiny.csv") # load Midland leaf traits data
leaf_traits$transect <- as.factor(leaf_traits$transect) # make "transect" a factor
# leaf_traits$transect <- unlist(leaf_traits$transect) # make "transect" not a list

ui <- fluidPage(theme=shinytheme("darkly"), # set theme of ui
                titlePanel("Midland Leaf Traits"), # set title of page
                sidebarLayout( # create a side bar/main panel layout
                  sidebarPanel("Data Visualization", # title the side bar 
                               checkboxGroupInput(inputId="transect", # create a checkbox widget that allows users to filter data by transect
                                            label="Select transect(s) to view:",
                                            choices=c(unique(leaf_traits$transect)),
                                            selected=c(unique(leaf_traits$transect)))), # all automatically selected when app is first run
                               # radioButtons(inputId="species",
                               #              label="Select a species to view:",
                               #              choices=c(unique(leaf_traits$species_cleaned)))),
                  mainPanel(
                    h4("Summary"),
                    verbatimTextOutput("table"),
                    h4("Graphical output"),
                            plotOutput(outputId="leaf_traits_plot"), #create a scatterplot in main panel of leaf thickness per species across all transects
                            ), 
                ))

server <- function(input,output){
  leaf_traits_reactive <- reactive({ # create a reactive data frame for leaf traits data
    leaf_traits %>%
      filter(transect==input$transect) # filter the data set by the transect input
      # filter(species_cleaned==input$species) # filter the data set by the species input
  })
  output$table <- renderTable({
    head(data=leaf_traits_reactive(), n = isolate(input$transect))
  })
  output$leaf_traits_plot <- renderPlot({
    ggplot(data=leaf_traits_reactive(),aes(x=transect,y=curling_mm,color=species_cleaned,shape=transect)) +
      geom_point()
  })
} 

shinyApp(ui=ui,server=server) # combine ui and server to create Shiny app
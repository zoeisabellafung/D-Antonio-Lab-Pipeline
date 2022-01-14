# D'Antonio Lab Shiny App; Zoe Fung

library(shiny)
library(tidyverse) #load packages

leaf_traits <- read_csv(midland_leaf_traits-Sheet1.csv) # load Midland leaf traits data

ui <- fluidPage(theme=shinytheme("darkly"), # set theme of ui
                titlePanel("Midland Leaf Traits"), # set title of page
                sidebarLayout( # create a side bar
                  sidebarPanel("Data Visualization", # title the side bar 
                               )
                ))

server <- function(input,output){} 

shinyApp(ui=ui,server=server) # combine ui and server to create Shiny app
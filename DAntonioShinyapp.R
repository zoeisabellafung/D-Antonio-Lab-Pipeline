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
library(bslib)

leaf_traits_shiny <- read_csv("leaf_traits_shiny.csv") # load Midland leaf traits data
# leaf_traits_shiny$transect <- as.factor(leaf_traits_shiny$transect) # make "transect" a factor
# mutate(leaf_traits$transect=fct_reorder(transect,"1","2","3","4","6","7","10","12")) # reorder transect by numerical order
# levels(leaf_traits_shiny$transect) <- c("1","2","3","4","6","7","10","12") # rename the transect levels 
# leaf_traits$transect <- unlist(leaf_traits$transect) # make "transect" not a list

species_summary <- leaf_traits_shiny$species_cleaned # create vector

ui <- navbarPage("D'Antonio Lab Shiny app", 
                 theme=bs_theme( # set custom theme
                   bg="",
                   fg="",
                   primary="",
                   base_font=(""),
                   code_font=("")
                 ), # set theme of ui
                 
                 # tab 1
                 tabPanel("Summary Statistics", # create tab for summary stats/overview of data
                          fluidRow(column(12,
                                          h1("Overview of Midland Leaf Traits Data")), # header of the tab page
                                   br(), # add a line of space
                                   br() # add another line of space
                          ),
                          fluidRow(column(12, align="center",
                                          h3("Midland Leaf Traits Summary Statistics"),
                                          verbatimTextOutput("summary_table"), # display table with summary statistics
                                          radioButtons(inputId="species",
                                                       label="Select a species to view:",
                                                       choices=c(unique(leaf_traits_shiny$species_cleaned))))
                                          )),
                 
                 # tab 2
                 tabPanel("Visualization", # create tab for data visualization
                          h1("Visualizing Midland Leaf Traits"), # header of the tab page
                          sidebarLayout( # create a side bar/main panel layout
                            sidebarPanel("Filter by transect and species", # title the side bar
                                         checkboxGroupInput(inputId="transect", # create a checkbox widget that allows users to filter data by transect
                                                            label="Select transect(s) to view:",
                                                            choices=c(unique(leaf_traits_shiny$transect)),
                                                            selected=c(unique(leaf_traits_shiny$transect))), # all automatically selected when app is first run
                                         conditionalPanel( # only show this panel if transect 1 is selected
                                           condition="input.transect=='1'",
                                           checkboxGroupInput(inputId="cond_species_1", # checkbox widget that allows users to filter the transect data by species
                                                              label="Select species to view:",
                                                              choices=c(unique(leaf_traits_shiny$species_cleaned[leaf_traits_shiny$transect==1])), # only the species in transect one are choices
                                                              selected=c(unique(leaf_traits_shiny$species_cleaned[leaf_traits_shiny$transect==1]))))), # automatically select all choices
                            mainPanel(
                              h4("Graphical output"),
                              plotOutput(outputId="leaf_traits_plot") #create a scatterplot in main panel of leaf thickness per species across all transects
                            ) 
                )))

server <- function(input,output){
  leaf_traits_reactive <- reactive({ # create a reactive data frame for leaf traits data visualization
    leaf_traits_shiny %>%
      filter(transect==input$transect) # filter the data set by the transect input
  })
  leaf_traits_reactive2 <- reactive({
    leaf_traits_shiny %>%
      filter(species_cleaned==input$species) # filter the data set by the species input
  })
  output$summary_table <- renderPrint({ # create a reactive table of summary stats
    summary(leaf_traits_reactive[,as.numeric(input$species)]) 
  })
  output$leaf_traits_plot <- renderPlot({
    ggplot(data=leaf_traits_reactive(),aes(x=transect,y=curling_mm,color=species_cleaned)) +
      geom_point()
  })
} 

shinyApp(ui=ui,server=server) # combine ui and server to create Shiny app
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

# load in overall Midland leaf traits data
leaf_traits <- read_csv("leaf_traits.csv") # load Midland leaf traits data
leaf_traits <- leaf_traits %>%
  mutate(transect=as.character(transect)) 
# %>%
#   mutate(transect=fct_relevel(transect,"1","2","3","4","6","7","10","12"))
# leaf_traits_shiny$transect <- as.character(leaf_traits_shiny$transect) # make "transect" a factor
# mutate(leaf_traits$transect=fct_reorder(transect,"1","2","3","4","6","7","10","12")) # reorder transect by numerical order
# levels(leaf_traits_shiny$transect) <- c("1","2","3","4","6","7","10","12") # rename the transect levels 
# leaf_traits$transect <- unlist(leaf_traits$transect) # make "transect" not a list

# load in summary data
curling_summary <- leaf_traits %>% # create data frame for summary stats of leaf curling
  group_by(species_cleaned) %>% # group the summary stats by species
  summarise(
    n=n(), # sample size
    mean=mean(curling_mm), # mean
    sd=sd(curling_mm) # std. deviation
  )
thickness_summary <- leaf_traits %>% # create data frame for summary stats of leaf thickness
  group_by(species_cleaned) %>% # group the summary stats by species
  summarise(
    n=n(), # sample size
    mean=mean(thickness_mm), # mean
    sd=sd(thickness_mm) # std. deviation
  )
mass_summary <- leaf_traits %>% # create data frame for summary stats of leaf mass
  group_by(species_cleaned) %>% # group the summary stats by species
  summarise(
    n=n(), # sample size
    mean=mean(mass_g), # mean
    sd=sd(mass_g) # std. deviation
  )
curling_thickness <- merge(curling_summary, thickness_summary, by="species_cleaned") # merge thickness and curling summary data frames
leaf_traits_summary <- merge(curling_thickness, mass_summary, by="species_cleaned") %>% # merge thickness/curling and mass summary data frames
  rename(curling_n=n.x) %>% # rename curling sample size column
  rename(curling_mean=mean.x) %>% # rename curling mean column
  rename(curling_sd=sd.x) %>% # rename curling sd column
  rename(thickness_n=n.y) %>% # rename thickness sample size column
  rename(thickness_mean=mean.y) %>% # rename thickness mean column
  rename(thickness_sd=sd.y) %>% # rename thicnkess sd column
  rename(mass_n=n) %>% # rename mass sample size column
  rename(mass_mean=mean) %>% # rename mass mean column
  rename(mass_sd=sd) # rename mass sd column
  

species_summary <- leaf_traits$species_cleaned # create vector

ui <- navbarPage("Midland Leaf Traits", 
                 theme=bs_theme(version=4,bootswatch="minty"), # set app theme
                 # bg="darkgray",
                 # fg="darkslateblue",
                 # primary="cornflowerblue",
                 # base_font=(""),
                 # code_font=("")
                 # set theme of ui
                
                 # tab 1
                 tabPanel("Home", #create home page
                          fluidPage( # create scrollable page
                            h3(p(strong("Welcome!"), # title the page
                                 style="text-align:center")), # center the title text
                            br(), # add a space
                            h4(p("Upload and visualize your data here:)", # put body text on the page
                                 style="text-align:center")) # center the text
                          )),
                 
                 # tab 2
                 tabPanel("Summary Statistics", # create tab for summary stats/overview of data
                          fluidRow(column(12,
                                          h1("Overview of Midland Leaf Traits Data")), # header of the tab page
                                   br(), # add a line of space
                                   br() # add another line of space
                          ),
                          fluidRow(column(12, align="center",
                                          h3("Midland Leaf Traits Summary Statistics"),
                                          verbatimTextOutput("summary_table"), # display table with summary statistics
                                          radioButtons(inputId="species", # radio buttons widget to choose a species' table
                                                       label="Select a species to view:",
                                                       choices=c(unique(leaf_traits$species_cleaned)))) # choices are the species
                                          )),
                 
                 # tab 2
                 tabPanel("Visualization", # create tab for data visualization
                          h1("Visualizing Midland Leaf Traits"), # header of the tab page
                          tabsetPanel(id="visualization", # create tabs within the page
                                      tabPanel(h4("Bar plots"), # first panel: bar plots
                                               sidebarLayout( # create a side bar/main panel layout
                                                 sidebarPanel(p(strong("Customize your bar plot!")), # title the side bar
                                                              radioButtons(inputId="variable", # radio buttons widget to choose a variable
                                                                           label="Choose a variable:",
                                                                           choices=c("Curling (mm)"="curling_mean", "Thickness (mm)"="thickness_mean", "Mass (g)"="mass_mean") # choices are the trait variables
                                                                           )), # CHOICES NEED TO BE FIXED
                                                 mainPanel( # create the main panel
                                                   plotOutput(outputId="leaf_traits_bar")))), # show the leaf traits bar plot
                                      
                                      tabPanel(h4("Scatterplots"), # second panel: scatterplots 
                                               sidebarLayout( # create a side bar/main panel layout
                                                 sidebarPanel("Filter by transect and species", # title the side bar
                                                              checkboxGroupInput(inputId="transect", # create a checkbox widget that allows users to filter data by transect
                                                                                 label="Select transect(s) to view:",
                                                                                 choices=c(unique(leaf_traits$transect)), # choices are transects
                                                                                 selected=c(unique(leaf_traits$transect))), # all automatically selected when app is first run
                                                              conditionalPanel( # only show this panel if transect 1 is selected
                                                                condition="input.transect=='1'&&'2'",
                                                                checkboxGroupInput(inputId="cond_species_1", # checkbox widget that allows users to filter the transect data by species
                                                                                   label="Select species to view:",
                                                                                   choices=c(unique(leaf_traits$species_cleaned[leaf_traits$transect==1])), # only the species in transect one are choices
                                                                                   selected=c(unique(leaf_traits$species_cleaned[leaf_traits$transect==1]))))), # automatically select all choices
                                                 mainPanel( # create main panel
                                                   h4("Graphical output"), # title main panel
                                                   plotOutput(outputId="leaf_traits_plot")) #create a scatterplot in main panel of leaf thickness per species across all transects
                            )))))

server <- function(input,output){
  leaf_traits_reactive <- reactive({ # create a reactive data frame for leaf traits data visualization
    leaf_traits %>%
      filter(transect==input$transect) # filter the data set by the transect input
  })
  
  leaf_traits_reactive2 <- reactive({ # create a reactive data frame for table options
    leaf_traits %>%
      filter(species_cleaned==input$species) # filter the data set by the species input
  })
  
  summary_reactive <- reactive({ # create a reactive data frame for summary stat visualization (bar plots and boxplots)
    leaf_traits_summary %>%
      filter()
  })
  output$summary_table <- renderPrint({ # create a reactive table of summary stats
    summary(leaf_traits_reactive[,as.numeric(input$species)]) 
  })
  output$leaf_traits_plot <- renderPlot({ # reactive scatterplot of transect vs. curling, point color depends on species
    ggplot(data=leaf_traits_reactive(),aes(x=transect,y=curling_mm,color=species_cleaned)) +
      geom_point()
  })
  
  output$leaf_traits_bar <- renderPlot({ # reactive bar graph of species vs. mean leaf curling
    ggplot(data=curling_summary_reactive, aes(x=species_cleaned, y=mean)) + # create plot of species x curling
      geom_bar(stat="identity", fill="plum4") + # plot = bar graph
      xlab("Species") + # x-axis label
      ylab("Curling(mm)") + # y-axis label
      geom_errorbar(aes(x=species_cleaned, ymin=mean-sd, ymax=mean+sd), color="black") # add error bars
  })
} 

shinyApp(ui=ui,server=server) # combine ui and server to create Shiny app
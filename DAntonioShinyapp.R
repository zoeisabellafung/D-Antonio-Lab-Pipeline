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
library(ggthemes)

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
var1 <- rep("curling", times=7) # create vector with 12 values "curling"
curling_summary <- curling_summary %>%
  add_column(trait=var1) # add "curling" vector to curling summary data frame

thickness_summary <- leaf_traits %>% # create data frame for summary stats of leaf thickness
  group_by(species_cleaned) %>% # group the summary stats by species
  summarise(
    n=n(), # sample size
    mean=mean(thickness_mm), # mean
    sd=sd(thickness_mm) # std. deviation
  )
var2 <- rep("thickness", times=7) # create vector with 12 values "curling"
thickness_summary <- thickness_summary %>%
  add_column(trait=var2) # add "curling" vector to curling summary data frame

mass_summary <- leaf_traits %>% # create data frame for summary stats of leaf mass
  group_by(species_cleaned) %>% # group the summary stats by species
  summarise(
    n=n(), # sample size
    mean=mean(mass_g), # mean
    sd=sd(mass_g) # std. deviation
  )
var3 <- rep("mass", times=7) # create vector with 12 values "curling"
mass_summary <- mass_summary %>%
  add_column(trait=var3) # add "curling" vector to curling summary data frame

leaf_traits_summary <- bind_rows(curling_summary, thickness_summary, mass_summary) # combine the three data sets 
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
                          sidebarLayout( # create side bar/main panel layout
                            sidebarPanel(p(strong("Customize your summary statistics table!")), # title the sidebar
                                         checkboxGroupInput(inputId="trait", # radio buttons widget to choose a species' table
                                                      label="Select a trait to view:",
                                                      choices=c("Curling (mm)"="curling", "Thickness (mm)"="thickness", "Mass (g)"="mass"), # choices are the traits
                                                      selected=c("Curling (mm)"="curling", "Thickness (mm)"="thickness", "Mass (g)"="mass"))), # all automatically selected when app is run
                            mainPanel(p(strong("Summary Table")), # title the main panel
                                      verbatimTextOutput("summary_table")))), # display summary table that changes based on trait input))) 
                 
                 # tab 3
                 tabPanel("Visualization", # create tab for data visualization
                          h1("Visualizing Midland Leaf Traits"), # header of the tab page
                          tabsetPanel(id="visualization", # create tabs within the page
                                      tabPanel(h4("Bar plots"), # first panel: bar plots
                                               sidebarLayout( # create a side bar/main panel layout
                                                 sidebarPanel(p(strong("Customize your bar plot!")), # title the side bar
                                                              radioButtons(inputId="traitbar", # radio buttons widget to choose a variable
                                                                           label="Choose a trait:",
                                                                           choices=c("Curling (mm)"="curling", "Thickness (mm)"="thickness", "Mass (g)"="mass") # choices are the trait variables
                                                                           )), 
                                                 mainPanel( # create the main panel
                                                   plotOutput(outputId="leaf_traits_bar")))), # show the leaf traits bar plot
                                      tabPanel(h4("Boxplots"), # second panel: boxplots
                                               sidebarLayout( # create side bar/main panel layout
                                                 sidebarPanel(p(strong("Customize your boxplot!")), # title the sidebar
                                                              radioButtons(inputId="traitbox", # radio buttons widget to choose a trait for the boxplot
                                                                           label="Choose a trait:",
                                                                           choices=c("Curling (mm)"="curling", "Thickness (mm)"="thickness", "Mass (g)"="mass"), # choices are the traits
                                                                           )),
                                                 mainPanel( # create main panel
                                                   plotOutput(outputId="leaf_traits_box") # show the leaf traits box plot
                                                   ))),
                                      
                                      tabPanel(h4("Scatterplots"), # third panel: scatterplots 
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
  
  summary_reactive <- reactive({ # create a reactive data frame for summary stat bar plot
    leaf_traits_summary %>%
      filter(trait==input$traitbar) # filter data set by trait input
  })
  
  summary_reactive2 <- reactive({ # create a reactive data frame for summary stat boxplot
    leaf_traits_summary %>%
      filter(trait==input$traitbox) # filter data set by trait input
  })
  
  output$summary_table <- renderPrint({ # create a reactive table of summary stats
    summary(leaf_traits_reactive[,as.numeric(input$species)]) 
  })
  
  output$leaf_traits_plot <- renderPlot({ # reactive scatterplot of transect vs. curling, point color depends on species
    ggplot(data=leaf_traits_reactive(),aes(x=transect,y=curling_mm,color=species_cleaned)) +
      geom_point()
  })
  
  output$leaf_traits_bar <- renderPlot({ # reactive bar plot of species vs. mean of each trait
    ggplot(data=summary_reactive(), aes(x=species_cleaned, y=mean)) + # create plot of species x all traits
      geom_bar(stat="identity", fill="darkseagreen") + # plot = bar plot
      xlab("Species") + # x-axis label
      ylab(input$traitbox) + # y-axis label
      geom_errorbar(aes(x=species_cleaned, ymin=mean-sd, ymax=mean+sd), color="black") + # add error bars
      theme_pander() # set theme
  })
  
}

shinyApp(ui=ui,server=server) # combine ui and server to create Shiny app
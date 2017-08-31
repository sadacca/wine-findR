
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

load("data/app_data.RData")

dataset1 <- wine_distances
dataset2 <- pc_wineload

fluidPage(
  
  title = "Wine-FindR: Using wine reviews to find Chardonnays that taste like Merlots",
  h2("Wine-FindR: Using wine reviews to determine what wines are similar to others"),
  h3("e.g. using wine reviews to find Chardonnays that taste like Merlots"),
  
  hr(),
  
  fluidRow(
    column(6, 
           
           h4("Where do all wine types fall in PCA space?"),
           
           plotOutput('plot_dummy')
           
    ),
    column(6,
           
           h4("Where do our selected wines fall in PCA space?"),
           
           
           plotOutput('plot_active')
           
    )
    
  ),
  
  
  
  fluidRow(
    column(4,
           
           h2("parameters"),
           
           selectInput('target', 'Find me this kind of wine', dataset2$Group.1, selected = 1),
           
           radioButtons("rb", "..which is:",
                        choiceNames = list(
                          "... identical to  ...",
                          "... totally different from  ..."
                        ),
                        
                        choiceValues = list(
                          "similarTo", "differentFrom"
                        )),
           
           selectInput('comparison', 'this kind of wine', dataset2$Group.1, selected = 1),
           
           br(),
           
           sliderInput('dollars', 'Max Wine Price', 
                       min=15, max=300,
                       value=min(30), 
                       step=5, round=0)
    ),
  
    column(5,
           
           h2("the wines"),
           
           tableOutput('plot_header')          
    )
    
  )
)
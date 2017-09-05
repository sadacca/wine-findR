
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(shinythemes)

load("data/new_app_data.RData")

dataset1 <- wine_distances
dataset2 <- pc_wineload

fluidPage(
  theme = shinytheme("sandstone"),
  
  #' tags$head(
  #'   tags$style(HTML("
  #'     @import url('//fonts.googleapis.com/css?family=Work+Sans:700|Roboto');
  #'     
  #'     h1 {
  #'       font-family: 'Work Sans', sans-serif;
  #'       font-weight: 700;
  #'       font-style: normal;
  #'       color: #2c0b51;
  #'     }
  #' 
  #'     h3 {
  #'       font-family: 'Roboto', sans-serif;
  #'       font-weight: 400;
  #'       color: #16032b;
  #'     }
  #'      
  #'     h2 {
  #'       font-family: 'Roboto', sans-serif;
  #' 
  #'       font-weight: 400;
  #'       color: #16032b;
  #'     }
  #'     h4 {
  #'       font-family: 'Roboto', sans-serif;
  #' 
  #'       font-weight: 400;
  #'       color: #16032b;
  #'     }
     

  #'  "))
  #'),
  
  title="Wine-FindR: Using wine reviews to find Chardonnays that taste a bit like Merlots",
  
  titlePanel("Wine-FindR:"),
  
  h3(" Using wine reviews to find Chardonnays that taste a bit like Merlots"),
  

  hr(),
  
  fluidRow(
    column(6, 
           
           h4("How do wine varieties relate to each other, on average?"),
           
           plotOutput('plot_dummy')
           
    ),
    column(6,
           
           h4("How do our selected wines relate to each wine variety?"),
           
           
           plotOutput('plot_active')
           
    )
    
  ),
  
  hr(),
  
  fluidRow(
    column(4,style = "background-color:#f9fbff;",
           
           h2("parameters"),
           
           selectInput('target', 'Find me this kind of wine',
                       list("Red, many reviews" = c("Cabernet Sauvignon", "Pinot Noir", "Sangiovese","Malbec","Red Blend"), 
                            "White, many reviews" = c("Chardonnay","Riesling","Sauvignon Blanc","White Blend"),
                            "Red, fewer reviews" = c("Aglianico","Bordeaux-style Red Blend","Cabernet Franc","Grenache","Merlot","Nebbiolo","Rhône-style Red Blend","Syrah","Tempranillo","Zinfandel"),
                            "White, fewer reviews"= c("Bordeaux-style White Blend","Garganega","Gewürztraminer","Grüner Veltliner","Pinot Grigio","Pinot Gris","Vernaccia","Viognier")), selected = "Chardonnay"),
           
           radioButtons("rb", "..which is:",
                        choiceNames = list(
                          "... pretty similar to  ...",
                          "... totally different from  ..."
                        ),
                        
                        choiceValues = list(
                          "similarTo", "differentFrom"
                        )),
           
           selectInput('comparison', 'this kind of wine',
                       list("Red, many reviews" = c("Cabernet Sauvignon", "Pinot Noir", "Sangiovese","Malbec","Red Blend"), 
                            "White, many reviews" = c("Chardonnay","Riesling","Sauvignon Blanc","White Blend"),
                            "Red, fewer reviews" = c("Aglianico","Bordeaux-style Red Blend","Cabernet Franc","Grenache","Merlot","Nebbiolo","Rhône-style Red Blend","Syrah","Tempranillo","Zinfandel"),
                            "White, fewer reviews"= c("Bordeaux-style White Blend","Garganega","Gewürztraminer","Grüner Veltliner","Pinot Grigio","Pinot Gris","Vernaccia","Viognier")), selected = "Merlot"),
           
           br(),
           
           sliderInput('dollars', 'Max Wine Price', 
                       min=15, max=300,
                       value=300, 
                       step=5, round=0)
    ),
  
    column(5,
           
           h2("the wines"),
           
           tableOutput('plot_header')          
    )
    
  )
)
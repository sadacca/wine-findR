#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
## app.R ##

library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggrepel)
library(wordVectors)
library(dplyr)
library(tm)
library(DT)

#load("data/new_app_data.RData")
load("data/comb_app_data.RData")
#model <-readRDS("data/model_100.RDS")
model<-as.VectorSpaceModel(model)

#wine_model<-readRDS("data/term_proj100.RDS")
wine_model <- as.VectorSpaceModel(wine_model)
includeCSS("www/custom.css")



ui <- dashboardPage( skin = 'black',
                     
                     # Header content and formatting
                     dashboardHeader(title = "wine findR: two-ways to find new wines"),
                     
                     ## Sidebar content and formatting
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Overview", tabName = "intro", icon = icon("glass")),
                         menuItem("Wine Analogies", tabName = "analogy", icon = icon("glass")),
                         menuItem("Wine Dreams", tabName = "review", icon = icon("glass"))
                       )
                     ),
                     
                     
                     ## Body content
                     dashboardBody(
                       
                       
                       tags$head(tags$style(HTML('
                              /* logo */
                              .skin-black .main-header .logo {
                              background-color: #dfca4e;
                              }
                              
                              /* logo when hovered */
                              .skin-black .main-header .logo:hover {
                              background-color: #dfca4e;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-black .main-header .navbar {
                              background-color: #dfca4e;
                              }        
                              
                              /* main sidebar */
                              .skin-black .main-sidebar {
                              background-color: #dfca4e;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #d4b63a;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-black .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #dfca4e;
                              color: #000000;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #fffcc0;
                              }
                              /* toggle button when hovered  */                    
                              .skin-black .main-header .navbar .sidebar-toggle:hover{
                              background-color: #fffcc0;
                              }
                              /* change the background */
                              .content-wrapper, .right-side {
                              background-color: #ffffff;
                              }
                              '))),
                       
                       
                       
                       tabItems(
                         
                         # First tab content
                         tabItem(tabName = "intro", 
                                 
                                 h1(" Overview:"),
                                 hr(),
                                 htmlOutput("inc")
                         ),
                         
                         tabItem(tabName = "analogy",
                                 
                                 h3(" Using wine reviews to find Chardonnays that taste a bit like a Pinot Noir"),
                                 
                                 hr(),
                                 
                                 fluidRow(
                                   column(4,style = "background-color:#f9fbff;",
                                          
                                          h2("parameters"),
                                          
                                          selectInput('target', 'Find me this kind of wine',
                                                      list("Red, many reviews" = c("Cabernet Sauvignon", "Pinot Noir", "Sangiovese","Malbec","Red Blend"), 
                                                           "White, many reviews" = c("Chardonnay","Riesling","Sauvignon Blanc","White Blend"),
                                                           "Red, fewer reviews" = c("Aglianico","Cabernet Franc","Grenache","Merlot","Nebbiolo","Rhône-style Red Blend","Syrah","Tempranillo","Zinfandel"),
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
                                                           "Red, fewer reviews" = c("Aglianico","Cabernet Franc","Grenache","Merlot","Nebbiolo","Rhône-style Red Blend","Syrah","Tempranillo","Zinfandel"),
                                                           "White, fewer reviews"= c("Bordeaux-style White Blend","Garganega","Gewürztraminer","Grüner Veltliner","Pinot Grigio","Pinot Gris","Vernaccia","Viognier")), selected = "Pinot Noir"),
                                          
                                          br(),
                                          
                                          sliderInput('dollars', 'Max Wine Price', 
                                                      min=15, max=300,
                                                      value=300, 
                                                      step=5, round=0)
                                   ),
                                   
                                   column(8,
                                          
                                          h4("How do our selected wines relate to each wine variety?"),
                                          
                                          plotOutput('plot_active')
                                          
                                   )
                                   
                                 ),
                                 
                                 hr(),
                                 
                                 fluidRow(
                                   
                                   
                                   column(12,
                                          
                                          h2("the wines"),
                                          
                                          tableOutput('plot_header')          
                                   )
                                   
                                 )
                                 
                                 
                         ),
                         
                         # Second tab content
                         tabItem(tabName = "review",

                                 fluidRow(column(12, 
                                   h2("dream up a wine review to find wines just like it"),
                                   textInput('neural_input', label=NULL, value = "a rich buttery chard with overwhelming peach flavor", width = 700, placeholder = NULL)
                                 )),
                                 
                                 fluidRow(column(12,
                                   h2("similar wines"),
                                  DT::dataTableOutput('plot_neural')          
                                 ))
                                 
                         )
                       )
                     )
                     
)

server <- function(input, output) { 
  
  clean_review<-function(x){
    x<-paste(unlist(x), collapse =" ")
    x<-tolower(x)
    x<-gsub("-", " ", x)
    x<-gsub("-", " ", x)
    x<-gsub('"', " ", x)
    x<-gsub('"', " ", x)
    x<-removePunctuation(x)%>%strsplit(" ")
    return(x)
  }
  
  
  dreamWineRev<-function(x,model,wine_model){
    
    dream_wine_review = c(x)
    
    cl_rev = unlist(clean_review(dream_wine_review))
    bi_rev = list()
    for (ii in 1:length(cl_rev)-1){
      bi_rev[ii] <-paste(cl_rev[ii],cl_rev[ii+1],sep = '_') 
    }
    tri_rev = list()
    for (ii in 1:length(bi_rev)-1){
      tri_rev[ii] <-paste(bi_rev[ii],cl_rev[ii+2],sep = '_') 
    }
    
    dream_review_ngram<-c(cl_rev,bi_rev,tri_rev,recursive=TRUE)
    
    neural_model<-model[[dream_review_ngram]]
    
    xxx=closest_to(wine_model,neural_model, n=52500, fancy_names=TRUE)
    colnames(xxx)<-c("Wine Name","Similarity (1=perfect, 0=awful)")
    return(xxx) 
  }
  
  
  N = 6
  
  sub_wine_distances <- wine_distances
  
  sub_wine_distances$price <- as.numeric(
    gsub('[$,]','',as.character(sub_wine_distances$price)))
  
  
  
  # Combine the selected variables into a new data frame
  targetWines <- reactive({
    
    subset(sub_wine_distances, Variety %in% input$target)
    
  })
  
  targetWinesByPrice <- reactive({
    target1 <- targetWines()
    subset(target1, price < as.numeric(input$dollars))
    
  })
  
  wineOrderIndex <- reactive({
    
    target2<-targetWinesByPrice()
    
    logic_var <- switch(input$rb,
                        "similarTo" = FALSE,
                        "differentFrom" = TRUE)
    
    order(target2[input$comparison], decreasing = logic_var)[1:N]
    
  })
  
  
  output$plot_active <- renderPlot({
    
    ndx <-wineOrderIndex()
    fully_subsetted_wines <- targetWinesByPrice()
    
    ggplot(data = pc_wineload)+ 
      #scale_color_manual(name = 'Wines:', breaks = c('Red', 'White'), values=c("#e83544", "#E69F00")) +
      geom_text_repel(aes(x = PC1, y = PC2, label= Group.1, color = Group.2),fontface = "bold", size = 3.5, 
                      # Add extra padding around each text label.
                      box.padding = unit(.2, 'lines'),
                      # Add extra padding around each data point.
                      point.padding = unit(.2, 'lines'),
                      # Color of the line segments.
                      segment.color = '#100000',
                      # Width of the line segments.
                      segment.size = 0,
                      # Draw an arrow from the label to the data point.
                      arrow = arrow(length = unit(0.01, 'npc')),
                      # Strength of the repulsion force.
                      force = 0.2,
                      # Maximum iterations of the naive repulsion algorithm O(n^2).
                      max.iter = 1e3
      )+ 
      geom_point(data=fully_subsetted_wines[ndx,], 
      aes(x = PC1, y = PC2, colour = 'Selected') , size = 5, shape=13)+
      scale_color_manual(name = 'Wines:', breaks = c('Red', 'Selected','White'), values=c("#e83544", "#56B4E9", "#E69F00"))+theme_minimal()+ theme(legend.justification=c(0.9,0.1)) #guides(color=FALSE)
    
  })
  
  
  output$plot_header <- renderTable({
    
    ndx <-wineOrderIndex()
    fully_subsetted_wines <- targetWinesByPrice()
    select_wines<-fully_subsetted_wines[,c(3,5,4,7,11,12)]
    colnames(select_wines)<-c("Vinyard","Name","Year","Price","Alc %","Variety")
    head(select_wines[ndx,],10)
    
  })
  
  
  # render HTML introduction/overivew for first tab
  getPage<-function() {
    return(includeHTML("www/include2.html"))
  }
  
  output$inc<-renderUI({getPage()})
  
  
  # clean the 'dream review', n-gram it, and compare it to all reviews for last tab
  prepDataTable <- reactive({
    xxx<-dreamWineRev(x=c(input$neural_input),model=model,wine_model = wine_model)
    return(xxx)
  })
  
  output$plot_neural <- renderDataTable({
    tempTable<-prepDataTable()
    DT::datatable(tempTable)
    })
  
  
}

shinyApp(ui, server)


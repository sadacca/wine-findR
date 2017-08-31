
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(ggrepel)
load("data/app_data.RData")

shinyServer(function(input, output, session) {
  

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
  
  
  output$plot_dummy <- renderPlot({

    ggplot(pc_wineload, aes(x = PC1, y = PC2, label= Group.1))+ 
      geom_point(color = 'orange')+
      geom_label_repel(label.size =NA, fontface = "bold", size = 3)
  })
  
  output$plot_active <- renderPlot({
    
    ndx <-wineOrderIndex()
    fully_subsetted_wines <- targetWinesByPrice()
    
    ggplot(data=pc_wineload[1:30])+
      geom_point(aes(x = PC1, y = PC2, colour = Group.2))+
      geom_point(data=fully_subsetted_wines[ndx,], 
                 aes(x = PC1, y = PC2, colour = 'Selected Wines'))
    
  })
  
  
  output$plot_header <- renderTable({
    
    ndx <-wineOrderIndex()
    fully_subsetted_wines <- targetWinesByPrice()
    
    head(fully_subsetted_wines[ndx,c(3,5,4,7,11,12)])
    
  })
  
  

})

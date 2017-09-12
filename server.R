
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
  
  sub_wine_distances$Price <- as.numeric(
    gsub('[$,]','',as.character(sub_wine_distances$Price)))
  
  
  
  # Combine the selected variables into a new data frame
  targetWines <- reactive({
    
    subset(sub_wine_distances, Variety %in% input$target)
    
  })
  
  targetWinesByPrice <- reactive({
    target1 <- targetWines()
    subset(target1, Price < as.numeric(input$dollars))
    
  })
  
  wineOrderIndex <- reactive({
    
    target2<-targetWinesByPrice()
    
    logic_var <- switch(input$rb,
           "similarTo" = FALSE,
           "differentFrom" = TRUE)
  
    order(target2[input$comparison], decreasing = logic_var)[1:N]
    
  })
  
  
  output$plot_dummy <- renderPlot({

    ggplot(pc_wineload, aes(x = PC1, y = PC2, label= Group.1, color = Group.2))+ 
      geom_point()+
      scale_color_manual(name = 'Wines:', breaks = c('Red', 'White'), values=c("#e83544", "#E69F00")) +
      geom_label_repel(label.size =NA, fontface = "bold", size = 3, 
                       # Add extra padding around each text label.
                       box.padding = unit(.5, 'lines'),
                       # Add extra padding around each data point.
                       point.padding = unit(1, 'lines'),
                       # Color of the line segments.
                       segment.color = '#000000',
                       # Width of the line segments.
                       segment.size = 0.5,
                       # Draw an arrow from the label to the data point.
                       arrow = arrow(length = unit(0.01, 'npc')),
                       # Strength of the repulsion force.
                       force = 2,
                       # Maximum iterations of the naive repulsion algorithm O(n^2).
                       max.iter = 3e4
                       ) + theme(legend.justification=c(0.9,0.1), legend.position=FALSE)
  })
  
  output$plot_active <- renderPlot({
    
    ndx <-wineOrderIndex()
    fully_subsetted_wines <- targetWinesByPrice()
    
    ggplot(data=pc_wineload[1:30])+
      geom_point(aes(x = PC1, y = PC2, colour = Group.2), size = 4, alpha = 1/2)+
      geom_point(data=fully_subsetted_wines[ndx,], 
                 aes(x = PC1, y = PC2, colour = 'Selected') , size = 6, shape=13)+
    
    scale_color_manual(name = 'Wines:', breaks = c('Red', 'Selected','White'), values=c("#e83544", "#56B4E9", "#E69F00")) +
      theme(legend.justification=c(0.9,0.1), legend.position=c(0.9,0.1))
    
  })
  
  
  output$plot_header <- renderTable({
    
    ndx <-wineOrderIndex()
    fully_subsetted_wines <- targetWinesByPrice()
    
    head(fully_subsetted_wines[ndx,c(3,5,4,7,11,12)])
    
  })
  
  

})

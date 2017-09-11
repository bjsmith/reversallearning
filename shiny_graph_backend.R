library(ggplot2)
library(shiny)
library(plotly)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Reversal learning behavior"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      numericInput("minPropCorrect", "Filter by minimum accuracy",value=0,min=0,max=1,step=0.1),
      numericInput("maxPropCorrect", "Filter by maximum accuracy",value=1,min=0,max=1,step=0.1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotlyOutput(outputId = "myPlot")
      
    )
  )
)
#install.packages("ggplot2movies")
#library(ggplot2movies) # movies is no longer contained within ggplot2 https://cran.r-project.org/web/packages/ggplot2movies/index.html

server<-function(input, output) {
  
  output$myPlot <- renderPlotly({
    
    filtered.ds<-accuracy.by.pres_seg.subid[!is.na(presentation_n_over_segments) & final.prop.correct>=input$minPropCorrect & final.prop.correct <=input$maxPropCorrect]
    main.prop.cor.ggplot<-
      ggplot(filtered.ds,
             aes(x=presentation_n_over_segments,y=prop.correct,group=subid))+
      geom_line(aes(colour=final.prop.correct),size=1.5,alpha=0.3)+ scale_colour_gradientn(colours=c("red","green","blue","violet"))+
      #scale_x_continuous(breaks=-8:4,labels=break.labels)+
      labs(#x="Presentation",
        y="Proportion correct across all images by user",
        title=paste0("proportion correct across all images by user\n from start to finish of reversal learning"))+
      geom_smooth(group=1,color="black",span=1)+
      #geom_line(data=accuracy.by.pres_seg.subid.summary,aes(x=presentation_n_over_segments,y=prop.correct.m,group=NULL))+
      facet_grid(Motivation ~ .)+
      #theme(strip.text.y=element_text(colour="orange"))+
      geom_vline(aes(xintercept=8),linetype=2)+
      reversal_learning_timeline_ggplot_commands+
      geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==5],
                 aes(yintercept = prop.correct.m),
                 linetype=2)+
      geom_hline(data=accuracy.by.pres_seg.subid.summary[presentation_n_over_segments==13],
                 aes(yintercept = prop.correct.m),
                 linetype=2)
    
    
    ggplotly(main.prop.cor.ggplot)
    
  })
}

shinyApp(ui, server)
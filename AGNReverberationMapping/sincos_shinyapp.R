library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  # App title 
  titlePanel("Cross Correlation"),
  
  #Sidebar layout with input and output
  sidebarLayout(
    
    #sidebar panel for inputs
    sidebarPanel(
      
      #Input: Slider for tau
      sliderInput(inputId="tau",
                  label="shift (radians):",
                  min=0,
                  max=2*pi,
                  value=0)
      
    ),
    
    #Main panel for displaying outputs
    mainPanel(
      plotOutput("sinPlot")
    )
    
    
  )
  
)

## Server

# Define a server function for the Shiny app

server <- function(input,output){
  
  output$sinPlot <- renderPlot({
    #plotting sin(x+tau)
    #plotting cos(x)
    
    x=seq(-2*pi,2*pi,length.out=30)
    y1=sin(x-input$tau)
    y2=cos(x)
    
    ggplot()+
      geom_line(aes(x,y1),colour='red')+
      geom_line(aes(x,y2),colour='blue')
      
    
    
    #plot(x,sin(x-input$tau),ylabel="",type='l')
  })
}

shinyApp(ui,server)





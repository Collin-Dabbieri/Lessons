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
                  label="lag:",
                  min=0,
                  max=6.28,
                  value=0)
      
    ),
    
    #Main panel for displaying outputs
    mainPanel(
      fluidRow(
        column(6,plotOutput("sinPlot",width="250px",height="300px")),
        column(6,plotOutput("CCF",width="250px",height="300px"))
      )

    )
    
    
  )
  
)

## Server

# Define a server function for the Shiny app

server <- function(input,output){
  
  output$sinPlot <- renderPlot({
    #plotting sin(x+tau)
    #plotting cos(x)
    
    x=seq(-2*pi,2*pi,length.out=50)
    y1=sin(x-input$tau)
    y2=cos(x)
    
    ggplot()+
      geom_line(aes(x,y1),colour='red')+
      geom_line(aes(x,y2),colour='blue')

  })
  
  output$CCF <-renderPlot({
    
    x=seq(-2*pi,2*pi,length.out=50)
    
    vals=ccf(sin(x),cos(x),plot=F)
    
    ccfs=vals$acf
    lags=vals$lag
    
    t=input$tau
    
    ccf_interp=approx(x=lags,y=ccfs,xout=t)
    
    
    
    plot(c(t,t),c(0,ccf_interp$y),
         type='l',
         xlab="lag",
         ylab="CCF",
         xlim=c(0,2*pi),
         ylim=c(-1,1)
         )
    #lines(lags,ccfs)
    lines(c(-2*pi,2*pi),c(0,0))
    
    
  })
}

shinyApp(ui,server)





library(shiny)
library(ggplot2)


Continuum=read.table("Continuum.txt")
colnames(Continuum)=c("HJD","Obs","Flux","eFlux")
Emission=read.table("Emission.txt")
colnames(Emission)=c("HJD","FHb","eFHb","FHeII","eFHeII")
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
xL=seq(min(Emission$HJD),max(Emission$HJD),length.out=length(Emission$HJD))
xC=seq(min(Continuum$HJD),max(Continuum$HJD),length.out=length(Continuum$HJD))
temp=approx(Emission$HJD,normalize(Emission$FHb),xL)
FHb_interp=temp$y
temp=approx(Continuum$HJD,normalize(Continuum$Flux),xC)
Fc_interp=temp$y


ui <- fluidPage(
  
  # App title 
  titlePanel("Variable Lag"),
  
  #Sidebar layout with input and output
  sidebarLayout(
    
    #sidebar panel for inputs
    sidebarPanel(
      
      #Input: Slider for tau
      sliderInput(inputId="tau",
                  label="lag:",
                  min=0,
                  max=60,
                  value=0)
      
    ),
    
    #Main panel for displaying outputs
    mainPanel(
      plotOutput("lagPlot")
      
    )
    
    
  )
  
)

## Server

# Define a server function for the Shiny app

server <- function(input,output){
  
  output$lagPlot <- renderPlot({
    #plotting sin(x+tau)
    #plotting cos(x)
    
    xC_plot=xC+input$tau
    
    ggplot()+
      geom_line(aes(xL,FHb_interp),colour='red')+
      geom_line(aes(xC_plot,Fc_interp),colour='blue')
    
  })
  

}

shinyApp(ui,server)
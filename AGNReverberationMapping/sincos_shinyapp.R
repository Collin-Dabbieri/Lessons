library(shiny)
library(ggplot2)


CCF<-function(C,L,x,lagmax){
  
  num_lags=lagmax #moving in intervals of 1
  N=length(C)
  
  sigmaC=sd(C)
  sigmaL=sd(L)
  Cbar=mean(C)
  Lbar=mean(L)
  
  ccf_out=c()
  lags=c()
  
  
  for(tau in 1:lagmax){
    
    
    count=0 #for normalization
    sum=0
    
    for(i in lagmax:N){ #we have to cut off early values so C(t_i-tau) makes sense
      
      count=count+1
      
      val=(L[i]-Lbar)*(C[i-tau]-Cbar)/(sigmaC*sigmaL) #discrete ccf for one i value
      
      sum=sum+val
    }
    
    ccf_out=append(ccf_out,sum/count)
    lags=append(lags,x[tau]-x[1])
    
  }
  
  lags=head(lags,-1)
  
  return(list(lags=lags,CCF=ccf_out))
  
}

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
    
    obj=CCF(sin(x),cos(x),x,40)
    
    ccf=obj$CCF
    lags=obj$lags
    t=input$tau
    
    ccf_interp=approx(x=lags,y=ccf,xout=t)
    
    plot(lags,ccf,type='l',xlab='lag (radians)',ylab='CCF',xlim=c(0,2*pi),ylim=c(-1,1))
    lines(c(t,t),c(0,ccf_interp$y))
    lines(c(0,2*pi),c(0,0))
    
    
  })
}

shinyApp(ui,server)





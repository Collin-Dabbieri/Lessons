library(shiny)
library(ggplot2)


Continuum=read.table("Mrk335_Continuum.txt")
colnames(Continuum)=c("HJD","Flux")
Emission=read.table("Mrk335_Emission.txt")
colnames(Emission)=c("HJD","FHb")
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
xout=seq(min(Emission$HJD),max(Emission$HJD),length.out=length(Emission$HJD))

temp=approx(Emission$HJD,Emission$FHb,xout)
temp=temp$y
FHb_interp=normalize(temp)
temp=approx(Continuum$HJD,Continuum$Flux,xout)
temp=temp$y
Fc_interp=normalize(temp)

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
    
    for(i in (tau+1):N){ #we have to cut off early values so C(t_i-tau) makes sense
      
      count=count+1
      
      val=(L[i]-Lbar)*(C[i-tau]-Cbar)/(sigmaC*sigmaL) #discrete ccf for one i value
      
      sum=sum+val
    }
    
    
    ccf_out=append(ccf_out,sum/count)
    lags=append(lags,x[tau]-x[1])
    
  }
  
  
  return(list(lags=lags,CCF=ccf_out))
  
}


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
                  max=40,
                  value=0)
      
    ),
    
    #Main panel for displaying outputs
    mainPanel(
      fluidRow(
        column(6,plotOutput("lagPlot",width="250px",height="300px")),
        column(6,plotOutput("CCF",width="250px",height="300px"))
      )

      
    )
    
    
  )
  
)

## Server

# Define a server function for the Shiny app

server <- function(input,output){
  
  output$lagPlot <- renderPlot({
    #plotting sin(x+tau)
    #plotting cos(x)
    
    xC_plot=xout+input$tau
    
    ggplot()+
      geom_line(aes(xout,FHb_interp),colour='red')+
      geom_line(aes(xC_plot,Fc_interp),colour='blue')+
      xlab("HJD")+
      ylab("Flux (normalized)")
    
  })
  
  output$CCF <- renderPlot({
    obj=CCF(Fc_interp,FHb_interp,xout,lagmax=40)
    ccf=obj$CCF
    lags=obj$lags
    t=input$tau
    
    ccf_interp=approx(x=lags,y=ccf,xout=t)
    
    plot(lags,ccf,type='l',xlab='lag (days)',ylab='CCF',xlim=c(0,40),ylim=c(min(ccf),1))
    lines(c(t,t),c(0,ccf_interp$y))
    lines(c(0,40),c(0,0))
  })
  

}

shinyApp(ui,server)
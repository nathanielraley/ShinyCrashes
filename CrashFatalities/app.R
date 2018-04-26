#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


ui <- fluidPage(
    
    #fluid page for dynamically adapting to screens of different resolutions.
  titlePanel("Car Crashes in Allegheny County (2004-2016)"),
    h3("Fatalities and Major Injuries"),
    sidebarLayout(
      sidebarPanel(
      
        selectInput("belt", 
                    label = "Wearing seatbelt?",
                    choices = list("Yes", 
                                   "No"),
                    selected = "Yes"),
        hr(),
       
        selectInput("alc", 
                    label = "Drunk driving related?",
                    choices = list("Yes", 
                                   "No"),
                    selected = "Yes"),
        hr(),
        #selectInput("type", 
        #            label = "Type of Collision",
        #            choices = list("Front", 
        #                           "Rear",
        #                           "Angle"),
        #            selected = "Front")
        #slider input for bins of histogram
        sliderInput("speed",
                   "Speed:",
                    min = 30,
                    max = 65,
                    value = 50),
        hr(),
        # Show a plot of the generated distribution
        helpText("Note. Speed limit is being used as a proxy for collision speed among accidents coded as being speed-related."),
helpText("The data dictionary is slightly unclear about variable definitions; please interpret these predictions generously."),
helpText("Bands show 95% CIs (normal theory); N=85483."),
        hr(),
        helpText("Source: \ncatalog.data.gov/dataset/allegheny-county-crash-data")
      ),
      mainPanel(
        plotOutput("logisticPlot"),
        hr(),
        column(12,h4("Probability of At Least One Death or Major Injury by Collision Type")),
        column(4,verbatimTextOutput("value1")),
        column(4,verbatimTextOutput("value2")),
        column(4,verbatimTextOutput("value3"))
      )
    )
  )


########writing server function
server<-shinyServer(function(input, output) {
  
  v <- reactiveValues(data = NULL)
  
  load('fit1')
  
  newdat<-data.frame(SPEED_LIMIT=seq(25,65,length.out = 1000),
                     SPEEDING_RELATED=rep(1,1000),
                     COLLISION_TYPE=rep(c('Rear','Front','Side'),1000),
                     DRINKING_DRIVER=rep(0,1000),
                     UNBELTED=rep(1,1000))
 observe({    
  if(input$alc=="Yes"){
    v$DRINKING_DRIVER=rep(1,1000)}
  
  if(input$alc=="No"){
    v$DRINKING_DRIVER=rep(0,1000)}

  if(input$belt=="Yes"){
    v$UNBELTED=rep(0,1000)
  }
  if(input$belt=="No"){
    v$UNBELTED=rep(1,1000)
  }
#  if(input$type=="Front"){
#    v$COLLISION_TYPE=rep("Front",1000)}
#  if(input$type=="Rear"){
#    v$COLLISION_TYPE=rep("Rear",1000)}
#  if(input$type=="Angle"){
#  v$COLLISION_TYPE=rep("Side",1000)}
 })
 
  #referring output distPlot in ui.r as output$distPlot
output$logisticPlot <- renderPlot({
 
   # if (is.null(v$COLLISION_TYPE)) return()
    
    #newdat$COLLISION_TYPE=v$COLLISION_TYPE
    newdat$DRINKING_DRIVER=v$DRINKING_DRIVER
    newdat$UNBELTED=v$UNBELTED  
    #referring input bins in ui.r as input$bins

    yhat<-predict(fit1,newdata = newdat,type = "link",se=T)
    newdat<-data.frame(newdat,yhat)
    
    std <- qnorm(0.95 / 2 + 0.5)
    newdat$ymin <- fit1$family$linkinv(newdat$fit - std * newdat$se)
    newdat$ymax <- fit1$family$linkinv(newdat$fit + std * newdat$se)
    newdat$fit <- fit1$family$linkinv(newdat$fit)
    
    
    new1<-predict(fit1, data.frame(SPEED_LIMIT=rep(input$speed,3),
                                   SPEEDING_RELATED=rep(1,3),
                                   COLLISION_TYPE=rep(c('Rear','Front','Side'),1),
                                   DRINKING_DRIVER=rep(v$DRINKING_DRIVER,3),
                                   UNBELTED=rep(v$UNBELTED,3)),type="response")
    
    ggplot(data=newdat,aes(y=fit,x=SPEED_LIMIT,fill=factor(COLLISION_TYPE,levels=c("Front","Side","Rear"))))+geom_ribbon(aes(ymin=ymin, ymax=ymax 
                                   ), alpha=0.5) + scale_y_continuous(limits=c(0,.70),labels = scales::percent,breaks=seq(0,1,.1)) +
      geom_line(data=newdat, aes(y=fit)) + theme(legend.title=element_blank())+
      geom_segment(aes(x = input$speed, xend=input$speed,y=0,yend=new1),linetype="dotted",size=.5,color="gray")+
      geom_segment(aes(x = 25, xend=input$speed,y=new1,yend=new1),lineend = "round",linetype="dotted",size=.5,color="darkgray")+
      labs(x="Speed Limit", y="Fatality or Major Injury")+theme_minimal()+theme(legend.title=element_blank(),axis.title=element_text(size=16,face="bold"),axis.text=element_text(size=14))
     })  

output$value1 <- renderText({
  new1<-predict(fit1, data.frame(SPEED_LIMIT=rep(input$speed,3),
                                                           SPEEDING_RELATED=rep(1,3),
                                                           COLLISION_TYPE=rep(c('Rear','Front','Side'),1),
                                                           DRINKING_DRIVER=rep(v$DRINKING_DRIVER,3),
                                                           UNBELTED=rep(v$UNBELTED,3)),type="response")
paste("Front Collision:\n", round(100*new1[2],1), "%",sep="")
})

output$value2 <- renderText({
  new1<-predict(fit1, data.frame(SPEED_LIMIT=rep(input$speed,3),
                                 SPEEDING_RELATED=rep(1,3),
                                 COLLISION_TYPE=rep(c('Rear','Front','Side'),1),
                                 DRINKING_DRIVER=rep(v$DRINKING_DRIVER,3),
                                 UNBELTED=rep(v$UNBELTED,3)),type="response")
  paste("Side Collision: \n", round(100*new1[3],1), "%",sep="")
})

output$value3 <- renderText({
  new1<-predict(fit1, data.frame(SPEED_LIMIT=rep(input$speed,3),
                                 SPEEDING_RELATED=rep(1,3),
                                 COLLISION_TYPE=rep(c('Rear','Front','Side'),1),
                                 DRINKING_DRIVER=rep(v$DRINKING_DRIVER,3),
                                 UNBELTED=rep(v$UNBELTED,3)),type="response")
  paste("Rear Collision: \n", round(100*new1[1],1), "%",sep="")
})

})

# Run the application 
shinyApp(ui = ui, server = server)


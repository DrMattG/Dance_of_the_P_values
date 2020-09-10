
library(tidyverse)
library(effectsize)
library(shiny)

shinyApp(
  
  # Define UI ----
  
  ui <- fluidPage(
    
    
    # App title ----
    titlePanel("It's the effect size that matters"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        sliderInput("N", label = "Sample size",
                    value = 50,
                    min=0,
                    max=100),
        sliderInput("mn_treatment", label = "Treatment mean",
                    value = 30,
                    min=0,
                    max=100),
        sliderInput("mn_control", label = "Control mean",
                    value = 20,
                    min=0,
                    max=100),
        sliderInput("SD", label = "Standard deviation",
                    min = 0, max = 100, value = 15)
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Tabset ----
        tabsetPanel(type = "tabs",
                    tabPanel("What is an effect size", plotOutput("plot")),
                    tabPanel("Why not just a p value", 
                             inputPanel(sliderInput("simstep", "simstep", 1, 100, 1, step = 1, 
                                                    animate=animationOptions(interval=1000, loop = T,
                                                                             playButton = "play", pauseButton = "pause"),)
                             ),
                             plotOutput("aniplot"))
        )
        
      )
    )
  ),
  
  # Define server  ----
  server <- function(input, output) {
    
    # Reactive expression to generate data ----
    
    d <- reactive({
      data.frame("Treatment"=round(rnorm(as.numeric(input$N),
                                         as.numeric(input$mn_treatment),
                                         as.numeric(input$SD))),
                 "Control"=round(rnorm(as.numeric(input$N),
                                       as.numeric(input$mn_control),
                                       as.numeric(input$SD))))
    })
    
    # Generate a plot of the data ----
    
    output$plot <- renderPlot({
      tmean=mean(d()$Treatment)
      cmean=mean(d()$Control)
      mn=data.frame(tmean=tmean, cmean=cmean)
      mn=mn %>% gather()
      ef=effectsize::hedges_g(as.numeric(d()$Treatment),as.numeric(d()$Control))
      p<-d() %>% gather() %>% 
        ggplot(aes(value, fill=key))+
        geom_density(alpha=0.8)+
        geom_vline(xintercept = tmean, lty=2)+
        geom_vline(xintercept = cmean,lty=2)+
        geom_point(data=mn,aes(value,0.1))+
        geom_line(data=mn, aes(value,0.1, group=1)) +
        ggthemes::theme_base()+
        theme(legend.position="none")
      p+annotate(geom="text", x=30, y=0.12, label=paste0("Hedges g: ", abs(round(ef[1],digits = 2))),
                 color="red")
    })
    
    # Generate dance of the p values ----
    aniGraph <- reactive({
      ef=effectsize::hedges_g(as.numeric(d()$Treatment),as.numeric(d()$Control))
      p<-matrix()
      for(i in 1:100){#as.numeric(input$nSims)){ #for each simulated experiment
        x<-rnorm(n = as.numeric(input$N), mean = 0, sd = 1) #produce N simulated participants
        y<-rnorm(n = as.numeric(input$N), mean = as.numeric(ef), sd = 1) #produce N simulated participants
        z<-t.test(x,y) #perform the t-test
        p[i]<-unlist(z[3])#get the p value
      }
      p<-as.data.frame(p)
      p$simulation<-seq(1:nrow(p))
      p_sim_sub <- p[p$simulation==input$simstep,]
      p_sim_sub %>% 
        ggplot(aes(p,simulation))+
        geom_point(colour="blue", size=3)+
        theme_classic()+
        xlim(0,1)+
        geom_vline(xintercept = 0.05, colour="red")+
        ggtitle(paste0("Hedges d = ", abs(round(ef[1],digits = 2)), "Power = ", sum(p$p<0.05)/100))
      #+
      #   gganimate::transition_reveal(p_sim$simulation, keep_last = FALSE)
    }) 
    
    # Show the graph
    output$aniplot <- renderPlot({
      aniGraph()
    })
    
    
  }
  
)



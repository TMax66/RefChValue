server<-shinyServer(function(input, output, session) {
  
  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath, header = TRUE, sep = ""
                   )
    return(df)
  })
  
  output$dati<- renderTable({
    head(data())
  })
  
  mod<-reactive({
    df<-data()
    mod = lmer(y ~ (1|id/prelievo), data=df)
    
  })
  
  
  output$rcv<-renderTable({  
    V<-tidy(mod())
    V1<-V %>%
      dplyr::select(estimate) %>%
      mutate('CV%'=100*(estimate/estimate[1]))%>%
      slice(2:4) %>% 
      rename('SD'=estimate) %>% 
      mutate("Variance Component"=c("Analytic Variability", "Inter-Individual Variability", "Intra-Individual Variability")) %>%
      dplyr::select("Variance Component",SD,'CV%')
    
  })
    
    output$rcv2<-renderTable({
      V<-tidy(mod()) %>% 
        dplyr::select(estimate) %>%
        mutate('CV%'=100*(estimate/estimate[1]))
        CVi<-V$'CV%'[4]
        CVg<-V$'CV%'[3]
        CVa<-V$'CV%'[2]
        Ioi<-((CVi^2+CVa^2)^0.5)/CVg
        RCV<-(1.96*2^0.5)*(CVi^2+CVa^2)^0.5
        x<-tibble("Index of individuality"=Ioi,"Reference Change Value"=RCV)
    })
    
    
 } )
  

  
  
  
  

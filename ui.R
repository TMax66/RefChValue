
ui <- fluidPage(
  titlePanel("References Change Values Calculator"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Carica file',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                tableOutput('dati')
                 
             ),
             mainPanel(
               tableOutput("rcv"), 
               
               tableOutput("rcv2")
               
               
             )
    )
  )



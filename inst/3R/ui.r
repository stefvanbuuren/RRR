
shinyUI(pageWithSidebar(
    headerPanel("TNO 3R Power Calculator"),
    
    sidebarPanel(
        selectInput(inputId = "disease",
                    label = "Disease model",
                    choices = c("Lung fibrosis" = "fibrosis",
                                "Another model"  = "another")
        ),
        
        conditionalPanel("input.disease == 'fibrosis'",
                        wellPanel(
                             selectInput(inputId = "outcome",
                                         label = "Outcome",
                                         choices = c("Histological fibrosis" = "hist",
                                                     "Collagen content" = "cola")
                             ),
                                                      
                         conditionalPanel(
                             condition = "input.outcome == 'hist'",
                                 numericInput("hist.pbs.mu",       "Mean PBS:       ", 0.64),
                                 numericInput("hist.pbs.sd",       "SD   PBS:       ", 0.93),
                                 numericInput("hist.bleomycin.mu", "Mean bleomycine:", 3.86),
                                 numericInput("hist.bleomycin.sd", "SD   bleomycine:", 1.49),
                                 numericInput("hist.compound.mu",  "Mean compound:  ", 2.65),
                                 numericInput("hist.compound.sd",  "SD   compound:  ", 1.34)
                             )
                         ,
                         
                         conditionalPanel(
                             condition = "input.outcome == 'cola'",
                                 numericInput("cola.pbs.mu",       "Mean PBS:       ", 308),
                                 numericInput("cola.pbs.sd",       "SD   PBS:       ", 81),
                                 numericInput("cola.bleomycin.mu", "Mean bleomycine:", 573),
                                 numericInput("cola.bleomycin.sd", "SD   bleomycine:", 210),
                                 numericInput("cola.compound.mu",  "Mean compound:  ", 469),
                                 numericInput("cola.compound.sd",  "SD   compound:  ", 169)
                             ))
                         ,
                         
                         wellPanel(
                             radioButtons(inputId = "comparison",
                                          label = "Comparison",
                                          choices = c("Bleomycin vs PBS" = "PBS",
                                                      "Bleomycin vs compound" = "compound"))
                             ,
                             
                             radioButtons(inputId = "scale",
                                          label = "Scale",
                                          choices = c("Absolute" = "absolute",
                                                      "Relative" = "relative"))
                             ,        
                             
                             selectInput(inputId = "ncompound",
                                         label = "Number of compound groups:",
                                         choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         selected = 3))
        )
        ,
        
        wellPanel(
            radioButtons(inputId = "alpha",
                         label = "Alpha",
                         choices = c("0.05" = "0.05",
                                     "0.01" = "0.01")),
            
            radioButtons(inputId = "power",
                         label = "Power",
                         choices = c("80%" = "80%",
                                     "90%" = "90%"))
        )
        ,
        
        downloadLink('downloadPdf', 'Download PDF')
    )
    ,
    
    # Show a plot of the generated distribution
    #     mainPanel(
    #         includeHTML(file.path(getwd(),"svgfiller.js")),
    #         reactiveSvg(outputId = "mainplot")
    #     )
    #     
    
    mainPanel(
        tabsetPanel(
            tabPanel("Plot", plotOutput("mainplot", height = "700px")), 
            tabPanel("Table", tableOutput("table")),
            tabPanel("Summary", verbatimTextOutput("summary"))
        )
    )
)
)



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
                                 numericInput("hist.induced.mu", "Mean induced: ", 3.86),
                                 numericInput("hist.induced.sd", "SD   induced: ", 1.49),
                                 numericInput("hist.control.mu", "Mean control: ", 0.64),
                                 numericInput("hist.control.sd", "SD   control: ", 0.93),
                                 numericInput("hist.treated.mu", "Mean treated: ", 2.65),
                                 numericInput("hist.treated.sd", "SD   treated: ", 1.34)
                             )
                             ,
                             
                             conditionalPanel(
                                 condition = "input.outcome == 'cola'",
                                 numericInput("cola.induced.mu", "Mean induced: ", 573),
                                 numericInput("cola.induced.sd", "SD   induced: ", 210),
                                 numericInput("cola.control.mu", "Mean control: ", 308),
                                 numericInput("cola.control.sd", "SD   control: ", 81),
                                 numericInput("cola.treated.mu", "Mean treated: ", 469),
                                 numericInput("cola.treated.sd", "SD   treated: ", 169)
                             ))
                         ,
                         
                         wellPanel(
#                              radioButtons(inputId = "comparison",
#                                           label = "Comparison",
#                                           choices = c("Induced vs control" = "control",
#                                                       "Induced vs treated" = "treated"))
#                              ,
                             
                             radioButtons(inputId = "scale",
                                          label = "Scale",
                                          choices = c("Absolute" = "absolute",
                                                      "Log" = "log"))
                             ,        
                             
                             selectInput(inputId = "ntreated",
                                         label = "Number of treated groups:",
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
                         choices = c("50%" = "50%",
                                     "80%" = "80%",
                                     "90%" = "90%"),
                         selected = "80%")
        )
        ,
        
        downloadLink('downloadPdf', 'Download PDF')
    )
    ,
        
    mainPanel(
        tabsetPanel(
            tabPanel("Table", tableOutput("table")),
            tabPanel("Plot", plotOutput("mainplot", height = "700px")), 
            tabPanel("Summary", verbatimTextOutput("summary"))
        )
    )
)
)


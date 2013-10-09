
shinyUI(pageWithSidebar(
    headerPanel("TNO 3R Power Calculator"),
    
    sidebarPanel(
        selectInput(inputId = "disease",
                    label = "Disease model",
                    choices = c("Lung fibrosis" = "fibrosis",
                                "Other model"  = "other")
        ),
        
        conditionalPanel(
            condition = "input.disease == 'fibrosis'",
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
                    selectInput(inputId = "hist.treated.pct",
                                label = "Percent reduction: ",
                                choices = seq(0, 100, 10),
                                selected = "30")
                ),
                
                conditionalPanel(
                    condition = "input.outcome == 'cola'",
                    numericInput("cola.induced.mu", "Mean induced: ", 573),
                    numericInput("cola.induced.sd", "SD   induced: ", 210),
                    numericInput("cola.control.mu", "Mean control: ", 308),
                    numericInput("cola.control.sd", "SD   control: ", 81),
                    selectInput(inputId = "cola.treated.pct",
                                label = "Percent reduction: ",
                                choices = seq(0, 100, 10),
                                selected = "50")
                )
            )
        ),
        
        conditionalPanel(
            condition = "input.disease == 'other'",
            wellPanel(
                selectInput(inputId = "otheroutcome",
                            label = "Outcome",
                            choices = c("Outcome 1" = "out1",
                                        "Outcome 2" = "out2")                
                ),
                
                conditionalPanel(
                    condition = "input.otheroutcome == 'out1'",
                    numericInput("out1.induced.mu", "Mean induced: ", 1),
                    numericInput("out1.induced.sd", "SD   induced: ", 1),
                    numericInput("out1.control.mu", "Mean control: ", 0),
                    numericInput("out1.control.sd", "SD   control: ", 1),
                    selectInput(inputId = "out1.treated.pct",
                                label = "Percent reduction: ",
                                choices = seq(0, 100, 10),
                                selected = "30")
                )
                ,
                
                conditionalPanel(
                    condition = "input.otheroutcome == 'out2'",
                    numericInput("out2.induced.mu", "Mean induced: ", 1),
                    numericInput("out2.induced.sd", "SD   induced: ", 1),
                    numericInput("out2.control.mu", "Mean control: ", 0),
                    numericInput("out2.control.sd", "SD   control: ", 1),
                    selectInput(inputId = "out2.treated.pct",
                                label = "Percent reduction: ",
                                choices = seq(0, 100, 10),
                                selected = "50")
                ))
        ),
        
        wellPanel(                                 
            radioButtons(inputId = "scale",
                         label = "Scale",
                         choices = c("Absolute" = "absolute",
                                     "Log" = "log"))
            ,        
            
            selectInput(inputId = "ntreated",
                        label = "Number of treated groups:",
                        choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                        selected = 3)
        ),
        
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
        ),
        
        downloadLink('downloadPdf', 'Download PDF')
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel(tableOutput("table")),
#                 "Table", includeMarkdown(
#                 file.path(path.package("RRR"),"md","table.Rmd"))),
            tabPanel("Plot", plotOutput("mainplot", height = "700px")), 
            tabPanel("Summary", verbatimTextOutput("summary"))
        )
    )
)
)


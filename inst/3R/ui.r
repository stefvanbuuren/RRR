
shinyUI(pageWithSidebar(
    headerPanel("TNO 3R Sample Size Calculator"),
    
    sidebarPanel(
        wellPanel(
            selectInput(inputId = "disease",
                    label = "Disease model",
                    choices = c("Model"  = "other",
                                "Lung fibrosis" = "fibrosis"),
                        selected = "Model"
        ),
        
        conditionalPanel(
            condition = "input.disease == 'fibrosis'",
                selectInput(inputId = "outcome",
                            label = "Parameter",
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
                                selected = "50")
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
                                selected = "70")
                )
        ),
        
        conditionalPanel(
            condition = "input.disease == 'other'",
                selectInput(inputId = "otheroutcome",
                            label = "Parameter",
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
                                selected = "80")
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
                                selected = "80")
                ))
        ),
        
        wellPanel(                                 
            selectInput(inputId = "ntreated",
                        label = "Number of treated groups:",
                        choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                        selected = 3),
            
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
        
        wellPanel(
        downloadButton('downloadTable', 'Download Table'),
        downloadButton('downloadPdf', 'Download Graph')
        # downloadButton('downloadSummary', 'Download Summary')
        )
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Table", 
                     tableOutput("tablesym"),
                     tableOutput("tableasym")),
            tabPanel("Graph", plotOutput("mainplot", height = "700px")), 
            tabPanel("Summary", 
                     uiOutput("summary")),
            tabPanel("About", includeMarkdown(
                file.path(path.package("RRR"),"md","about.Rmd"))),
            id = "panel"
        )
    )
)
)


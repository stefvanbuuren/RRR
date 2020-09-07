shinyUI(
  fluidPage(

    tags$head(tags$style(HTML("h1 {color: #016E98; font-size: 31px; }
                                   a  {color: #016E98; }"))),
    fluidRow(
      column(width = 3,
             a(img(src = "https://www.tno.nl/media/4710/logo-tno.gif", vspace = "12"),
               href = "https://www.tno.nl/")),
      column(width = 9,
             h1("3R Sample Size Calculator", align = "CENTER"))),
    fluidRow(
      column(width = 3,
             wellPanel(
               selectInput(inputId = "disease",
                           label = "Disease model",
                           choices = c("Model"  = "other",
                                       "Lung fibrosis" = "fibrosis"),
                           selected = "other"
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
                               choices = as.character(seq(0, 100, 10)),
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
                               choices = as.character(seq(0, 100, 10)),
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
                               choices = as.character(seq(0, 100, 10)),
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
                               choices = as.character(seq(0, 100, 10)),
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
                            choices = c("0.50" = "0.50",
                                        "0.80" = "0.80",
                                        "0.90" = "0.90"),
                            selected = "0.80")
             ),
             conditionalPanel(
               condition = "input.panel == 'Table'",
               downloadButton('downloadTable', 'Download')
             ),
             conditionalPanel(
               condition = "input.panel == 'Graph'",
               downloadButton('downloadGraph', 'Download')
             ),
             conditionalPanel(
               condition = "input.panel == 'Summary'",
               downloadButton('downloadSummary', 'Download')
             )
      ),
      column(width = 8, offset = 1,
             tabsetPanel(
               tabPanel("Table",
                        tableOutput("tablesym"),
                        tableOutput("tableasym")),
               tabPanel("Graph",
                        plotOutput("mainplot", height = "700px")),
               tabPanel("Summary",
                        uiOutput("summary")),
               tabPanel("How to use",
                        includeRmd(file.path(path.package("RRR"), "md", "help.Rmd"))),
               tabPanel("About",
                        includeRmd(file.path(path.package("RRR"), "md", "about.Rmd"))),
               id = "panel"
             )
      )
    )
  )
)


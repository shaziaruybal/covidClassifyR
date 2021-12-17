######### covidClassifyR UI ##########################
# Authors: Shazia Ruybal-Pesántez
######################################################

# TODO:


library(shiny)
library(markdown)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(janitor)
library(drc)

shinyUI(navbarPage(title = "covidClassifyR",
                   fluid = TRUE,
                   collapsible = TRUE,
                   
                   # ----------------------------------
                   # tab panel 1 - About
                   tabPanel("About", 
                            includeMarkdown("instructions.rmd")
                            ),
                    
                   # ----------------------------------
                   # tab panel 2 - Raw data import
                   tabPanel("Import data", 
                            p("You can import the .xlsx directly to this app for data processing. Check that the file looks as expected using the tab 'Check the raw data'."),
                            p("In order to match the sample IDs with the raw data, import your .xlsx plate layout file (click here to view an example)."),
                            sidebarLayout(sidebarPanel(
                                textInput(inputId = "experiment_name",
                                          label = "Experiment Name:",
                                          value = "experiment1"),
                                textInput(inputId = "date",
                                          label = "Date:",
                                          value = Sys.Date()),
                                textInput(inputId = "experiment_notes",
                                          label = "Experiment notes:",
                                          value = "Enter experiment details/notes here"),
                                fileInput(inputId = "raw_data_file",
                                          label = "Raw data:",
                                          accept = c(".xlsx")),
                                fileInput(inputId = "plate_layout_file",
                                          label = "Plate layout:",
                                          accept = c(".xlsx"))),
                                mainPanel(tabsetPanel(
                                    tabPanel("Check the raw data", textOutput("raw_data_filename"), tableOutput("alldata")),
                                    tabPanel("Check the run info", tableOutput("runinfo")),
                                    tabPanel("Check the plate layout", textOutput("plate_layout_filename"), tableOutput("plate"))
                                )))),
                   
                   # ----------------------------------
                   # tab panel 3 - Quality control
                   tabPanel("Quality control",
                            mainPanel(tabsetPanel(
                                tabPanel("Standard curves", 
                                         plotOutput("stdcurve")),
                                tabPanel("Plate QC",
                                         "Any wells with <15 beads/well are indicated in red, and should be double-checked manually",
                                         plotOutput("plateqc")),
                                tabPanel("Blank samples",
                                         "Blank samples should record MFI<50 for each protein. If any proteins are above the dashed line, they should be double-checked manually",
                                         plotOutput("blanks")),
                                tabPanel("Model results",
                                         plotOutput("model")),
                                tabPanel("Sample results",
                                         mainPanel(DTOutput("results"))),
                                tabPanel("Download data/report",
                                         downloadButton("report", "Download quality control report", disabled = "disabled"),
                                         downloadButton("downloadStds", "Download standard curve data", disabled = "disabled"),
                                         downloadButton("downloadData", "Download MFI/RAU data", disabled = "disabled"))
                            ))),
                   # ----------------------------------
                   # tab panel 4 - Data visualizations
                   
                   tabPanel("Data visualization",
                            "COMING SOON....."),
                   
                   # ----------------------------------
                   # tab panel 5 - Classification
                   tabPanel("Classify exposure", 
                            sidebarLayout(sidebarPanel(
                                radioButtons(inputId = "algorithm",
                                             label = "Classification algorithm (choose one):",
                                             choices = c("PNG algorithm", "Melbourne algorithm"))),
                                mainPanel(tabsetPanel(
                                    tabPanel("Classification results", textOutput("algorithm_choice"), DTOutput("classification_results")),
                                    tabPanel("Download data",
                                             downloadButton("downloadSero", "Download classification data", disabled = "disabled")),
                                    tabPanel("More details PNG algorithm", includeHTML("www/PNG_classification_algorithm.html")),
                                    tabPanel("More details Melbourne algorithm", "COMING SOON....")
                                ))))
                   
                   
        )
)

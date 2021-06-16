######### covidClassifyR UI ##########################
# Authors: Shazia Ruybal-Pesántez
######################################################

# TODO:


library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(janitor)
library(drc)

shinyUI(fluidPage(
    theme = shinytheme("cosmo"),
    
    titlePanel("covidClassifyR"),

    tabsetPanel(type = "tabs",
                tabPanel("About", includeMarkdown("instructions.Rmd")),
                tabPanel("Import data", 
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
                                       accept = c(".xlsx")),
                             downloadButton("report", "Download quality control report")),
                         mainPanel(tabsetPanel(
                             tabPanel("Check the raw data", textOutput("raw_data_filename"), tableOutput("alldata")),
                             tabPanel("Check the run info", tableOutput("runinfo")),
                             tabPanel("Check the plate layout", textOutput("plate_layout_filename"), tableOutput("plate"))
                         )))),
                #tabPanel("Editable table (TEST)"), DTOutput("edit_tab"),
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
                         sidebarLayout(sidebarPanel(
                             downloadButton("downloadData", "Download data")),
                    mainPanel(DTOutput("results"))))
                    
                )
        )
)
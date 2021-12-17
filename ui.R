######### covidClassifyR UI ##########################
# Authors: Shazia Ruybal-Pesántez
######################################################

# TODO:
# - add data visualization options
#   - box plots per antigen MFI and RAU options
#   - proportion seropos/neg
#   - summary seropos/neg?
#   - user uploaded processed data for classification?


library(shiny)
library(rmarkdown)
library(bslib)
library(shinyWidgets)
library(DT)
library(janitor)
library(drc)

shinyUI(navbarPage(title = "covidClassifyR",
                   fluid = TRUE,
                   collapsible = TRUE,
                   theme = bs_theme(bootswatch = "sandstone"),
                   footer = includeHTML("footer.html"),
                   
                   # ----------------------------------
                   # tab panel 1 - About
                   tabPanel("About", 
                            # includeMarkdown("home.rmd")
                            fluidPage(
                                br(),
                                uiOutput("home"))
                            ),
                   
                   # ----------------------------------
                   # tab panel 2 - Tutorial
                   tabPanel("Tutorial", 
                            # includeMarkdown("tutorial.rmd")
                            fluidPage(
                                br(),
                                uiOutput("tutorial"))
                   ),
                    
                   # ----------------------------------
                   # tab panel 3 - Raw data import
                   tabPanel("Import data", 
                            p("You can directly import the .xlsx output file from the Luminex machine to this app for data processing. Check that the file looks as expected using the tab 'Check the raw data' and that the run info is correct using the tab 'Check the run info'. Click here to look at an example file."),
                            p("In order to match the sample IDs with the raw data, import your .xlsx plate layout file (click here to view an example). Make sure your plate layout is formatted correctly compared to the example, you can check using the tab 'Check the plate layout'. There should be no extra rows or columns other than the plate map."),
                            sidebarLayout(sidebarPanel(
                                textInput(inputId = "experiment_name",
                                          label = "Enter your experiment name:",
                                          value = "experiment1"),
                                textInput(inputId = "date",
                                          label = "Enter date:",
                                          value = Sys.Date()),
                                textInput(inputId = "experiment_notes",
                                          label = "Enter any experiment notes:",
                                          value = "Enter experiment details/notes here"),
                                fileInput(inputId = "raw_data_file",
                                          label = "Upload raw data:",
                                          accept = c(".xlsx")),
                                fileInput(inputId = "plate_layout_file",
                                          label = "Upload plate layout:",
                                          accept = c(".xlsx"))),
                                mainPanel(tabsetPanel(
                                    tabPanel("Check the raw data", textOutput("raw_data_filename"), tableOutput("alldata")),
                                    tabPanel("Check the run info", tableOutput("runinfo")),
                                    tabPanel("Check the plate layout", textOutput("plate_layout_filename"), 
                                             p("Important things to note: make sure that your standards are labeled appropriately starting with 'S' so that the app can recognize your standards and if you have more than one blank sample make sure you label them as 'Blank 1', 'Blank2' etc (for more details on the specific layour requirements, see 'Tutorial')"),
                                             tableOutput("plate"))
                                )))),
                   
                   # ----------------------------------
                   # tab panel 4 - Quality control
                   tabPanel("Quality control",
                            p("The automated data processing in this app allows you to convert your median fluorescence units (MFI) data into relative antibody units (RAU) by fitting a 5-parameter logistic function to the standard curve on a per-antigen level."),
                            p("You can use the different tabs for quality control of your run, click on each tab for more details."),
                            p("When you are finished you can download the data and quality control report by clicking on the 'Download' tab."),
                            mainPanel(tabsetPanel(
                                tabPanel("Standard curves", 
                                         p("Check the standard curves for each protein below."),
                                         plotOutput("stdcurve")),
                                tabPanel("Plate QC",
                                         "For quality control of each plate, we check that each well should have ≥15 beads/well. Any wells with <15 beads/well are indicated in red below and should be double-checked manually",
                                         plotOutput("plateqc")),
                                tabPanel("Blank samples",
                                         "Blank samples should record MFI<50 for each protein. If any proteins are above the dashed line, they should be double-checked manually. If you have more than one blank sample in your plate, make sure to label them as 'Blank1', 'Blank2' and so forth, otherwise the data shown here will be a cumulatie result of all your blanks.",
                                         plotOutput("blanks")),
                                tabPanel("Model results",
                                         "The 5-paramater logistic function is a log-log model that is used to obtain a more linear relationship. Check that the results from the model look relatively linear for each protein.",
                                         plotOutput("model")),
                                tabPanel("Sample results",
                                         "The table below displays the RAU conversions, go to the next tab to download your processed data.",
                                         mainPanel(DTOutput("results"))),
                                tabPanel("Download",
                                         p("Click the buttons to download."),
                                         downloadButton("report", "Download quality control report", disabled = "disabled"),
                                         downloadButton("downloadStds", "Download standard curve data", disabled = "disabled"),
                                         downloadButton("downloadData", "Download MFI/RAU data", disabled = "disabled"))
                            ))),
                   # ----------------------------------
                   # tab panel 5 - Data visualizations
                   
                   tabPanel("Data visualization",
                            "COMING SOON....."),
                   
                   # ----------------------------------
                   # tab panel 6 - Classification
                   tabPanel("Classify exposure", 
                            sidebarLayout(sidebarPanel(
                                p("These classification algorithms have been developed..... For more information about the algorithms, click on the tab 'Algorithm details'."),
                                radioButtons(inputId = "algorithm",
                                             label = "Classification algorithm (choose one):",
                                             choices = c("PNG algorithm", "Melbourne algorithm"))),
                                mainPanel(tabsetPanel(
                                    tabPanel("Classification results", textOutput("algorithm_choice"), DTOutput("classification_results")),
                                    tabPanel("Download data",
                                             downloadButton("downloadSero", "Download classification data", disabled = "disabled"))
                                )))),
                   
                   # ----------------------------------
                   # tab panel 7 - Classification details
                   tabPanel("Algorithm details",
                            tabsetPanel(
                                tabPanel("PNG algorithm details",
                                         fluidPage(
                                             br(),
                                             uiOutput("pngAlgorithm"))), 
                                tabPanel("Melbourne algorithm details", "COMING SOON....")
                            ))
                   
        )
)

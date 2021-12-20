######### covidClassifyR UI ##########################
# Authors: Shazia Ruybal-Pesántez
######################################################

# TODO:
#   - summary seropos/neg?
#   - user uploaded processed data for classification?

library(shiny)
library(rmarkdown)
library(bslib)
library(shinyWidgets)
library(DT)
library(janitor)
library(drc)
library(plotly)

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
                            h3("Import your data"),
                            p("You can directly import the .xlsx output file from the Luminex machine to this app for data processing. Check that the file looks as expected using the tab 'Check the raw data' and that the run info is correct using the tab 'Check the run info'. Click below to download example files."),
                            p("In order to match the sample IDs with the raw data, import your .xlsx plate layout file (click below to download an example file). Make sure your plate layout is formatted correctly compared to the example, you can check using the tab 'Check the plate layout'. There should be no extra rows or columns other than the plate map."),
                            a(href="example_data.zip", "Click here to download the example data.", download=NA, target="_blank"),
                            br(),
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
                                          label = "Upload raw data (.xlsx):",
                                          accept = c(".xlsx")),
                                fileInput(inputId = "plate_layout_file",
                                          label = "Upload plate layout (.xlsx):",
                                          accept = c(".xlsx"))),
                                mainPanel(tabsetPanel(
                                    tabPanel("Check the raw data",
                                             br(),
                                             textOutput("raw_data_filename"), 
                                             tableOutput("alldata")),
                                    tabPanel("Check the run info", tableOutput("runinfo")),
                                    tabPanel("Check the plate layout", 
                                             br(),
                                             textOutput("plate_layout_filename"), 
                                             tableOutput("plate"),
                                             br(),
                                             p("Important things to note:"),
                                             p("Make sure that your standards are labeled appropriately starting with 'S' so that the app can recognize your standards and if you have more than one blank sample make sure you label them as 'Blank 1', 'Blank2' etc (for more details on the specific layout requirements, see 'Tutorial')"))
                                )))),
                   
                   # ----------------------------------
                   # tab panel 4 - Quality control
                   tabPanel("Quality control",
                            h3("Quality control"),
                            p("The automated data processing in this app allows you to convert your median fluorescence units (MFI) data into relative antibody units (RAU) by fitting a 5-parameter logistic function to the standard curve on a per-antigen level."),
                            p("You can use the different tabs for quality control of your run, click on each tab for more details."),
                            p("When you are finished you can download the data and quality control report by clicking on the 'Download' tab."),
                            mainPanel(tabsetPanel(
                                tabPanel("Standard curves", 
                                         br(),
                                         p("Check the standard curves for each protein below."),
                                         plotOutput("stdcurve")),
                                tabPanel("Plate QC",
                                         br(),
                                         "For quality control of each plate, we check that each well should have ≥15 beads/well. Any wells with <15 beads/well are indicated in red below and should be double-checked manually",
                                         plotOutput("plateqc")),
                                tabPanel("Blank samples",
                                         br(),
                                         "Blank samples should record MFI<50 for each protein. If any proteins are above the dashed line, they should be double-checked manually. If you have more than one blank sample in your plate, make sure to label them as 'Blank1', 'Blank2' and so forth, otherwise the data shown here will be a cumulatie result of all your blanks.",
                                         plotOutput("blanks")),
                                tabPanel("Model results",
                                         br(),
                                         "The 5-paramater logistic function is a log-log model that is used to obtain a more linear relationship. Check that the results from the model look relatively linear for each protein.",
                                         plotOutput("model")),
                                tabPanel("Sample results",
                                         br(),
                                         "The table below displays the RAU conversions and is interactive (i.e., you can search and filter as required), go to the next tab to download your processed data.",
                                         mainPanel(DTOutput("results"))),
                                tabPanel("Download",
                                         br(),
                                         p("Click the buttons to download."),
                                         downloadButton("downloadData", "Download MFI/RAU data", disabled = "disabled"),
                                         downloadButton("downloadStds", "Download standard curve data", disabled = "disabled"),
                                         downloadButton("report", "Download quality control report", disabled = "disabled")
                                         )
                            ))),
                   
                   # ----------------------------------
                   # tab panel 5 - Classification
                   tabPanel("Classify exposure", 
                            h3("Classify exposure"),
                            p("Our multi-antigen panel was developed for research purposes as a serological tool that can detect individuals with past exposure to SARS-CoV-2 without the need for knowledge of previous exposure a priori. By simultaneously measuring antibodies to multiple SARS-CoV-2 antigens and other viral antigens, we have leveraged these antibody measurements to train a random forest machine learning algorithm based on the combined serological signatures of multiple antigens. We trained our classification algorithm using a dataset with samples from individuals with a confirmed SARS-CoV-2 PCR positive result (positive samples) from several independent studies, as well as negative samples from various independent sources (pre-pandemic negative controls, Australian Red Cross and Victorian Blood Donor Registry samples). Given the variability in time since symptom onset and overall exposure to SARS-CoV-2 in unknown samples from epidemiological studies, we have trained three separate classifier algorithms based on different stratifications of the data: 1) negative controls and all positive samples in the data set, 2) negative controls and positive samples that were more than two weeks but up to three months post symptom onset, and 3) negative controls and positive samples collected between three and six months post symptom onset. If knowledge of time since symptom onset is known, this can be leveraged by selecting the most appropriate classifier, otherwise we recommend making an educated decision based on the results from all three classifiers. For more information about the algorithms, click on the tab 'Algorithm details'."),
                            br(),
                            p("Disclaimer: the results obtained from this classification are for research purposes only and should not be considered a diagnosis"),
                            sidebarLayout(sidebarPanel(
                                p("Below you can choose the algorithm best suited for your purposes. The PNG algorithm has a sensitivity of 93.44-98.51% and specificity of 94.12-100% depending on the classifier."),
                                radioButtons(inputId = "algorithm",
                                             label = "Classification algorithm (choose one):",
                                             choices = c("PNG algorithm", "Melbourne algorithm")),
                                p("Below you can also upload data that has already been processed using the app in order to apply the classification algorithm. Note that the data must be supplied in the appropriate format for the classification to work. (COMING SOON)"),
                                fileInput("RAUdataUpload", "Upload your MFI/RAU data:", multiple = FALSE, accept = ".csv")
                                ),
                            mainPanel(tabsetPanel(
                                tabPanel("Classification results", 
                                         br(),
                                         textOutput("algorithm_choice"), 
                                         br(),
                                         DTOutput("classification_results")),
                                tabPanel("Download data",
                                         br(),
                                         textOutput("algorithm_choice_remind"), 
                                         br(),
                                         downloadButton("downloadSeroPNG", "Download classification data (PNG algorithm)", disabled = "disabled"),
                                         br(),
                                         downloadButton("downloadSeroMel", "Download classification data (Melbourne algorithm)", disabled = "disabled"))
                            )))),
                   
                   # ----------------------------------
                   # tab panel 6 - Data visualizations
                   
                   tabPanel("Data visualization",
                            h3("Data visualization"),
                            p("Below you can take a quick look at your data through the interactive plots."),
                            tabsetPanel(
                                tabPanel("MFI boxplots", plotlyOutput("boxplotMFI")),
                                tabPanel("RAU boxplots", plotlyOutput("boxplotRAU")),
                                tabPanel("Exposure status",
                                         p("The exposure status results for each of the classifiers can be found below. The algorithm was trained on three dataset partitions: 1) all samples from >2 weeks from symptom onset, 2) samples within >2 weeks from sympton onset and up to 3 months post-symptom onset, and 3) samples from >3 to 6 months post-symptom onset (for more details see 'Algorithm details')."),
                                         p("Disclaimer: the results obtained from this classification are for research purposes only and should not be considered a diagnosis"),
                                         plotlyOutput("propSero")
                                ))),
                   
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

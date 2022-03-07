######### covidClassifyR server ##########################
# Authors: Shazia Ruybal-Pesántez
######################################################

# TODO:
# - can the run info be flexible (ie row number?) or does this change depending on machine settings
# - make plate layout user-entered?

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(readr)
library(DT)
library(janitor)
library(drc)
library(randomForest)
library(pROC)

source("functions.R")

load("algorithms/png_classifiers.Rdata")
load("algorithms/mel_classifiers.Rdata")

shinyServer(function(input, output, session){
    
    ### make inputs reactive
    experiment_name <- reactive({
        req(input$experiment_name)
        input$experiment_name
    })
    
    date <- reactive({
        req(input$date)
        input$date
    })
    
    experiment_notes <- reactive({
        req(input$experiment_notes)
        input$experiment_notes
    })
    
    raw_data <- reactive({
        req(input$raw_data_file)
        input$raw_data_file
        })
    
    raw_data_filename <- reactive({
        req(input$raw_data_file)
        input$raw_data_file$name
    })
    
    plate_layout <- reactive({
        req(input$plate_layout_file)
        input$plate_layout_file
    })
    
    plate_layout_filename <- reactive({
        req(input$plate_layout_file)
        input$plate_layout_file$name
    })
    
    algorithm <- reactive({
        req(input$algorithm)
        input$algorithm
    })
    
    ### print the filenames for cross-checking
    output$raw_data_filename <- renderText(paste0("Raw data filename: ", raw_data_filename()))
    output$plate_layout_filename <- renderText(paste0("Plate layout filename: ", plate_layout_filename()))
    
    ### print the algorithm choice for cross-checking
    output$algorithm_choice <- renderText(paste0("Algorithm choice: ", algorithm()))
    
    ### print the algorithm choice for cross-checking when downloading
    output$algorithm_choice_remind <- renderText(paste0("Based on your algorithm choice (", algorithm(), ") download your data below:"))
    
    ### read imported file and print raw data for checking
    output$alldata <- renderTable({
        readSeroData(raw_data())[[1]]
    })
    
    ### read imported file and print run info for checking
    output$runinfo <- renderTable({
        readSeroData(raw_data())[[6]]
        })

    output$plate <- renderTable({
        readPlateLayout(plate_layout())
        })
    
    # output$counts <- renderTable({
    #     getCounts(raw_data())
    # })
    
    stdcurve_plot <- reactive({
        plotStds(raw_data(), experiment_name()) 
    })

    plateqc_plot <- reactive({
        plotCounts(raw_data(), experiment_name())
    })
    
    blanks_plot <- reactive({
        plotBlanks(raw_data(), experiment_name())
    })
    
    model_plot <- reactive({
        plotModel(raw_data(), plate_layout())
    })
        
    model_results <- reactive({
        runModel(raw_data(), plate_layout())[[3]]
    })
    
    mfi_plot <- reactive({
        plotBoxplotMFI(raw_data(), plate_layout())
    })
    
    rau_plot <- reactive({
        plotBoxplotRAU(raw_data(), plate_layout())
    })
    
    sero_plot <- reactive({
        if(algorithm() == "PNG algorithm"){
            
            plotProportionSero(raw_data(), plate_layout(), algorithm(), png_rf_all, png_rf_3months, png_rf_G3months)
        }
        
        else{
            
            plotProportionSeroMel(raw_data(), plate_layout(), algorithm(), mel_rf_all, mel_rf_3months, mel_rf_G3months)
            
        }
    })
    
    #### Output objects
    output$stdcurve <- renderPlot(stdcurve_plot())
    output$plateqc <- renderPlot(plateqc_plot())
    output$blanks <- renderPlot(blanks_plot())
    output$model <- renderPlot(model_plot())
    
    output$results <- DT::renderDataTable({
        runModel(raw_data(), plate_layout())[[2]]
    })
    
    #### Interactive output visualizations
    output$boxplotMFI <- renderPlotly(mfi_plot())
    output$boxplotRAU <- renderPlotly(rau_plot())
    output$propSero <- renderPlotly(sero_plot())
    
    #### Classification algorithm
    output$classification_results <- DT::renderDataTable({
        if(algorithm() == "PNG algorithm"){

            classifyExposure(raw_data(), plate_layout(), png_rf_all, png_rf_3months, png_rf_G3months)

        }

        else{
            classifyExposureMel(raw_data(), plate_layout(), mel_rf_all, mel_rf_3months, mel_rf_G3months)

        }
    })
    
    # Downloadable report of QC ----
    output$report <- downloadHandler(
        filename = paste0(experiment_name(), "_", Sys.Date(), "_QCreport.html"),
        content = function(file) {
            tempReport <- file.path(tempdir(), "template.Rmd")
            file.copy("template.Rmd", tempReport, overwrite = TRUE)

            rmarkdown::render(tempReport,
                              output_file = file,
                              params = list(raw_data_filename = raw_data_filename(),
                                            plate_layout_filename = plate_layout_filename(),
                                            experiment_name = experiment_name(),
                                            date = date(),
                                            experiment_notes = experiment_notes(),
                                            stdcurve_plot = stdcurve_plot(),
                                            plateqc_plot = plateqc_plot(),
                                            blanks_plot = blanks_plot(),
                                            model_results = model_results()),
                              envir = new.env(parent = globalenv()),
            )
        }
    )
    
    # Downloadable csv of standards ----
    output$downloadStds <- downloadHandler(
        filename = function() {
            paste0(experiment_name(), "_", Sys.Date(), "_stdcurve.csv", sep = "")
        },
        content = function(file) {
            write.csv(readSeroData(raw_data())[[5]], file, row.names = FALSE)
        }
    )
    
    # Downloadable csv of MFI/RAU results file ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(experiment_name(), "_", Sys.Date(), "_MFI_RAU.csv", sep = "")
        },
        content = function(file) {
            write.csv(runModel(raw_data(), plate_layout())[[2]], file, row.names = FALSE)
        }
    )
    
    # Downloadable csv of classification results file ----
    output$downloadSeroPNG <- downloadHandler(
        filename = function() {
            paste0(experiment_name(), "_", Sys.Date(), "_classification_", algorithm(), ".csv", sep = "")
        },

        content = function(file) {
                write.csv(classifyExposure(raw_data(), plate_layout(), png_rf_all, png_rf_3months, png_rf_G3months), 
                          file, row.names = FALSE)
        }
    )
    
    output$downloadSeroMel <- downloadHandler(
        filename = function() {
            paste0(experiment_name(), "_", Sys.Date(), "_classification_", algorithm(), ".csv", sep = "")
        },
        
        content = function(file) {
            write.csv(classifyExposureMel(raw_data(), plate_layout(), mel_rf_all, mel_rf_3months, mel_rf_G3months), 
                      file, row.names = FALSE)
        }
    )
    
    # output$downloadSero <- downloadHandler(
    #         filename = function() {
    #             paste0(experiment_name(), "_", Sys.Date(), "_classification.csv", sep = "")
    #         },
    # 
    #         content = function(file) {
    #             if(algorithm == "PNG algorithm") {
    #             write.csv(classifyExposure(raw_data(), plate_layout(), png_rf_all, png_rf_3months, png_rf_G3months),
    #                       file, row.names = FALSE)}
    #             else if (algorithm == "Melbourne algorithm"){
    #             write.csv(classifyExposureMel(raw_data(), plate_layout(), mel_rf_all, mel_rf_3months, mel_rf_G3months),
    #                       file, row.names = FALSE)}
    #         }
    # )

    output$home <- renderUI({
        tags$iframe(src="home.html",width="100%",frameBorder="0",height="500px")
    })
    
    output$tutorial <- renderUI({
        tags$iframe(src="tutorial.html",width="100%",frameBorder="0",height="500px")
    })
        
    output$pngAlgorithm <- renderUI({
        tags$iframe(src="PNG_classification_algorithm.html",width="100%",frameBorder="0",height="500px")
    })
})

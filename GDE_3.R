## GDE_3.R ##

# Get from where is launch the script
get_current_file_path <- function(){
  
  this_file <- grep("^--file=", commandArgs(), value = TRUE)
  this_file <- gsub("^--file=", "", this_file)
  if (length(this_file) == 0) this_file <- rstudioapi::getSourceEditorContext()$path
  
  return(dirname(this_file))}

## packages for GDE_3.R ####
librarian::shelf(shiny)
librarian::shelf(shinydashboard)
librarian::shelf(shinyWidgets)
librarian::shelf(shinycssloaders)
#librarian::shelf(shinyDesktop)     # to get the focus on window for selecting data file with choose.file()
librarian::shelf(DT)               # rendering of DT table
librarian::shelf(knitr)            # for reporting
librarian::shelf(rmarkdown)        # for rendering html created with RMD
librarian::shelf(rhandsontable)    # for rendering of rHandSonTable
librarian::shelf(lubridate)        # for is.POSIXct()
librarian::shelf(dygraphs)         # time series plots
librarian::shelf(htmltools)        # for rendering dygraphs
librarian::shelf(threadr)          # converting dataframe to xts for dygraphs
librarian::shelf(ggplot2)          # for scatterplots
librarian::shelf(shinyjs)          # to give focus to specific menuItem of the sidebarmenu
librarian::shelf(shinyalert)       # shiny message, inflo and alert
librarian::shelf(gridExtra)        # for plotting with grid.arrange of moments
librarian::shelf(moments)          # skewness and kurtosis. we could use package psych instead


## packages for Functions4GDFE.R ####
librarian::shelf(see, quiet = T)        # needed for visualisation of "Performance"
librarian::shelf(performance, quiet = T)# Check Linear model assumptions
librarian::shelf(qqplotr, quiet = T)    # for performance, plotting qqplot
librarian::shelf(skedastic, quiet = T)  # Check Linear model homogeneity of variance assumption (Breusch pagan test)
librarian::shelf(rempsyc, quiet = T)    # checking for normality, homoskedasticity, autocorrelation
librarian::shelf(MethComp, quiet = T)   # for TLS and Deming regression with standard error of coefficients from 1000 bootstrap samples
librarian::shelf(deming, quiet = T)     # for Deming regression with constant or variable errors on x and y!
librarian::shelf(tls, quiet = T)        # Another package for TLS regression with standard error of coefficients from normal method of bootstrap samples
librarian::shelf(quantreg, quiet = T)   # Quantilsregression mit Spezialfall Medianregression
librarian::shelf(MASS, quiet = T)
librarian::shelf(pracma, quiet = T)     # for the Orthogonal Regression - MG: I use MethComp::Deming, it gives the same results with one function and package less
librarian::shelf(lmtest, quiet = T)     # Tests for the linear Regression  - MG: I use package performance or skedastic
librarian::shelf(car, quiet = T)        # Linear Regression analysis
librarian::shelf(ggplot2, quiet = T)
librarian::shelf(ggtext, quiet = T)     # for element_markdown
librarian::shelf(data.table, quiet = T) # for data.table computation

# Colour for time series
colour_vector <- c("red", "blue", "black", "green", "cornflowerblue", "chocolate4", "darkblue",
                   "darkgoldenrod3", "darkorange", "darkolivegreen4", "goldenrod4", "darkred",
                   "darkmagenta", "darkgreen", "darkcyan", "red", "blue", "black", "green",
                   "cornflowerblue", "chocolate4", "darkblue", "darkgoldenrod3", "darkorange", "darkolivegreen4",
                   "goldenrod4", "darkred")

# Sourcing necessary functions for computation
# for function get.DQO(), source Functions4ASE.R 
# Download updated version of Functions4ASE.R and 151016 Sensor_Toolbox.R on Github
futile.logger::flog.info("Checking version of \"Functions4ASE.R\" and \"151016 Sensor_Toolbox.R\" on Github")
WD <- get_current_file_path()
# cat(paste0("WD: ", WD, "\n"))
# source(file.path(WD,"Load_Files_Github.R"))
# source(file.path(WD,"Functions4ASE.R"))
# source(file.path(WD,"151016 Sensor_Toolbox.R"))

# Local files
source(file.path(dirname(WD),"Functions4ASE.R"))
source(file.path(dirname(WD),"151016 Sensor_Toolbox.R"))

# Sourcing necessary functions for computation
source("Functions4GDE.R")


# For the App
source("sidebar.R")
source("body.R")

# Put them together into a dashboardPage

ui <- dashboardPage(
  dashboardHeader(title = "GDE vs 0.1"),
  sidebar,
  body
)

server <- function(input, output, session) {
  
  # for changing visibility of menuItem of sidebarmenu()
  shinyjs::useShinyjs()
  
  # Observe button click to shut down the server
  observeEvent(input$shutdown, {
    # Stop the current Shiny app
    stopApp()
    
    # Inject JavaScript to close the browser tab
    runjs("window.close();")
  })
  
  # Dynamic text with file, pollutant, instrument ...
  output$dynamic_text <- renderText({
    paste0("File: ",ifelse(!is.null(Data$File), basename(Data$File), " "), ", Directive: ", input$Directive,
           ", Pollutant: ", input$Pollutant, ", ", input$Type, ", averaging period: ", input$Averaging.Period, 
           ", Analyser: ", ifelse(!is.null(input$Instrument), input$Instrument, " "))
  })
  
  # initialise Data reactive values, place holder for all data
  Data <- reactiveValues()
  
  # Determine DQO
  observe({
    #to avoid missing input$
    shiny::req(shiny::isTruthy(input$Pollutant) &&
                 shiny::isTruthy(input$Averaging.Period) &&
                 shiny::isTruthy(input$unit.ref) &&
                 shiny::isTruthy(input$Directive))
    
    # get.DQO needs a few updates for the last Directive
    Data$All.DQOs <- get.DQO(
      name.gas         = input$Pollutant,
      Averaging.Period = input$Averaging.Period,
      unit.ref         = input$unit.ref,
      Directive        = input$Directive)
  })
  SelectDQO <- reactive({
    if(input$Type %in% c("Fixed type testing", "Fixed on-going")){
      DQO <- "DQO.0"
    } else if(input$Type %in% c("Indicative type testing","Indicative on-going")){
      DQO <- "DQO.1"
    }
    data.table::data.table(LV  = Data$All.DQOs$LV,
                           DQO = Data$All.DQOs[[DQO]],
                           UR  = paste0(round(Data$All.DQOs[[DQO]] / Data$All.DQOs$LV*100),"%"))})
  output$SelectDQO <- renderTable(SelectDQO())
  
  # Select data file
  observeEvent(input$chooseFile,{
    
    Data$File <- utils::choose.files(default = paste0(file.path(getwd(), "General_data"), "/*.csv"),
                                     caption = "Select csv data file to upload",
                                     filters = rbind(c("csv files (*.csv)","*.csv"),
                                                     c("Rdata files (*.Rdata)","*.Rdata"),
                                                     c("Text files (*.txt)","*.txt")),
                                     multi = FALSE,
                                     index = 1)
  })
  
  # updating Type of testing if input$Directive changes, e.g. if EN TS 17660 is selected type can only be indicative
  observe({
    if(input$Directive == "EN TS 17660"){
      shinyWidgets::updatePickerInput(
        session = session,
        inputId  = "Type",
        selected = "Indicative type testing",
        choices =c("Indicative type testing","Indicative on-going"))
    } else {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId  = "Type",
        selected = "Fixed type testing",
        choices = c("Fixed type testing", "Fixed on-going","Indicative type testing","Indicative on-going"))
    } 
  })
  
  # Modify tabs of statistics according to input$Type
  observeEvent(input$Type,{
    if(input$Type == "Fixed type testing"){
      shiny::hideTab(inputId = "tabStat1", target = "CM_OLS")
      shiny::hideTab(inputId = "tabStat1", target = "CM_Weighted")
    } else  if(input$Type == "Fixed on-going"){
      shiny::showTab (inputId = "tabStat1", target = "CM_OLS")
      shiny::showTab (inputId = "tabStat1", target = "CM_Weighted")
    }
  })
  
  # loading data 
  observeEvent(Data$File,{
    
    # Reading data file
    Data$DT <- data.table::fread(Data$File)
    
    # Check content of data file (Date, RM1, RM2, CM1, CM2)
    Data$Name.Date <- grep(paste(c("date", "Date"), collapse = "|"), names(Data$DT), value = T)
    if(length(Data$Name.Date) != 1){
      # MESSAGE NO DATE
      my_message <- paste0("[GDE_App, Data] ERROR, cannot find column \"date\" or \"Date\", reload correct data. The App may crash")
      futile.logger::flog.error(my_message)
      shinyalert::shinyalert(title = "ERROR data file",
                             text = my_message,
                             closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                             html = FALSE,
                             type = "error",
                             showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                             timer             = 5000,
                             imageUrl          = "",
                             animation         = FALSE)
      # Re-opening window for selecting data file 
      shinyjs::click("chooseFile")
    } else {
      # Convert Date to POSIXct and change name of date column if needed
      if (Data$Name.Date != "date"){
        data.table::setnames(Data$DT, Data$Name.Date, "date")
        Data$Name.Date <- "date"}
      if(!lubridate::is.POSIXct(Data$DT[[Data$Name.Date]])){
        data.table::set(Data$DT, j = Data$Name.Date, 
                        value = as.POSIXct(Data$DT[[Data$Name.Date]],  tz = "UTC",
                                           tryFormats = c("%d.%m.%Y", "%d/%m/%Y", "%Y-%m-%d", "%Y-%m-%d", "%m/%d/%Y",
                                                          "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H.%M.%S",
                                                          "%Y-%m-%d %H:%M", "%m/%d/%Y %H:%M", "%d/%m/%Y %H:%M")))}
      if(!lubridate::is.POSIXct(Data$DT[[Data$Name.Date]])){
        # MESSAGE NO POSIXct
        my_message <- paste0("[GDE_App, Data] ERROR, cannot convert ", Data$Name.Date, " to POSIXct, reload correct data. The App may crash")
        futile.logger::flog.error(my_message)
        shinyalert::shinyalert(title = "ERROR data file",
                               text = my_message,
                               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                               html = FALSE,
                               type = "error",
                               showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                               timer             = 5000,
                               imageUrl          = "",
                               animation         = FALSE)
        # Re-opening window for selecting data file 
        shinyjs::click("chooseFile")
      } else {
        Data$Name.CM <- grep("CM", names(Data$DT), value = T)
        if(length(Data$Name.CM) == 0){
          # MESSAGE NO CM
          my_message <- paste0("[GDE_App, Data] ERROR, column(s) for CM is(are) missing. Reload correct data. The App may crash")
          cat(my_message)
          shinyalert::shinyalert(title = "ERROR data file",
                                 text = my_message,
                                 closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                 html = FALSE,
                                 type = "error",
                                 showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                 timer             = 5000,
                                 imageUrl          = "",
                                 animation         = FALSE)
          # opening window for selecting data again
          shinyjs::click("chooseFile")
        } else {
          Data$Name.SN <- grep("SN", names(Data$DT), value = T)
          if(length(Data$Name.SN) == 0){
            # MESSAGE NO SN
            my_message <- paste0("[GDE_App, Data] WARN, column(s) for SN of CM is(are) missing. Columns with SN(s) A, B, C ... are added to the data file")
            cat(my_message)
            shinyalert::shinyalert(title = "WARNING data file",
                                   text = my_message,
                                   closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                   html = FALSE,
                                   type = "warning",
                                   showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                   timer             = 5000,
                                   imageUrl          = "",
                                   animation         = FALSE)
            # Adding SN
            for(SN in seq_along(Data$Name.CM)){
              Data$DT[,(paste0("SN",SN)) :=  rep(LETTERS[SN], nrow(Data$DT))]}
          }
          if(length(Data$Name.CM) != length(Data$Name.SN)){
            # MESSAGE number of "SN" different from number of "CM"
            my_message <- paste0("[GDE_App, Data] ERROR, the number of CM columns is different than SN columns. Reload correct data. The App may crash")
            cat(my_message)
            shinyalert::shinyalert(title = "ERROR data file",
                                   text = my_message,
                                   closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                   html = FALSE,
                                   type = "error",
                                   showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                   timer             = 5000,
                                   imageUrl          = "",
                                   animation         = FALSE)
            # opening window for selecting data again
            shinyjs::click("chooseFile")
          } else {
            Data$Name.RM <- grep("RM", names(Data$DT), value = T)
            if(length(Data$Name.RM) == 0){
              # MESSAGE NO RM
              my_message <- paste0("[GDE_App, Data] ERROR, column(s) for RM is(are) missing. Reload correct data. The App may crash")
              cat(my_message)
              shinyalert::shinyalert(title = "ERROR data file",
                                     text = my_message,
                                     closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                     html = FALSE,
                                     type = "error",
                                     showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK",
                                     confirmButtonCol  = "#AEDEF4",
                                     timer             = 5000,
                                     imageUrl          = "",
                                     animation         = FALSE)
              # opening window for selecting data again
              shinyjs::click("chooseFile")
            } else {
              
              # Define a reactive expression to compute summary statistics of Data$DT
              summaryFile <- reactive({
                data_summary <- Data$DT[, .(
                  Count = .N,
                  Mean = mean(Data$DT[[Data$Name.RM[1]]], na.rm = TRUE),
                  SD = sd(Data$DT[[Data$Name.RM[1]]], na.rm = TRUE),
                  Min = min(Data$DT[[Data$Name.RM[1]]], na.rm = TRUE),
                  Max = max(Data$DT[[Data$Name.RM[1]]], na.rm = TRUE)
                )]
                data_summary
              })
              
              # Render the summary table
              output$summaryFile <- renderTable({
                summaryFile()
              }, rownames = TRUE)
              
              # Updating Input$type according to number of RM
              observeEvent(Data$Name.RM,{
                length.Name.RM <- length(Data$Name.RM)
                if(length.Name.RM < 2){
                  CHOICES <- c("Fixed on-going","Indicative type testing","Indicative on-going")
                  VALUE   <- "Fixed on-going"
                } else {
                  CHOICES <- c("Fixed type testing", "Fixed on-going","Indicative type testing","Indicative on-going")
                  VALUE   <- "Fixed type testing"}
                # Set Type
                updatePickerInput(
                  inputId = "Type",
                  label   = "Type",
                  choices = CHOICES,
                  selected = VALUE,
                  options = list(
                    title = "Choose type")
                )
              })
              
              # Converting Data$Name.CM and Data$Name.RM to numeric if needed
              for(Measurement in c(Data$Name.CM, Data$Name.RM)){
                if(!is.numeric(Data$DT[[Measurement]])) data.table::set(Data$DT, j = Measurement, value = suppressWarnings(as.numeric(Data$DT[[Measurement]])))}
              
              # Discarding rows with empty reference or empty candidate (one CM or one RM is sufficient to keep rows)
              All.NA.CM <-Reduce(`&`, lapply(Data$Name.CM, function(col) is.na(Data$DT[[col]])))
              All.NA.RM <-Reduce(`&`, lapply(Data$Name.RM, function(col) is.na(Data$DT[[col]])))
              Data$DT <- Data$DT[setdiff(1:nrow(Data$DT), which(All.NA.CM|All.NA.RM))]
              if(nrow(Data$DT) < as.numeric(input$min.N)){
                # MESSAGE Not enough data
                my_message <- paste0("[GDE_App, Data] ERROR, after discarding rows without at least one valid number in the \"CMi\" columns or one in the \"RMi\" columns are discarded there less than ", input$min.N, " data left. 
                                     Reload correct data. The App may crash")
                cat(my_message)
                shinyalert::shinyalert(title = "ERROR data file",
                                       text = my_message,
                                       closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                       html = FALSE,
                                       type = "error",
                                       showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                       timer             = 5000,
                                       imageUrl          = "",
                                       animation         = FALSE)
                # opening window for selecting data again
                shinyjs::click("chooseFile")
              } else {
                
                # change name of column "size fraction to Pollutant, for correctly selecting data with DQO, update pollutant PickerInput
                if("Size Fraction" %in% names(Data$DT)) data.table::setnames(Data$DT, "Size Fraction", "Pollutant")
                Data$Name.Pollutant <- unique(Data$DT$Pollutant)
                
                if(length(Data$Name.Pollutant) == 0){
                  # MESSAGE NO Pollutant column
                  my_message <- paste0("[GDE_App, Data] ERROR, column \"Pollutant\" or \"Size Fraction\" missing in the data file. Reload correct data. The App may crash")
                  cat(my_message)
                  shinyalert::shinyalert(title = "ERROR data file",
                                         text = my_message,
                                         closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                         html = FALSE,
                                         type = "error",
                                         showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                         timer             = 5000,
                                         imageUrl          = "",
                                         animation         = FALSE)
                  # opening window for selecting data again
                  shinyjs::click("chooseFile")
                } else {
                  
                  updatePickerInput(session = session, inputId = "Pollutant",
                                    choices  = Data$Name.Pollutant,
                                    selected = Data$Name.Pollutant[1])
                  
                  # Listing instrument and updating ui
                  Data$Name.Instrument <- unique(Data$DT[Pollutant == Data$Name.Pollutant[1]]$Instrument)
                  if(length(Data$Name.Instrument) == 0){
                    # MESSAGE NO Pollutant column
                    my_message <- paste0("[GDE_App, Data] WARNING, column \"Instrument\" missing in the data file. New column \"Instrument\" set to \"unknow\"")
                    cat(my_message)
                    shinyalert::shinyalert(title = "WARNING data file",
                                           text = my_message,
                                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                           html = FALSE,
                                           type = "warning",
                                           showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                           timer             = 5000,
                                           imageUrl          = "",
                                           animation         = FALSE)
                    
                    # opening window for selecting data again
                    Data$DT[, Instrument :=  rep("unknown", nrow(Data$DT))]
                  }
                  
                  output$uiInstrument <- shiny::renderUI({
                    shinyWidgets::pickerInput(
                      inputId  = "Instrument",
                      label    = "Instrument",
                      choices  = Data$Name.Instrument,
                      selected = Data$Name.Instrument[1],
                      options  = list(
                        title = "Choose INstrument"))})
                  
                  # updating list of instruments and choice of Averaging time if input$Pollutant changes
                  observeEvent(input$Pollutant,{
                    
                    if(input$Pollutant == "CO"){
                      shinyWidgets::updatePickerInput(
                        session = session,
                        inputId  = "Averaging.Period",
                        selected = "24hour",
                        choices = c("8hour", "24hour"))
                    } else if(input$Pollutant %in% c("NO2", "SO2")){
                      shinyWidgets::updatePickerInput(
                        session = session,
                        inputId  = "Averaging.Period",
                        selected = "1hour",
                        choices = c("1hour", "24hour", "1year"))
                    } else if(input$Pollutant %in% c("PM10", "PM2.5")){
                      shinyWidgets::updatePickerInput(
                        session = session,
                        inputId  = "Averaging.Period",
                        selected = "24hour",
                        choices = c("24hour", "1year"))
                    } else if(input$Pollutant == "O3"){
                      shinyWidgets::updatePickerInput(
                        session = session,
                        inputId  = "Averaging.Period",
                        selected = "24hour",
                        choices = c("8hour", "24hour"))
                    } else if(input$Pollutant %in% c("PM10", "PM2.5")){
                      shinyWidgets::updatePickerInput(
                        session = session,
                        inputId  = "Averaging.Period",
                        selected = "24hour",
                        choices = c("24hour", "1year"))
                    }
                    
                    # If Pollutant is modified, it has an effect on the selection of Instrument
                    shiny::req(Data$DT)
                    Data$Name.Instrument <- unique(Data$DT[Pollutant == input$Pollutant]$Instrument)
                    shinyWidgets::updatePickerInput(session = session,
                                                    inputId = "Instrument",
                                                    choices  = Data$Name.Instrument,
                                                    selected = Data$Name.Instrument[1])
                  }, ignoreNULL = TRUE, ignoreInit = TRUE)
                  
                  # Listing Campaigns and removing seasons
                  if(!"Campaign" %in% names(Data$DT) || length(grep("Campaign",names(Data$DT))) != 1){
                    # MESSAGE NO Campaing column
                    my_message <- paste0("[GDE_App, Data] ERROR, column \"Campaign\"missing in the data file. Reload correct data. The App may crash")
                    cat(my_message)
                    shinyalert::shinyalert(title = "ERROR data file",
                                           text = my_message,
                                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                           html = FALSE,
                                           type = "error",
                                           showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                           timer             = 5000,
                                           imageUrl          = "",
                                           animation         = FALSE)
                    # opening window for selecting data again
                    shinyjs::click("chooseFile")
                  } else {
                    
                    # Checking if "Season" is included in "Campaign"
                    if(any(grepl(pattern = paste(c("Winter","winter", "Summer","summer"), collapse = "|"), Data$DT$Campaign))){
                      
                      #rows to change
                      n.Season <- grep(pattern = paste(c("Winter","winter", "Summer","summer"), collapse = "|"), Data$DT$Campaign)
                      
                      # creating new column Season
                      data.table::set(Data$DT, i = n.Season, j = "Season", value = unlist(regmatches(Data$DT$Campaign[n.Season], gregexpr("(?i)(winter|summer)", Data$DT$Campaign[n.Season], perl = TRUE))))
                      
                      # Deleting Season from Instrument
                      data.table::set(Data$DT, i = n.Season, j = "Campaign", value = gsub(pattern = paste(c("Winter","winter", "Summer","summer"), collapse = "|"), replacement = "", Data$DT$Campaign[n.Season]))
                      
                      # MESSAGE Season created
                      my_message <- paste0("[GDE_App, Data] WARNING, column \"Campaign\" includes both Campaign site and season information. Season information is moved to new columns \"Season\"")
                      cat(my_message)
                      shinyalert::shinyalert(title = "WARNING data file",
                                             text = my_message,
                                             closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                             html = FALSE,
                                             type = "warning",
                                             showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                             timer             = 10000,
                                             imageUrl          = "",
                                             animation         = FALSE)
                    }
                    
                    # Updating data for plotting and data treatment
                    Data.DT <- shiny::reactive({
                      shiny::req(shiny::isTruthy(input$Instrument))
                      
                      # Averaging RM if needed
                      if(!"RM" %in% names(Data$DT)){
                        if(length(Data$Name.RM) > 1){
                          Data$DT[, RM := rowMeans(Data$DT[,.SD,.SDcols = Data$Name.RM], na.rm = TRUE)]
                          if(length(Data$Name.RM) == 2) Data$DT[, RM_DELTA := Data$DT[[Data$Name.RM[1]]] - Data$DT[[Data$Name.RM[2]]]]
                        } else {
                          Data$DT[, RM := Data$DT[[Data$Name.RM]]]
                        }
                      }
                      
                      # Adding delta of CM
                      if(length(Data$Name.CM) == 2){
                        Data$DT[, CM_DELTA := Data$DT[[Data$Name.CM[1]]] - Data$DT[[Data$Name.CM[2]]]]}
                      # Delta for Data$Name.CM to RM
                      for(CM in Data$Name.CM){
                        Data$DT[, (paste0(CM,"_DELTA")) :=  Data$DT[[CM]] - Data$DT$RM]}
                      
                      # Returning only data for Instrument and Pollutant
                      return(Data$DT[Instrument == input$Instrument & Pollutant == input$Pollutant])
                    })
                    
                    # Table of data rhandsometable
                    output$hot <- rhandsontable::renderRHandsontable({
                      shiny::req(Data.DT())
                      rhandsontable::rhandsontable(data = Data.DT(), stretchH = "all") # , useTypes = FALSE
                    })
                    
                    # Scatterplot of raw data ####
                    #output$Scatterplot1 <- renderPlot(Plot.Scatterplot1()$Ggplot , width = 'auto', height = function() {session$clientData$output_Scatterplot1_height - 40})
                    output$Scatterplot  <- renderPlot(Plot.Scatterplot())
                    #  Reactive FUN for Scatterplot
                    Plot.Scatterplot <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.Scatterplot] INFO, Plotting scatter plot of candidate/sensor data", value = 0.5)
                      
                      futile.logger::flog.info(paste0("[GDE_App, Plot.Scatterplot] Scatterplot of CM1 or CM1 and CM2 vs RM for Instrument ", input$Instrument))
                      
                      progress$set(message = "[GDE_App, Plot.Scatterplot] INFO, plotting scatter plot of candidate/sensor data", value = 1)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      if(input$Type == "Fixed type testing"){
                        return(gridExtra::grid.arrange(Plot.Scatterplot1()$Ggplot, Plot.Scatterplot2()$Ggplot, nrow = 1, ncol = 2))
                      } else if(input$Type == "Fixed on-going"){
                        return(Plot.Scatterplot1()$Ggplot)
                      } else if(input$Type == "Indicative"){
                        
                      } 
                    })
                    Plot.Scatterplot1 <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.Scatterplot1] INFO, Pplotting scatter plot of candidate/sensor data", value = 0.5)
                      
                      futile.logger::flog.info(paste0("[GDE_App, Plot.Scatterplot1] Scatterplot of CM1 vs RM for Instrument ", input$Instrument))
                      # next in case no data to be calibrated
                      if(is.null(Data.DT()[[Data$Name.CM[1]]]) || all(is.na(Data.DT()[[Data$Name.CM[1]]]))) {
                        
                        my_message <- paste0("[GDE_App, Plot.Scatterplot1] Warn, No candidate/indicative data for CM1 of Instrument ", input$Instrument, "\n")
                        cat(my_message)
                        shinyalert::shinyalert(title = "ERROR Scatterplot1 data",
                                               text = my_message,
                                               closeOnEsc = TRUE,
                                               closeOnClickOutside = TRUE,
                                               html = FALSE,
                                               type = "error",
                                               showConfirmButton = TRUE,
                                               showCancelButton  = FALSE,
                                               confirmButtonText = "OK",
                                               confirmButtonCol  = "#AEDEF4",
                                               timer             = 5000,
                                               imageUrl          = "",
                                               animation         = FALSE)
                      } else {
                        
                        EtalLim <- Etalonnage( x = Data.DT()[["RM"]],
                                               s_x = NULL,
                                               y = Data.DT()[[Data$Name.CM[1]]],
                                               s_y = NULL,
                                               AxisLabelX = paste0("Reference data in ", input$unit.ref, " (averaged in case of multiple references)"),
                                               AxisLabelY = "CM1",
                                               Title = paste0("CM1 of instrument ", input$Instrument," from ",
                                                              format(Data.DT()[1]$date,"%Y-%m-%d")," to ",format(Data.DT()[.N]$date,"%Y-%m-%d")), 
                                               Marker = 1,
                                               Couleur = "blue",
                                               ligne = 'p',
                                               XY_same = TRUE,
                                               lim     = NULL,
                                               steps = c(10,10),
                                               digitround = c(1,1),
                                               Ggplot = TRUE,
                                               Sites = Data.DT()$Campaign)}
                      
                      progress$set(message = "[GDE_App, Plot.Scatterplot1] INFO, lotting scatter plot of candidate/sensor data", value = 1)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      if (exists("EtalLim")) return(EtalLim)
                    })
                    Plot.Scatterplot2 <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.Scatterplot1] INFO, Plotting scatter plot of candidate/sensor data", value = 0.5)
                      
                      futile.logger::flog.info(paste0("[GDE_App, Plot.Scatterplot1] Scatterplot of CM2 vs RM for Instrument ", input$Instrument))
                      # next in case no data to be calibrated
                      if(is.null(Data.DT()[[Data$Name.CM[2]]]) || all(is.na(Data.DT()[[Data$Name.CM[2]]]))) {
                        
                        my_message <- paste0("[GDE_App, Plot.Scatterplot1] Warn, No candidate/indicative data for CM2 of Instrument ", input$Instrument, "\n")
                        cat(my_message)
                        shinyalert::shinyalert(title = "ERROR Scatterplot1 data",
                                               text = my_message,
                                               closeOnEsc = TRUE,
                                               closeOnClickOutside = TRUE,
                                               html = FALSE,
                                               type = "error",
                                               showConfirmButton = TRUE,
                                               showCancelButton  = FALSE,
                                               confirmButtonText = "OK",
                                               confirmButtonCol  = "#AEDEF4",
                                               timer             = 5000,
                                               imageUrl          = "",
                                               animation         = FALSE)
                      } else {
                        
                        EtalLim <- Etalonnage( x = Data.DT()[["RM"]],
                                               s_x = NULL,
                                               y = Data.DT()[[Data$Name.CM[2]]],
                                               s_y = NULL,
                                               AxisLabelX = paste0("Reference data in ", input$unit.ref, " (averaged in case of multiple references)"),
                                               AxisLabelY = "CM2",
                                               Title = paste0("CM2 of instrument ", input$Instrument," from ",
                                                              format(Data.DT()[1]$date,"%Y-%m-%d")," to ",format(Data.DT()[.N]$date,"%Y-%m-%d")), 
                                               Marker = 1,
                                               Couleur = "blue",
                                               ligne = 'p',
                                               XY_same = TRUE,
                                               lim = NULL,
                                               steps = c(10,10),
                                               digitround = c(1,1),
                                               Ggplot = TRUE,
                                               Sites = Data.DT()$Campaign)}
                      
                      progress$set(message = "[GDE_App, Plot.Scatterplot1] INFO, lotting scatter plot of candidate/sensor data", value = 1)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      if (exists("EtalLim")) return(EtalLim)
                    })
                    
                    # Time series of data
                    output$ts_dygraphs <- renderUI(Plot.ts_dygraphs())
                    Plot.ts_dygraphs <- shiny::reactive({
                      
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.ts_dygraphs] INFO, plotting Candidate/indicative data with reference data", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      cat("\n-----------------------------------------------------------------------------------\n")
                      futile.logger::flog.info("[GDE_App, ts_dygraphs] INFO, plotting time series of Candidate/indicative measurements vs Reference measurements")
                      # Checking that there are data to plot
                      if(!shiny::isTruthy(Data.DT()[, .SD, .SDcols = c(Data$Name.CM, Data$Name.RM)])){
                        
                        my_message <- paste0("[GDE_App, ts_dygraphs] ERROR, All or some CM/sensor time series are empty, not plotting any times series\n")    
                        futile.logger::flog.warn(my_message)
                        shinyalert::shinyalert(title = "ERROR no data to plot",
                                               text = my_message,
                                               closeOnEsc = FALSE,
                                               closeOnClickOutside = FALSE,
                                               html = FALSE,
                                               type = "error",
                                               showConfirmButton = TRUE,
                                               showCancelButton  = FALSE,
                                               confirmButtonText = "OK",
                                               confirmButtonCol  = "#AEDEF4",
                                               timer             = 5000,
                                               imageUrl          = "",
                                               animation         = FALSE)
                      } else {
                        # plotting time series
                        futile.logger::flog.info(paste0("[GDE_App, ts_dygraphs] INFO, plotting Candidate/indicative data with reference data, data available"))
                        
                        # plotting time series
                        # avoid checking same tz
                        options(xts_check_TZ = FALSE)
                        time_series <- threadr::data_frame_to_timeseries(
                          as.data.frame(Data.DT()[, .SD, .SDcols = c(Data$Name.Date, c(Data$Name.CM, Data$Name.RM))]), tz = "UTC")
                        colour_vector <- colour_vector[1:length(c(Data$Name.CM, Data$Name.RM))]
                        
                        ## Make interactive time-series plot
                        # Define Height of the Combined plots
                        Height <- paste0(round(1/length(time_series) * 83),"vh")
                        #initialize list
                        plot_ts_CM_RM <- list()
                        for (i in seq_along(time_series)) {
                          ts_CM_RM <- time_series[[i]]
                          plot_ts  <- dygraphs::dygraph(ts_CM_RM, group = "Sensors", height = Height, width = "100%") %>% #
                            dygraphs::dySeries(label = names(time_series)[i], color = colour_vector[i]) %>%
                            dygraphs::dyAxis("y", label = names(time_series)[i]) %>%
                            dygraphs::dyRangeSelector(height = 10) %>%
                            dygraphs::dyOptions(useDataTimezone = TRUE)
                          #dyOptions(labelsUTC = T) %>%  set time zone to UTC
                          plot_ts_CM_RM[[i]] <- plot_ts  #add each element to list
                        }
                        cat("-----------------------------------------------------------------------------------\n\n")
                        
                        progress$set(message = "[GDE_App, Plot.ts_dygraphs] INFO, plotting Candidate/indicative data with reference data", value = 1)
                        # render the dygraphs objects using htmltools
                        plot_ts_CM_RM <- htmltools::tagList(plot_ts_CM_RM)
                        return(htmltools::tagList(plot_ts_CM_RM))}})
                    
                    # Menu Statistics ####
                    # Body Tab "Reference data" ----
                    output$VerifReference <- renderPlot(Plot.VerifReference()$combined_plot) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    Plot.VerifReference <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.VerifReference] INFO, Verifying Reference data", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      futile.logger::flog.info(paste0("[GDE_App, Plot.VerifReference] Verifying Reference data for Instrument ", input$Instrument))
                      
                      # Checking availability of complete data
                      if(nrow(Data.DT()[is.finite(rowSums(Data.DT()[,.SD,.SDcols = Data$Name.RM]))]) < as.numeric(input$min.N)){
                        
                        # MESSAGE Not enough RM data
                        my_message <- paste0("[GDE_App, VerifReference] ERROR, not enough pairs of valid RM data. Best would be to reload data or decrease min.N. The App may crash")
                        cat(my_message)
                        shinyalert::shinyalert(title = "ERROR number of RM data pairs",
                                               text = my_message,
                                               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                               html = FALSE,
                                               type = "error",
                                               showConfirmButton = TRUE, showCancelButton  = FALSE, confirmButtonText = "OK", confirmButtonCol  = "#AEDEF4",
                                               timer             = 10000,
                                               imageUrl          = "",
                                               animation         = FALSE)
                        # # opening window for selecting data again
                        # shinyjs::click("chooseFile")
                        
                        return(list(
                          combined_plot  = NULL,
                          ubs_rm         = NULL,
                          mean_diff      = NULL
                        ))
                      } else {
                        # Preparing plots
                        return(create_reference_plots(df = Data.DT(),
                                                      Type = input$Type,
                                                      Unit = input$unit.ref,
                                                      Name.RM = Data$Name.RM,
                                                      i.Ref.outliers = i.Ref.outliers(),
                                                      Max.Ref.Bias = as.numeric(input$Max.Ref.Bias), 
                                                      min.N = as.numeric(input$min.N),
                                                      Max.ubsRM = as.numeric(input$Max.ubsRM)))  
                      }
                    })
                    
                    # Body Tab " Filtered Reference" ----
                    # Index of RM outliers for Data.DT()
                    i.Ref.outliers <- shiny::reactive({
                      i.Ref.outliers <- Data.DT()[abs(RM_DELTA) > as.numeric(input$Max.Ref.Bias), which = TRUE]
                      if(length(i.Ref.outliers) > 0) Data.DT()[i.Ref.outliers, outlier := paste0("> ", input$Max.Ref.Bias)]
                      Data.DT()[setdiff(1:.N,i.Ref.outliers), outlier := paste0("≤ ", input$Max.Ref.Bias)]
                      return(i.Ref.outliers)
                    })
                    output$hot.RM.Filtered <- rhandsontable::renderRHandsontable({
                      
                      shiny::req(Data.DT())
                      
                      
                      # https://stackoverflow.com/questions/39752455/changing-background-color-of-several-rows-in-rhandsontable
                      # https://github.com/jrowen/rhandsontable/issues/116
                      myIndex <- i.Ref.outliers() - 1 # it seems that index 0 is the fist one + there is an error with the non complete rows #########################################################################
                      # Convert myIndex to JavaScript array string format
                      myIndexJS <- paste0("[", paste(myIndex, collapse = ", "), "]")
                      
                      
                      # The good solution with ChatGPT: how to change the color of rows in rhandsometable + I want to change the color of row number given in a vector myIndex
                      rhandsontable::rhandsontable(data = Data.DT(), stretchH = "all") %>% # [is.finite(rowSums(Data.DT()[, .SD,.SDcols = c(Data$RM, Data$Name.CM)]))]
                        hot_cols(renderer = sprintf("
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          var myIndex = %s;
          if (myIndex.includes(row)) {
            td.style.backgroundColor = 'lightcoral';
          }
        }
        ", myIndexJS)
                        )
                    })
                    
                    # Body Tab item "Ubs ----
                    # df_ubs: datatable per bin with ubs for RM and CM
                    df_ubs <- shiny::reactive({
                      # Computing bins for ubsCM discarding any NA for Data$CM. No outliers discarding
                      ubs.CM <- DT_Weighing(DT = Data.DT(),
                                            name.date = "date", name.V1 = Data$Name.CM[1], name.V2 = Data$Name.CM[2],
                                            Lag = as.numeric(input$Bin.Width), 
                                            outliers = FALSE, Verbose = TRUE)$DT.weighted
                      # Computing bins for ubsRM only with RM data discarded for outliers, and discarding any NA for Data$CM and Data$RM
                      ubs.RM <- DT_Weighing(DT = Data.DT()[setdiff(1:.N, i.Ref.outliers())],
                                            name.date = "date", name.V1 = Data$Name.RM[1], name.V2 = Data$Name.RM[2],
                                            Lag = as.numeric(input$Bin.Width), 
                                            outliers = FALSE, Verbose = TRUE)$DT.weighted
                      return(merge(ubs.RM, ubs.CM, by = "Class", all = T))
                    })
                    # reactive table of ubs per bin for RM and CM
                    # Computung ubsRM and ubsCM from df_ubs. Preparing for info icons ubsRM and ubsCM
                    ubs <- shiny::reactive({
                      
                      # CM, using Lags LV.Interval % around LV with at least min.Count measurements, row to consider
                      n.Rows <- df_ubs()[Count.y >= as.numeric(input$min.Count) &
                                           Class + as.numeric(input$Bin.Width)/2 <= (1+as.numeric(input$LV.Interval)) * SelectDQO()$LV & 
                                           Class + as.numeric(input$Bin.Width)/2 >= (1-as.numeric(input$LV.Interval)) * SelectDQO()$LV, which = T]
                      if(length(n.Rows) > 0){
                        ubsCM <- median(df_ubs()[n.Rows]$ubs.y, na.rm = T)
                      } else ubsCM <- NA
                      if(!is.na(ubsCM) && ubsCM <= as.numeric(input$Max.ubsCM)){
                        ubsCM.Icon <- icon("thumbs-up", lib = "glyphicon")
                        ubsCM.Color <- "green"
                      } else {
                        ubsCM.Icon <- icon("thumbs-down", lib = "glyphicon")
                        ubsCM.Color <- "red"
                      }  
                      
                      # RM ######################################################################## What is the correct method to compute GDE of per lags? (two methods are proposed below#######################################################################
                      RM_DELTA <- Data.DT()[setdiff(1:.N, i.Ref.outliers())]$RM_DELTA
                      ubsRM <- sqrt(sum(RM_DELTA^2, na.rm = T) /(2 * length(RM_DELTA[!is.na(RM_DELTA)])))
                      ubsRM <- median(df_ubs()$ubs.x, na.rm = T)
                      if(!is.na(ubsRM) && ubsRM < as.numeric(input$Max.ubsRM)){
                        ubsRM.Icon <- icon("thumbs-up", lib = "glyphicon")
                        ubsRM.Color <- "green"
                      } else {
                        ubsRM.Icon <- icon("thumbs-down", lib = "glyphicon")
                        ubsRM.Color <- "red"
                      }
                      
                      return(list(ubsCM = ubsCM, ubsCM.Icon = ubsCM.Icon, ubsCM.Color = ubsCM.Color,
                                  ubsRM = ubsRM, ubsRM.Icon = ubsRM.Icon, ubsRM.Color = ubsRM.Color))
                    })
                    output$ubs <- renderPlot(Plot.ubs()) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    Plot.ubs <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.Ubs] INFO, Check ubsRM and ubsCM dependance on concentration level for ", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      futile.logger::flog.info(paste0("[GDE_App, Plot.Ubs] INFO, Check ubsRM and ubsCM dependance on concentration levels for ", input$Instrument))
                      
                      # Preparing plots and Return 1 combined plot
                      return(ubs_plots(df_ubs= df_ubs(),
                                       Instrument = input$Instrument,
                                       Pollutant = input$Pollutant,
                                       Unit = input$unit.ref,
                                       bin_width = as.numeric(input$Bin.Width),
                                       SelectDQO = SelectDQO(),
                                       min.Count = as.numeric(input$min.Count), LV.Interval = as.numeric(input$LV.Interval)))
                    })
                    
                    # Body Tab item "Linearity Check ----
                    output$LinearityCheck <- renderPlot(LinearityCheck()$final_plot)
                    LinearityCheck <- shiny::reactive({
                      
                      # avoid Hyper reactivity
                      shiny::req(input$tabStat1 == "Linearity check")
                      
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.LinearityCheck] INFO, Plotting Linearity Check", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      # Preparing plots and Return combined plot. RM outliers, incomplete RM and CM discarded. Check I think incomplete CM should be kept ######################################################################
                      return(
                        linearity_check(
                          df = Data.DT()[setdiff(1:.N, i.Ref.outliers())][is.finite(rowSums(Data.DT()[setdiff(1:.N, i.Ref.outliers())][,.SD,.SDcols =c(Data$Name.CM, Data$Name.RM)]))],
                          Type = input$Type, # "Fixed on-going" or indicative
                          r2_threshold = as.numeric(input$Min.R2),
                          sd_rel_threshold = as.numeric(input$Max.Rel.SD),
                          LV_list = SelectDQO(),
                          Name.CM = Data$Name.CM, Name.RM = Data$Name.RM))
                    })
                    
                    # Body Tab item "CM_Raw" ####
                    # Sub Body Tab item "Differences" ----
                    output$CM_Raw <- renderPlot(Plot.CM_Raw()) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    Plot.CM_Raw <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.CM_Raw] INFO, visual check of CM_Raw level differences", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      futile.logger::flog.info(paste0("[GDE_App, Plot.Ubs] INFO, visual check of CM_Raw level differences for ", input$Instrument))
                      Data.DT_noRM <- Data.DT()[setdiff(1:.N, i.Ref.outliers())]
                      Data.DT()[setdiff(1:.N, i.Ref.outliers())][is.finite(rowSums(Data.DT()[setdiff(1:.N, i.Ref.outliers())][,.SD,.SDcols = Data$Name.CM]))]
                      
                      # Preparing plots and Return 1 combined plot, filter Reference with delta > target
                      return(
                        create_cm_analysis_plots(df = Data.DT()[setdiff(1:.N, i.Ref.outliers())][is.finite(rowSums(Data.DT()[setdiff(1:.N, i.Ref.outliers())][,.SD,.SDcols = Data$Name.CM]))],
                                                 Name.CM = Data$Name.CM, Name.SN = Data$Name.SN, Name.RM = Data$Name.RM,
                                                 Type =  input$Type, low_thr = as.numeric(input$Max.Ref.Bias)))
                    })
                    
                    # Sub Body Tab "OLS Diagnostics" ----
                    CM_Raw <- shiny::reactive({
                      
                      shiny::req(input$Type %in% c("Fixed type testing", "Indicative type testing"))
                      
                      # Initialising return list
                      U_orth_DF <- list()
                      
                      # Selecting regression methods for test
                      Tested.Models <- c("OLS", "Deming", "TLS")
                      
                      # Selecting RM and CM in long format
                      DT <- melt(
                        Data.DT()[setdiff(1:.N, i.Ref.outliers())][,.SD, .SDcols =c("date", "Campaign", "RM", Data$Name.CM, Data$Name.SN)],
                        id.vars = c("date", "RM", "Campaign"),
                        measure.vars = patterns("^CM", "^SN"),
                        value.name = c("CM", "SN"),
                        variable.name = "Cm_Type")
                      DT[, Cm_Type := NULL]
                      data.table::setnames(DT, c("date","RM", "CM"), c("Date", "xis", "yis"))
                      
                      # Default regresson method of GDE for candidate method and of sensor of TS 17660
                      if(input$Type == "Fixed type testing") {
                        Regression = "TLS"
                      } else Regression = "OLS"
                      
                      U_orth_DF <- U_orth_DF(Mat = DT, Regression = Regression, Tested.Models = Tested.Models,
                                             variable.ubsRM = input$Variable.ubsRM, ubsRM = ubs()$ubsRM, perc.ubsRM = 0.02,
                                             variable.ubss  = input$Variable.ubsCM, ubss  = ubs()$ubsCM, perc.ubss  = NULL, Add.ubss = FALSE,
                                             Fitted.RS = as.logical(input$Fitted.RS), Forced.Fitted.RS = FALSE, ID = NULL,
                                             Verbose = FALSE, Keep.Cols = TRUE)
                      
                      # Adding CM corrected values to Data.DT, computing differences between corrected CM and differences to RM for each CM corrected
                      CM_Corrected <- copy(Data.DT())
                      for(Model in Tested.Models){
                        for(CM in Data$Name.CM){
                          CM_Corrected[,  paste0(CM, "_",Model)           := Meas_Function(y =  CM_Corrected[[CM]], Mod_type = "Linear", Model = U_orth_DF[[Model]], Verbose = TRUE)]
                          CM_Corrected[,  paste0(CM, "_",Model, "_DELTA") := CM_Corrected[[paste0(CM, "_",Model)]] - CM_Corrected[["RM"]]]
                        }
                        CM_Corrected[, paste0("CM_",Model,"_DELTA") := CM_Corrected[[paste0(Data$Name.CM[1], "_",Model)]] - CM_Corrected[[paste0(Data$Name.CM[2], "_",Model)]]]
                      }
                      
                      return(list(CM_Corrected = CM_Corrected, U_orth_DF = U_orth_DF))
                    })
                    output$Raw.OLS.Models <- renderPlot(CM_Raw()$U_orth_DF$OLS.Models)
                    
                    # Sub Body Tab "Orthogonal" ----
                    output$TLS.Scatterplot <- renderPlot(CM_Raw()$U_orth_DF$Scatter)
                    output$TLS.Residuals <-   renderPlot(CM_Raw()$U_orth_DF$Resid)
                    
                    # Sub Body Tab "Regression Lines"
                    output$Table.Reg.Lines.Raw <- DT::renderDataTable({
                      datatable(
                        CM_Raw()$U_orth_DF$Lin.Reg,
                        options = list(
                          dom = 't'  # Only show table, hide other elements
                        )
                      )
                    })
                    
                    # Sub Body Tab "U per SNi all sites" ----
                    U.CM_Raw.All <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, U.CM_Raw.All] INFO, Uncertainty", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      futile.logger::flog.info(paste0("[GDE_App, U.CM_Raw.All] INFO, Uncertainty plot for ", input$Instrument))
                      
                      return(
                        U.by.Model(CM_Corrected = CM_Raw(),
                                   Name.CM = Data$Name.CM,
                                   Model_Type = "Raw",
                                   LV.Interval = as.numeric(input$LV.Interval.U),
                                   min.N = as.numeric(input$min.N.U),
                                   SelectDQO = SelectDQO(),
                                   unit.ref = input$unit.ref,
                                   All.Campaigns = TRUE,
                                   Fitted.RS = as.logical(input$Fitted.RS))
                      )
                    })
                    output$Plot.U.CM_Raw.All <- renderPlot(U.CM_Raw.All()$GGPLOT) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    output$Table.U.CM_Raw.All <- renderDT({
                      datatable(as.data.frame(U.CM_Raw.All()$CM_Corrected$U_orth_DF$Mat)[sort(unique(unlist(U.CM_Raw.All()$Lengths$N.LV))),
                                                                                         setdiff(names(U.CM_Raw.All()$CM_Corrected$U_orth_DF$Mat),
                                                                                                 c("Date","ubss","Weights", "dens", "col", "Max.ubsRM", "Max.RSD", "Rel.RSS", "Rel.bias", "Ur", "fitted", "bias", "residuals", "RS",
                                                                                                   "GDE.fitted", "GDE.bias", "GDE.RD"))],
                                rownames = FALSE,
                                options = list(
                                  pre = list(
                                    filter = list(
                                      list(  # First filter condition
                                        column = 10,  # Column index (0-based) SNi
                                        value = "SN1"  # Filter value
                                      )
                                    )
                                  ),
                                  pageLength = 18  # This is the correct way to set the number of rows
                                ))
                    })
                    
                    # Sub Body Tab "U per SNi and per site ----
                    U.CM_Raw.Sites <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, U.CM_Raw.Sites] INFO, Uncertainty", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      futile.logger::flog.info(paste0("[GDE_App, U.CM_Raw.Sites] INFO, Uncertainty plot for ", input$Instrument))
                      
                      return(
                        U.by.Model(CM_Corrected = CM_Raw(),
                                   Name.CM = Data$Name.CM,
                                   Model_Type = "Raw",
                                   LV.Interval = as.numeric(input$LV.Interval.U),
                                   min.N = as.numeric(input$min.N.U),
                                   SelectDQO = SelectDQO(),
                                   unit.ref = input$unit.ref,
                                   All.Campaigns = FALSE,
                                   Fitted.RS = as.logical(input$Fitted.RS))
                      )
                    })
                    output$U.CM_Raw.Sites <- renderPlot(U.CM_Raw.Sites()$GGPLOT)
                    output$Table.U.CM_Raw.Sites <- renderDT({
                      datatable(as.data.frame(U.CM_Raw.Sites()$CM_Corrected$U_orth_DF$Mat)[sort(unique(unlist(U.CM_Raw.Sites()$Lengths$N.LV))),
                                                                                           setdiff(names(U.CM_Raw.All()$CM_Corrected$U_orth_DF$Mat),
                                                                                                   c("Date","ubss","Weights", "dens", "col", "Max.ubsRM", "Max.RSD", "Rel.RSS", "Rel.bias", "Ur", "fitted", "bias", "residuals", "RS",
                                                                                                     "GDE.fitted", "GDE.bias", "GDE.RD"))],
                                rownames = FALSE,
                                options = list(
                                  pageLength = 18  # This is the correct way to set the number of rows
                                ))
                    })
                    
                    # Sub Body Tab "U per SNi and per site ----
                    output$Table.U.CM_Raw.SNi <- renderTable({
                      as.matrix(
                        U.CM_Raw.All()$Lengths[, .SD,.SDcols = names(U.CM_Raw.All()$Lengths)[names(U.CM_Raw.All()$Lengths) != "N.LV"]])
                    }, digits = 2)
                    output$Table.U.CM_Raw.Sites.SNi <- renderTable({
                      as.matrix(
                        U.CM_Raw.Sites()$Lengths[, .SD,.SDcols = names(U.CM_Raw.Sites()$Lengths)[names(U.CM_Raw.Sites()$Lengths) != "N.LV"]])
                    }, digits = 2)
                    
                    
                    
                    # Body Tab item "CM_Orth" ####
                    # Sub Body Tab item "Differences" ----
                    output$CM_TLS <- renderPlot(Plot.CM_TLS()) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    Plot.CM_TLS <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, Plot.CM_OLS] INFO, visual check of CM_Orth level differences", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      futile.logger::flog.info(paste0("[GDE_App, Plot.Ubs] INFO, visual check of CM_Orth level differences for ", input$Instrument))
                      
                      return(
                        create_cm_analysis_plots(df = CM_Raw()$CM_Corrected[setdiff(1:.N, i.Ref.outliers())][is.finite(rowSums(Data.DT()[setdiff(1:.N, i.Ref.outliers())][,.SD,.SDcols = Data$Name.CM]))],
                                                 Name.CM =  paste0(Data$Name.CM, "_TLS"), Name.SN = Data$Name.SN, Name.RM = Data$Name.RM,
                                                 Type =  input$Type, low_thr = as.numeric(input$Max.Ref.Bias)))
                    })
                    
                    # Sub Body Tab "OLS Diagnostics" ----
                    CM_Orth <- shiny::reactive({
                      
                      shiny::req(input$Type %in% c("Fixed type testing", "Indicative type testing"))
                      
                      # Initialising return list
                      U_orth_DF <- list()
                      
                      # Selecting regression methods for test
                      Tested.Models <- c("OLS", "Deming", "TLS")
                      
                      # Selecting RM and CM in long format
                      DT <- melt(
                        CM_Raw()$CM_Corrected[setdiff(1:.N, i.Ref.outliers())][,.SD, .SDcols =c("date", "Campaign", "RM", paste0(Data$Name.CM, "_TLS"), Data$Name.SN)],
                        id.vars = c("date", "RM", "Campaign"),
                        measure.vars = patterns("^CM", "^SN"),
                        value.name = c("CM", "SN"),
                        variable.name = "Cm_Type")
                      DT[, Cm_Type := NULL]
                      data.table::setnames(DT, c("date","RM", "CM"), c("Date", "xis", "yis"))
                      
                      # Default regresson method of GDE for candidate method and of sensor of TS 17660
                      if(input$Type == "Fixed type testing") {
                        Regression = "TLS"
                      } else Regression = "OLS"
                      
                      U_orth_DF <- U_orth_DF(Mat = DT, Regression = Regression, Tested.Models = Tested.Models,
                                             variable.ubsRM = input$Variable.ubsRM, ubsRM = ubs()$ubsRM, perc.ubsRM = 0.02,
                                             variable.ubss  = input$Variable.ubsCM, ubss  = ubs()$ubsCM, perc.ubss  = NULL, Add.ubss = FALSE,
                                             Fitted.RS = as.logical(input$Fitted.RS), Forced.Fitted.RS = FALSE, ID = NULL,
                                             Verbose = FALSE, Keep.Cols = TRUE)
                      
                      # Adding CM corrected values not needed as data are already corrected
                      CM_Corrected <- copy(Data.DT())
                      for(Model in Tested.Models){
                        for(CM in Data$Name.CM){
                          CM_Corrected[,  paste0(CM, "_",Model)           := Meas_Function(y =  CM_Corrected[[CM]], Mod_type = "Linear", Model = U_orth_DF[[Model]], Verbose = TRUE)]
                          CM_Corrected[,  paste0(CM, "_",Model, "_DELTA") := CM_Corrected[[paste0(CM, "_",Model)]] - CM_Corrected[["RM"]]]
                        }
                        CM_Corrected[, paste0("CM_",Model,"_DELTA") := CM_Corrected[[paste0(Data$Name.CM[1], "_",Model)]] - CM_Corrected[[paste0(Data$Name.CM[2], "_",Model)]]]
                      }
                      
                      return(list(CM_Corrected = CM_Corrected, U_orth_DF = U_orth_DF))
                    })
                    output$TLS.OLS.Models <- renderPlot(CM_Orth()$U_orth_DF$OLS.Models)
                    
                    # Sub Body Tab "Orthogonal" ----
                    output$TLS.TLS.Scatterplot <- renderPlot(CM_Orth()$U_orth_DF$Scatter)
                    output$TLS.TLS.Residuals <-   renderPlot(CM_Orth()$U_orth_DF$Resid)
                    
                    # Sub Body Tab "Regression Lines"
                    output$Table.Reg.Lines.TLS <- DT::renderDataTable({
                      datatable(
                        CM_Orth()$U_orth_DF$Lin.Reg,
                        options = list(
                          dom = 't'  # Only show table, hide other elements
                        )
                      )
                    })
                    
                    # Sub Body Tab "U per SNi all sites" ----
                    U.CM_Orth.All <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, U.CM_Orth.All] INFO, Uncertainty", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      futile.logger::flog.info(paste0("[GDE_App, U.CM_Orth.All] INFO, Uncertainty plot for ", input$Instrument))
                      
                      return(
                        U.by.Model(CM_Corrected = CM_Orth(),
                                   Name.CM = Data$Name.CM,
                                   Model_Type = "TLS",
                                   LV.Interval = as.numeric(input$LV.Interval.U),
                                   min.N = as.numeric(input$min.N.U),
                                   SelectDQO = SelectDQO(),
                                   unit.ref = input$unit.ref,
                                   All.Campaigns = TRUE,
                                   Fitted.RS = as.logical(input$Fitted.RS))
                      )
                    })
                    output$Plot.U.CM_Orth.All <- renderPlot(U.CM_Orth.All()$GGPLOT) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    output$Table.U.CM_Orth.All <- renderDT({
                      datatable(as.data.frame(U.CM_Orth.All()$CM_Corrected$U_orth_DF$Mat)[sort(unique(unlist(U.CM_Orth.All()$Lengths$N.LV))),
                                                                                          setdiff(names(U.CM_Orth.All()$CM_Corrected$U_orth_DF$Mat),
                                                                                                  c("Date", "ubss","Weights", "dens", "col", "Max.ubsRM", "Max.RSD", "Rel.RSS", "Rel.bias", "Ur"))],
                                rownames = FALSE,
                                options = list(
                                  pageLength = 18  # This is the correct way to set the number of rows
                                ))
                    })
                    
                    # Sub Body Tab "U per SNi and per site ----
                    U.CM_Orth.Sites <- shiny::reactive({
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "[GDE_App, U.CM_Orth.Sites] INFO, Uncertainty", value = 0.5)
                      # Make sure it closes when we exit this reactive, even if there's an error
                      on.exit(progress$close())
                      
                      futile.logger::flog.info(paste0("[GDE_App, U.CM_Orth.Sites] INFO, Uncertainty plot for ", input$Instrument))
                      
                      return(
                        U.by.Model(CM_Corrected = CM_Orth(),
                                   Name.CM = Data$Name.CM,
                                   Model_Type = "TLS",
                                   LV.Interval = as.numeric(input$LV.Interval.U),
                                   min.N = as.numeric(input$min.N.U),
                                   SelectDQO = SelectDQO(),
                                   unit.ref = input$unit.ref,
                                   All.Campaigns = FALSE,
                                   Fitted.RS = as.logical(input$Fitted.RS))
                      )
                    })
                    output$U.CM_Orth.Sites <- renderPlot(U.CM_Orth.Sites()$GGPLOT)
                    output$Table.U.CM_Orth.Sites <- renderDT({
                      datatable(as.data.frame(U.CM_Orth.Sites()$CM_Corrected$U_orth_DF$Mat)[sort(unique(unlist(U.CM_Orth.Sites()$Lengths$N.LV))),
                                                                                            setdiff(names(U.CM_Orth.Sites()$CM_Corrected$U_orth_DF$Mat),
                                                                                                    c("Date", "ubss","Weights", "dens", "col", "Max.ubsRM", "Max.RSD", "Rel.RSS", "Rel.bias", "Ur"))],
                                rownames = FALSE,
                                options = list(
                                  pageLength = 18  # This is the correct way to set the number of rows
                                ))
                    })
                    
                    # # Body Tab item "CM_OLS" ####
                    # output$CM_OLS <- renderPlot(Plot.CM_OLS()) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    # Plot.CM_OLS <- shiny::reactive({
                    #   # Create a Progress object
                    #   progress <- shiny::Progress$new()
                    #   progress$set(message = "[GDE_App, Plot.CM_OLS] INFO, visual check of CM_OLS level differences", value = 0.5)
                    #   # Make sure it closes when we exit this reactive, even if there's an error
                    #   on.exit(progress$close())
                    #   
                    #   futile.logger::flog.info(paste0("[GDE_App, Plot.Ubs] INFO, visual check of CM_OLS level differences for ", input$Instrument))
                    #   
                    #   # Preparing plots and Return 1 combined plot
                    #   return(
                    #     create_cm_analysis_plots(df = CM_Raw()$CM_Corrected[setdiff(1:.N, i.Ref.outliers())][is.finite(rowSums(Data.DT()[setdiff(1:.N, i.Ref.outliers())][,.SD,.SDcols = Data$Name.CM]))],
                    #                              Name.CM =  paste0(Data$Name.CM, "_OLS"), Name.SN = Data$Name.SN, Name.RM = Data$Name.RM,
                    #                              Type =  input$Type, low_thr = as.numeric(input$Max.Ref.Bias)))
                    # })
                    # output$CM_Weighted <- renderPlot(Plot.CM_Weighted()) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    # Plot.CM_Weighted <- shiny::reactive({
                    #   # Create a Progress object
                    #   progress <- shiny::Progress$new()
                    #   progress$set(message = "[GDE_App, Plot.CM_OLS] INFO, visual check of CM_Weighted level differences", value = 0.5)
                    #   # Make sure it closes when we exit this reactive, even if there's an error
                    #   on.exit(progress$close())
                    #   
                    #   futile.logger::flog.info(paste0("[GDE_App, Plot.Ubs] INFO, visual check of CM_Weighted level differences for ", input$Instrument))
                    #   
                    #   return(
                    #     create_cm_analysis_plots(df = CM_Raw()$CM_Corrected[setdiff(1:.N, i.Ref.outliers())][is.finite(rowSums(Data.DT()[setdiff(1:.N, i.Ref.outliers())][,.SD,.SDcols = Data$Name.CM]))],
                    #                              Name.CM =  paste0(Data$Name.CM, "_Deming"), Name.SN = Data$Name.SN, Name.RM = Data$Name.RM,
                    #                              Type =  input$Type, low_thr = as.numeric(input$Max.Ref.Bias)))
                    # })
                    # output$U.CM_OLS <- renderPlot(Plot.U.CM_OLS()$GGPLOT) # , height = function() {session$clientData$output_Scatterplot1_height - 40}
                    # Plot.U.CM_OLS <- shiny::reactive({
                    #   # Create a Progress object
                    #   progress <- shiny::Progress$new()
                    #   progress$set(message = "[GDE_App, Plot.U.CM_OLS] INFO, Uncertainty", value = 0.5)
                    #   # Make sure it closes when we exit this reactive, even if there's an error
                    #   on.exit(progress$close())
                    #   
                    #   futile.logger::flog.info(paste0("[GDE_App, Plot.U.CM_OLS] INFO, Uncertainty plot for ", input$Instrument))
                    #   
                    #   return(
                    #     U.by.Model(CM_Corrected = CM_Corrected(),
                    #                Name.CM = Data$Name.CM,
                    #                Model_Type = "OLS",
                    #                LV.Interval = as.numeric(input$LV.Interval.U),
                    #                min.N = as.numeric(input$min.N.U),
                    #                SelectDQO = SelectDQO(),
                    #                unit.ref = input$unit.ref)
                    #   )
                    # })
                    
                    # menu "About" Version ----
                    output$VersionInfo <- renderPrint(AboutVersions(DisqueFieldtest = WD,
                                                                    FirstLineText   = "# Version History ====",
                                                                    LastLineText    = "# Content ===="), width = getOption("width"))
                    
                    # Info Icons
                    output$info.ubsRM <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = "ubs.RM", 
                        value = paste0(round(ubs()$ubsRM, digit = 2), " ",input$unit.ref),
                        icon = ubs()$ubsRM.Icon,
                        color = ubs()$ubsRM.Color)
                    })
                    output$info.ubsCM <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = "ubs.CM",
                        value = paste0(round(ubs()$ubsCM, digit = 2), " ",input$unit.ref),
                        icon = ubs()$ubsCM.Icon,
                        color = ubs()$ubsCM.Color)
                    })
                    Linearity.Stats <- shiny::reactive({
                      R2_stats <- list()
                      SD_stats <- list()
                      status_list <- list()
                      status_list.Icon <- list()
                      status_list.Color <- list()
                      
                      # OLS REGRESSION and criteria for Linearity
                      for(CM in Data$Name.CM){
                        
                        R2_stats[[CM]] <- LinearityCheck()$R2_stats[which(LinearityCheck()$R2_stats$CM == CM), "R2"]
                        SD_stats[[CM]] <- LinearityCheck()$SD_stats[which(LinearityCheck()$SD_stats$CM == CM), "SD_res"]
                        
                        if(LinearityCheck()$R2_stats[which(LinearityCheck()$R2_stats$CM == CM), "R2_Pass"] && 
                           LinearityCheck()$SD_stats[which(LinearityCheck()$SD_stats$CM == CM), "SD_Pass"]){
                          status_list[[CM]]       <- TRUE
                          status_list.Icon[[CM]]  <- icon("thumbs-up", lib = "glyphicon")
                          status_list.Color[[CM]] <- "green"
                        } else {
                          status_list[[CM]]       <- FALSE
                          status_list.Icon[[CM]]  <- icon("thumbs-down", lib = "glyphicon")
                          status_list.Color[[CM]] <- "red"
                        }
                      }
                      
                      return(list(R2_stats = R2_stats, SD_stats = SD_stats,
                                  status_list = status_list, status_list.Icon = status_list.Icon, status_list.Color = status_list.Color))
                    })
                    output$info.Linearity.1 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = "Linearity (R2, SD%)",
                        value = paste0(round(Linearity.Stats()$R2_stats[[Data$Name.CM[1]]], digit = 2), ", ", round(Linearity.Stats()$SD_stats[[Data$Name.CM[1]]]/SelectDQO()$LV *100, digit = 1)),
                        icon  = Linearity.Stats()$status_list.Icon[[Data$Name.CM[1]]],
                        color = Linearity.Stats()$status_list.Color[[Data$Name.CM[1]]]
                      )
                    })
                    output$info.Linearity.2 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = "Linearity (R2, SD%)",
                        value = paste0(round(Linearity.Stats()$R2_stats[[Data$Name.CM[2]]], digit = 2), ", ", round(Linearity.Stats()$SD_stats[[Data$Name.CM[2]]]/SelectDQO()$LV * 100, digit = 1)),
                        icon = Linearity.Stats()$status_list.Icon[[Data$Name.CM[2]]],
                        color = Linearity.Stats()$status_list.Color[[Data$Name.CM[2]]]
                      )
                    })
                    # Stat of Delta (CMi - RM)
                    Delta <- shiny::reactive({
                      MBE <- list()
                      MBE.Icon <- list()
                      MBE.Color <- list()
                      SD  <- list()
                      SD.Icon <- list()
                      SD.Color <- list()
                      # Skewness
                      Skw <- list()
                      Skw.Icon <- list()
                      Skw.Color <- list()
                      # Kurtosis
                      Kurt <- list()
                      Kurt.Icon <- list()
                      Kurt.Color <- list()
                      
                      
                      for(CM in Data$Name.CM){
                        
                        # MBE
                        MBE[[CM]] <- mean(Data.DT()[setdiff(1:.N, i.Ref.outliers())][[paste0(CM, "_DELTA")]], na.rm = T)
                        if(abs(MBE[[CM]]) <= as.numeric(input$Max.MBE)){
                          MBE.Icon[[CM]]  <- icon("thumbs-up", lib = "glyphicon")
                          MBE.Color[[CM]] <- "green"
                        } else {
                          MBE.Icon[[CM]]  <- icon("thumbs-down", lib = "glyphicon")
                          MBE.Color[[CM]] <- "red"}
                        
                        #SD
                        SD[[CM]]  <- sd(Data.DT()[setdiff(1:.N, i.Ref.outliers())][[paste0(CM, "_DELTA")]], na.rm = T)
                        if(SD[[CM]] <=  as.numeric(input$Max.SD.Diff)){
                          SD.Icon[[CM]]  <- icon("thumbs-up", lib = "glyphicon")
                          SD.Color[[CM]] <- "green"
                        } else {
                          SD.Icon[[CM]]  <- icon("thumbs-down", lib = "glyphicon")
                          SD.Color[[CM]] <- "red"}
                        
                        #Skewness
                        Skw[[CM]] <- moments::skewness(Data.DT()[setdiff(1:.N, i.Ref.outliers())][[paste0(CM, "_DELTA")]], na.rm = TRUE)
                        if(abs(Skw[[CM]]) <= as.numeric(input$Max.Skewness)){
                          Skw.Icon[[CM]]  <- icon("thumbs-up", lib = "glyphicon")
                          Skw.Color[[CM]] <- "green"
                        } else {
                          Skw.Icon[[CM]]  <- icon("thumbs-down", lib = "glyphicon")
                          Skw.Color[[CM]] <- "orange"}
                        
                        #Kurtosis
                        Kurt[[CM]] <- moments::kurtosis(Data.DT()[setdiff(1:.N, i.Ref.outliers())][[paste0(CM, "_DELTA")]], na.rm = TRUE)
                        if(abs(Kurt[[CM]]) <= as.numeric(input$Max.Kurtosis)){
                          Kurt.Icon[[CM]]  <- icon("thumbs-up", lib = "glyphicon")
                          Kurt.Color[[CM]] <- "green"
                        } else {
                          Kurt.Icon[[CM]]  <- icon("thumbs-down", lib = "glyphicon")
                          Kurt.Color[[CM]] <- "orange"}
                      }
                      
                      return(list(MBE = MBE, MBE.Icon = MBE.Icon, MBE.Color = MBE.Color,
                                  SD  = SD,  SD.Icon  = SD.Icon,  SD.Color  = SD.Color,
                                  Skw  = Skw,   Skw.Icon   = Skw.Icon,  Skw.Color  = Skw.Color,
                                  Kurt  = Kurt, Kurt.Icon  = Kurt.Icon, Kurt.Color = Kurt.Color))
                    })
                    output$info.MBE.1 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = paste0("MBE (CMi -RM)"),
                        value = paste0(round(Delta()$MBE[[Data$Name.CM[1]]], digit = 1), " ", input$unit.ref),
                        icon = Delta()$MBE.Icon[[Data$Name.CM[1]]],
                        color = Delta()$MBE.Color[[Data$Name.CM[1]]]
                      )
                    })
                    output$info.SD.1 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = paste0("SD (CMi -RM)"),
                        value = paste0(round(Delta()$SD[[Data$Name.CM[1]]], digit = 1), " ",input$unit.ref),
                        icon = Delta()$SD.Icon[[Data$Name.CM[1]]],
                        color = Delta()$SD.Color[[Data$Name.CM[1]]]
                      )
                    })
                    output$info.Skewness.1 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = paste0("Skewness"),
                        value = paste0(round(Delta()$Skw[[Data$Name.CM[1]]], digit = 1)),
                        icon = Delta()$Skw.Icon[[Data$Name.CM[1]]],
                        color = Delta()$Skw.Color[[Data$Name.CM[1]]]
                      )
                    })
                    output$info.Kurtosis.1 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = paste0("Kurtosis"),
                        value = paste0(round(Delta()$Kurt[[Data$Name.CM[1]]], digit = 1)),
                        icon = Delta()$Kurt.Icon[[Data$Name.CM[1]]],
                        color = Delta()$Kurt.Color[[Data$Name.CM[1]]]
                      )
                    })
                    U.SNi <- shiny::reactive({
                      
                      status_list <- list()
                      status_list.Icon <- list()
                      status_list.Color <- list()
                      
                      # Icon Uncertainty using Raw data except if tab "CM_Orth." is selected
                      if(input$tabStat1 == "CM_Orth."){
                        Lengths       <- U.CM_Orth.All()$Lengths
                        Lengths.Sites <- U.CM_Orth.Sites()$Lengths
                      } else if(input$tabStat1 == "CM_Raw"){
                        Lengths       <- U.CM_Raw.All()$Lengths
                        Lengths.Sites <- U.CM_Raw.Sites()$Lengths
                      }
                      
                      for(SN in Data$Name.SN){
                        
                        # Checking that All uncertainties are < DQO
                        if(all(Lengths[      SNi == SN, .SD,.SDcols = c("U.95th", "U.MBE_SD","U.GDE")] < SelectDQO()$DQO) &&
                           all(Lengths.Sites[SNi == SN, .SD,.SDcols = c("U.95th", "U.MBE_SD","U.GDE")] < SelectDQO()$DQO)){
                          status_list[[SN]]       <- TRUE
                          status_list.Icon[[SN]]  <- icon("thumbs-up", lib = "glyphicon")
                          status_list.Color[[SN]] <- "green"
                        } else {
                          status_list[[SN]]       <- FALSE
                          status_list.Icon[[SN]]  <- icon("thumbs-down", lib = "glyphicon")
                          status_list.Color[[SN]] <- "red"
                        }
                      }
                      return(list(Lengths = Lengths, Lengths.Sites = Lengths.Sites,
                                  status_list  = status_list,  status_list.Icon  = status_list.Icon,status_list.Color  = status_list.Color
                      ))
                    })
                    output$info.U.1 <- shinydashboard::renderInfoBox({
                      shiny::req(input$tabStat1 %in% c("CM_Raw", "CM_Orth."))
                      shinydashboard::infoBox(
                        title = paste0("U for SNi, SNi/Sites"),
                        value = paste0("CM:",paste(round(U.SNi()$Lengths[      SNi == Data$Name.SN[1], .(U.95th, U.MBE_SD, U.GDE)], digits = 2), collapse = ", "), "\n",
                                       "Site:",paste(round(U.SNi()$Lengths.Sites[SNi == Data$Name.SN[1]]$U.95th, digit =2), collapse = ", ")),
                        icon  = U.SNi()$status_list.Icon[[Data$Name.SN[1]]],
                        color = U.SNi()$status_list.Color[[Data$Name.SN[1]]]
                      )
                    })
                    output$info.MBE.2 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = paste0("MBE (CMi -RM)"),
                        value = paste0(round(Delta()$MBE[[Data$Name.CM[2]]], digit = 1), " ",input$unit.ref),
                        icon = Delta()$MBE.Icon[[Data$Name.CM[2]]],
                        color = Delta()$MBE.Color[[Data$Name.CM[2]]]
                      )
                    })
                    output$info.SD.2 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = paste0("SD (CMi -RM)"),
                        value = paste0(round(Delta()$SD[[Data$Name.CM[2]]], digit = 1), " ",input$unit.ref),
                        icon = Delta()$SD.Icon[[Data$Name.CM[2]]],
                        color = Delta()$SD.Color[[Data$Name.CM[2]]]
                      )
                    })
                    output$info.Skewness.2 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = paste0("Skewness"),
                        value = paste0(round(Delta()$Skw[[Data$Name.CM[2]]], digit = 1)),
                        icon = Delta()$Skw.Icon[[Data$Name.CM[2]]],
                        color = Delta()$Skw.Color[[Data$Name.CM[2]]]
                      )
                    })
                    output$info.Kurtosis.2 <- shinydashboard::renderInfoBox({
                      shinydashboard::infoBox(
                        title = paste0("Kurtosis"),
                        value = paste0(round(Delta()$Kurt[[Data$Name.CM[2]]], digit = 1)),
                        icon = Delta()$Kurt.Icon[[Data$Name.CM[2]]],
                        color = Delta()$Kurt.Color[[Data$Name.CM[2]]]
                      )
                    })
                    output$info.U.2 <- shinydashboard::renderInfoBox({
                      shiny::req(input$tabStat1 %in% c("CM_Raw", "CM_Orth."))
                      shinydashboard::infoBox(
                        title = paste0("U for SNi, SNi/Sites"),
                        value = paste0("CM:", paste(round(U.SNi()$Lengths[      SNi == Data$Name.SN[2], .(U.95th, U.MBE_SD, U.GDE)], digits = 2), collapse = ", "), "\n",
                                       "Site:", paste(round(U.SNi()$Lengths.Sites[SNi == Data$Name.SN[2]]$U.95th, digit =2), collapse = ", ")),
                        icon  = U.SNi()$status_list.Icon[[Data$Name.SN[2]]],
                        color = U.SNi()$status_list.Color[[Data$Name.SN[2]]]
                      )
                    })
                    
                    # Hiding Candidate/Corrections/report if not linear
                    # observeEvent(LinearityCheck()$status,{
                    #   
                    #   # avoid Hyper reactivity
                    #   shiny::req(input$tabStat1 == "Linearity check")
                    #   
                    #   if(all(grepl("NOT LINEAR", LinearityCheck()$status))){
                    #     # All Instruments NOT LINEAR
                    #     Title <- "ERROR Instrument not LINEAR"
                    #     my_message <- paste0("[GDE_App, Plot.LinearityCheck] ERROR, No instrument are found linear, cannot proceed with sensor evaluation")
                    #     Alert.Type <- "error"
                    #     
                    #     # Hiding TabPanel to avoid mistakes
                    #     runjs("$('#correction_item').parent().hide();")  # Hide menu item with ID correction_item
                    #     runjs("$('#Report_item').parent().hide();")      # Hide menu item with ID Report_item
                    #   } else {
                    #     # A least one Instrument is LINEAR
                    #     Title <- "INFO at least one instrument is Linear"
                    #     my_message <- paste0("[GDE_App, Plot.LinearityCheck] INFO, at least one instrument is found linear, sensor evaluation can proceed.")
                    #     Alert.Type <- "info"
                    #     
                    #     shiny::showTab(inputId = "tabStat1", target = "CM_Raw")
                    #     #runjs("$('#correction_item').parent().show();")  # Show menu item with ID correction_item
                    #     #runjs("$('#Report_item').parent().show();")      # Show menu item with ID Report_item
                    #   }
                    #   shinyalert::shinyalert(title = Title,
                    #                          text = my_message,
                    #                          closeOnEsc = TRUE,
                    #                          closeOnClickOutside = TRUE,
                    #                          html = FALSE,
                    #                          type = Alert.Type,
                    #                          showConfirmButton = TRUE,
                    #                          showCancelButton  = FALSE,
                    #                          confirmButtonText = "OK",
                    #                          confirmButtonCol  = "#AEDEF4",
                    #                          timer             = 5000,
                    #                          imageUrl          = "",
                    #                          animation         = FALSE)
                    # }, ignoreNULL = T)
                    
                    
                    
                  }# End of checking presence of column Campaign
                }# End of checking presence of column Pollutant
              }# End of checking minimum data rows (100?
            }# End of checking presence of RM data
          }# number of "SNi" different from "CMi"
        }# End of conversion of "date" to POSIXct
      }# End of checking presence of CM data
    }# End of checking date or Date in Data File
  })
}

# run App
chrome_paths <- c(
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
)

chrome_path <- chrome_paths[file.exists(chrome_paths)][1]

if (!is.na(chrome_path)) {
  options(browser = chrome_path)
} else {
  message("Chrome non trovato")
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
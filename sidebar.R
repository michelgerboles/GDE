# SideBar of Shiny Dashboard GDE.R
sidebar <- dashboardSidebar(
  # Load shinyjs
  useShinyjs(),
  
  sidebarMenu(id = "sidebar",
              
              # Load data
              menuItem(text = "Data", tabName = "Data", icon = icon("folder-open"),
                       
                       # Selecting data file
                       actionButton(inputId = "chooseFile", "Select file", icon = icon("folder-open")),
                       
                       #          startExpanded = TRUE
                       # ),
                       # 
                       # # Select DQO parameters
                       # menuItem(text = "DQO & Data", tabName = "DQO", icon = icon("scale-balanced"),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Pollutant",
                         label   = "Pollutant",
                         choices = c("CO", "NO2", "PM10", "PM2.5", "O3", "SO2"),
                         selected = "PM2.5",
                         options = list(
                           title = "Choose pollutant")),
                       
                       shiny::uiOutput("uiInstrument"),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Type",
                         label = "Type",
                         choices = c("Fixed type testing", "Fixed on-going","Indicative"),
                         selected = "Fixed type testing",
                         options = list(
                           title = "Choose type")),
                       
                       shinyWidgets::pickerInput(
                         inputId  = "Directive",
                         label    = "Directive",
                         choices  = c("2008/50/EC", "2024/2881/EC", "EN TS 17660"),
                         selected = "2024/2881/EC",
                         options  = list(
                           title = "Choose Directive")),
                       
                       shinyWidgets::pickerInput(
                         inputId  = "Averaging.Period",
                         label    = "Averaging time",
                         choices  = c("1hour", "8hour", "24hour", "Season", "1year"),
                         selected = "24hour",
                         options  = list(
                           title = "Choose type")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "unit.ref",
                         label = "Data unit",
                         choices = c("\u00b5g/m\u00b3","ug/m3", "mg/m3", "mg/m\u00b3", "ppb", "ppm", "percent", "Celsius", "hPa"),
                         selected = "ug/m3",
                         options = list(
                           title = "Choose unit")),
                       
                       startExpanded = TRUE
              ),
              
              # handsome table and plots of raw data,
              menuItem("View Data", tabName = "ViewData", icon = icon("chart-line", lib = "font-awesome")),
              
              menuItem(text = "ubs", tabName = "ubs", icon = icon("folder-open"),
                       
                       shinyWidgets::pickerInput(
                         inputId = "min.N",
                         label = "min.N",
                         choices = seq(0,200, 10),
                         selected = 100,
                         options = list(
                           title = "Min row of reference data")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Max.ubsRM",
                         label = "Max.ubsRM",
                         choices = seq(0.1,2, 0.1),
                         selected = 1,
                         options = list(
                           title = "Max allowed ubsRM")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Max.Ref.Bias",
                         label = "Max Ref.Bias",
                         choices = seq(1:10),
                         selected = 2,
                         options = list(
                           title = "Max Ref.Bias")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Max.ubsCM",
                         label = "Max.ubsCM",
                         choices = seq(0.25,3, 0.25),
                         selected = 2.5,
                         options = list(
                           title = "Max allowed ubsCM")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Min.R2",
                         label = "Min R2",
                         choices = seq(0.85, 0.99, 0.01),
                         selected = 0.95,
                         options = list(
                           title = "Minimum R2")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Max.Rel.SD",
                         label = "Max relative SD",
                         choices = seq(0.05, 0.15, 0.005),
                         selected = 0.125,
                         options = list(
                           title = "Maximum relative SD")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Bin.Width",
                         label = "Bin.Width",
                         choices = seq(1,15),
                         selected = 5,
                         options = list(
                           title = "Bin width for UbsCM")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "min.Count",
                         label = "min.Count",
                         choices = seq(1,10),
                         selected = 4,
                         options = list(
                           title = "Min. counts/bin for UbsCM")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "LV.Interval",
                         label = "LV Interval",
                         choices = seq(0.1,1, 0.1),
                         selected = 0.3,
                         options = list(
                           title = "Selected bins around LV for UbsCM")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Max.Skewness",
                         label = "Max skewness",
                         choices = seq(0.1,1, 0.1),
                         selected = 0.5,
                         options = list(
                           title = "Maximum skewness")),
                       
                       shinyWidgets::pickerInput(
                         inputId = "Max.Kurtosis",
                         label = "Max Kurtosis",
                         choices = seq(0.5 , 4, 0.5),
                         selected = 4,
                         options = list(
                           title = "Maximum kurtosis")),
                       
                       startExpanded = FALSE
              ),
              
              menuItem(text = "UConf", tabName = "UConf", icon = icon("folder-open"),

                       shinyWidgets::pickerInput(
                         inputId = "Fitted.RS",
                         label = "Fitted.RS",
                         choices = c("TRUE", "FALSE"),
                         selected = "FALSE",
                         options = list(
                           title = "RSS is fitted or averaged?")),


                       startExpanded = FALSE
              ),

              # Data treatment and computation,
              menuItem("Statistics", tabName = "Statistics", icon = icon("calculator")
                       # ,
                       # shinyWidgets::pickerInput(
                       #   inputId = "bin_width",
                       #   label = "bin width",
                       #   choices = seq(1:10),
                       #   selected = 5,
                       #   options = list(
                       #     title = "Choose bin width")),
                       # 
                       # startExpanded = FALSE
              ),
              
              # # Data Correction,
              # menuItem("Data correction", tabName = "correction", id = "correction_item", icon = icon("calculator")),
              useShinyjs(),
              
              # Reporting,
              menuItem("Report", tabName = "Report", id = "Report_item", icon = icon("list")),
              
              
              # Add Quit button to sidebar
              actionButton("shutdown", "Shutdown", icon("power-off"))
              
  )
)

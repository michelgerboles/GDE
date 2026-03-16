# SideBar of Shiny Dashboard GDE.R
sidebar <- dashboardSidebar(
  # Load shinyjs
  useShinyjs(),
  
  sidebarMenu(id = "sidebar",
              
              # Load data
              shinydashboard::menuItem(text = "Data & DQO", tabName = "Data", icon = icon("folder-open"),
                                       
                                       # Selecting data file
                                       shiny::actionButton(inputId = "chooseFile",
                                                           label = "Select file",
                                                           icon = icon("folder-open")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Pollutant",
                                         label   = "Pollutant",
                                         choices = c("CO", "NO2", "PM10", "PM2.5", "O3", "SO2"),
                                         selected = "PM2.5",
                                         options = list(
                                           title = "Choose pollutant")),
                                       
                                       shiny::uiOutput("uiInstrument"),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId  = "Directive",
                                         label    = "Directive",
                                         choices  = c("2008/50/EC", "2024/2881/EC", "EN TS 17660"),
                                         selected = "2024/2881/EC",
                                         options  = list(
                                           title = "Choose Directive")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Type",
                                         label = "Type",
                                         choices = c("Fixed type testing", "Fixed on-going","Indicative type testing","Indicative on-going"),
                                         selected = "Fixed type testing",
                                         options = list(
                                           title = "Choose type")),
                                       
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
              shinydashboard::menuItem("View Data", tabName = "ViewData", icon = icon("chart-line", lib = "font-awesome")),
              
              # Data treatment and computation,
              shinydashboard::menuItem("Statistics", tabName = "Statistics", icon = icon("calculator")),
              
              shinydashboard::menuItem(text = "\u0394(RMi), ubs", tabName = "ubs", icon = icon("folder-open"),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "min.N",
                                         label = "min.N",
                                         choices = seq(0,200, 10),
                                         selected = 100,
                                         options = list(
                                           title = "Min row of reference data")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Max.Ref.Bias",
                                         label = "Max Ref.Bias",
                                         choices = seq(1:10),
                                         selected = 2,
                                         options = list(
                                           title = "Max Ref.Bias")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Max.ubsRM",
                                         label = "Max.ubsRM",
                                         choices = seq(0.1,2, 0.1),
                                         selected = 1,
                                         options = list(
                                           title = "Max allowed ubsRM")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Max.ubsCM",
                                         label = "Max.ubsCM",
                                         choices = seq(0.25,3, 0.25),
                                         selected = 2.5,
                                         options = list(
                                           title = "Max allowed ubsCM")),
                                       
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
                                         label = "LV.Interval",
                                         choices = seq(0.1,1, 0.1),
                                         selected = 0.3,
                                         options = list(
                                           title = "Selected bins around LV for UbsCM")),
                                       
                                       startExpanded = FALSE
              ),
              
              shinydashboard::menuItem(text = "Linearity, CMi-RM", tabName = "Linearity", icon = icon("folder-open"),
                                       
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
                                         inputId = "Max.MBE",
                                         label = "Max. MBE",
                                         choices = seq(0.1 , 2, 0.1),
                                         selected = 1,
                                         options = list(
                                           title = "Maximum MBE")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Max.SD.Diff",
                                         label = "Max SD CMi-RM",
                                         choices = seq(0.5,4, 0.25),
                                         selected = 2.5,
                                         options = list(
                                           title = "Maximum skewness")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Max.Skewness",
                                         label = "Max. skewness",
                                         choices = seq(0.1,1, 0.1),
                                         selected = 0.5,
                                         options = list(
                                           title = "Maximum skewness")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Max.Kurtosis",
                                         label = "Max. Kurtosis",
                                         choices = seq(0.5 , 4, 0.5),
                                         selected = 4,
                                         options = list(
                                           title = "Maximum kurtosis")),
                                       
                                       startExpanded = FALSE
              ),
              
              shinydashboard::menuItem(text = "U", tabName = "UConf", icon = icon("folder-open"),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "LV.Interval.U",
                                         label = "LV Interval for U",
                                         choices = seq(0.1,1, 0.1),
                                         selected = 0.3,
                                         options = list(
                                           title = "Selected concentration around LV for U")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "min.N.U",
                                         label = "min.N per site for U at LV",
                                         choices = seq(5,25,1),
                                         selected = 20,
                                         options = list(
                                           title = "Min data to estimate U at LV")),
                                       
                                       shinyWidgets::pickerInput(
                                         inputId = "Fitted.RS",
                                         label = "Fitted.RS",
                                         choices = c("TRUE", "FALSE"),
                                         selected = "FALSE",
                                         options = list(
                                           title = "RSS is fitted or averaged?")),
                                       
                                       shinyWidgets::switchInput(
                                         inputId = "Variable.ubsRM",
                                         #label = "Variable.ubsRM",
                                         value = FALSE,
                                         label = "variable ubsRM for deming?",
                                         width = "auto"),

                                       shinyWidgets::switchInput(
                                         inputId = "Variable.ubsCM",
                                         #label = "Variable.ubsCM",
                                         value = FALSE,
                                         label = "variable ubsCM for deming?",
                                         width = "auto"),
                                       
                                       startExpanded = FALSE
              ),
              
              # # Data Correction,
              # shinydashboard::menuItem("Data correction", tabName = "correction", id = "correction_item", icon = icon("calculator")),
              useShinyjs(),
              
              # Reporting,
              shinydashboard::menuItem("Report", tabName = "Report", id = "Report_item", icon = icon("list")),
              
              # About help,
              shinydashboard::menuItem("About",     tabName = "About",    icon = icon("list")),
              
              # Add Quit button to sidebar
              actionButton("shutdown", "Shutdown", icon("power-off"))
              
  )
)

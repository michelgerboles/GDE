body <- dashboardBody(
  tags$style(HTML("
      #tabBox-container {
        height: calc(100vh - 100px); /* Adjust based on header/footer */
        overflow-y: auto; /* Add scrolling if needed */
      }
    ")),
  shinydashboard::tabItems(#id="main_tabs",
    
    shinydashboard::tabItem(tabName = "Data",
                            shiny::fluidRow(
                              shinydashboard::box(title = "Summary", width = 12, shiny::tableOutput("summaryFile"))
                            )
    ),
    
    # shinydashboard::tabItem(tabName = "DQO",
    #                         shiny::textOutput("dynamic_text"),
    #                         h2("Data tab content"),
    # ),
    
    shinydashboard::tabItem(tabName = "ViewData",
                            # Create a TextOuput with a dynamic text: file, Directive, pollutant, instrument ...
                            shiny::textOutput("dynamic_text"),
                            shiny::fluidRow(
                              shinydashboard::tabBox(
                                title = "View Data",
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1", width = 12, height = "900px",  # , height = "850px"
                                shiny::tabPanel(title = "DQO",        icon = icon("scale-balanced"), shiny::tableOutput('SelectDQO')),
                                shiny::tabPanel(title = "Data table", icon = icon("list-ol"), shinycssloaders::withSpinner(
                                  rhandsontable::rHandsontableOutput("hot"), type = 8)),
                                shiny::tabPanel("Time_series", icon = icon("stats", lib = "glyphicon"), shinycssloaders::withSpinner(
                                  htmlOutput("ts_dygraphs", height = "auto", width = "100%"), type = 8)),
                                shiny::tabPanel("Scatterplots", icon = icon("chart-line", lib = "font-awesome"), 
                                                shinycssloaders::withSpinner(plotOutput("Scatterplot", width = "100%", height = "800px"), type = 8))
                              )
                            )
    ),
    
    shinydashboard::tabItem(tabName = "Statistics",
                            # All computation of statistics: Filtering ...
                            shiny::textOutput("dynamic_text"),
                            shiny::fluidRow(
                              
                              
                              shiny::column(width = 10,  # Column of width 10 for the tabBox
                                            
                                     shinydashboard::tabBox(
                                       title = "Statistics",
                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       id = "tabset2",
                                       width = 12,
                                       height = "100%",  # , height = "850px"
                                       shiny::tabPanel(title = "Reference data",     icon = icon("chart-line", lib = "font-awesome"),
                                                       shinycssloaders::withSpinner(plotOutput("VerifReference", height = "850"), type = 8)),
                                       
                                       shiny::tabPanel(title = "Filtered Reference", icon = icon("list-ol"), shinycssloaders::withSpinner(
                                         rhandsontable::rHandsontableOutput("hot.RM.Filtered"), type = 8)),
                                       
                                       shiny::tabPanel(title = "Ubs",                icon = icon("chart-line", lib = "font-awesome"),
                                                       shinycssloaders::withSpinner(plotOutput("Ubs", height = "850"), type = 8)),
                                       
                                       shiny::tabPanel(title = "Linearity check",    icon = icon("chart-line", lib = "font-awesome"),
                                                       shinycssloaders::withSpinner(plotOutput("LinearityCheck", height = "850"), type = 8)),
                                       
                                       shiny::tabPanel(title = "CM_Raw",         icon = icon("chart-line", lib = "font-awesome"),
                                                       shinydashboard::tabBox(
                                                         title = "Statistics",
                                                         width = 12,
                                                         height = "100%",  # , height = "850px"
                                                         
                                                         shiny::tabPanel(title = "Differences", icon = icon("chart-line", lib = "font-awesome"),
                                                                  shinycssloaders::withSpinner(plotOutput("CM_Raw", height = "850"), type = 8)),
                                                         shiny::tabPanel(title = "Uncertainty", icon = icon("chart-line", lib = "font-awesome"),
                                                                         shinycssloaders::withSpinner(plotOutput("U.CM_Raw", height = "850"), type = 8))
                                                       )
                                       ),
                                       
                                       shiny::tabPanel(title = "CM_OLS",         icon = icon("chart-line", lib = "font-awesome"),
                                                       shinydashboard::tabBox(
                                                         title = "Statistics",
                                                         width = 12,
                                                         height = "100%",  # , height = "850px"
                                                         
                                                         shiny::tabPanel(title = "Differences",
                                                                  shinycssloaders::withSpinner(plotOutput("CM_OLS", height = "850"), type = 8)),
                                                         shiny::tabPanel(title = "Uncertainty",         icon("chart-line", lib = "font-awesome"),
                                                                         shinycssloaders::withSpinner(plotOutput("U.CM_OLS", height = "850"), type = 8))
                                                       )
                                       ),
                                       
                                       shiny::tabPanel(title = "CM_Orth.",       icon = icon("chart-line", lib = "font-awesome"),
                                                       shinydashboard::tabBox(
                                                         title = "Statistics",
                                                         width = 12,
                                                         height = "100%",  # , height = "850px"
                                                         
                                                         shiny::tabPanel(title = "Differences",
                                                                  shinycssloaders::withSpinner(plotOutput("CM_TLS", height = "850"), type = 8)),
                                                         shiny::tabPanel(title = "Uncertainty",         icon("chart-line", lib = "font-awesome"),
                                                                         shinycssloaders::withSpinner(plotOutput("U.CM_TLS", height = "850"), type = 8))
                                                       )
                                       ),
                                       
                                       shiny::tabPanel(title = "CM_Weighted",         icon = icon("chart-line", lib = "font-awesome"),
                                                       shinydashboard::tabBox(
                                                         title = "Statistics",
                                                         width = 12,
                                                         height = "100%",  # , height = "850px"
                                                         
                                                         shiny::tabPanel(title = "Differences",
                                                                  shinycssloaders::withSpinner(plotOutput("CM_Weighted", height = "850"), type = 8)),
                                                         shiny::tabPanel(title = "Uncertainty",         icon("chart-line", lib = "font-awesome"),
                                                                         shinycssloaders::withSpinner(plotOutput("U.CM_Weighted", height = "850"), type = 8))
                                                       )
                                       ),
                                       
                                       shiny::tabPanel(title = "Regression lines",  icon = icon("list-ol"), shinycssloaders::withSpinner(
                                         DT::dataTableOutput("Table.Reg.Lines"), type = 8)),

                                       # ,
                                       # 
                                       # shiny::tabPanel(title = "Regression",         icon("list-ol", lib = "font-awesome"),
                                       #                 shinycssloaders::withSpinner(renderTable("Regressions", height = "850"), type = 8),
                                       # )
                                     )
                              ),
                              
                              shiny::column(width = 2,  # Column of width 2 for the info boxes
                                     
                                     # Parameters common to all CM
                                     shiny::fluidRow(shinydashboard::infoBoxOutput("info.ubsRM", width = NULL)),
                                     shiny::fluidRow(shinydashboard::infoBoxOutput("info.ubsCM", width = NULL)),
                                     
                                     # Parameters specific for each CM
                                     shiny::fluidRow(
                                       shinydashboard::tabBox(
                                         title = "Criteria",
                                         # The id lets us use input$tabset1 on the server to find the current tab
                                         id = "tab.Stat.1",
                                         width = 12,
                                         height = "100%",  # , height = "850px"
                                         
                                         shiny::tabPanel(title = "CM1", icon = icon("list-ol"),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.Linearity.1", width = NULL)),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.MBE.1",       width = NULL)),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.SD.1",        width = NULL)),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.Skewness.1",  width = NULL)),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.Kurtosis.1",  width = NULL))),
                                         
                                         shiny::tabPanel(title = "CM2", icon = icon("list-ol"),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.Linearity.2", width = NULL)),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.MBE.2",       width = NULL)),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.SD.2",        width = NULL)),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.Skewness.2",  width = NULL)),
                                                         shiny::fluidRow(shinydashboard::infoBoxOutput("info.Kurtosis.2",  width = NULL))
                                                         
                                         )
                                       )
                                     )
                              )
                            )
    )
    #,
    # shiny::fluidRow(
    #     shinydashboard::box(
    #         title = "Interactive Table",
    #         DT::dataTableOutput("myTable"),
    #         width = 12
    #     )
    # )
  )
)

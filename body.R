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
    
    shinydashboard::tabItem(tabName = "ViewData",
                            # Create a TextOuput with a dynamic text: file, Directive, pollutant, instrument ...
                            shiny::fluidRow(
                              shiny::textOutput("dynamic_text"),
                              shinydashboard::tabBox(
                                title = "View Data",
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1", width = 12, height = "100%",  # , height = "850px"
                                
                                #shiny::tabPanel(title = "Summary  of RM", width = 12, shiny::tableOutput("summaryFile")),
                                shiny::tabPanel(title = "Data table", icon = icon("list-ol"),
                                                shinycssloaders::withSpinner(rhandsontable::rHandsontableOutput("hot"), type = 8)),
                                shiny::tabPanel("Time series", icon = icon("stats", lib = "glyphicon"),
                                                shinycssloaders::withSpinner(htmlOutput("ts_dygraphs", width = "100%", height = "auto"), type = 8)),
                                shiny::tabPanel("Scatterplots", icon = icon("chart-line", lib = "font-awesome"), 
                                                shinycssloaders::withSpinner(plotOutput("Scatterplot", width = "100%", height = "850px"), type = 8)),
                                shiny::tabPanel(title = "DQO", icon = icon("scale-balanced"), shiny::tableOutput('SelectDQO'))
                              )
                            )
    ),
    
    shinydashboard::tabItem(tabName = "About",
                            shiny::fluidRow(
                              shinydashboard::tabBox(
                                title = "About",
                                id = "tabAbout", width = 12, height = "850px",  # , height = "850px"
                                
                                shiny::tabPanel("User Manual", value = "Help", icon = icon("question"),
                                                shiny::mainPanel(
                                                  tags$iframe(style = "height:900px; width:100%; scrolling=yes", src = "Contents.pdf"),
                                                  # tags$iframe(class = "shiny-plot-output",
                                                  #             #src = "https://docs.google.com/document/d/e/2PACX-1vSH7N4piil32823BM5jJxNElQkwkm17RXczmgR6qyMXNOJyoY3BpxJoqL444o9s54VoNpxDZp74dwQB/pub?embedded=true")
                                                  width = 12)),
                                
                                shiny::tabPanel("VersionInfo", icon = icon("info-circle"),
                                                verbatimTextOutput("VersionInfo"))
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
                                              id = "tabStat1",
                                              width = 12, height = "850px", #height = "100%", height = "850px" The height of a box, in pixels or other CSS unit. By default the height scales automatically with the content.
                                              shiny::tabPanel(title = "Reference data", icon = icon("chart-line", lib = "font-awesome"),
                                                              shinycssloaders::withSpinner(plotOutput("VerifReference", height = "850px"), type = 8)),
                                              
                                              shiny::tabPanel(title = "Filtered Reference", icon = icon("list-ol"), shinycssloaders::withSpinner(
                                                rhandsontable::rHandsontableOutput("hot.RM.Filtered"), type = 8)),
                                              
                                              shiny::tabPanel(title = "ubs", icon = icon("chart-line", lib = "font-awesome"),
                                                              shinycssloaders::withSpinner(plotOutput("ubs", height = "850px"), type = 8)),
                                              
                                              shiny::tabPanel(title = "Linearity check",    icon = icon("chart-line", lib = "font-awesome"),
                                                              shinycssloaders::withSpinner(plotOutput("LinearityCheck", height = "850px"), type = 8)),
                                              
                                              shiny::tabPanel(title = "CM_Raw", icon = icon("chart-line", lib = "font-awesome"),
                                                              shinydashboard::tabBox(
                                                                title = "Raw Data",
                                                                width = "100%", height = "800px",  # , height = "850px"
                                                                
                                                                shiny::tabPanel(title = "Differences", icon = icon("chart-line", lib = "font-awesome"),
                                                                                shinycssloaders::withSpinner(plotOutput("CM_Raw", height = "800px"), type = 8)),
                                                                shiny::tabPanel(title = "OLS Diagnostics", icon = icon("chart-line", lib = "font-awesome"),
                                                                                shinycssloaders::withSpinner(plotOutput("Raw.OLS.Models", height = "800px"), type = 8)),
                                                                shiny::tabPanel(title = "Orthogonal", icon = icon("chart-line", lib = "font-awesome"),
                                                                                fluidRow(
                                                                                  column(6, shinycssloaders::withSpinner(plotOutput("TLS.Scatterplot", height = "800px"), type = 8)),
                                                                                  column(6, shinycssloaders::withSpinner(plotOutput("TLS.Residuals", height = "800px"), type = 8))
                                                                                )
                                                                ),
                                                                shiny::tabPanel(title = "Regression",  icon = icon("list-ol"),
                                                                                shinycssloaders::withSpinner(DT::dataTableOutput("Table.Reg.Lines.Raw"), type = 8)),
                                                                shiny::tabPanel(title = "U. /SNi", icon = icon("chart-line", lib = "font-awesome"),
                                                                                shinycssloaders::withSpinner(plotOutput("Plot.U.CM_Raw.All", height = "800px"), type = 8)),
                                                                shiny::tabPanel(title = "U table /SNi",  icon = icon("list-ol"), shinycssloaders::withSpinner(
                                                                  DT::DTOutput("Table.U.CM_Raw.All"), type = 8)),
                                                                shiny::tabPanel(title = "U /SNi, /site", icon = icon("chart-line", lib = "font-awesome"),
                                                                                shinycssloaders::withSpinner(plotOutput("U.CM_Raw.Sites", height = "800px"), type = 8)),
                                                                shiny::tabPanel(title = "U table /SN, /site",  icon = icon("list-ol"), shinycssloaders::withSpinner(
                                                                  DT::DTOutput("Table.U.CM_Raw.Sites"), type = 8)),
                                                                shiny::tabPanel(title = "Table U per SNi and SNi/Sites", icon = icon("chart-line", lib = "font-awesome"),
                                                                                fluidRow(
                                                                                  # Table per SNi
                                                                                  h3("Table per SNi"),
                                                                                  shinycssloaders::withSpinner(shiny::tableOutput("Table.U.CM_Raw.SNi"), type = 8),
                                                                                  h3("Table per SNi and per site"),
                                                                                  shinycssloaders::withSpinner(shiny::tableOutput("Table.U.CM_Raw.Sites.SNi"), type = 8),
                                                                                )
                                                                ),
                                                              )
                                              ),
                                              
                                              shiny::tabPanel(title = "CM_Orth.", icon = icon("chart-line", lib = "font-awesome"),
                                                              shinydashboard::tabBox(
                                                                title = "Orth. Correction",
                                                                width = 12,
                                                                height = "100%", # height = "800px",  # , height = "850px"
                                                                
                                                                shiny::tabPanel(title = "Differences",
                                                                                shinycssloaders::withSpinner(plotOutput("CM_TLS", height = "800px"), type = 8)),
                                                                shiny::tabPanel(title = "OLS Diagnostics", icon = icon("chart-line", lib = "font-awesome"),
                                                                                shinycssloaders::withSpinner(plotOutput("TLS.OLS.Models", height = "800px"), type = 8)),
                                                                shiny::tabPanel(title = "Orthogonal", icon = icon("chart-line", lib = "font-awesome"),
                                                                                fluidRow(
                                                                                  column(6, shinycssloaders::withSpinner(plotOutput("TLS.TLS.Scatterplot", height = "800px"), type = 8)),
                                                                                  column(6, shinycssloaders::withSpinner(plotOutput("TLS.TLS.Residuals", height = "800px"), type = 8))
                                                                                )
                                                                ),
                                                                shiny::tabPanel(title = "Regression",  icon = icon("list-ol"),
                                                                                shinycssloaders::withSpinner(DT::dataTableOutput("Table.Reg.Lines.TLS"), type = 8)),
                                                                shiny::tabPanel(title = "U. /SNi", icon = icon("chart-line", lib = "font-awesome"),
                                                                                shinycssloaders::withSpinner(plotOutput("Plot.U.CM_Orth.All", height = "800px"), type = 8)),
                                                                shiny::tabPanel(title = "U table SNi",  icon = icon("list-ol"), shinycssloaders::withSpinner(
                                                                  DT::DTOutput("Table.U.CM_Orth.All"), type = 8)),
                                                                shiny::tabPanel(title = "U /SNi, /site", icon = icon("chart-line", lib = "font-awesome"),
                                                                                shinycssloaders::withSpinner(plotOutput("U.CM_Orth.Sites", height = "800px"), type = 8)),
                                                                shiny::tabPanel(title = "U elements /SN, /site",  icon = icon("list-ol"), shinycssloaders::withSpinner(
                                                                  DT::DTOutput("Table.U.CM_Orth.Sites"), type = 8))
                                                              )
                                              ),
                                              
                                              shiny::tabPanel(title = "CM_OLS", icon = icon("chart-line", lib = "font-awesome"),
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
                                              
                                              shiny::tabPanel(title = "CM_Weighted", icon = icon("chart-line", lib = "font-awesome"),
                                                              shinydashboard::tabBox(
                                                                title = "Statistics",
                                                                width = 12,
                                                                height = "100%",  # , height = "850px"
                                                                
                                                                shiny::tabPanel(title = "Differences",
                                                                                shinycssloaders::withSpinner(plotOutput("CM_Weighted", height = "850"), type = 8)),
                                                                shiny::tabPanel(title = "Uncertainty",         icon("chart-line", lib = "font-awesome"),
                                                                                shinycssloaders::withSpinner(plotOutput("U.CM_Weighted", height = "850"), type = 8))
                                                              )
                                              )
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
                                                                shiny::fluidRow(shinydashboard::infoBoxOutput("info.Kurtosis.1",  width = NULL)),
                                                                shiny::fluidRow(shinydashboard::infoBoxOutput("info.U.1",  width = NULL))
                                                ),
                                                
                                                shiny::tabPanel(title = "CM2", icon = icon("list-ol"),
                                                                shiny::fluidRow(shinydashboard::infoBoxOutput("info.Linearity.2", width = NULL)),
                                                                shiny::fluidRow(shinydashboard::infoBoxOutput("info.MBE.2",       width = NULL)),
                                                                shiny::fluidRow(shinydashboard::infoBoxOutput("info.SD.2",        width = NULL)),
                                                                shiny::fluidRow(shinydashboard::infoBoxOutput("info.Skewness.2",  width = NULL)),
                                                                shiny::fluidRow(shinydashboard::infoBoxOutput("info.Kurtosis.2",  width = NULL)),
                                                                shiny::fluidRow(shinydashboard::infoBoxOutput("info.U.2",  width = NULL))
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

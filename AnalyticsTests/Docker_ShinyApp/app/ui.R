
ui <- tagList(
  # shinythemes::themeSelector(),
  navbarPage(
    # useShinyalert(),  # Set up shinyalert
    # theme = "flatly",  # <--- To use a theme, uncomment this
    "Innova Analytics Tests",
    tabPanel(
      "Asset Level",

      sidebarPanel(
        width = 2,
        fileInput("target_upload", "Choose file to upload",
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            ".csv"
          )
        ),

        radioButtons("separator", "Separator: ", choices = c(";", ",", ":"), selected = ",", inline = TRUE),
        # tags$h5("Default actionButton:"),
        # actionButton("action", "Search"),
        br(),
        br() # ,
        # tags$h5("Download Data:"),
        # actionButton("downloadData", "Download", class = "btn-primary")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "RAW DATA", # Raw data----
            fluidRow(
              div(
                class = "col-sm-16 col-md-16 col-lg-16",
                box(
                  width = "100%", title = "Analytics Output Data",
                  withLoader(DT::dataTableOutput("raw_table"), type = "html", loader = "dnaspin")
                )
              )
            )
          ),
          tabPanel(
            "EXPECTED OUTPUT", # Expected output----
            fluidRow(
              div(
                class = "col-sm-16 col-md-16 col-lg-16",
                box(
                  width = "100%", title = "Expected Output",
                  withLoader(DT::dataTableOutput("expected_table"), type = "html", loader = "dnaspin")
                  # tableOutput("expected_table")
                )
              )
            )
          ),
          tabPanel(
            "ERROR MARGINS", # Diff table----
            fluidRow(
              div(
                class = "col-sm-16 col-md-16 col-lg-16",
                box(
                  width = "100%", title = "Diff Table",
                  withLoader(reactable::reactableOutput("diff_table"), type = "html", loader = "dnaspin")
                )
              )
            )
          )
        )
      )
    ), #ASSET CATEGORY----------------------------------------------------------------------------------------------------
    tabPanel("Asset Category Level",

             sidebarPanel(
               width = 2,
               fileInput("category_target_upload", "Choose file to upload",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values",
                           ".csv"
                         )
               ),

               radioButtons("separator", "Separator: ", choices = c(";", ",", ":"), selected = ",", inline = TRUE),
               # tags$h5("Default actionButton:"),
               # actionButton("action", "Search"),
               br(),
               br() # ,
               # tags$h5("Download Data:"),
               # actionButton("downloadData", "Download", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   "ASSET CATEGORY RAW DATA", # Raw data----
                   fluidRow(
                     div(
                       class = "col-sm-16 col-md-16 col-lg-16",
                       box(
                         width = "100%", title = "Analytics  Asset Category  Output Data",
                         withLoader(DT::dataTableOutput("category_raw_table"), type = "html", loader = "dnaspin")
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "ASSET CATEGORY EXPECTED OUTPUT", # Expected output----
                   fluidRow(
                     div(
                       class = "col-sm-16 col-md-16 col-lg-16",
                       box(
                         width = "100%", title = "Expected Asset Category Output",
                         withLoader(DT::dataTableOutput("category_expected_table"), type = "html", loader = "dnaspin")
                         # tableOutput("expected_table")
                       )
                     )
                   )
                 ),
                 tabPanel(
                   "ASSET CATEGORY ERROR MARGINS", # Diff table----
                   fluidRow(
                     div(
                       class = "col-sm-16 col-md-16 col-lg-16",
                       box(
                         width = "100%", title = " Asset Category Diff Table",
                         withLoader(reactable::reactableOutput("category_diff_table"), type = "html", loader = "dnaspin")
                       )
                     )
                   )
                 )
               )
             )),
    tabPanel("Navbar 3", "This panel is intentionally left blank"), # ,
    navbarMenu(
      "Options",
      "Theme",
      tabPanel(
        "Theme Selector",
        # Add theme selection----
        themeSelector()
      )
      # "----",
      # "Section header",
      # tabPanel("Table"),
      # navbarMenu("Even more",
      #            tabPanel("whatever")
    )
  )
)

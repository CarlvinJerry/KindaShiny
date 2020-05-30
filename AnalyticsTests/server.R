
server <- function(input, output) {
  options(scipen = 999)
  options(digits = 8)

#Importer----
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile)) {
      return(NULL)
    }
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$separator)
    return(df)
  })
#
  # Raw Table----
  output$raw_table <- DT::renderDataTable({
    withProgress(message = "Generating data", style = "notification", {
      df <- df_products_upload()
      DT::datatable(df, extensions = "Buttons", class = "cell-border stripe", options = list(
        autoWidth = TRUE, scrollX = TRUE, pageLength = 90, scrollY = "700px",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      ))
    })
  })
#


  dfCalc <- reactive(assetLevel(df_products_upload())) # Calculate differences and expected output----

  # Expected Output----
  output$expected_table <- DT::renderDataTable( { #server = FALSE,
    DT::datatable(dfCalc(), extensions = c("Buttons", "KeyTable"), class = "cell-border stripe", options = list(
      autoWidth = TRUE, scrollX = TRUE, pageLength = 90, dom = "Bfrtip", scrollY = "600px",
      buttons = c("copy", "csv", "excel", "pdf", "print"), fixedHeader = TRUE, keys = TRUE
    )) %>%
      formatRound(columns = c(4:49), digits = 6) %>%
      formatStyle(c(
        "diffUnits", "diffWeights", "diffUnitPrice", "diffPerformance", "diffweightedPerformance",
        "diffBenchmarkPerf", "diffBenchmarkWeight", "diffWeightedBMPerf", "diffActiveWeight", "diffActiveReturn",
        "diffSelectionEffect", "diffAllocationEffect", "diffInteractionEffect", "diffActiveManagement"
      ),
      color = styleInterval(c(-0.0001, 0.0001), c("red", "green", "red")), fontWeight = "bold"
      ) %>%
      formatPercentage(columns = c(14:49), digits = 4)
  })
#
  # Diff Output----
  output$diff_table <- reactable::renderReactable({
    withProgress(message = "Generating data", style = "notification", {
      # df <- df_products_upload()
      data <- dfCalc() %>% select(
        HoldingsDate, SecurityId, SecurityName, OpeningMarketValue,
        Creations,
        Redemptions,
        ClosingMarketValue,
        diffUnits,
        diffUnitPrice,
        diffPerformance,
        diffWeights,
        diffweightedPerformance,
        diffBenchmarkPerf,
        diffBenchmarkWeight,
        diffWeightedBMPerf,
        diffActiveWeight,
        diffActiveReturn,
        diffSelectionEffect,
        diffAllocationEffect,
        diffInteractionEffect,
        diffActiveManagement
      )
    })

    withProgress(message = "Formatting grid, this might take a while...", style = "notification", { # notification/old
      reactable::reactable(data,
        wrap = FALSE, fullWidth = TRUE,
        defaultPageSize = 90, bordered = TRUE, compact = TRUE, height = 650, width = 1200,
        showPageSizeOptions = TRUE,
        resizable = TRUE, searchable = TRUE,
        columns = list(
          HoldingsDate = colDef(format = colFormat(date = TRUE, locales = "en-GB"), footer = "TOTAL", filterable = F),
          SecurityId = colDef(
            align = "left"
          ),

          SecurityName = colDef(
            align = "left"
          ),

          OpeningMarketValue = colDef(
            align = "left",
            format = colFormat(percent = F, separators = TRUE, digits = 4),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
            # defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0) {
                color <- "#777"
              } else if (as.numeric(as.character(values)) < 0) {
                color <- "#e00000"
              } else {
                color <- "#777"
              }
              list(color = color, fontWeight = "bold")
            }
          ),
          Creations = colDef(
            align = "left",
            format = colFormat(percent = F, separators = TRUE, digits = 4),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0) {
                color <- "#777"
              } else if (as.numeric(as.character(values)) < 0) {
                color <- "#e00000"
              } else {
                color <- "#777"
              }
              list(color = color, fontWeight = "bold")
            }
          ),
          Redemptions = colDef(
            align = "left",
            format = list(
              cell = colFormat(percent = F, separators = TRUE, digits = 4),
              aggregated = colFormat(percent = TRUE, digits = 6)
            ),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0) {
                color <- "#777"
              } else if (as.numeric(as.character(values)) < 0) {
                color <- "#e00000"
              } else {
                color <- "#777" # "#777"
              }
              list(color = color, fontWeight = "bold")
            }
          ),
          ClosingMarketValue = colDef(
            align = "left",
            format = colFormat(percent = F, separators = TRUE, digits = 4),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0) {
                color <- "#777"
              } else if (as.numeric(as.character(values)) < 0) {
                color <- "#e00000"
              } else {
                color <- "#777"
              }
              list(color = color, fontWeight = "bold")
            }
          ),
          diffUnits = colDef(
            align = "left",
            format = colFormat(percent = F, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffUnitPrice = colDef(
            align = "left",
            format = colFormat(percent = F, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffPerformance = colDef(
            align = "left",
            format = colFormat(percent = T, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffWeights = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffweightedPerformance = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffBenchmarkPerf = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffBenchmarkWeight = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffWeightedBMPerf = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffActiveWeight = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffActiveReturn = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffSelectionEffect = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffAllocationEffect = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < 0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffInteractionEffect = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) < -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          ),
          diffActiveManagement = colDef(
            align = "left",
            format = colFormat(percent = TRUE, digits = 6),
            footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
            style = function(values) {
              if (as.numeric(as.character(values)) > 0.0001) {
                color <- " #e00000"
              } else if (as.numeric(as.character(values)) > -0.0001) {
                color <- "#e00000"
              } else {
                color <- "#008000"
              }
              list(color = color, fontWeight = "regular")
            }
          )
        ),
        # Column groups----
        columnGroups = list(
          colGroup(name = "Valuation", columns = c("HoldingsDate", "OpeningMarketValue", "Creations", "Redemptions", "ClosingMarketValue")),
          colGroup(name = "Performance", columns = c(
            "diffUnits", "diffUnitPrice", "diffPerformance",
            "diffWeights", "diffweightedPerformance", "diffBenchmarkPerf",
            "diffBenchmarkWeight", "diffWeightedBMPerf"
          )),
          colGroup(name = "Attribution", columns = c(
            "diffActiveWeight", "diffActiveReturn", "diffSelectionEffect",
            "diffWeights", "diffweightedPerformance", "diffBenchmarkPerf",
            "diffAllocationEffect", "diffInteractionEffect", "diffActiveManagement"
          ))
        ),

        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      )
    })
  })
  #

  ####Category LEVEL--------------------------------------------------------------------------------------------------------------------

  category_df_products_upload <- reactive({
    inFile <- input$category_target_upload
    if (is.null(inFile)) {
      return(NULL)
    }
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$separator)
    return(df)
  })

  # Raw Table----
  output$category_raw_table <- DT::renderDataTable({
    withProgress(message = "Generating data", style = "notification", {
      df <- category_df_products_upload()
      DT::datatable(df, extensions = "Buttons", class = "cell-border stripe", options = list(
        autoWidth = TRUE, scrollX = TRUE, pageLength = 90, scrollY = "700px",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      ))
    })
  })


#Data calc------
  category_dfCalc <- reactive(df_products_upload() %>% group_by(HoldingsDate) %>%
                                summarize(expOpeningMarketValue = sum(OpeningMarketValue), expCreations = sum(Creations), expRedemptions = sum(Redemptions), expClosingMarketValue = sum(ClosingMarketValue)) %>%
                                mutate(expUnits = expClosingMarketValue + expCreations + expRedemptions, expUnitPrice = expClosingMarketValue/expUnits, expPerformance = expUnitPrice - 1,
                                       expWeight = expClosingMarketValue/expClosingMarketValue, expWeightedPerf = expWeight * expPerformance, expBMPerf = 1, expBMWeight = 1,
                                       expWeightedBMPerf = expBMPerf * expBMWeight, expActiveWeight = expWeight - expBMWeight, expActiveReturn = expPerformance-expBMPerf,
                                       expSelectionEffect = (expPerformance - expBMPerf)*expBMWeight , expAllocationEffect = (expBMPerf * expWeight)-(expBMWeight*expBMPerf),
                                       expInteractionEffect = expActiveReturn - expAllocationEffect-expSelectionEffect,
                                       expActiveManagementEffect = expAllocationEffect + expSelectionEffect )) #%>%# Calculate differences and expected output----



    #   #Active Weight----
    # mutate(test_ActiveWeight = test_Weight - test_BenchmarkWeight ) %>% #Active Weight
    #   mutate(diffActiveWeight = test_ActiveWeight - (ActiveWeight/100)) %>% #Diff  Active weight
    #
    #   #Active Return----
    # mutate(test_ActiveReturn = test_ActualPerformance - test_BenchmarkPerf ) %>% #Active per
    #   mutate(diffActiveReturn  = test_ActiveReturn - (ActiveReturn/100)) %>% #Diff  Active perf

    #   #selection Effect----
    # mutate(test_SelectionEffect = (test_ActualPerformance - test_BenchmarkPerf) * test_BenchmarkWeight ) %>% #
    #   mutate(diffSelectionEffect  = test_SelectionEffect - (SelectionEffect/100)) %>% #

    #   #Allocation effect----
    # mutate(test_AllocationEffect = (test_BenchmarkPerf * test_Weight) - (test_BenchmarkWeight * test_BenchmarkPerf) ) %>% #
    #   mutate(diffAllocationEffect  = test_AllocationEffect - (AllocationEffect/100)) %>% #
#
#       #Interaction effect----
#     mutate(test_InteractionEffect = test_ActiveReturn - test_AllocationEffect - test_SelectionEffect ) %>% #=
#       mutate(diffInteractionEffect  = test_InteractionEffect - (InteractionEffect/100)) %>% #=

    #   #Active Management----
    # mutate(test_ActiveManagementEffect = test_AllocationEffect + test_SelectionEffect ) %>% #
    #   mutate(diffActiveManagement  = test_ActiveManagementEffect - (ActiveManagementEffect/100))  #


  # Expected Category Output----
  output$category_expected_table <- DT::renderDataTable(server = FALSE, {
    DT::datatable(category_dfCalc(),
                  extensions = c("Buttons", "KeyTable"), class = "cell-border stripe", options = list(
      autoWidth = TRUE, scrollX = TRUE, pageLength = 90, dom = "Bfrtip", scrollY = "600px",
      buttons = c("copy", "csv", "excel", "pdf", "print"), fixedColumns =  c(1:3), fixedHeader = TRUE, keys = TRUE
    ))
    # %>%#formatRound(columns = c(2:47), digits = 8) %>%
    #   formatStyle(c(
    #     "diffUnits", "diffWeights", "diffUnitPrice", "diffPerformance", "diffweightedPerformance",
    #     "diffBenchmarkPerf", "diffBenchmarkWeight", "diffWeightedBMPerf", "diffActiveWeight", "diffActiveReturn",
    #     "diffSelectionEffect", "diffAllocationEffect", "diffInteractionEffect", "diffActiveManagement"
    #   ),
    #   color = styleInterval(c(-0.001, 0.001), c("red", "green", "red")), fontWeight = "bold"
    #   ) %>%
    #   formatPercentage(columns = c(12:47), digits = 4)
  })

  # Diff Category Output----
  output$category_diff_table <- reactable::renderReactable({
    withProgress(message = "Generating data", style = "notification", {
      # df <- df_products_upload()
      data <- dfCalc() %>% select(
        HoldingsDate, OpeningMarketValue,
        Creations,
        Redemptions,
        ClosingMarketValue,
        diffUnits,
        diffUnitPrice,
        diffPerformance,
        diffWeights,
        diffweightedPerformance,
        diffBenchmarkPerf,
        diffBenchmarkWeight,
        diffWeightedBMPerf,
        diffActiveWeight,
        diffActiveReturn,
        diffSelectionEffect,
        diffAllocationEffect,
        diffInteractionEffect,
        diffActiveManagement
      )
    })

    withProgress(message = "Formatting grid, this might take a while...", style = "notification", { # notification/old
      reactable::reactable(data,
                           wrap = FALSE, fullWidth = TRUE,
                           defaultPageSize = 90, bordered = TRUE, compact = TRUE, height = 650, width = 1200,
                           showPageSizeOptions = TRUE,
                           resizable = TRUE, searchable = TRUE,
                           columns = list(
                             HoldingsDate = colDef(format = colFormat(date = TRUE, locales = "en-GB"), footer = "TOTAL", filterable = F),
                             OpeningMarketValue = colDef(
                               align = "left",
                               format = colFormat(percent = F, separators = TRUE, digits = 4),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
                               # defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0) {
                                   color <- "#777"
                                 } else if (as.numeric(as.character(values)) < 0) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#777"
                                 }
                                 list(color = color, fontWeight = "bold")
                               }
                             ),
                             Creations = colDef(
                               align = "left",
                               format = colFormat(percent = F, separators = TRUE, digits = 4),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0) {
                                   color <- "#777"
                                 } else if (as.numeric(as.character(values)) < 0) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#777"
                                 }
                                 list(color = color, fontWeight = "bold")
                               }
                             ),
                             Redemptions = colDef(
                               align = "left",
                               format = list(
                                 cell = colFormat(percent = F, separators = TRUE, digits = 4),
                                 aggregated = colFormat(percent = TRUE, digits = 6)
                               ),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0) {
                                   color <- "#777"
                                 } else if (as.numeric(as.character(values)) < 0) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#777" # "#777"
                                 }
                                 list(color = color, fontWeight = "bold")
                               }
                             ),
                             ClosingMarketValue = colDef(
                               align = "left",
                               format = colFormat(percent = F, separators = TRUE, digits = 4),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0) {
                                   color <- "#777"
                                 } else if (as.numeric(as.character(values)) < 0) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#777"
                                 }
                                 list(color = color, fontWeight = "bold")
                               }
                             ),
                             diffUnits = colDef(
                               align = "left",
                               format = colFormat(percent = F, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values)))),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffUnitPrice = colDef(
                               align = "left",
                               format = colFormat(percent = F, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffPerformance = colDef(
                               align = "left",
                               format = colFormat(percent = T, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffWeights = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffweightedPerformance = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffBenchmarkPerf = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffBenchmarkWeight = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffWeightedBMPerf = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffActiveWeight = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffActiveReturn = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffSelectionEffect = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffAllocationEffect = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < 0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffInteractionEffect = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) < -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             ),
                             diffActiveManagement = colDef(
                               align = "left",
                               format = colFormat(percent = TRUE, digits = 6),
                               footer = function(values) sprintf("%.4f", sum(as.numeric(as.character(values))) * 100),
                               style = function(values) {
                                 if (as.numeric(as.character(values)) > 0.001) {
                                   color <- " #e00000"
                                 } else if (as.numeric(as.character(values)) > -0.001) {
                                   color <- "#e00000"
                                 } else {
                                   color <- "#008000"
                                 }
                                 list(color = color, fontWeight = "regular")
                               }
                             )
                           ),
                           # Column groups----
                           columnGroups = list(
                             colGroup(name = "Valuation", columns = c("HoldingsDate", "OpeningMarketValue", "Creations", "Redemptions", "ClosingMarketValue")),
                             colGroup(name = "Performance", columns = c(
                               "diffUnits", "diffUnitPrice", "diffPerformance",
                               "diffWeights", "diffweightedPerformance", "diffBenchmarkPerf",
                               "diffBenchmarkWeight", "diffWeightedBMPerf"
                             )),
                             colGroup(name = "Attribution", columns = c(
                               "diffActiveWeight", "diffActiveReturn", "diffSelectionEffect",
                               "diffWeights", "diffweightedPerformance", "diffBenchmarkPerf",
                               "diffAllocationEffect", "diffInteractionEffect", "diffActiveManagement"
                             ))
                           ),

                           defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      )
    })
  })



}

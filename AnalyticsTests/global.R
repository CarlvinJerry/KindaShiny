
# # Global Options---------
# options(scipen = 999)
# options(digits = 8)

#
# ################################################
# #
# # 		ANALYTICS TESTS----
# #
# ###############################################
#
#
#
# # ---- PART1 : Check that the currently-installed version of R is at least the minimum required version.
# R_min_version <- "3.1"
# R_version <- paste0(R.Version()$major, ".", R.Version()$minor)
# if (compareVersion(R_version, R_min_version) < 0) {
#   stop(
#     "You do not have the latest required version of R installed.\n",
#     "Launch should fail.\n",
#     "Go to http://cran.r-project.org/ and update your version of R."
#   )
# }


#
# ############# ----- PART2: Install basic required packages if not available/installed.----
# install_missing_packages <- function(pkg,
#                                      version = NULL,
#                                      verbose = TRUE) {
#   availpacks <- .packages(all.available = TRUE)
#   # source("http://bioconductor.org/biocLite.R")
#   missingPackage <- FALSE
#   if (!any(pkg %in% availpacks)) {
#     if (verbose) {
#       message(
#         "The following package is missing.\n",
#         pkg,
#         "\n",
#         "Installation will be attempted..."
#       )
#     }
#     missingPackage <- TRUE
#   }
#   if (!is.null(version) & !missingPackage) {
#     # version provided and package not missing, so compare.
#     if (compareVersion(
#       a = as.character(packageVersion(pkg)),
#       b = version
#     ) < 0) {
#       if (verbose) {
#         message(
#           "Current version of package\n",
#           pkg,
#           "\t",
#           packageVersion(pkg),
#           "\n",
#           "is less than required.
#                 Update will be attempted."
#         )
#       }
#       missingPackage <- TRUE
#     }
#   }
#   if (missingPackage) {
#     # biocLite(i, suppressUpdates = TRUE)
#     print(pkg)
#     print(paste("---- installing a more recent version of", pkg, sep = ""))
#     install.packages(pkg, repos = "http://cran.r-project.org")
#   }
# }
#
#
# # Package list----
# # Define list of package names and required versions.
# deppkgs <- c(
#   shiny = "0.14.2",
#   plotly = "4.5.6",
#   ggplot2 = "2.2.0",
#   DT = "0.2",
#   shinythemes = "1.1",
#   shinyAce = "0.2.1",
#   RColorBrewer = "1.1.2",
#   qualV = "0.3.2",
#   colourpicker = "0.2",
#   gtools = "3.5.0",
#   RODBC = "1.3-16",
#   sodium = "1.1",
#   shinyjs = "1.0",
#   shinydashboard = "0.7.1",
#   reactable = "0.1.0",
#   shinycustomloader = "0.9.0",
#   formattable = "0.2.0.1",
#   dplyr = "0.8.3",
#   formattable = "0.2.0.1"
# )

# # Define list of package names and required versions.
#
  library(shiny)
  library(plotly)
  library(DT)
  library(shinythemes)
  library(shinyAce)
  library(shinyjs)
  library(shinydashboard)
  library(reactable)
  library(shinycustomloader)
  library(formattable)
  library(dplyr)
 library(formattable)


# # # Loop on package check, install, update----
# pkg1 <- mapply(
#   install_missing_packages,
#   pkg = names(deppkgs),
#   version = deppkgs,
#   MoreArgs = list(verbose = TRUE),
#   SIMPLIFY = FALSE,
#   USE.NAMES = TRUE
# )
# #################################################################################
# # Load packages that must be fully-loaded
# ################################################################################
# for (i in names(deppkgs)) {
#   library(i, character.only = TRUE)
#   message(i, " package version:\n", packageVersion(i))
# }
# ################################################################################
#
#


############### Theme selector function-----
themeSelector <- function() {
  div(
    div(
      selectInput("shinytheme-selector", "Choose a theme",
        c("default", shinythemes:::allThemes()),
        selectize = FALSE
      )
    ),
    tags$script(
      "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        //curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
    )
  )
}



# FUNCTIONS----
# Asset level expected output----
assetLevel <- function(data) {
  difs <- data %>%
    group_by(HoldingsDate) %>% # , AssetCategory

    # Units----
    mutate(test_Units = OpeningMarketValue + Creations + Redemptions) %>% # Units
    mutate(diffUnits = test_Units - Units) %>% # DIff Units

    # Unit Price----
    mutate(test_UnitPRice = ifelse(!ClosingMarketValue | !OpeningMarketValue, 1, ClosingMarketValue / test_Units)) %>% # Unit Price
    mutate(diffUnitPrice = test_UnitPRice - UnitPrice) %>% # Diff Unit Price

    # Portfolio Perf----
    mutate(test_ActualPerformance = ifelse((test_UnitPRice * 1) == 0,0,( test_UnitPRice - 1) )) %>% # ActualPerf
    mutate(diffActualPerformance = test_ActualPerformance - (ActualPerformance / 100)) %>% # Diff ActualPerformance

    # Portfolio Weight----
    mutate(test_Weight = ifelse(!test_Units, 0, test_Units / sum(test_Units))) %>% # Weights
    mutate(diffWeights = test_Weight - (Weight / 100)) %>% # Diff weights

    # Weighted Perf----
    mutate(test_weightedPerformance = test_Weight * test_ActualPerformance) %>% # Weighted Performance
    mutate(diffweightedPerformance = test_weightedPerformance - (WeightedPerformance / 100)) %>% # Diff Weighted Performance

    # TO DO - custom benchmarks perf----
    mutate(test_BenchmarkPerf = BenchmarkReturn / 100) %>% # Benchmark Perf to do
    mutate(diffBenchmarkPerf = test_BenchmarkPerf - (BenchmarkReturn / 100)) %>% # diff benchmark perf

    # Benchmark Weights----
    mutate(test_BenchmarkWeight = BenchmarkWeight/100) %>% # BM Weight
    mutate(diffBenchmarkWeight = test_BenchmarkWeight - BenchmarkWeight/100) %>% # Diff

    # Weighted Benchmark Perf----
    mutate(test_WeightedBMPerformance = test_BenchmarkWeight * test_BenchmarkPerf) %>%
    mutate(diffWeightedBMPerf = test_WeightedBMPerformance - (WeightedBenchmarkReturn / 100)) %>%

    # Active Weight----
    mutate(test_ActiveWeight = test_Weight - test_BenchmarkWeight) %>% # Active Weight
    mutate(diffActiveWeight = test_ActiveWeight - (ActiveWeight / 100)) %>% # Diff  Active weight

    # Active Return----
    mutate(test_ActiveReturn = test_ActualPerformance - test_BenchmarkPerf) %>% # Active per
    mutate(diffActiveReturn = test_ActiveReturn - (ActiveReturn / 100)) %>% # Diff  Active perf

    # selection Effect----
    mutate(test_SelectionEffect = (test_ActualPerformance - test_BenchmarkPerf) * test_BenchmarkWeight) %>% #
    mutate(diffSelectionEffect = test_SelectionEffect - (SelectionEffect / 100)) %>% #

    # Allocation effect----
    mutate(test_AllocationEffect = (test_BenchmarkPerf * test_Weight) - (test_BenchmarkWeight * test_BenchmarkPerf)) %>% #
    mutate(diffAllocationEffect = test_AllocationEffect - (AllocationEffect / 100)) %>% #

    # Interaction effect----
    mutate(test_InteractionEffect = test_ActiveReturn - test_AllocationEffect - test_SelectionEffect) %>% # =
    mutate(diffInteractionEffect = test_InteractionEffect - (InteractionEffect / 100)) %>% # =

    # Active Management----
    mutate(test_ActiveManagementEffect = test_AllocationEffect + test_SelectionEffect) %>% #
    mutate(diffActiveManagement = test_ActiveManagementEffect - (ActiveManagementEffect / 100)) #

 #Diffs table----
  difs %>%
    mutate(
      Units = Units, UnitPrice = UnitPrice, ActualPerformance1 = (ActualPerformance / 100),
      Weight1 = (Weight / 100), WeightedPerf = (WeightedPerformance / 100),BenchmarkWeight = (BenchmarkWeight/100),  BenchmarkPerf = (BenchmarkReturn / 100),
      WeightedBenchmarkPerf = (WeightedBenchmarkReturn / 100), ActiveWeight1 = (ActiveWeight / 100),
      ActiveReturn1 = (ActiveReturn / 100), SelectionEffect1 = (SelectionEffect / 100), AllocationEffect1 = (AllocationEffect / 100),
      InteractionEffect1 = (InteractionEffect / 100), ActiveManagementEffect1 = (ActiveManagementEffect / 100)
    ) %>%
    select(
      HoldingsDate,SecurityId, SecurityName, OpeningMarketValue,
      Creations,
      Redemptions,
      ClosingMarketValue,
      expUnits = test_Units, Units, diffUnits,
      expUnitPrice = test_UnitPRice, UnitPrice, diffUnitPrice,
      expPerformance = test_ActualPerformance, Performance = ActualPerformance1, diffPerformance = diffActualPerformance,
      expWeight = test_Weight, Weight = Weight1, diffWeights,
      expWeightedPerf = test_weightedPerformance, WeightedPerf, diffweightedPerformance,
      expBenchmkPerf = test_BenchmarkPerf, BenchmarkPerf, diffBenchmarkPerf,
      expBenchmarkWeight = test_BenchmarkWeight, BenchmarkWeight, diffBenchmarkWeight,
      expWeightedBmPerf = test_WeightedBMPerformance, WeightedBenchmarkPerf, diffWeightedBMPerf,
      expActiveWeight = test_ActiveWeight, ActiveWeight = ActiveWeight1, diffActiveWeight,
      expActiveReturn = test_ActiveReturn, ActiveReturn = ActiveReturn1, diffActiveReturn,
      expSelectionEffect = test_SelectionEffect, SelectionEffect = SelectionEffect1, diffSelectionEffect,
      expAllocationEff = test_AllocationEffect, AllocationEffect = AllocationEffect1, diffAllocationEffect,
      expInteractionEffect = test_InteractionEffect, InteractionEffect = InteractionEffect1, diffInteractionEffect,
      expActiveManagementEffect = test_ActiveManagementEffect, ActiveManagementEffect = ActiveManagementEffect1, diffActiveManagement
    )
}


# assetLevel(equities)



# timedeposits <- read.csv("C:\\Users\\carlvinm\\Desktop\\Analytics tests\\timedeposits.csv", stringsAsFactors = FALSE)

#equities <- read.csv("C:\\Users\\carlvinm\\Desktop\\Analytics tests\\equities.csv.csv", stringsAsFactors = FALSE)

# equities %>%
#   group_by(HoldingsDate) %>%
#   summarize(expOpeningMarketValue = sum(OpeningMarketValue), expCreations = sum(Creations), expRedemptions = sum(Redemptions), expClosingMarketValue = sum(ClosingMarketValue)) %>%
#   mutate(expUnits = expClosingMarketValue + expCreations + expRedemptions, expUnitPrice = expClosingMarketValue/expUnits, expPerformance = expUnitPrice - 1,
#          expWeight = expClosingMarketValue/expClosingMarketValue, expWeightedPerf = expWeight * expPerformance, expBMPerf = 1, expBMWeight = 1,
#          expWeightedBMPerf = expBMPerf* expBMWeight)




# View( assetLevel(timedeposits))

# # Asset level difference output----
# assetLevelDiff <- function(data) {
#
#   #Weights----
#   difs <- data %>%  group_by(HoldingsDate) %>% #, AssetCategory
#
#     #Units----
#   mutate(test_Units = OpeningMarketValue + Creations + Redemptions ) %>%  #Units
#     mutate(difUnits = test_Units - Units) %>% #DIff Units
#
#     #Unit Price----
#   mutate(test_UnitPRice = ifelse(!ClosingMarketValue,0,ClosingMarketValue/test_Units )) %>% #Unit Price
#     mutate(difUnitPrice = test_UnitPRice - UnitPrice ) %>% #Diff Unit Price
#
#     #Portfolio Perf----
#   mutate(test_ActualPerformance = test_UnitPRice - 1 ) %>% #ActualPerf
#     mutate(diffActualPerformance = test_ActualPerformance - (ActualPerformance/100) ) %>% #Diff ActualPerformance
#
#     #Portfolio Weight----
#   mutate(test_Weight = ifelse(!ClosingMarketValue,0,ClosingMarketValue/sum(ClosingMarketValue))) %>% #Weights
#     mutate(diffWeights = test_Weight - (Weight/100)) %>% #Diff weights
#
#     #Weighted Perf----
#   mutate(test_weightedPerformance = test_Weight * test_ActualPerformance) %>% #Weighted Performance
#     mutate(diffweightedPerformance = test_weightedPerformance - (WeightedPerformance/100)) %>% #Diff Weighted Performance
#
#     #TO DO - custom benchmarks perf----
#   mutate(test_BenchmarkPerf = BenchmarkReturn/100) %>% #Benchmark Perf to do
#     mutate(diffBenchmarkPerf = test_BenchmarkPerf - (BenchmarkReturn/100)) %>% #diff benchmark perf
#
#     #Benchmark Weights----
#   mutate(test_BenchmarkWeight = BenchmarkWeight) %>% #BM Weight
#     mutate(diffBenchmarkWeight = test_BenchmarkWeight - BenchmarkWeight) %>% #Diff  BM weight===NOT PERC----
#
#     #Weighted Benchmark Perf----
#   mutate(test_WeightedBMPerformance = test_BenchmarkWeight * test_BenchmarkPerf ) %>%
#     mutate(diffWeightedBMPerf = test_WeightedBMPerformance - (WeightedBenchmarkReturn/100)) %>%
#
#     #Active Weight----
#   mutate(test_ActiveWeight = test_Weight - test_BenchmarkWeight ) %>% #Active Weight
#     mutate(diffActiveWeight = test_ActiveWeight - (ActiveWeight/100)) %>% #Diff  Active weight
#
#     #Active Return----
#   mutate(test_ActiveReturn = test_ActualPerformance - test_BenchmarkPerf ) %>% #Active per
#     mutate(diffActiveReturn  = test_ActiveReturn - (ActiveReturn/100)) %>% #Diff  Active perf
#
#     #selection Effect----
#   mutate(test_SelectionEffect = (test_ActualPerformance - test_BenchmarkPerf) * test_BenchmarkWeight ) %>% #
#     mutate(diffSelectionEffect  = test_SelectionEffect - (SelectionEffect/100)) %>% #
#
#     #Allocation effect----
#   mutate(test_AllocationEffect = (test_BenchmarkPerf * test_Weight) - (test_BenchmarkWeight * test_BenchmarkPerf) ) %>% #
#     mutate(diffAllocationEffect  = test_AllocationEffect - (AllocationEffect/100)) %>% #
#
#     #Interaction effect----
#   mutate(test_InteractionEffect = test_ActiveReturn - test_AllocationEffect - test_SelectionEffect ) %>% #=
#     mutate(diffInteractionEffect  = test_InteractionEffect - (InteractionEffect/100)) %>% #=
#
#     #Active Management----
#   mutate(test_ActiveManagementEffect = test_AllocationEffect + test_SelectionEffect ) %>% #
#     mutate(diffActiveManagement  = test_ActiveManagementEffect - (ActiveManagementEffect/100))  #
#
# #Filter variables----
#   difs %>% select(
#     HoldingsDate, OpeningMarketValue,
#     Creations,
#     Redemptions,
#     ClosingMarketValue,
#     diffUnits,
#     diffUnitPrice,
#     diffActualPerformance,
#     diffWeights,
#     diffweightedPerformance,
#     diffBenchmarkPerf,
#     diffBenchmarkWeight,
#     diffWeightedBMPerf,
#     diffActiveWeight,
#     diffActiveReturn,
#     diffSelectionEffect,
#     diffAllocationEffect,
#     diffInteractionEffect,
#     diffActiveManagementEffect
#   )
# }
#




# names(timedeposits)
# assetLevelDiff(timedeposits)
# TO do
# grouping variable........


#
#   fileInput('target_upload', 'Choose file to upload',
#             accept = c(
#               'text/csv',
#               'text/comma-separated-values',
#               '.csv'
#             )),
#   radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
#   DT::dataTableOutput("sample_table")
# )
# )
#
# # Define server logic
# server <- shinyServer(function(input, output) {
#
#   df_products_upload <- reactive({
#     inFile <- input$target_upload
#     if (is.null(inFile))
#       return(NULL)
#     df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
#     return(df)
#   })
#
#   output$sample_table<- DT::renderDataTable({
#     df <- df_products_upload()
#     DT::datatable(df)
#   })
#
# }
# )
#
# # Run the application
# shinyApp(ui = ui, server = server)

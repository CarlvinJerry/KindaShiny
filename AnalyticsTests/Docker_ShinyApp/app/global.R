

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
  mutate(test_UnitPRice = ifelse(!ClosingMarketValue, 0, ClosingMarketValue / test_Units)) %>% # Unit Price
    mutate(diffUnitPrice = test_UnitPRice - UnitPrice) %>% # Diff Unit Price

    # Portfolio Perf----
  mutate(test_ActualPerformance = ifelse((test_UnitPRice * 1)==0,0,( test_UnitPRice - 1) )) %>% # ActualPerf
    mutate(diffActualPerformance = test_ActualPerformance - (ActualPerformance / 100)) %>% # Diff ActualPerformance

    # Portfolio Weight----
  mutate(test_Weight = ifelse(!ClosingMarketValue, 0, ClosingMarketValue / sum(ClosingMarketValue))) %>% # Weights
    mutate(diffWeights = test_Weight - (Weight / 100)) %>% # Diff weights

    # Weighted Perf----
  mutate(test_weightedPerformance = test_Weight * test_ActualPerformance) %>% # Weighted Performance
    mutate(diffweightedPerformance = test_weightedPerformance - (WeightedPerformance / 100)) %>% # Diff Weighted Performance

    # TO DO - custom benchmarks perf----
  mutate(test_BenchmarkPerf = BenchmarkReturn / 100) %>% # Benchmark Perf to do
    mutate(diffBenchmarkPerf = test_BenchmarkPerf - (BenchmarkReturn / 100)) %>% # diff benchmark perf

    # Benchmark Weights----
  mutate(test_BenchmarkWeight = BenchmarkWeight) %>% # BM Weight
    mutate(diffBenchmarkWeight = test_BenchmarkWeight - BenchmarkWeight) %>% # Diff  BM weight===NOT PERC----

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


  difs %>%
    mutate(
      Units = Units, UnitPrice = UnitPrice, ActualPerformance1 = (ActualPerformance / 100),
      Weight1 = (Weight / 100), WeightedPerf = (WeightedPerformance / 100), BenchmarkPerf = (BenchmarkReturn / 100),
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


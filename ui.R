# shiny packages
library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(fontawesome)
library(shinyBS)
library(fresh)
# useful R packages
library(R.utils)
library(tidyverse)
library(plotly)
library(datasets)

R.utils::sourceDirectory("./R/", modifiedOnly = F, recursive = F)

custom_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#35a4bf"
  ),
  fresh::adminlte_sidebar(
    width = "300px",
    dark_color = "#2c3b41"
  ),
  fresh::adminlte_global(
    content_bg = "white"
  )
)

# pretty_theme <- fresh::create_pretty(
#   primary = "#35a4bf",
#   output_file = "www/pretty.css"
# )

# Define UI for application
ui <- dashboardPage(
  # Header ---------------------------------------------------------------------
  dashboardHeader(
    disable = TRUE
  ),

  # Sidebar --------------------------------------------------------------------
  dashboardSidebar(
    disable = TRUE
  ),

  # Body -----------------------------------------------------------------------
  dashboardBody(
    # Setup --------------------------------------------------------------------
    shinyjs::useShinyjs(),
    htmltools::includeCSS("www/niss.css"),

    # Header -------------------------------------------------------------------
    div(
      id = "chalkboard",
      # span(fontawesome::fa("book-open"),
      #      position = "absolute", float = "left"),
      div("How have NAEP reading scores changed over time?", align = "center")
    ),

    # Info button --------------------------------------------------------------
    actionBttn(
      inputId = "help", style = "material-circle",
      size = "sm", icon = icon("info")
    ) %>%
      tagAppendAttributes(style = "position: absolute; top: 40px; right: 40px; background: #93b9a0; color: white"),
    shinyBS::bsPopover(
      id = "help",
      title = "NAEP Reading Scores",
      content = 'The National Assessment of Educational Progress (NAEP) is an educational assessment given every 2 years to a representative sample of 4th, 8th, and 12th grade students across the United States. The NAEP measures student progress in subjects such as reading and mathematics at the district, state, and national level. For more information, click <a href="https://www.nationsreportcard.gov/reading?grade=8">here</a>.',
      trigger = c("click", "hover")
    ),

    # Dashboard Theme ----------------------------------------------------------
    fresh::use_theme(custom_theme),
    fresh::use_pretty("www/pretty.css"),
    fresh::use_googlefont("Walter Turncoat"),

    # Main content -------------------------------------------------------------
    br(),
    fluidRow(
      # US Map Box -------------------------------------------------------------
      pretty_box(
        # Year slider ----------------------------------------------------------
        innerFluidRow(
          column(1),
          column(
            9,
            div(
              style = "margin: auto; width: 90%",
              sliderTextInput(
                inputId = "year", label = NULL,
                animate = animationOptions(loop = FALSE),
                choices = seq(2003, 2019, by = 2), grid = TRUE,
                hide_min_max = TRUE, width = "100%"
              ) %>%
                display_inline()
            )
          ),
        ),
        # US Map ---------------------------------------------------------------
        plotlyOutput("us_map", height = "100%"),
        # Click state text and source ------------------------------------------
        uiOutput("post_text"),

        # Box settings ---------------------------------------------------------
        width = 7, box_only = TRUE,
        title = "NAEP Reading Scores By State",
        color = "custom"
      ),

      # Sidebox ----------------------------------------------------------------
      pretty_box(

        # Trend button ---------------------------------------------------------
        radio_group_buttons(
          "trend_type",
          label = NULL, individual = TRUE,
          choices = c("Overall Trends", "Trends By Census Region")
        ) %>%
          tagAppendAttributes(style = "font-family: 'Walter Turncoat'"),

        # Stat text ------------------------------------------------------------
        uiOutput("pre_text"),

        # Click on year to compare ---------------------------------------------
        div(
          id = "compare_year_div",
          style = "margin-left: 40px; margin-top: 10px;",
          angle_right_link("compare_year_link",
            label = "Click to compare to a different year"
          ),
          picker_input("compare_year",
            label = NULL, choices = NULL,
            width = "fit"
          ) %>%
            tagAppendAttributes(style = "margin-left: 14px;") %>%
            display_inline() %>%
            hidden()
        ) %>% tagAppendAttributes(class = "invisible"),

        # Time series plot -----------------------------------------------------
        uiOutput("side_plot"),

        # Census region info ---------------------------------------------------
        conditionalPanel(
          "input.trend_type == 'Trends By Census Region'",
          div(
            id = "census_info_div",
            style = "margin-left: 40px; margin-top: 10px;",
            angle_right_link("census_info",
              label = "Learn how we compute these averages"
            ),
            shinydashboardPlus::blockQuote("Census region averages are computed by taking an average over all states within a region. This average is not adjusted for differences in the population because we do not know the underlying mechanism by which students are recuited into the sample. For this same reason, standard errors for census region averages are not reported. The census regions used here are those defined by the United States Census Bureau.") %>%
              tagAppendAttributes(
                id = "census_info_text",
                style = "font-size: 14px; margin-left: 10px; padding: 6px 12px;"
              ) %>%
              hidden()
          )
        ),

        # Click on state to compare --------------------------------------------
        div(
          id = "compare_state_div",
          style = "margin-left: 40px; margin-top: 10px;",
          angle_right_link("compare_state_link",
                           label = "Click to compare to other states"
          ),
          picker_multiple_input(
            "compare_state",
            label = NULL, maxOptions = 3, width = "fit",
            choices = c(state.name, "District of Columbia")
          ) %>%
            tagAppendAttributes(style = "margin-left: 14px;") %>%
            display_inline() %>%
            hidden()
        ) %>% tagAppendAttributes(class = "invisible"),

        # Back to national trends button ---------------------------------------
        div(
          style = "margin-left: 40px; margin-top: 10px;",
          rotate_left_link("back_to_nation",
            label = "Back to national trends"
          ) %>%
            hidden()
        ),

        # Box settings ---------------------------------------------------------
        title = htmlOutput("sidebox_title"),
        width = 5,
        color = "custom"
      )
    )
  )
)

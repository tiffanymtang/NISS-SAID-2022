#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output, session) {

  # retrieve education data from files
  getData <- reactive({
    data_dir <- file.path("Data")

    census_regions <- read.csv(
      file.path(data_dir, "census_regions.csv"),
      header = TRUE
    )

    df_educ <- read.csv(
      file.path(data_dir, "education.csv"),
      header = TRUE
    )

    df_educ_se <- read.csv(
      file.path(data_dir, "education_se.csv"),
      header = TRUE
    )

    df_educ_nation <- read.csv(
      file.path(data_dir, "education_avg.csv"),
      header = TRUE,
      check.names = FALSE
    ) %>%
      dplyr::select(!tidyselect::contains("SE")) %>%
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = "Year", values_to = "Score"
      ) %>%
      dplyr::mutate(Year = as.numeric(Year))

    df_educ_nation_se <- read.csv(
      file.path(data_dir, "education_avg.csv"),
      header = TRUE,
      check.names = FALSE
    ) %>%
      dplyr::select(tidyselect::contains("SE")) %>%
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = "Year", values_to = "SE"
      ) %>%
      dplyr::mutate(Year = as.numeric(stringr::str_remove(Year, "\\_SE")))

    df_educ_long <- df_educ %>%
      tidyr::pivot_longer(
        cols = -State,
        values_to = "Score", names_to = "Year"
      ) %>%
      dplyr::mutate(Year = as.numeric(stringr::str_remove(Year, "Year"))) %>%
      dplyr::filter(Year >= 2003)

    df_educ_se_long <- df_educ_se %>%
      tidyr::pivot_longer(
        cols = -State,
        values_to = "SE", names_to = "Year"
      ) %>%
      dplyr::mutate(Year = as.numeric(stringr::str_remove(Year, "Year"))) %>%
      dplyr::filter(Year >= 2003)

    data_long <- dplyr::inner_join(df_educ_long, df_educ_se_long,
      by = c("State", "Year")
    ) %>%
      dplyr::left_join(census_regions %>% dplyr::select(State, Region),
        by = "State"
      )

    data_region_long <- data_long %>%
      dplyr::group_by(Year, Region) %>%
      dplyr::summarise(Score = round(mean(Score), 2), .groups = "keep")

    data_nation_long <- dplyr::inner_join(df_educ_nation, df_educ_nation_se,
      by = "Year"
    ) %>%
      dplyr::filter(Year >= 2003)

    return(list(
      data_long = data_long,
      data_region_long = data_region_long,
      data_nation_long = data_nation_long
    ))
  })

  # compute state-level stats
  getStats <- reactive({
    data <- getData()

    if (is.null(input$compare_year) | identical(input$compare_year, "")) {
      compare_year <- 2003
    } else {
      compare_year <- as.numeric(input$compare_year)
    }

    yearly_stats <- data$data_long %>%
      dplyr::group_by(Year) %>%
      dplyr::mutate(
        Ranking = rank(-Score, ties.method = "first"),
      ) %>%
      dplyr::ungroup()

    yearly_improvement_stats <- yearly_stats %>%
      dplyr::mutate(PrevYear = compare_year) %>%
      dplyr::left_join(., ., by = c("State", "PrevYear" = "Year")) %>%
      dplyr::filter(!is.na(Score.y)) %>%
      dplyr::mutate(
        RankImproved = Ranking.y - Ranking.x,
        ScoreImproved = Score.x - Score.y,
        PercentImproved = (Score.x - Score.y) / Score.y * 100
      ) %>%
      dplyr::select(
        State, Year, PrevYear, RankImproved,
        ScoreImproved, PercentImproved
      )

    stats_data <- data$data_long %>%
      dplyr::left_join(yearly_stats %>% dplyr::select(State, Year, Ranking),
        by = c("State", "Year")
      ) %>%
      dplyr::left_join(yearly_improvement_stats, by = c("State", "Year"))

    stats_text_data <- stats_data %>%
      dplyr::mutate(
        State_str = add_class("state", State),
        Year_str = add_class("year", Year),
        Score_str = add_class("score", Score),
        SE = abs(SE),
        Ranking_str = add_class("rank", Ranking),
        RankImproved_str = add_class(
          "rank_improved", abs(RankImproved),
          color = ifelse(RankImproved > 0, "#417064",
            ifelse(RankImproved < 0, "#BC7965", "black")
          )
        ),
        ScoreImproved_str = add_class(
          "score_improved", Score + ScoreImproved,
          color = ifelse(ScoreImproved > 0, "#417064",
            ifelse(ScoreImproved < 0, "#BC7965", "black")
          )
        ),
        PercentImproved_str = add_class(
          "percent_improved",
          paste0(formatC(abs(PercentImproved), digits = 2, format = "f"), "%%"),
          color = ifelse(ScoreImproved > 0, "#417064",
            ifelse(ScoreImproved < 0, "#BC7965", "black")
          )
        )
      )
    return(stats_text_data)
  })

  # get click data from map
  clickData <- reactive({
    click_data <- plotly::event_data("plotly_click", source = "map")

    if (!is.null(click_data)) {
      shinyjs::show("back_to_nation")
      shinyjs::hide("trend_type")
      shinyjs::removeClass("compare_state_div", "invisible")
      shinyjs::hide("census_info_div")
      v$national <- FALSE

      return(click_data$customdata %>% setNames("state"))
    } else {
      v$national <- TRUE
      shinyjs::hide("back_to_nation")
      shinyjs::show("trend_type")
      shinyjs::addClass("compare_state_div", "invisible")
      shinyjs::show("census_info_div")

      return(click_data)
    }
  })

  # compute nation-level stats
  getNationalTrends <- reactive({
    data <- getData()

    year <- input$year
    if (is.null(input$compare_year) | identical(input$compare_year, "")) {
      prev_year <- 2003
    } else {
      prev_year <- as.numeric(input$compare_year)
    }

    Score <- data$data_nation_long$Score[data$data_nation_long$Year == year]
    change <- data$data_nation_long$Score[data$data_nation_long$Year == year] - data$data_nation_long$Score[data$data_nation_long$Year == prev_year]
    pct_change <- change * 100 / data$data_nation_long$Score[data$data_nation_long$Year == prev_year]

    n_improved <- getStats() %>%
      dplyr::filter(Year == !!year, PrevYear == !!prev_year) %>%
      dplyr::summarise(n = sum(ScoreImproved > 0)) %>%
      pull(n)

    text_data <- dplyr::tibble(
      Year = year,
      Score = Score,
      change = change,
      prev_year = prev_year,
      n_improve = n_improved,
      Year_str = add_class("year", Year),
      Score_str = add_class("score", Score),
      ScoreImproved_str = add_class(
        "score_improved", Score + change,
        color = ifelse(change > 0, "#417064",
          ifelse(change < 0, "#BC7965", "black")
        )
      ),
      Change_str = add_class(
        "score_improved", abs(change),
        color = ifelse(change > 0, "#417064",
          ifelse(change < 0, "#BC7965", "black")
        )
      ),
      PercentImproved_str = add_class(
        "percent_improved",
        paste0(formatC(abs(pct_change), digits = 2, format = "f"), "%%"),
        color = ifelse(pct_change > 0, "#417064",
          ifelse(pct_change < 0, "#BC7965", "black")
        )
      ),
      N_Improve_str = add_class("n_improve", n_improve, color = "#417064")
    )

    return(text_data)
  })

  # compute census-level stats
  getRegionalTrends <- reactive({
    data <- getData()

    year <- input$year
    if (is.null(input$compare_year) | identical(input$compare_year, "")) {
      prev_year <- 2003
    } else {
      prev_year <- as.numeric(input$compare_year)
    }

    region_colors <- c("#9585AA", "#64B892", "#F5CD57", "#F4B071")

    grab_region_info <- function(region, year, region_colors) {
      df_region <- data$data_region_long %>% filter(Region == !!region)
      Score <- df_region$Score[df_region$Year == year]
      change <- df_region$Score[df_region$Year == year] - df_region$Score[df_region$Year == prev_year]
      pct_change <- change * 100 / df_region$Score[df_region$Year == prev_year]
      region_color <- "black"
      if (region == "Northeast") {
        region_color <- region_colors[1]
      } else if (region == "Midwest") {
        region_color <- region_colors[2]
      } else if (region == "South") {
        region_color <- region_colors[3]
      } else {
        region_color <- region_colors[4]
      }
      text_data <- dplyr::tibble(
        Region = region,
        Region_str = add_class("score_improved", region, color = region_color),
        Year = year,
        Score = Score,
        change = change,
        prev_year = prev_year,
        Year_str = add_class("year", Year),
        Score_str = add_class("score", formatC(Score, digits = 1, format = "f")),
        ScoreImproved_str = add_class(
          "score_improved", Score + change,
          color = ifelse(change > 0, "#417064",
            ifelse(change < 0, "#BC7965", "black")
          )
        ),
        Change_str = add_class(
          "score_improved", formatC(abs(change), digits = 1, format = "f"),
          color = ifelse(change > 0, "#417064",
            ifelse(change < 0, "#BC7965", "black")
          )
        ),
        PercentImproved_str = add_class(
          "percent_improved",
          paste0(formatC(abs(pct_change), digits = 2, format = "f"), "%%"),
          color = ifelse(pct_change > 0, "#417064",
            ifelse(pct_change < 0, "#BC7965", "black")
          )
        )
      )
    }

    region_data <- lapply(unique(data$data_region_long$Region), grab_region_info, year = year, region_colors = region_colors) %>% bind_rows()

    return(region_data)
  })

  # make national trends text
  output$national_trends <- renderText({
    if (input$trend_type == "Overall Trends") {
      if (input$year == 2003) {
        "Over the last two decades, average NAEP reading scores across the United States have remained <b>relatively stable</b>, with a slight trend upward."
      } else {
        national_trends <- getNationalTrends()
        text_template1 <- "In %s, the average NAEP reading score in the United States was %s."
        if (national_trends$change > 0) {
          text_template2 <- paste0(
            "This was an increase of %s point",
            ifelse(abs(national_trends$change) == 1,
              " ", "s "
            ),
            "from %s."
          )
        } else if (national_trends$change == 0) {
          text_template2 <- "This was the same as the score from %2$s."
        } else {
          text_template2 <- paste0(
            "This was a decrease of %s point",
            ifelse(abs(national_trends$change) == 1,
              " ", "s "
            ),
            "from %s."
          )
        }
        text_template3 <- paste0(
          "A total of <b>%s</b> state",
          ifelse(national_trends$n_improve == 1,
            " ", "s "
          ),
          "increased their average NAEP reading score since %s."
        )
        paste(
          sprintf(
            text_template1,
            national_trends$Year_str, national_trends$Score_str
          ),
          sprintf(
            text_template2,
            national_trends$Change_str, national_trends$prev_year
          ),
          sprintf(
            text_template3,
            national_trends$N_Improve_str, national_trends$prev_year
          )
        )
      }
    } else if (input$trend_type == "Trends By Census Region") {
      if (input$year == 2003) {
        "Over the last two decades, average NAEP reading scores in each census region have remained <b>relatively stable</b>, with the Northeast and Midwest consistently scoring higher on average. However, the West and the South have the greatest improvement in reading scores since 2003."
      } else {
        regional_trends <- getRegionalTrends()
        opening_text <- paste(sprintf("In %s, the average NAEP reading scores across census regions were... <ul>", regional_trends$Year_str[1]))
        generate_text_region <- function(region) {
          region_trends <- regional_trends %>% filter(Region == !!region)
          if (region_trends$change > 0) {
            text <- "<li> %s in the %s (%s point increase from %s) </li>"
          } else if (region_trends$change < 0) {
            text <- "<li> %s in the %s (%s point decrease from %s) </li>"
          } else {
            text <- "<li> %s in the %s (No change from %s) </li>"
          }
          sprintf(
            text,
            region_trends$Score_str, region_trends$Region_str,
            region_trends$Change_str, region_trends$prev_year
          )
        }

        paste(
          opening_text,
          paste(sapply(
            c("Northeast", "Midwest", "West", "South"),
            generate_text_region
          ),
          collapse = " "
          ),
          "</ul>"
        )
      }
    }
  })

  # make state trends text
  output$state_facts <- renderText({
    click_data <- clickData()
    req(click_data)
    stats_text_data <- getStats()
    year <- input$year
    state <- click_data[["state"]]

    state_data <- stats_text_data %>%
      dplyr::filter(State == !!state, Year == !!year) %>%
      dplyr::select(-Region) %>%
      dplyr::slice(1)

    if (year == 2003) {
      text_template <- "In %s, the average NAEP reading score in %s was %s. This was rank %s out of 51 in NAEP reading scores."
      sprintf(
        text_template,
        state_data$Year_str, state_data$State_str,
        state_data$Score_str, state_data$Ranking_str
      )
    } else {
      text_template1 <- "In %s, %s was ranked %s out of 51 in NAEP reading score."
      if (is.na(state_data$RankImproved)) {
        return()
      }
      if (abs(state_data$RankImproved) == 1) {
        place_str <- "place"
      } else {
        place_str <- "places"
      }
      if (state_data$RankImproved > 0) {
        text_template2 <- paste("This is %s", place_str, "higher than in %s.")
      } else if (state_data$RankImproved < 0) {
        text_template2 <- paste("This is %s", place_str, "lower than in %s.")
      } else {
        text_template2 <- "This is the same rank as in %2$s."
      }
      if (state_data$ScoreImproved > 0) {
        text_template3 <- "The average NAEP reading score increased from %s to %s between %s and %s."
      } else if (state_data$ScoreImproved < 0) {
        text_template3 <- "The average NAEP reading score decreased from %s to %s between %s and %s."
      } else {
        text_template3 <- "The average NAEP reading score remained the same at %s between %3$s and %4$s."
      }

      paste(
        sprintf(
          text_template1, state_data$Year_str, state_data$State_str,
          state_data$Ranking_str
        ),
        sprintf(text_template2, state_data$RankImproved_str, input$compare_year),
        sprintf(
          text_template3, state_data$Score_str,
          state_data$ScoreImproved_str,
          input$compare_year, input$year
        )
      )
    }
  })

  # click on state text
  output$click_state_text <- renderText({
    "<b>How is your state doing? </b> Click a state on the map to learn more."
  })

  # census region time series
  output$census_time_series_plotly <- renderPlotly({
    data <- getData()
    df_plt <- data$data_nation_long %>%
      dplyr::mutate(SE_text = paste0("SE: ", SE))

    nation_color <- "#5CA1BB"
    region_colors <- c("#9585AA", "#64B892", "#F4B071", "#F5CD57")

    if (input$trend_type == "Overall Trends") {
      plt <- df_plt %>%
        ggplot2::ggplot(aes(x = Year, y = Score)) +
        ggplot2::geom_vline(
          xintercept = input$year, color = "gray", alpha = 0.8
        ) +
        ggplot2::geom_line(color = nation_color) +
        ggplot2::geom_point(ggplot2::aes(text = SE_text),
          color = nation_color
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(x = Year, ymin = Score - 1 * SE, ymax = Score + 1 * SE),
          alpha = 0.2, fill = nation_color
        ) +
        ggplot2::labs(y = "Average NAEP Reading Score") +
        ggplot2::coord_cartesian(
          xlim = c(2003, 2019.3),
          ylim = c(
            255,
            275
          ),
          expand = FALSE
        ) +
        theme_vmodern(bg_color = "white", grid_color = "white")

      plt1 <- plotly_build(plt)
      plt1$x$data[[1]]$hoverinfo <- "none"
      plt1$x$data[[2]]$hoverinfo <- "none"
      plt1$x$data[[4]]$hoverinfo <- "none"
      plotly::ggplotly(plt1, tooltip = "text") %>%
        plotly::layout(
          showlegend = FALSE,
          xaxis = list(
            ticktext = seq(2003, 2019, by = 2),
            tickvals = seq(2003, 2019, by = 2),
            tickmode = "array",
            fixedrange = T
          ),
          yaxis = list(fixedrange = T)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    } else if (input$trend_type == "Trends By Census Region") {
      df_plt <- rbind(
        data$data_region_long,
        data$data_nation_long %>%
          dplyr::select(Year, Score) %>%
          dplyr::mutate(Region = "Overall US")
      ) %>%
        dplyr::mutate(Region = factor(Region,
          levels = c(
            "Overall US", "Northeast",
            "Midwest", "West", "South"
          )
        ))
      plt <- df_plt %>%
        ggplot2::ggplot(aes(x = Year, y = Score, color = Region)) +
        ggplot2::geom_vline(xintercept = input$year, color = "gray", alpha = 0.8) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        theme_vmodern(bg_color = "white", grid_color = "white") +
        ggplot2::coord_cartesian(
          xlim = c(2003, 2019.3),
          ylim = c(
            255,
            275
          ),
          expand = FALSE
        ) +
        ggplot2::guides(text = "none", color = "none") +
        ggplot2::ylab("Average NAEP Reading Score") +
        ggplot2::scale_color_manual(values = c(nation_color, region_colors))

      plotly::ggplotly(plt) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(
            ticktext = seq(2003, 2019, by = 2),
            tickvals = seq(2003, 2019, by = 2),
            tickmode = "array",
            fixedrange = T
          ),
          yaxis = list(fixedrange = T)
        )
    }
  })

  # us map
  output$us_map <- renderPlotly({
    data <- getData()

    fig <- data$data_long %>%
      dplyr::filter(Year == !!input$year) %>%
      dplyr::left_join(
        data.frame(
          State = c(state.name, "District of Columbia"),
          StateAbb = c(state.abb, "DC")
        ),
        by = "State"
      ) %>%
      plotly::plot_geo(
        locationmode = "USA-states", source = "map", height = 415
      ) %>%
      plotly::add_trace(
        z = ~Score, locations = ~StateAbb, color = ~Score,
        customdata = ~State, text = ~ paste(State, paste0("Score: ", Score), sep = "<br>"),
        hoverinfo = "text", showlegend = FALSE,
        colorscale = list(
          list(0, "#BC7965"),
          list(0.5, "#D4A98D"),
          list(0.625, "#ECDBCD"),
          list(0.7, "#B8CFC7"),
          list(0.75, "#7A9F94"),
          list(1, "#417064")
        )
      ) %>%
      plotly::layout(
        geo = list(scope = "usa", projection = list(type = "albers usa")),
        hoverlabel = list(font = list(size = 14)),
        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
      ) %>%
      plotly::event_register("plotly_click") %>%
      plotly::colorbar(limits = c(238, 278)) %>%
      plotly::config(displayModeBar = FALSE)
    fig
  })

  # state time series
  output$state_time_series <- renderPlotly({
    click_data <- clickData()
    req(click_data)
    data <- getData()

    plot_state_series <- function(data_long, state, year, comparison_states) {
      state_color <- "#F4B071"
      nation_color <- "#5CA1BB"
      comparison_colors <- c("#64B892", "#9585AA", "#F5CD57")
      df_plt <- data_long %>%
        dplyr::bind_rows(
          dplyr::bind_cols(State = "US", data$data_nation_long)
        ) %>%
        dplyr::mutate(
          SE = abs(SE),
          SE_text = paste0("SE: ", SE)
        ) %>%
        dplyr::filter(State == !!state | State == "US")

      if (!is.null(comparison_states)) {
        df_plt <- rbind(
          df_plt,
          data_long %>%
            dplyr::mutate(
              SE = abs(SE),
              SE_text = paste0("SE: ", SE)
            ) %>%
            dplyr::filter(State %in% comparison_states)
        ) %>%
          dplyr::mutate(
            State = factor(State, levels = c(state, comparison_states, "US"))
          )
        color_vector <- c(
          state_color,
          comparison_colors[1:length(comparison_states)],
          nation_color
        )
      } else {
        df_plt$State <- factor(df_plt$State, levels = c(state, "US"))
        color_vector <- c(state_color, nation_color)
      }


      plt <- df_plt %>%
        dplyr::rename(CensusRegion = Region, Region = State) %>%
        ggplot2::ggplot() +
        ggplot2::geom_vline(
          xintercept = input$year, color = "gray", alpha = 0.8
        ) +
        ggplot2::geom_line(
          ggplot2::aes(x = Year, y = Score, color = Region)
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = Year, y = Score, color = Region, text = SE_text)
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = Year,
            ymin = Score - 1 * SE, ymax = Score + 1 * SE,
            fill = Region
          ),
          alpha = 0.2
        ) +
        theme_vmodern(bg_color = "white", grid_color = "white") +
        ggplot2::scale_color_manual(name = "", values = color_vector) +
        ggplot2::scale_fill_manual(values = color_vector, guide = "none") +
        ggplot2::scale_x_continuous(
          breaks = seq(2003, 2019, by = 2),
          limits = c(2003, 2019.3),
          expand = c(0, 0)
        ) +
        ggplot2::ylab("Average NAEP Reading Score") +
        ggplot2::coord_cartesian(
          xlim = c(2003, 2019.3),
          ylim = c(236, 280),
          expand = FALSE
        )
      plt1 <- plotly_build(plt)
      if (!is.null(comparison_states)) {
        k <- length(comparison_states) + 2
        plt1$x$data[[2]]$name <- state
        plt1$x$data[[2 * k + 2]]$hoverinfo <- "none"
        for (i in 2:(length(comparison_states) + 1)) {
          plt1$x$data[[i + 1]]$name <- comparison_states[i - 1]
          plt1$x$data[[2 * k + i + 1]]$hoverinfo <- "none"
        }
        plt1$x$data[[i + 2]]$name <- "United States"
        plt1$x$data[[2 * k + i + 2]]$hoverinfo <- "none"
      } else {
        plt1$x$data[[2]]$name <- state
        plt1$x$data[[3]]$name <- "United States"
        # Don't hover over interval:
        plt1$x$data[[5 + 1]]$hoverinfo <- "none"
        plt1$x$data[[6 + 1]]$hoverinfo <- "none"
      }

      plotly::ggplotly(plt1) %>%
        plotly::config(displayModeBar = FALSE)
    }
    plot_state_series(
      data$data_long, click_data["state"],
      input$year, input$compare_state
    )
  })

  # keep track of national vs state side box
  v <- reactiveValues(
    national = FALSE
  )

  # go back to national trends button
  observeEvent(input$back_to_nation, {
    shinyjs::hide("back_to_nation")
    shinyjs::show("trend_type")
    shinyjs::addClass("compare_state_div", "invisible")
    shinyjs::show("census_info_div")
    v$national <- TRUE
  })

  # text before plot
  output$pre_text <- renderUI({
    if (v$national) {
      fluidPage(htmlOutput("national_trends"))
    } else {
      fluidPage(htmlOutput("state_facts"))
    }
  })

  # render side plot
  output$side_plot <- renderUI({
    if (v$national) {
      fluidPage(plotlyOutput("census_time_series_plotly"))
    } else {
      fluidPage(plotlyOutput("state_time_series"))
    }
  })

  # data source text
  output$source_text <- renderText({
    'Source: <a href="https://nces.ed.gov/programs/digest/d19/tables/dt19_221.60.asp">National Center for Education Statistics</a>'
  })

  # text after plot
  output$post_text <- renderUI({
    fluidPage(
      htmlOutput("click_state_text"),
      htmlOutput("source_text")
    )
  })

  # title of sidebox
  output$sidebox_title <- renderText({
    if (v$national) {
      "National Trends"
    } else {
      paste0("How is ", clickData()["state"], " doing?")
    }
  })

  # update compare state choices
  observe({
    click_data <- clickData()
    req(click_data)
    updatePickerInput(session, "compare_state",
      choices = setdiff(
        c(state.name, "District of Columbia"),
        click_data["state"]
      )
    )
  })

  # update compare year choices
  observe({
    year <- input$year
    if (year != 2003) {
      if (year != 2005) {
        shinyjs::removeClass("compare_year_div", "invisible")
      }
      updatePickerInput(session, "compare_year",
        selected = year - 2,
        choices = seq(2003, year - 1, by = 2)
      )
    } else {
      shinyjs::addClass("compare_year_div", "invisible")
    }
  })

  # toggle compare year
  observeEvent(input$compare_year_link, {
    if (input$year != 2003) {
      shinyjs::toggle("compare_year")
    }
  })

  # toggle census info
  observeEvent(input$census_info, {
    shinyjs::toggle("census_info_text")
  })

  # toggle compare state
  observeEvent(input$compare_state_link, {
    shinyjs::toggle("compare_state")
  })

  # make active slider text bold
  observeEvent(input$year, {
    shinyjs::removeClass(selector = ".irs-grid>.irs-grid-text", class = "bold-class")
    shinyjs::addClass(
      selector = sprintf(".irs-grid>.irs-grid-text:contains('%s')", input$year),
      class = "bold-class"
    )
  })
})

#' Add custom styling to action link with angle-right icon
angle_right_link <- function(...) {
  actionLink(..., icon = icon("angle-right", style = "font-size: 16px;"))
}

#' Add custom styling to action link with the rotate-left icon
rotate_left_link <- function(...) {
  actionLink(..., icon = icon("rotate-left", style = "font-size: 12px;"))
}

#' Add class to text element
add_class <- function(class, text, color = NULL, el = "span") {
  if (is.null(color)) {
    sprintf(paste0("<%s class=%s >", text, "</%s>"), el, class, el)  
  } else {
    sprintf(
      paste0("<%s class=%s style=\'color: %s !important;\'>", text, "</%s>"),
      el, class, color, el
    )
  }
}

#' Display inline block
display_inline <- function(obj) {
  obj %>% tagAppendAttributes(style = "display: inline-block;")
}

#' Remove margins
remove_margins <- function(obj) {
  obj %>% tagAppendAttributes(style = "margin: 0px;")
}

#' Full width box
pretty_box <- function(..., title = NULL, width = 12, color = NULL,
                       status = "primary", solidHeader = TRUE, 
                       box_only = FALSE) {
  if (!is.null(title)) {
    title <- htmltools::tags$b(title)
  }
  if (!box_only) {
    fluidRow(box(..., title = title, color = color,
                 width = width, status = status, solidHeader = solidHeader))
  } else {
    box(..., title = title, color = color,
        width = width, status = status, solidHeader = solidHeader)
  }
}

#' Inner fluidPage that removes negative margins when used inside pretty_box()
innerFluidRow <- function(...) {
  fluidRow(...) %>% remove_margins()
}

#' Copy of vthemes::vmodern so don't have to install vthemes for deployment
theme_vmodern <- function (base_family = "", size_preset = NULL, bg_color = "grey98", 
                           strip_bg_color = "#2c3e50", grid_color = "grey90", axis_line_width = 1, 
                           show_ticks = TRUE, x_text_angle = FALSE, axis_title_size = 10, 
                           axis_text_size = 7, legend_title_size = 10, legend_text_size = 8, 
                           strip_text_size = 9, title_size = 12, ...) 
{
  if (!is.null(size_preset)) {
    if (!(size_preset %in% c("small", "medium", "large")) && 
        !(stringr::str_detect(size_preset, ".*large$") && 
          identical(unique(strsplit(stringr::str_replace(size_preset, 
                                                         "large$", ""), "")[[1]]), "x"))) {
      stop("Unknown size_preset. size_preset must be one of 'small', 'medium', ", 
           "'large', 'xlarge', 'xxlarge', or '{some number of x's}large'.")
    }
    num_x <- stringr::str_count(size_preset, "x")
    axis_title_size <- dplyr::case_when(size_preset == "small" ~ 
                                          10, size_preset == "medium" ~ 14, TRUE ~ 18 + num_x * 
                                          2)
    axis_text_size <- dplyr::case_when(size_preset == "small" ~ 
                                         7, size_preset == "medium" ~ 10, TRUE ~ 14 + num_x * 
                                         2)
    legend_title_size <- axis_title_size
    legend_text_size <- dplyr::case_when(size_preset == "small" ~ 
                                           8, size_preset == "medium" ~ 10, TRUE ~ 14 + num_x * 
                                           2)
    strip_text_size <- dplyr::case_when(size_preset == "small" ~ 
                                          9, size_preset == "medium" ~ 12, TRUE ~ 16 + num_x * 
                                          2)
    title_text_size <- dplyr::case_when(size_preset == "small" ~ 
                                          12, size_preset == "medium" ~ 16, TRUE ~ 20 + num_x * 
                                          2)
  }
  thm <- ggplot2::theme_grey(base_family = base_family) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, 
                                                                                                            face = "bold"), axis.text = ggplot2::element_text(size = axis_text_size), 
                                                                         axis.line = ggplot2::element_line(size = axis_line_width, 
                                                                                                           color = "black"), axis.ticks = ggplot2::element_line(size = ifelse(show_ticks, 
                                                                                                                                                                              ggplot2::rel(1), 0), colour = "black"), axis.text.x = ggplot2::element_text(angle = ifelse(x_text_angle, 
                                                                                                                                                                                                                                                                         45, 0), hjust = ifelse(x_text_angle, 1, 0.5)), panel.grid.major = ggplot2::element_line(colour = grid_color, 
                                                                                                                                                                                                                                                                                                                                                                 size = ggplot2::rel(0.5)), panel.grid.minor = ggplot2::element_blank(), 
                                                                         panel.background = ggplot2::element_rect(fill = bg_color), 
                                                                         strip.background = ggplot2::element_rect(fill = strip_bg_color, 
                                                                                                                  color = strip_bg_color), strip.text = ggplot2::element_text(size = strip_text_size, 
                                                                                                                                                                              color = "white", face = "bold"), legend.key = ggplot2::element_rect(fill = "grey98"), 
                                                                         legend.text = ggplot2::element_text(size = legend_text_size), 
                                                                         legend.title = ggplot2::element_text(size = legend_title_size, 
                                                                                                              face = "bold"), plot.title = ggplot2::element_text(size = title_size, 
                                                                                                                                                                 face = "bold")) + ggplot2::theme(...)
  return(thm)
} 
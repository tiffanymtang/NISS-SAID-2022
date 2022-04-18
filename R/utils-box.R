box <- function(..., title = NULL, footer = NULL, status = NULL,
                solidHeader = FALSE, background = NULL, width = 6,
                height = NULL, collapsible = FALSE, collapsed = FALSE,
                dropdown = NULL, color = NULL) {
  
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    # validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (!is.null(color)) {
    boxClass <- paste0(boxClass, " box-", color)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    # validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", title)
  }
  
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    
    collapseIcon <- if (collapsed) "plus" else "minus"
    
    collapseTag <- div(class = "box-tools pull-right",
                       tags$button(class = paste0("btn btn-box-tool"),
                                   `data-widget` = "collapse",
                                   shiny::icon(collapseIcon)
                       )
    )
  } 
  
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header",
                     titleTag,
                     collapseTag
    )
  }
  
  dropdownTag <- NULL
  if (!is.null(dropdown)) {
    dropdownTag <- div(class = "box-settings-dropdown", dropdown)
  }
  
  div(class = if (!is.null(width)) paste0("col-sm-", width),
      div(class = boxClass,
          style = if (!is.null(style)) style,
          headerTag,
          div(class = "box-body", dropdownTag, ...),
          if (!is.null(footer)) div(class = "box-footer", footer)
      )
  )
}


validateColor <- function(color) {
  validColors <- c("red", "yellow", "aqua", "blue", "light-blue", "green",
                   "navy", "teal", "olive", "lime", "orange", "fuchsia",
                   "purple", "maroon", "black")
  
  if (color %in% validColors) {
    return(TRUE)
  }
  
  stop("Invalid color: ", color, ". Valid colors are: ",
       paste(validColors, collapse = ", "), ".")
}


validateStatus <- function(status) {
  validStatuses <- c("primary", "success", "info", "warning", "danger")
  
  if (status %in% validStatuses) {
    return(TRUE)
  }
  
  stop("Invalid status: ", status, ". Valid statuses are: ",
       paste(validStatuses, collapse = ", "), ".")
}
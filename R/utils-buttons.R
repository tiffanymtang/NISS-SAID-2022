#' Wrapper around `prettyRadioButtons()`
#'
#' @description Wrapper around `prettyRadioButtons()` but with different
#'   default settings.
#'
#' @inheritParams shinyWidgets::prettyRadioButtons
#' @param ... Additional arguments to pass to
#'   `shinyWidgets::prettyRadioButtons()`.
#'
radio_buttons <- function(inputId, label, choices, selected = NULL,
                          status = "primary", animation = "jelly",
                          icon = shiny::icon("check"), bigger = TRUE, ...) {
  prettyRadioButtons(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  )
}


#' Wrapper around `radioGroupButtons()`
#'
#' @description Wrapper around `radioGroupButtons()` but with different
#'   default settings.
#'
#' @inheritParams shinyWidgets::radioGroupButtons
#' @param ... Additional arguments to pass to
#'   `shinyWidgets::radioGroupButtons()`.
#'
radio_group_buttons <- function(inputId, label, choices, selected = NULL,
                                status = "default", size = "sm",
                                justified = FALSE, ...) {
  radioGroupButtons(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    size = size,
    justified = justified,
    ...
  )
}


#' Wrapper around `pickerInput()` to select one choice
#'
#' @description Wrapper around `pickerInput()` to select one choice but with
#'   different default settings.
#'
#' @inheritParams shinyWidgets::pickerInput
#' @param ... Additional arguments to pass to `shinyWidgets::pickerInput()`.
#'
picker_input <- function(inputId, label, choices, selected = NULL,
                         options = NULL, ...) {
  if (is.null(options)) {
    options <- list(`live-search` = TRUE,
                    size = 5,
                    title = "Nothing selected")
  }
  pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    options = options,
    ...
  )
}


#' Wrapper around `pickerInput()` to select multiple choices
#'
#' @description Wrapper around `pickerInput()` to select multiple choices but
#'   with different default settings.
#'
#' @inheritParams shinyWidgets::pickerInput
#' @param actionsBox Logical. Whether or not to include actions box.
#' @param maxOptions Numeric. Max number of options that can be selected.
#' @param ... Additional arguments to pass to `shinyWidgets::pickerInput()`.
#'
picker_multiple_input <- function(inputId, label, choices, selected = NULL,
                                  actionsBox = TRUE, maxOptions = NULL,
                                  options = NULL, ...) {

  if (is.null(options)) {
    options <- list(`live-search` = TRUE,
                    size = 5,
                    `selected-text-format` = "count > 3",
                    title = "Nothing selected",
                    multipleSeparator = ", ",
                    `actions-box` = actionsBox,
                    `max-options` = maxOptions)
  }

  pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = TRUE,
    options = options,
    ...
  )
}


#' Wrapper around `prettyCheckbox()`
#'
#' @description Wrapper around `prettyCheckbox()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::prettyCheckbox
#' @param ... Additional arguments to pass to `shinyWidgets::prettyCheckbox()`.
#'
checkbox <- function(inputId, label, value = FALSE,
                     status = "primary", animation = "jelly",
                     icon = shiny::icon("check"), bigger = TRUE, ...) {
  prettyCheckbox(
    inputId = inputId,
    label = label,
    value = value,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  )
}


#' Wrapper around `prettyCheckboxGroup()`
#'
#' @description Wrapper around `prettyCheckboxGroup()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::prettyCheckboxGroup
#' @param ... Additional arguments to pass to 
#'   `shinyWidgets::prettyCheckboxGroup()`.
#'
checkbox_group <- function(inputId, label, choices, selected = NULL,
                           status = "primary", animation = "jelly",
                           icon = shiny::icon("check"), bigger = TRUE, ...) {
  prettyCheckboxGroup(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  )
}


#' Wrapper around `materialSwitch()`
#'
#' @description Wrapper around `materialSwitch()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::materialSwitch
#' @param ... Additional arguments to pass to `shinyWidgets::materialSwitch()`.
#'
material_switch <- function(inputId, label, value = TRUE, status = "primary",
                            right = FALSE, inline = FALSE, ...) {
  # prettySwitch(
  #   inputId = inputId,
  #   label = tags$b(label),
  #   value = value,
  #   status = status,
  #   inline = inline,
  #   ...
  # )
  
  materialSwitch(
    inputId = inputId,
    label = tags$b(label),
    value = value,
    status = status,
    inline = inline,
    right = right,
    ...
  )
}


#' Submit button
#'
#' @description Wrapper around `actionButton` to create a submit button
#' 
#' @inheritParams shiny::actionButton
#' @param id ID.
#' @param br Logical indicating whether or not to add breaks above and below
#'    button.
#' @param ... Additional arguments to pass to `shiny::actionButton()`.
#' 
submit_button <- function(inputId, label = "Submit", br = TRUE, 
                          style = "material-circle", color = "danger", ...) {
  btn <- actionBttn(inputId = inputId, label = label, style = style, ...) %>%
    tagAppendAttributes(
      style = paste0("background-color: ", color, ";")
    ) %>%
    display_inline()
  
  if (br) {
    return(tagList(br(), btn, br()))
  } else {
    return(btn)
  }
}


#' Wrapper around `dropdown()`
#'
#' @description Wrapper around `dropdown()` with different default settings
#'
#' @inheritParams shinyWidgets::dropdown
#'
options_dropdown <- function(...,
                             status = "primary", size = "sm",
                             icon = shiny::icon("cog"), width = "300px",
                             right = TRUE, style = "material-circle",
                             tooltip = FALSE) {
  dropdown(
    ...,
    status = status, size = size, icon = icon, width = width, style = style,
    tooltip = tooltip, right = right
  )
}

#' Significance level input
#'
#' @description Customized numeric input button using `numericInput()`
#'
#' @inheritParams shiny::numericInput
#' @param id ID.
#'
alpha_input <- function(id, prefix = NULL, value = 0.05, step = 0.001) {
  ns <- NS(id)
  numericInput(
    inputId = ns_prefix(ns, prefix, "alpha"), label = "Significance Level",
    value = value, min = 0, max = 1, step = step
  )
}


#' Reset button
#'
#' @description Customized action button using `actionButton()`.
#'
#' @inheritParams shiny::actionButton
#' @param ... Additional arguments to pass to `shiny::actionButton()`.
#'
reset_button <- function(inputId = "reset_input", label = "Reset all inputs",
                         ...) {
  div(style = "display:inline-block; width:48%; text-align:center;",
      actionButton(inputId = inputId, label = label, ...))
}


#' Reset event
reset_event <- function(input, session) {
  observeEvent(input$reset_input, {
    confirmSweetAlert(
      session, inputId = "confirm_reset",
      title = "Are you sure you want to reset all inputs?",
      type = "info",
      btn_labels = c("No", "Yes"),
      btn_colors = c("#FE642E", "#04B404"),
      showCloseButton = T
    )
  })
  observeEvent(input$confirm_reset, {
    if (isTRUE(input$confirm_reset)) {
      shinyjs::refresh()
      # updateRadioGroupButtons(session, "tab", selected = "instructions")
      # shinyjs::reset("sidebarItemExpanded")
    }
  }, ignoreNULL = TRUE)
}


#' Continue button
#'
#' @description Customized action button using `actionButton()`.
#'
#' @inheritParams shiny::actionButton
#' @param ... Additional arguments to pass to `shiny::actionButton()`.
#'
continue_button <- function(inputId = "continue", label = "Next stage", ...) {
  div(style = "display:inline-block; width:48%; text-align:center;",
      conditionalPanel(
        'input.tab != "sensitivity"',
        actionButton(inputId = inputId, label = label, ...)
      ))
}


#' Disable tab walking and reversing method inputs
disable_walking <- function(input) {
  observe({
    shinyjs::disable("tab")
    if (input$tab == "instructions") {
      js$closeMenuItems()
      js$openMenuItem("data_input")
      shinyjs::disable(selector = "a[href='#shiny-tab-method_input']")
      addClass(id = "menu_method_input", "disabled")
      shinyjs::disable(selector = "a[href='#shiny-tab-sensitivity_input']")
      addClass(id = "menu_sensitivity_input", "disabled")
    } else if (input$tab == "eda") {
      js$disableMenuInputs("data_input")
    } else if (input$tab == "design") {
      js$closeMenuItems()
      shinyjs::enable(selector = "a[href='#shiny-tab-method_input']")
      removeClass(id = "menu_method_input", "disabled")
      js$openMenuItem("method_input")
    } else if (input$tab == "analysis") {
      js$disableMenuInputs("method_input")
    } else if (input$tab == "sensitivity") {
      js$closeMenuItems()
      shinyjs::enable(selector = "a[href='#shiny-tab-sensitivity_input']")
      removeClass(id = "menu_sensitivity_input", "disabled")
      js$openMenuItem("sensitivity_input")
    }
  })
}


#' Go to next stage
continue_event <- function(input, session) {
  observeEvent(input$continue, {
    if (input$tab == "instructions") {
      msg <- "Are you sure you want to continue?"
    } else if (input$tab == "eda") {
      msg <- "Are you sure you want to continue?"
    } else if (input$tab == "design") {
      msg <- "Does covariate balance look ok?"
    } else if (input$tab == "analysis") {
      msg <- "Are you sure you want to continue?"
    }
    confirmSweetAlert(
      session, inputId = "confirm",
      title = msg,
      type = "info",
      btn_labels = c("No", "Yes"),
      btn_colors = c("#FE642E", "#04B404"),
      showCloseButton = TRUE
    )
  })

  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      if (input$tab == "instructions") {
        updateRadioGroupButtons(session, "tab", selected = "eda")
      } else if (input$tab == "eda") {
        if (input$eda_tab == "Data Overview") {
          updateRadioGroupButtons(session, "eda_tab", 
                                  selected = "Build Your Own EDA")
        } else {
          updateRadioGroupButtons(session, "tab", selected = "design")
        }
      } else if (input$tab == "design") {
        updateRadioGroupButtons(session, "tab", selected = "analysis")
      } else if (input$tab == "analysis") {
        updateRadioGroupButtons(session, "tab", selected = "sensitivity")
      }
    }
  }, ignoreNULL = TRUE)
}


#' Collapsible sidebar button
collapsible_sidebar_button <- function() {
  div(HTML('<button class="bttn-collapse action-button bttn bttn-simple bttn-sm bttn-default bttn-no-outline" id="collapse_sidebar" type="button"><i class="fa fa-bars"></i></button>'))
}


#' Collapsible sidebar button event
collapsible_sidebar_event <- function(input) {
  observeEvent(input$collapse_sidebar, {
    shinyjs::toggle(id = "side-panel")
    toggleCssClass("main-panel", "col-sm-9")
    toggleCssClass("main-panel", "col-sm-12")
  })
}


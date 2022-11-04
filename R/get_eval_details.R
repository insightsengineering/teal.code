## Module ----
#' Shows Evaluation Details Modal
#'
#' @description `r lifecycle::badge("deprecated")`
#' Chunks are being deprecated `qenv` objects should be used instead
#'
#' Use the [shiny::showModal()] function to show the errors generated from chunks.
#'
#' @param chunks (`chunks`)\cr
#'  object, from which the messages, warnings and errors are gathered
#'
#' @return `shiny.tag`
#' @export
#'
#' @references [shiny::showModal()] [get_eval_details_srv()]
show_eval_details_modal <- function(chunks) {
  showModal(modalDialog(
    tagList(
      tags$p(modalButton("Dismiss"), style = "margin-bottom: 15px;"),
      tags$label("Main evaluation message"),
      tags$pre(paste0(chunks$get_eval_msg(), collapse = "\n")),
      tags$hr(),
      do.call(
        teal.widgets::panel_group,
        lapply(
          chunks$eval_info(),
          function(x) {
            teal.widgets::panel_item(
              title = tagList(
                x$id,
                `if`(x$eval_info$flag, span(class = "text-success", icon("check")), NULL),
                `if`(x$message_info$flag, span(class = "text-info", icon("comment")), NULL),
                `if`(x$warning_info$flag, span(class = "text-warning", icon("triangle-exclamation")), NULL),
                `if`(x$error_info$flag, span(class = "text-danger", icon("circle-exclamation")), NULL)
              ),
              tagList(
                tags$label("Code:"),
                tags$pre(paste0(styler::style_text(x$code), collapse = "\n")),
                tags$label("Evaluated?"),
                tags$pre(x$eval_info$flag),
                tags$label("Messages:"),
                tags$pre(paste0(crayon::strip_style(x$message_info$msg), collapse = "\n")),
                tags$label("Warnings:"),
                tags$pre(paste0(crayon::strip_style(x$warning_info$msg), collapse = "\n")),
                tags$label("Errors:"),
                tags$pre(paste0(crayon::strip_style(x$error_info$msg), collapse = "\n"))
              )
            )
          }
        )
      )
    ),
    title = NULL,
    footer = tagList(
      modalButton("Dismiss")
    ),
    size = "l",
    easyClose = TRUE
  ))
}

#' Server part of \code{get_chunks_info}
#'
#' @description `r lifecycle::badge("deprecated")`
#' Chunks are being deprecated `qenv` objects should be used instead
#' @inheritParams shiny::moduleServer
#' @inheritParams show_eval_details_modal
#'
#' @return an observer for \code{evaluation_details actionButton}
#'
#' @export
#'
get_eval_details_srv <- function(id, chunks) {
  lifecycle::deprecate_warn(
    when = "0.2.1",
    what = "get_eval_details_srv()",
    details = "Chunks are being deprecated qenv objects should be used instead"
  )

  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  moduleServer(id, function(input, output, session) {
    output$eval_details_count <- renderUI({
      summary <- chunks$get_reactive_summary()
      eval_info_summary <- c(
        notes = sum(summary$msgs),
        warnings = sum(summary$warnings),
        errors = sum(summary$errors)
      )

      if (isTRUE(all(eval_info_summary == 0))) {
        icon("check")
      } else {
        span(
          `if`(
            eval_info_summary["notes"] > 0,
            get_number_in_box(eval_info_summary["notes"], box_color = "#0087c8", char_color = "#ffffff"),
            span()
          ),
          `if`(
            eval_info_summary["warnings"] > 0,
            get_number_in_box(
              eval_info_summary["warnings"],
              box_color = "#ff9d00",
              char_color = "#ffffff"
            ),
            span()
          ),
          `if`(
            eval_info_summary["errors"] > 0,
            get_number_in_box(eval_info_summary["errors"], box_color = "#ff0000", char_color = "#ffffff"),
            span()
          )
        )
      }
    })

    observeEvent(input$evaluation_details, {
      show_eval_details_modal(chunks = chunks)
    })
  })
}

#' The `UI` part of get chunks info module
#'
#' @description `r lifecycle::badge("deprecated")`
#' Chunks are being deprecated `qenv` objects should be used instead
#'
#' @param id (`character`)\cr
#' id of a shiny module
#'
#' @return (\code{shiny.tag})
#'
#' @export
get_eval_details_ui <- function(id) {
  ns <- NS(id)
  tagList(
    include_css_files(pattern = "debug_info_fa"),
    actionButton(
      ns("evaluation_details"),
      div(
        "Debug Info",
        uiOutput(
          ns("eval_details_count"),
          inline = TRUE
        )
      ),
      width = "100%"
    )
  )
}



#' Creates a `shiny::tag` element, which represents a colored box
#' with a character inside of it.
#'
#' @details Uses the Font-Awesome library of icons to get the box icon.
#'
#' @param character (`character` or `numeric` value)\cr
#'   a single string put inside a box
#' @param box_color  (`character` value)\cr
#'   hex code of a color (including #) or a recognized CSS3 color name
#' @param char_color (`character` value)\cr
#'   hex code of a color (including #) or a recognized CSS3 color name
#'
#' @keywords internal
#' @return `shiny::tag` with a character of color `char_color` inside
#'   a box of color `box_color`
get_number_in_box <- function(character, box_color, char_color) {
  checkmate::assert_string(as.character(character))
  checkmate::assert_string(box_color)
  checkmate::assert_string(char_color)

  tags$span(
    class = "fa-stack",
    div(
      style = paste0("color:", box_color),
      icon("square", class = "fa-stack-2x")
    ),
    tags$strong(
      character,
      class = "fa-stack-1x",
      style = paste0("color:", char_color)
    )
  )
}

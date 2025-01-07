#' Display `qenv` object
#'
#' Prints the `qenv` object.
#'
#' @param object (`qenv`)
#'
#' @return `object`, invisibly.
#'
#' @examples
#' q <- qenv()
#' q1 <- eval_code(q, expression(a <- 5, b <- data.frame(x = 1:10)))
#' q1
#'
#' @aliases show-qenv
#'
#' @importFrom methods show
#' @export
setMethod("show", "qenv", function(object) {
  env <- get_env(object)
  header <- cli::col_blue(sprintf("<environment: %s>", rlang::env_label(env)))
  parent <- sprintf("Parent: <environment: %s>", rlang::env_label(rlang::env_parent(env)))
  cat(cli::style_bold(header), "\U1F512", "\n")
  cat(parent, "\n")

  shown <- ls(object)
  if (length(shown > 0L)) cat(cli::style_bold("Bindings:\n"))
  lapply(shown, function(x) {
    cat(
      sprintf(
        "- %s: [%s]\n",
        deparse(rlang::sym(x), backtick = TRUE),
        class(object[[x]])[1]
      )
    )
  })

  hidden <- setdiff(ls(object, all.names = TRUE), shown)
  lapply(hidden, function(x) {
    cat(
      cli::style_blurred(
        sprintf(
          "- %s: [%s]\n",
          deparse(rlang::sym(x), backtick = TRUE),
          class(object[[x]])[1]
        )
      )
    )
  })

  invisible(object)
})

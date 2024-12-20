#' Instantiates a `qenv` environment
#'
#' @description
#' `r badge("stable")`
#'
#' Instantiates a `qenv` environment.
#'
#' @details
#' `qenv` class has following characteristics:
#'
#' - It inherits from the environment and methods such as [`$`], [get()], [ls()], [as.list()],
#' [parent.env()] work out of the box.
#' - `qenv` is a locked environment, and data modification is only possible through the [eval_code()]
#'   and [within.qenv()] functions.
#' - It stores metadata about the code used to create the data (see [get_code()]).
#' - It supports slicing (see [`subset-qenv`])
#' - It is immutable which means that each code evaluation does not modify the original `qenv`
#'   environment directly. See the following code:
#'
#'   ```
#'   q1 <- qenv()
#'   q2 <- eval_code(q1, "a <- 1")
#'   identical(q1, q2) # FALSE
#'   ```
#'
#' @name qenv
#'
#' @return `qenv` environment.
#'
#' @seealso [eval_code()], [get_var()], [`subset-qenv`], [get_env()],[get_warnings()], [join()], [concat()]
#' @examples
#' q <- qenv()
#' q2 <- within(q, a <- 1)
#' ls(q2)
#' q2$a
#' @export
qenv <- function() {
  methods::new("qenv")
}

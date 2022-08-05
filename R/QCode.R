setClass(
  "QCode",
  representation(code = "character", id = "integer"),
  prototype(code = character(0), id = integer(0)),
  validity = function(object) {
    if (length(object@code) != length(object@id)) {
      return("@code and @id slots must have the same length.")
    }
    if (any(duplicated(object@id))) {
      return("@id contains duplicated values.")
    }

    TRUE
  }
)

setGeneric("new_qcode", function(code = character(0)) {
  standardGeneric("new_qcode")
})

setMethod(
  "new_qcode", signature(code = "character"),
  function(code) {
    code <- .keep_code_name_unique(code)
    id <- sample.int(.Machine$integer.max, size = length(code))
    new("QCode", code = code, id = id)
  }
)

setMethod(
  "new_qcode", signature(code = "missing"),
  function(code) {
    new("QCode")
  }
)

#' If two `QCode` can be joined
#'
#' Checks if two `QCode` objects can be combined.
#' They can't be combined if (and):
#' - both share the same code (identified by `id`)
#' - indices of the shared code are not consecutive or don't start from 1
#' @param x
#' @param y
#' @param
#' @return logical(1)
setGeneric("check_joinable", function(x, y) {
  standardGeneric("check_joinable")
})

setMethod(
  "check_joinable", signature(x = "QCode", y = "QCode"),
  function(x, y) {
    shared_ids <- intersect(x@id, y@id)
    if (length(shared_ids) == 0) return(TRUE)

    shared_in_x <- match(shared_ids, x@id)
    shared_in_y <- match(shared_ids, y@id)

    # indices of shared ids should be 1:n in both slots
    if (identical(shared_in_x, shared_in_y) && identical(shared_in_x, seq_along(shared_ids))) {
      TRUE
    } else if (!identical(shared_in_x, shared_in_y)) {
      paste(
        "Common code of joined objects doesn't have the same indices. Thismeans that `x` and `y` have ",
        "can't be joined together as it's not possible to determine order of the evaluation.",
        collapse = ""
      )
    } else {
      paste(
        "Common code of joined object doesn't start from index = 1. This means that joined object(x) have ",
        "some extra code elements before.",
        collapse = ""
      )
    }
  }
)

#' Join two `Quosure` objects
#'
#' Combine two `Quosure` object by merging their environments and the code.
#' Not every `Quosure` objects can be combined:
#' - if their environments contains objects of the same name but not identical
#' - if `object2` has unique code element placed before common element. This means that `object2`
#' in the environment of the `object2` was evaluated some extra code before which can influence
#' reproducibility
#' - more cases todo
#' @param object (`Quosure`)
#' @param object2 (`Quosure`)
#' @export
setGeneric("join", function(x, y) {
  standardGeneric("join")
})

setMethod(
  "join", signature(x = "QCode", y = "QCode"),
  function(x, y) {
    join_validation <- check_joinable(x, y)
    if (!isTRUE(join_validation)) {
      stop(join_validation)
    }

    id_unique <- !y@id %in% x@id
    x@id <- c(x@id, y@id[id_unique])
    x@code <- .keep_code_name_unique(x@code, y@code[id_unique])

    x
  }
)

setMethod(
  "join", signature(x = "QCode", y = "character"),
  function(x, y) {
    qcode2 <- new_qcode(y)
    join(x, qcode2)
  }
)


.keep_code_name_unique <- function(x, y = character(0)) {
  combined <- c(x, y)
  if (length(names(combined)) == 0) return(combined)
  setNames(combined, make.unique(names(combined)))
}
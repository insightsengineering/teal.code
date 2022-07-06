setClass(
  "Quosure",
  representation(code = "character", env = "environment"),
  prototype(code = character(0), env = new.env(parent = parent.env(.GlobalEnv)))
)

setGeneric("new_quosure", function(code = character(0), env = parent.env(.GlobalEnv)) {
  standardGeneric("new_quosure")
})

setMethod(
  "new_quosure", signature(code = "missing", env = "missing"),
  function(code, env) {
    new("Quosure")
  }
)

setMethod(
  "new_quosure", signature(code = "character"),
  function(code = character(0), env = parent.env(.GlobalEnv)) {
    checkmate::check_class(env, "environment")
    checkmate::check_string(code)

    lockEnvironment(env)

    if (is.null(names(code))) {
      names(code) <- "code"
    }
    new("Quosure", code = code, env = env)
  }
)

setGeneric("get_var", function(object, var) {
  standardGeneric("get_var")
})
setGeneric("get_code", function(object, deparse = FALSE) {
  standardGeneric("get_code")
})
setGeneric("join", function(object, object2, overwrite = FALSE) {
  standardGeneric("join")
})

#' Evaluate the code in the `Quosure` environment
#'
#' Given code is evaluated in the `Quosure` environment and appended to the `code` slot. This means
#' that state of the environment is always a result of the stored code (if `Quosure` was initialized)
#' with reproducible code.
#'
#' @name eval_code
#'
#' @param object (`Quosure`)
#' @param code (`character(1)` or `language`) code to evaluate. Also acceptps and stores comments
#' @param name (`character(1)`) name of the code element.
#'
#' @export
setGeneric("eval_code", function(object, code, name = "code") {
  standardGeneric("eval_code")
})

#' @rdname eval_code
#' @export
setMethod(
  "eval_code", signature("Quosure", "character"),
  function(object, code, name = "code") {
    # combine code vector (and make names unique)
    nm <- make.unique(c(names(object@code), name))
    object@code <- setNames(c(object@code, code), nm)

    # need to copy the objects from old env to new env
    # to avoid "chunks" sharing the same environment
    object@env <- .copy_env(object@env)
    eval(parse(text = code), envir = object@env)
    lockEnvironment(object@env)
    object
  }
)

#' @rdname eval_code
#' @export
setMethod(
  "eval_code", signature("Quosure", "language"),
  function(object, code, name = "code") {
    code_char <- deparse1(code)
    eval_code(object, code_char, name = name)
  }
)

#' @rdname eval_code
#' @export
setMethod(
  "eval_code", signature("Quosure", "expression"),
  function(object, code, name = "code") {
    code_char <- as.character(code)
    eval_code(object, code_char, name = name)
  }
)

#' Get object from the `Quosure` environment
#'
#' Get object from the `Quosure` environment.
#' @param object (`Quosure`)
#' @param var (`character(1)`)
setMethod("get_var", signature("Quosure", "character"), function(object, var) {
  get(var, envir = object@env)
})

#' Get the code from the `Quosure`
#'
#' @param object (`Quosure`)
setMethod("get_code", signature("Quosure"), function(object) {
  object@code
})


#' Join two `Quosure` objects
#'
#' Combine two `Quosure` object by merging their environments and the code.
#' Not every `Quosure` objects can be combined:
#' -
#' @param object (`Quosure`)
#' @param object2 (`Quosure`)
setMethod("join", signature("Quosure", "Quosure"), function(object, object2, overwrite = FALSE) {

  # object2 can't have modified object of the same name! See chunks_push_chunks
  common_names <- intersect(ls(object@env), ls(object2@env))
  is_identical_obj <- vapply(common_names, function(x) {
    identical(get(x, object@env), get(x, object2@env))
  }, logical(1))
  if (any(isFALSE(is_identical_obj)) && !overwrite) {
    stop(
      "join does not allow overwriting already calculated values.",
      " Following variables would have been overwritten:",
      paste("    -", names(is_identical_obj), collapse = "\n"),
      sep = "\n"
    )
  }

  # join expressions
  # > join should be an inteligent union
  unique_expr <- !(object2@code %in% object@code & names(object2@code) %in% names(object@code))
    # duplicated expr will probably have the same name also
    # unique_expr should have TRUE only at the end of the vector.
  object@code <- c(object@code, object2@code[unique_expr])

  # insert new objects to env
  new_names <- setdiff(ls(object2@env), ls(object@env))

  object@env <- .copy_env(object@env)
  lapply(new_names, function(x) assign(x, get(x, object2@env), object@env))

  object
})


.copy_env <- function(env) {
  # need to copy the objects from old env to new env
  # to avoid "chunks" sharing the same environment
  env_new <- new.env()
  for (i in ls(env)) {
    env_new[[i]] <- env[[i]]
  }
  env_new
}

.resolve_code_vector <- function(code, code2) {
  # only possible pattern for extra code: both are completelly different
  # then A <- B (but there should be no common variable names!)? what about rm method?

  # if  B has extra code which is not  at the end
  # only one object can have extra code at the

  # if B has extra code at the end and no object conflict
  # if A has extra code at the end and no object conflict
  #

  code_only_in_a <- which(!code %in% code2)
  code_only_in_b <- which(!code2 %in% code)

  if (!identical(
    code_only_in_a,
    tail(seq_along(code), length(code_only_in_a))
  )) {
    # a can only have extra code at the end of the queue
    stop("only last index")
  }

  if (!identical(
    code_only_in_b,
    tail(seq_along(code2), length(code_only_in_b))
  )) {
    # b can only have extra code at the end of the queue
    stop("only last index")
  }

}
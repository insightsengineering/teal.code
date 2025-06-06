% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qenv-c.R, R/qenv-join.R
\name{c.qenv}
\alias{c.qenv}
\alias{c.qenv.error}
\alias{join}
\alias{join,qenv,qenv-method}
\alias{join,qenv,qenv.error-method}
\alias{join,qenv.error,ANY-method}
\title{Join \code{qenv} objects}
\usage{
\method{c}{qenv}(...)

\method{c}{qenv.error}(...)

join(x, y)
}
\arguments{
\item{...}{(\code{qenv} or \code{qenv.error}).}

\item{x}{(\code{qenv})}

\item{y}{(\code{qenv})}
}
\value{
\code{qenv} object.
}
\description{
Checks and merges two \code{qenv} objects into one \code{qenv} object.

The \code{join()} function is superseded by the \code{c()} function.
}
\details{
Any common code at the start of the \code{qenvs} is only placed once at the start of the joined \code{qenv}.
This allows consistent behavior when joining \code{qenvs} which share a common ancestor.
See below for an example.

There are some situations where \code{join()} cannot be properly performed, such as these three scenarios:
\enumerate{
\item Both \code{qenv} objects contain an object of the same name but are not identical.

Example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{x <- eval_code(qenv(), expression(mtcars1 <- mtcars))
y <- eval_code(qenv(), expression(mtcars1 <- mtcars['wt']))

z <- c(x, y)
# Error message will occur
}\if{html}{\out{</div>}}

In this example, \code{mtcars1} object exists in both \code{x} and \code{y} objects but the content are not identical.
\code{mtcars1} in the \verb{x qenv} object has more columns than \code{mtcars1} in the \verb{y qenv} object (only has one column).
\item \code{join()} will look for identical code elements in both \code{qenv} objects.
The index position of these code elements must be the same to determine the evaluation order.
Otherwise, \code{join()} will throw an error message.

Example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{common_q <- eval_code(qenv(), expression(v <- 1))
x <- eval_code(
  common_q,
  "x <- v"
)
y <- eval_code(
  common_q,
  "y <- v"
)
z <- eval_code(
  y,
  "z <- v"
)
q <- c(x, y)
join_q <- c(q, z)
# Error message will occur

# Check the order of evaluation based on the id slot
}\if{html}{\out{</div>}}

The error occurs because the index position of common code elements in the two objects is not the same.
\item The usage of temporary variable in the code expression could cause \code{join()} to fail.

Example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{common_q <- qenv()
x <- eval_code(
  common_q,
  "x <- numeric(0)
   for (i in 1:2) \{
     x <- c(x, i)
   \}"
)
y <- eval_code(
  common_q,
  "y <- numeric(0)
   for (i in 1:3) \{
     y <- c(y, i)
   \}"
)
q <- join(x,y)
# Error message will occur

# Check the value of temporary variable i in both objects
x$i # Output: 2
y$i # Output: 3
}\if{html}{\out{</div>}}

\code{c()} fails to provide a proper result because of the temporary variable \code{i} exists
in both objects but has different value.
To fix this, we can set \code{i <- NULL} in the code expression for both objects.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{common_q <- qenv()
x <- eval_code(
  common_q,
  "x <- numeric(0)
   for (i in 1:2) \{
     x <- c(x, i)
   \}
   # dummy i variable to fix it
   i <- NULL"
)
y <- eval_code(
  common_q,
  "y <- numeric(0)
   for (i in 1:3) \{
     y <- c(y, i)
   \}
   # dummy i variable to fix it
   i <- NULL"
)
q <- c(x,y)
}\if{html}{\out{</div>}}
}
}
\examples{
q <- qenv()
q1 <- within(q, {
  iris1 <- iris
  mtcars1 <- mtcars
})
q1 <- within(q1, iris2 <- iris)
q2 <- within(q1, mtcars2 <- mtcars)
qq <- c(q1, q2)
cat(get_code(qq))

q <- qenv()
q1 <- eval_code(q, expression(iris1 <- iris, mtcars1 <- mtcars))
q2 <- q1
q1 <- eval_code(q1, "iris2 <- iris")
q2 <- eval_code(q2, "mtcars2 <- mtcars")
qq <- join(q1, q2)
cat(get_code(qq))

common_q <- eval_code(q, quote(x <- 1))
y_q <- eval_code(common_q, quote(y <- x * 2))
z_q <- eval_code(common_q, quote(z <- x * 3))
join_q <- join(y_q, z_q)
# get_code only has "x <- 1" occurring once
cat(get_code(join_q))

}

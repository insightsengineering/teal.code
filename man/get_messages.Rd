% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qenv-get_messages.r
\name{get_messages}
\alias{get_messages}
\alias{get_messages,qenv-method}
\alias{get_messages,qenv.error-method}
\alias{get_messages,NULL-method}
\title{Get messages from \code{qenv} object}
\usage{
get_messages(object)
}
\arguments{
\item{object}{(\code{qenv})}
}
\value{
\code{character} containing warning information or \code{NULL} if no messages.
}
\description{
Retrieve all messages raised during code evaluation in a \code{qenv}.
}
\examples{
data_q <- qenv()
data_q <- eval_code(data_q, "iris_data <- iris")
warning_qenv <- eval_code(
  data_q,
  bquote(p <- hist(iris_data[, .("Sepal.Length")], ff = ""))
)
cat(get_messages(warning_qenv))

}

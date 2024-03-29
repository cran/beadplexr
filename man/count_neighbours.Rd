% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{count_neighbours}
\alias{count_neighbours}
\title{Count neighbours for each element in a binary matrix}
\usage{
count_neighbours(x)
}
\arguments{
\item{x}{The matrix to count the neighbours in.}
}
\value{
A matrix of the same size as \code{x} giving the number of elements with non-zero
values in neighbouring cells of the matrix.
}
\description{
Count neighbours for each element in a binary matrix
}
\examples{

x <- matrix(
c(1L, 1L, 0L,
  1L, 1L, 0L,
  0L, 1L, 1L),
nrow = 3,
ncol = 3,
byrow = TRUE
)
beadplexr:::count_neighbours(x)

x <- matrix(
  c(TRUE, TRUE, FALSE,
    TRUE, TRUE, FALSE,
   FALSE, TRUE, TRUE),
  nrow = 3,
  ncol = 3,
 byrow = TRUE
)
beadplexr:::count_neighbours(x)

x <- matrix(
  c(1, 1, 0, 0,
    1, 0, 0, 0,
    0, 1, 1 ,1,
    0, 1, 1, 1),
  nrow = 4,
  ncol = 4,
  byrow = TRUE
)
beadplexr:::count_neighbours(x)

\dontrun{
  x <- matrix(
    c("1", "1", "0",
      "1", "1", "0",
      "0", "1", "1"),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  #beadplexr:::count_neighbours(x)
}

}
\keyword{internal}

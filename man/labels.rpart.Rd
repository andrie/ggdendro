\name{labels.rpart}
\alias{labels.rpart}
\title{Make the nice labels used by print and summary.}
\usage{
  labels.rpart(object, digits = 4, minlength = 1L, pretty,
    collapse = TRUE, ...)
}
\arguments{
  \item{digits}{obvious}

  \item{minlength}{0 = don't abbrev factors; 1 = use single
  letters; 2+= the same arg as the "abbreviate" function}

  \item{collapse}{an oddly named argument: FALSE = return a
  matrix with two columns, containing the labels of the
  left and right descendants of each node; TRUE = return a
  vector of 1 column, with the label of the parent}

  \item{pretty:}{for historical compatability: 0 ->
  minlength = 0; NULL -> minlength = 1; TRUE -> minlength =
  4}

  \item{...}{= other args for abbreviate()}
}
\description{
  Make the nice labels used by print and summary.
}
\keyword{internal}


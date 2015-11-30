\name{reducir}
\alias{reducir}
\title{
Reduce a matrix, ftable or data.frame
}
\description{
Reduce the matrix, ftable o data.frame suppressing rows or columns whose elements are all equal to one of the values of valores.
}
\usage{
reducir(x,valores=NA,filas=TRUE,columnas=TRUE)
}

\arguments{
  \item{x}{ Matrix, ftable or data.frame}
  \item{valores}{ Vector values to  compare }
  \item{filas}{If TRUE (default) the rows with all values in valores will be deleted}
  \item{columnas}{If TRUE (default) the columns with all values in valores will be deleted}
}

\value{
Returns a matrix or data.frame without the rows o columns with all values equals to a value from the valores.
If x is a ftable returns an array with two attributes, cabColumna and cabFila  which contain an array of header rows and columns. 
}
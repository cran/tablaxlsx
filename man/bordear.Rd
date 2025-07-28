\name{bordear}
\alias{bordear}
\title{Draw borders an area in an Excel sheet}
\description{
Draw borders an area in an Excel sheet, with the characteristics indicated in the parameters
}
\usage{
bordear(wb, hoja, fila = 1, columna = 1, ancho = 1, alto = 1, 
        estilo = createStyle(border = "topleftbottomright", 
                            borderStyle = "double", borderColour = "blue4"))
}

\arguments{
  \item{wb}{An object of Workbook class}
  \item{hoja}{The name, if it is a text, or the position, if it is numeric, of the sheet}
  \item{fila}{The row of the upper-left corner of the area}
  \item{columna}{The column of the upper-left corner of the area}
  \item{ancho}{The number of cells in the width of the area}
  \item{alto}{The number of cells in the height of the area}
  \item{estilo}{The border style. It must be an object of class Style, and he border styles are used}
}
\details{Of the parameter estilo is used: border, borderColour and borderStyle. The last two can be vectors, in such a way that each feature is applied to the corresponding edge, according to the order established in border}

\seealso{\code{\link[openxlsx]{createStyle}}}
\examples{
  ## Example
  require(openxlsx)
  wb=createWorkbook()
  addWorksheet(wb,"Primera")
  bordear(wb,"Primera",3,2,5,4)
  ##openXL(wb)
  
  ## Otro ejemplo
  wb=createWorkbook()
  addWorksheet(wb,"Primera")
  bordear(wb,"Primera",3,2,5,4,
          estilo=createStyle(border="bottomtop",borderColour=c("red","blue"),
                            borderStyle=c("double","thin")))
  ##openXL(wb)
  
}

\keyword{table}
\keyword{excel}
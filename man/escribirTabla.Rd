\name{escribirTabla}
\alias{escribirTabla}
\title{Write a matrix or data.frame with styles in a sheet of a excel workbook}
\description{
Write a matrix or data.frame with format in a sheet of a excel workbook, In the position and styles indicated in the parameters
}
\usage{
escribirTabla(tabla, wb=NULL,hoja=NULL,fichero=NULL, 
              limpiarFilas=TRUE,limpiarColumnas=TRUE,limpiarValores=NA,
              cabecera = "", fuente = "", notas = "",
              fila = 7, columna = 3, decimales = 1, porcentaje = FALSE,
              cabecerasFila = TRUE, cabecerasColumna = TRUE, 
              cabeceraFilas="",cabeceraColumnas="",
              anchoCabecera = 10,anchoDatos = 14, 
              escudo = NULL,posEscudo=c(1,1),
              tamEscudo=c(2.7,4.5),unidadesEscudo="cm",
              estilos=options("tablaxlsx.Estilos")[[1]],
              bordes = c("TABLA","CABECERA","CABECERASFILA","CABECERASCOLUMNA","DATOS"),
              estilosBordes = NULL)
}

\arguments{
  \item{tabla}{The matrix, data.frame or ftable to write}
  \item{wb}{The Workbook. If NULL is created}
  \item{hoja}{The name o position of Worksheet. If it does not exist or is NULL is created}
  \item{fichero}{The name of a file where the Workbook is saved. If NULL it is not saved}
  \item{limpiarFilas}{If it is TRUE are deleted rows in which all values are in limpiarValores}
  \item{limpiarColumnas}{If it is TRUE are deleted columns in which all values are in limpiarValores}
  \item{limpiarValores}{Values used for cleaning}
  \item{cabecera}{A text to be written in the header of the table}
  \item{fuente}{A text that is written at the end of the table}
  \item{notas}{One or more texts to write after the \code{fuente}}
  \item{fila}{The row of tht upper-left cell where the table will be written}
  \item{columna}{The column of tht upper-left cell where the table will be written}
  \item{decimales}{Number of decimal places that are written with the numerical data. It can be a vector, in which case it is applied successively to each column, recycling the values if necessary}
  \item{porcentaje}{If the data is given percentage format or not. It may be a vector, in which case it is applied successively to each column, recycling values if required}
  \item{cabecerasFila}{If you write or not the rows headers, which will be the names of the rows of the matrix or the corresponding elements of a ftable}
  \item{cabecerasColumna}{If you write or not the columns headers, which will be the names of the rows of the matrix or the corresponding elements of a ftable}
  \item{cabeceraFilas}{Text to write above the row's headers}
  \item{cabeceraColumnas}{Text to write above the row's headers}
  \item{anchoCabecera}{The width, in characters, of the columns of the row header}
  \item{anchoDatos}{The width, in characters, of the data columns}
  \item{escudo}{If it is NULL does not any figure, if it is a text is inserted the figure that is in the file}
  \item{posEscudo}{Position, row and column, of the figure, if escudo is not NULL}
  \item{tamEscudo}{A vector with Height and  Width of the figure}
  \item{unidadesEscudo}{Units of the height of the image. By default "cm"}
  \item{estilos}{A list with the styles to apply to the various elements of the table.}
  \item{bordes}{A vector with the names of the elements in the table will be to surround with edges,  the possible values are:
CABECERA, CABECERASFILA, CABECERASCOLUMNA, DATOS, TABLA (the entire table)}
  \item{estilosBordes}{A list with the styles of the various edges. Each item will be called with one of the possible names on bordes.}
}
\details{
Edges are drawn in the order: CABECERA,CABECERASFILA,CABECERASCOLUMNA,DATOS,TABLA
}
\value{
If it have created a Workbook returns the Workbook created, otherwise returns a vector with the row and column of the lower-right cell of the table with notes and source in the worksheet and the lower-right cell of the table only.
}

\seealso{\link{createStyle}}

\examples{
## Example 1
matriz=matrix(runif(20),4,5)
wb=createWorkbook()
hoja=addWorksheet(wb,"Prueba")
escribirTabla(matriz,wb,hoja,fila=4,columna=4)
if(interactive()) openXL(wb)

# Example 2
f1=ftable(mtcars[,c(2,8:11)],col.vars=3:4)
wb=escribirTabla(f1,estilos=estilosDGE,bordes=c("TABLA","DATOS"),
                 escudo=system.file("img/logoJMR.png",package="tablaxlsx"))
l1=escribirTabla(f1,wb,"otra",anchoCabecera=20,estilos=estilosDGE,bordes=c("TABLA","DATOS"),
                escudo=system.file("img/logoJMR.png",package="tablaxlsx"),
                posEscudo=c(2,2),tamEscudo=c(1,1))
if(interactive()) openXL(wb)
}

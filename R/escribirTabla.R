escribirTabla <-
function(tabla,wb=NULL,hoja=NULL,fichero=NULL,
         limpiarFilas=TRUE,limpiarColumnas=TRUE,limpiarValores=NA,
         cabecera="",fuente="",notas="",
         fila=7,columna=3,decimales=1,porcentaje=FALSE,
         cabecerasFila=TRUE,cabecerasColumna=TRUE,
         anchoCabecera=10,anchoDatos=14,
         escudo=NULL,posEscudo=c(1,1),tamEscudo=c(2.7,4.5),unidadesEscudo="cm",
         estilos=NULL,bordes=c("TABLA","CABECERA","CABECERASFILA","CABECERASCOLUMNA","DATOS"),
         estilosBordes=NULL){
  # bordes puede ser TABLA,CABECERA,CABECERASFILA,CABECERASCOLUMNA,DATOS
  
  if(length(dim(tabla))!=2) stop("tabla: incorrect number of dimensions")
  wbCreado=FALSE
  
  if(is.null(wb)){
    wb=createWorkbook()
    wbCreado=TRUE
  }
  if(!inherits(wb,"Workbook")) return(NULL)
  if(is.null(hoja)) hoja=1
  if(is.character(hoja) & !(hoja %in% names(wb))) addWorksheet(wb,hoja[1])
  if(is.numeric(hoja) & (hoja[1]>length(names(wb)))) addWorksheet(wb,hoja[1])
  
  #if(inherits(tabla,"ftable")) tabla=reducir(tabla,filas=limpiarFilas,columnas=limpiarColumnas,valores=limpiarValores)
  tabla=reducir(tabla,filas=limpiarFilas,columnas=limpiarColumnas,valores=limpiarValores)
  decimales=rep(decimales,ncol(tabla))[1:ncol(tabla)]
  formatoNumero=rep("#,##0",ncol(tabla))
  porcentaje=rep(porcentaje,ncol(tabla))[1:ncol(tabla)]
  for(i in 1:ncol(tabla)){
    if(decimales[i]>0){
      formatoNumero[i]=paste0(formatoNumero[i],".",paste(rep("0",decimales[i]),collapse=""))
    }
    if(porcentaje[i]) formatoNumero[i]=paste0(formatoNumero[i],"%")
    #tabla[,i]=round(tabla[,i],decimales[i])
  }
  
  estiloCabeceraColumna=estilos$estiloCabeceraColumna
  estiloCabeceraFila=estilos$estiloCabeceraFila
  estiloCeldas=estilos$estiloCeldas
  estiloTitulo=estilos$estiloTitulo
  estiloFuente=estilos$estiloFuente
  
  if(is.null(estiloCabeceraColumna)) estiloCabeceraColumna <- createStyle()
  if(is.null(estiloCabeceraFila))    estiloCabeceraFila <- createStyle()
  if(is.null(estiloCeldas))          estiloCeldas <- createStyle()
  if(is.null(estiloTitulo))          estiloTitulo <- createStyle()
  if(is.null(estiloFuente))          estiloFuente <- createStyle()
  
  #a1=formatC(tabla,format="f",dec=",",digits=decimales,big.mark=".")
  filadatos=fila+(cabecera!="")
  columnadatos=columna

  if(cabecerasFila){
    if(!is.null(attr(tabla,"cabFila"))){
       columnadatos=columnadatos+ncol(attr(tabla,"cabFila"))
    }else{
       columnadatos=columnadatos+1      
    }
  }
  if(cabecerasColumna){
    if(!is.null(attr(tabla,"cabColumna"))){
      filadatos=filadatos+nrow(attr(tabla,"cabColumna"))
    }else{
      filadatos=filadatos+1      
    }
  }
  
  if(!is.null(names(estilosBordes))) names(estilosBordes)=toupper(names(estilosBordes))

  if(cabecera!=""){
    writeData(wb,hoja,cabecera,colNames=FALSE,rowNames=FALSE,startCol=columna,startRow=fila)
    mergeCells(wb,hoja,(columna:(columnadatos+ncol(tabla)-1)),fila)
    addStyle(wb, sheet = hoja, estiloTitulo, 
             rows = fila, 
             cols = (columna:(columnadatos+ncol(tabla)-1)),gridExpand =TRUE)
    if("CABECERA" %in% toupper(bordes)){
       if(is.null(estilosBordes$CABECERA) | (!("Style" %in% class(estilosBordes$CABECERA)))){
         bordear(wb,hoja,fila =fila ,columna =columna ,
                 ancho =(columnadatos+ncol(tabla)-columna),alto = 1)
       }else{
         bordear(wb,hoja,fila =fila ,columna =columna ,
                 ancho =(columnadatos+ncol(tabla)-columna),
                 alto = 1,estilo=estilosBordes$CABECERA)
       }
    }
    fila=fila+1
  }
  
  ###################Escribir las cabeceras

  if(cabecerasColumna){
      tcab=attr(tabla,"cabColumna") 
      if(!is.null(tcab)){
        writeData(wb,hoja,tcab,startCol =columnadatos,
                  startRow = fila,rowNames = FALSE,colNames = FALSE)
        for(i in 1:nrow(tcab)){
          columna1=columnadatos
          fila1=fila+i-1
          for(j in 1:ncol(tcab)){
            if(attr(tcab,"colspan")[i,j]>1){
              mergeCells(wb,hoja,cols=columna1+j-2+(1:(attr(tcab,"colspan")[i,j])),rows=fila1)
            }
          }
        }
      }else{
        if(is.null(colnames(tabla))){
          writeData(wb,hoja,matrix(paste("Columna",1:ncol(tabla)),nrow=1),startCol =columnadatos,
                    startRow = fila,rowNames = FALSE,colNames = FALSE)
        }else{
          writeData(wb,hoja,matrix(colnames(tabla),nrow=1),startCol =columnadatos,
                    startRow = fila,rowNames = FALSE,colNames = FALSE) 
        }
      }
      addStyle(wb,hoja,estiloCabeceraColumna,
               rows=fila:(filadatos-1),
               cols=columnadatos+(0:(ncol(tabla)-1)),
               gridExpand = TRUE, stack = TRUE)
      if("CABECERASCOLUMNA" %in% toupper(bordes)){
        if(is.null(estilosBordes$CABECERASCOLUMNA) | (!("Style" %in% class(estilosBordes$CABECERASCOLUMNA)))){
          bordear(wb,hoja,fila =fila ,columna =columnadatos ,
                  ancho =ncol(tabla),alto = filadatos-fila)
        }else{
          bordear(wb,hoja,fila =fila ,columna =columnadatos ,
                  ancho =ncol(tabla),alto = filadatos-fila,
                  estilo=estilosBordes$CABECERASCOLUMNA)
        }
      } 
  }
  
  if(cabecerasFila){
    tcab=attr(tabla,"cabFila") 
    if(!is.null(tcab)){
      writeData(wb,hoja,tcab,startCol =columna,
                startRow = filadatos,rowNames = FALSE,colNames = FALSE)
      for(j in 1:ncol(tcab)){
        fila1=filadatos
        columna1=columna+j-1
        for(i in 1:nrow(tcab)){
          if(attr(tcab,"rowspan")[i,j]>1){
            mergeCells(wb,hoja,rows=fila1+i-2+(1:(attr(tcab,"rowspan")[i,j])),cols=columna1)
          }
        }
      }
    }else{
      if(is.null(rownames(tabla))){
        writeData(wb,hoja,matrix(paste("Fila",1:nrow(tabla)),ncol=1),startCol =columna,
                  startRow = filadatos,rowNames = FALSE,colNames = FALSE)
      }else{
        writeData(wb,hoja,matrix(rownames(tabla),ncol=1),startCol =columna,
                  startRow = filadatos,rowNames = FALSE,colNames = FALSE) 
      }
    }
    addStyle(wb,hoja,estiloCabeceraFila,
             cols=columna:(columnadatos-1),
             rows=filadatos+(0:(nrow(tabla)-1)),
             gridExpand = TRUE, stack = TRUE)
    if("CABECERASFILA" %in% toupper(bordes)){
      if(is.null(estilosBordes$CABECERASFILA) | (!("Style" %in% class(estilosBordes$CABECERASFILA)))){
        bordear(wb,hoja,fila =filadatos ,columna =columna ,
                ancho =columnadatos-columna,alto = nrow(tabla))
      }else{
        bordear(wb,hoja,fila =filadatos ,columna =columna ,
                ancho =columnadatos-columna,alto = nrow(tabla),
                estilo=estilosBordes$CABECERASFILA)
        
      }
    } 
  }
  
  ####################Escribir los datos
  writeData(wb,hoja,tabla,startCol = columnadatos, startRow = filadatos,rowNames=FALSE,
            colNames=FALSE,borderStyle="thin")
  addStyle(wb, sheet = hoja, estiloCeldas, 
           rows = filadatos:(filadatos+nrow(tabla)-1), 
           cols = columnadatos:(columnadatos+ncol(tabla)-1), 
           gridExpand = TRUE)
  for(i in 1:ncol(tabla)){
    addStyle(wb, sheet = hoja, createStyle(numFmt=formatoNumero[i]), 
             rows = filadatos:(filadatos+nrow(tabla)-1), 
             cols = columnadatos+i-1,stack=TRUE,gridExpand = TRUE)
  }
  if("DATOS" %in% toupper(bordes)){
    if(is.null(estilosBordes$DATOS) | (!("Style" %in% class(estilosBordes$DATOS)))){
      bordear(wb,hoja,fila =filadatos,columna =columnadatos ,
              ancho =ncol(tabla),alto = nrow(tabla))
    }else{
      bordear(wb,hoja,fila =filadatos,columna =columnadatos ,
              ancho =ncol(tabla),alto = nrow(tabla),
              estilo=estilosBordes$DATOS)
    }
  }  
  
  
  if("TABLA" %in% toupper(bordes)){
    if(is.null(estilosBordes$TABLA) | (!("Style" %in% class(estilosBordes$TABLA)))){
      bordear(wb,hoja,fila =fila ,columna =columna ,
              ancho =ncol(tabla)+(columnadatos-columna),
              alto = nrow(tabla)+filadatos-fila)
    }else{
      bordear(wb,hoja,fila =fila ,columna =columna ,
              ancho =ncol(tabla)+(columnadatos-columna),
              alto = nrow(tabla)+filadatos-fila,estilo=estilosBordes$TABLA)
    }
  }

  #setColWidths(wb, sheet=hoja, cols=columna, widths="auto")
  
  setColWidths(wb, sheet=hoja, cols=columnadatos:(columnadatos+ncol(tabla)-1), widths=anchoDatos)

  if(cabecerasFila){
    setColWidths(wb, sheet=hoja, cols=columna:(columnadatos-1), widths=anchoCabecera)
  }
  
  if(!is.null(escudo)) insertImage(wb, hoja, escudo,startRow=posEscudo[1],startCol=posEscudo[2],height=tamEscudo[1],width=tamEscudo[2],units=unidadesEscudo)
  #   setRowHeights(wb,hoja,rows=1,heights=10)
  
  fila=filadatos+nrow(tabla)
  if(fuente!=""){
    writeData(wb,hoja,fuente,colNames=FALSE,rowNames=FALSE,startCol=columna,startRow=fila)
    addStyle(wb,hoja,estiloFuente,rows=fila,cols=columna)
    mergeCells(wb,hoja,cols=(columna:(columnadatos+ncol(tabla)-1)),rows=fila)
    fila=fila+1
  } 
  for(nota in notas){
    if(nota!=""){
      writeData(wb,hoja,nota,colNames=FALSE,rowNames=FALSE,startCol=columna,startRow=fila)
      addStyle(wb,hoja,estiloFuente,rows=fila,cols=columna)
      mergeCells(wb,hoja,cols=(columna:(columnadatos+ncol(tabla)-1)),rows=fila)
      fila=fila+1
    }
  }
  if(!is.null(fichero)) saveWorkbook(wb,fichero)
  if(wbCreado){
    return(invisible(wb))
  }else{
    return(invisible(c(Fila=(fila-1),Columna=(columnadatos+ncol(tabla)-1))))
  }  
}

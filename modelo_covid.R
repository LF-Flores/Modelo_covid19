# Preámbulo
rm(list = ls())                     # Se limpia la memoria previo a uso.         
setwd("~/Porfolio/Modelo_covid19")  # Se cambia de directorio automáticamente
                                    # Por facilidad de uso local.
                                    # (Note que es directorio absoluto)

######## FUNCIONES ##########################################################
covid <- function(estimado, param, grafica = FALSE, k){
  # Leyendo la data
  Data <- read.csv("TS.csv")
  Linea_tiempo <- Data[4:dim(Data)[1],]
  Indicadores <- read.csv("Indicadores.csv")
  indice <- which(Indicadores$PAIS==estimado)
  
  # Extrayendo data del país a estimar 
  Php <- as.numeric(Indicadores[indice,])[-1][1:9]
  #Phe <- as.numeric(Indicadores[1,])[-1]
  Indicadores <- Indicadores[-indice,]
  
  
  # Número de datos faltantes:
  sum(is.na(Indicadores)) 
  # PROBLEMA ACTUAL: Faltan datos de: 
  #  - China en Pruebas por millon
  #  - Haití en conectividad
  # SOLUCIÓN:
  # a) Colocar la conectividad de Haiti en 0
  haiti <- which(Indicadores$PAIS=="Haiti")
  Indicadores$Conectividad_PasajerosTransportadosEnMilesEnUnA_o_BM_2018[haiti] <- 0
  rm(haiti)
  # b) Eliminar China (#3)
  china <- which(Indicadores$PAIS=="China All")
  Indicadores <- Indicadores[-china,]
  china <- which(colnames(Linea_tiempo)=="China.All")
  Linea_tiempo <- Linea_tiempo[,-china]
  sum(is.na(Indicadores)) 
  
  
  # Días sin cuarentena:
  #   PROBLEMA: Días negativos, volverlos positivos:
  make_positive <- function(x) return((x>0)*x)
  china <- which(colnames(Data)=="China.All")
  indice <- which(colnames(Data)==estimado)
  x <- make_positive(as.vector(as.numeric(t(Data[2,-1])))[-c(china,indice)+1])
  #China se ha quitado manualmente junto con el estimado
  rm(china)
  
  
  
  
  
  #########################################
  # Definiendo los parámetros por grupos  #
  #########################################
  # Culturales
  P_C <- cbind(
    Indicadores$DensidadPob_Per_km2_BM, 
    Indicadores$Poblacion_EnAreaUrbanaOMS, 
    Indicadores$Conectividad_PasajerosTransportadosEnMilesEnUnA_o_BM_2018/
      Indicadores$x_Habitantes_EnMiles__BM_2018
  )
  colnames(P_C) <- c("Densidad","Urbana","Conect")
  
  # Socioeconómicos
  P_S <- cbind(
    Indicadores$IndiceDePobreza__1__dia_OMS_2015,
    Indicadores$GrossNationaLINCOMEPerCapita____OMS_2014,
    Indicadores$IndiceDeGini_BM,
    Indicadores$EscolaridadAnosPromedio_UN2018
  )
  colnames(P_S) <- c("Pobreza", "Ingreso", "Gini", "Escolaridad")
  
  # Biológicos
  P_B <- cbind(
    Indicadores$EdadPromedio_OMS_2013,
    Indicadores$ExpdeVidaAlNacer_UN_2018
  ) 
  colnames(P_B) <- c("EdadPromedio","ExpectVida")
  
  # De estrategia
  P_E <- cbind(
    Indicadores$Tests_MillonPop_29_4_20,
    Indicadores$x_TasaDeMortalidad_29_4_20,
    x
  )
  colnames(P_E) <- c("Tests", "Mortalidad", "DiasSinCuarentena")
  
  # Limpiando memoria
  #rm(Data, Indicadores,x)
  
  # Kernels posibles
  norma <- function(x) return(sqrt(x%*%x))
  kernel1 <- function(H,O) return(1/abs(H-O))
  kernel2 <- function(H,O) return(1/sqrt(abs(H-O)))
  kernel3 <- function(H,O,sigma=param) return(exp(-sqrt(abs(H-O))/(2*sigma^2))/sqrt(2*pi*sigma))
  kernel4 <- function(H,O,sigma=param) return(exp(-sqrt(apply(H-O, 1, norma)/(2*sigma^2))))
  kernel <- cbind(kernel1, kernel2, kernel3, kernel4)[k][[1]]
  
  # Definición de datos para modelo lineal
  Predictores <- data.frame(kernel(Php,cbind(P_C, P_S, P_B)))
  respuesta <- apply(P_E, 1, prod)
  #respuesta <- P_E[1]
  
  # Modelo Lineal
  model <- lm(respuesta ~.-1, data = Predictores)
  #model$coefficients <- as.vector(na.omit(model$coefficients))
  model$coefficients <- make_positive(model$coefficients)
  
  omega <- as.matrix(Predictores)%*%t(matrix(as.numeric(model$coefficients),1))
  
  # Construyendo el estimador
  Xreal <- as.numeric(as.vector(na.omit(Linea_tiempo$Honduras)))
  indice <- which(colnames(Linea_tiempo)==estimado)
  Linea_tiempo <- Linea_tiempo[-c(1,indice)]
  Linea_tiempo <- as.matrix(Linea_tiempo)
  mode(Linea_tiempo) <- "numeric"
  Xhat <- as.integer(Linea_tiempo%*%omega/sum(omega))

  
  # Residuales
  Data <- read.csv("TS.csv")
  Linea_tiempo <- Data[4:dim(Data)[1],]
  indice <- which(colnames(Linea_tiempo)==estimado)
  x <- as.vector(Linea_tiempo[,indice])
  mode(x) <- "numeric"
  
  if(grafica == TRUE){
    plot(ts(Xhat), 
         type="b", 
         main = paste("Estimación de serie de tiempo de", 
                      estimado, 
                      "\n con kernel", 
                      as.character(k),
                      "y parámetro",
                      as.character(param)))
    lines(x)
  }
  res <- Xhat/x
  #print(sum(res)/length(res))
  return(list(resid_locales = res, prom = sum(abs(res))/length(res), Estimacion = Xhat))
}
Rendimiento <- function(param, k){
  paises <- names(read.csv("TS.csv"))[c(-1,-4,-10)]
  res <- c()
  for(i in paises){
    res <- c(res,covid(i, param, FALSE, k)[2][[1]])
  }
  x <- data.frame(paises, res)
  return(list(sum(x$res)/length(paises), x))
}

######## OBSERVACIONES  ######################################################
# KERNELS:
# kernel1 <- 1/abs(H-O)                                                     Sencillo
# kernel2 <- 1/sqrt(abs(H-O)                                                raiz
# kernel3 <- exp(-sqrt(abs(H-O))/(2*sigma^2))/sqrt(2*pi*sigma)              Gaussiano por componente
# kernel4 <- exp(-sqrt(apply(H-O, 1, norma)/(2*sigma^2))/sqrt(2*pi*sigma))  Gaussiano completo
#
# Aparentemente óptimo: Rendimiento(5.58,4)[[1]]

######## TESTS ##############################################################
paramem <- 5.58
covid("Mexico", paramem,TRUE, 4)
covid("El.Salvador", paramem,TRUE, 4)
covid("Italy", paramem, TRUE, 4)
covid("US", paramem, TRUE, 4)
covid("Honduras", paramem, TRUE,4)
Rendimiento(1.8,4)[[1]]
Rendimiento(5.58,4)[[2]]
x <- Rendimiento(5.58,4)[[2]]
y <- data.frame(paises = x$paises, res = x$res - Rendimiento(1.8,4)[[2]]$res)
y$paises[which(abs(y$res) > 2)]

###### TAREA POR REALIZAR ###################################################
# - Trabajar en posibles formas para reducir los residuales
# 1) Introducir localidad temporal al modelo que además contemple etapas de estrategia/respuesta.
# 2) Reformular la forma del término de estrategia en la etapa de ajuste lineal (Todos) 
# 3) Ampliar intervalo de tiempo y/o utilizar tiempos más recientes en lugar de los iniciales
# 4) Contruir una medida de calidad de datos (CPI, Cantidad de datos) (Luis)
# 5) A*exp(x/t) a partir de un tiempo t. (Danny)
#
# - Investigar países con sospechoso crecimiento (Ejemplo: Namibia) y lidiar con ello al:
# 1) Removerlos de la data
# 2) Utilizar el CPI (aun inutilizado) para filtrarlo
# 3) Utilizar boletines internacionales y noticias para rastrear causas directas de dicho crecimiento
#
# - Idear mejores formas de medir los residuales y diagnosticar al modelo.
# - Buscar posibles kernels nuevos que mejoren rendimento o dinamisen el espacio de estimaciones.
#





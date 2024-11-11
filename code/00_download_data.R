library(Rblpapi)
library(readxl)
library(dplyr)
library(lubridate)

# Conectar a Bloomberg
blpConnect()

# Funci?n para obtener los precios diarios en un ?nico DataFrame
obtener_precios_diarios <- function(tickers, fecha_inicial, fecha_final) {
  precios_diarios <- bdh(tickers, "PX_LAST", start.date = as.Date(fecha_inicial), end.date = as.Date(fecha_final))
  
  # Convertir la lista de precios a un data.frame con fechas como filas y tickers como columnas
  precios_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), precios_diarios)
  colnames(precios_df)[-1] <- tickers  # Renombrar las columnas con los nombres de los tickers
  return(precios_df)
}

# Funci?n para obtener las series de tiempo de Market Cap y Price-to-Book desde una fecha inicial hasta una fecha final
obtener_datos_trimestrales <- function(tickers, fecha_inicial, fecha_final) {
  # Usamos 'bdh' para obtener las series de Market Cap y Price-to-Book entre las fechas
  market_cap_series <- bdh(tickers, "CUR_MKT_CAP", start.date = as.Date(fecha_inicial), end.date = as.Date(fecha_final))
  price_to_book_series <- bdh(tickers, "PX_TO_BOOK_RATIO", start.date = as.Date(fecha_inicial), end.date = as.Date(fecha_final))
  
  # Convertir las listas de datos a data.frames con fechas como filas y tickers como columnas
  market_cap_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), market_cap_series)
  price_to_book_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), price_to_book_series)
  
  colnames(market_cap_df)[-1] <- tickers  # Renombrar las columnas con los nombres de los tickers
  colnames(price_to_book_df)[-1] <- tickers  # Renombrar las columnas con los nombres de los tickers
  
  return(list(market_cap = market_cap_df, price_to_book = price_to_book_df))
}

# Funci?n para generar las fechas de rebalanceo seg?n la periodicidad
generar_fechas_rebalanceo <- function(fecha_inicial, fecha_final, periodicidad) {
  if (periodicidad == "quarterly") {
    return(seq(as.Date(fecha_inicial), as.Date(fecha_final), by = "quarter"))
  } else if (periodicidad == "semiannually") {
    return(seq(as.Date(fecha_inicial), as.Date(fecha_final), by = "6 months"))
  } else if (periodicidad == "yearly") {
    return(seq(as.Date(fecha_inicial), as.Date(fecha_final), by = "year"))
  } else {
    stop("Periodicidad no soportada.")
  }
}

# Cargar las hojas de Excel (de todas las hojas, eem hasta vwo)
# C:/Users/geral/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios/
ruta_archivo <- "C:/Users/gj.camposm/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios"
setwd(ruta_archivo)
hojas <- c("eem", "eemo", "eemv", "fem", "ieem", "psrm", "vwo")
# hojas <- c("vwo")


# Cargar periodicidad de rebalanceo desde la columna 'Rebalancing periods'
rebalanceo_df <- read_excel(ruta_archivo, sheet = "etfs")

# Cargar los tickers por hoja (ETF)
tickers_dfs <- lapply(hojas, function(hoja) {
  read_excel(ruta_archivo, sheet = hoja)
})

# Fecha inicial y final para los datos
fecha_inicial <- "2012-01-01"
fecha_final <- "2023-12-31"

# Crear listas para almacenar los resultados por ETF
resultados_por_etf <- list()

# Iterar sobre cada ETF y hoja de tickers
for (i in seq_along(hojas)) {
  etf <- hojas[i]
  tickers_df <- tickers_dfs[[i]]
  
  cat("ETF: ", etf, "\n")
  
  # Obtener la periodicidad de rebalanceo
  periodicidad <- rebalanceo_df %>% filter(ETF == etf) %>% pull("rebalancing")
  
  # Generar las fechas de rebalanceo seg?n la periodicidad
  fechas_rebalanceo <- generar_fechas_rebalanceo(fecha_inicial, fecha_final, periodicidad)
  
  # Inicializar lista para almacenar los datos por ETF y por trimestre, semestre o a?o
  datos_etf <- list()
  
  # Iterar sobre los periodos de rebalanceo seg?n la periodicidad
  num_periodos <- ncol(tickers_df)  # N?mero total de trimestres en la hoja de tickers
  
  # Definir el salto de columnas seg?n la periodicidad
  if (periodicidad == "quarterly") {
    step_size <- 1  # Trimestral, vamos columna por columna
    period_duration <- months(3)  # Cada trimestre dura 3 meses
  } else if (periodicidad == "semiannually") {
    step_size <- 2  # Semestral, saltamos una columna (2 trimestres)
    period_duration <- months(6)  # Cada semestre dura 6 meses
  } else if (periodicidad == "yearly") {
    step_size <- 4  # Anual, saltamos tres columnas (4 trimestres)
    period_duration <- years(1)  # Cada a?o dura 12 meses
  }
  
  # Definir el vector de ?ndices ajustados seg?n la periodicidad
  indices_ajustados <- seq(1, num_periodos, by = step_size)
  
  # Iterar sobre las columnas seg?n el step_size
  for (j in indices_ajustados) {
    # Obtener el nombre de la columna que corresponde al periodo actual
    periodo_column <- colnames(tickers_df)[j]
    
    # Extraer los tickers de esa columna (que representa la composici?n en ese periodo)
    tickers <- tickers_df[[periodo_column]]
    
    # Verificar si la columna tiene tickers v?lidos
    if (all(is.na(tickers))) {
      next  # Si la columna est? vac?a, saltar al siguiente periodo
    }
    
    # Eliminar duplicados de los tickers y filtrar los NAs
    tickers_unicos <- unique(tickers[!is.na(tickers)])
    
    # Obtener el ?ndice de fechas de rebalanceo correspondiente al periodo actual
    indice_fecha <- which(indices_ajustados == j)
    
    # Verificar que el ?ndice sea v?lido y que est? dentro del rango de fechas de rebalanceo
    if (length(indice_fecha) == 0 || is.na(fechas_rebalanceo[indice_fecha])) {
      stop(paste("Error: No se encontr? una fecha de rebalanceo v?lida para el ?ndice", j))
    }
    
    # Definir las fechas de precios diarios: desde el rebalanceo actual hasta el siguiente
    fecha_inicio <- fechas_rebalanceo[indice_fecha]
    
    # Si no es el ?ltimo periodo, la fecha final ser? justo antes del siguiente rebalanceo
    if (indice_fecha < length(fechas_rebalanceo)) {
      fecha_final_precios <- fechas_rebalanceo[indice_fecha + 1] - 1  # Hasta justo antes del pr?ximo rebalanceo
    } else {
      # Si es el ?ltimo periodo, calcular la fecha final como el ?ltimo d?a del periodo seg?n la periodicidad
      fecha_final_precios <- fecha_inicio + period_duration - days(1)  # ?ltimo d?a del trimestre, semestre o a?o
    }
    
    # Mostrar las fechas para obtener precios
    print(paste("Fechas para obtener precios: inicio =", fecha_inicio, ", final =", fecha_final_precios))  # Depuraci?n
    
    # Obtener precios diarios en el rango definido
    precios <- obtener_precios_diarios(tickers_unicos, fecha_inicio, fecha_final_precios)
    
    # Obtener Market Cap y Price-to-Book como series de tiempo en el rango definido
    datos_trimestrales <- obtener_datos_trimestrales(tickers_unicos, fecha_inicio, fecha_final_precios)
    
    # Almacenar las tres tablas en una lista bajo el nombre del periodo evaluado
    datos_etf[[paste(etf, periodo_column, sep = "_")]] <- list(
      precios = precios,
      market_cap = datos_trimestrales$market_cap,
      price_to_book = datos_trimestrales$price_to_book
    )
  }
  
  # Almacenar los resultados del ETF
  
  resultados_por_etf[[etf]] <- datos_etf
}


# Importar el archivo Excel con los datos del T-Bill
tbill_data <- read_excel(paste0(ruta_archivo,"/data_raw/rf_raw_data.xlsx"))

# Asumimos que el archivo tiene columnas 'Date' y 'PX_LAST'
# Asegúrate de ajustar 'Date' y 'PX_LAST' si los nombres de tus columnas son diferentes
tbill_data <- tbill_data %>%
  rename(date = "date", PX_LAST = "rf") %>%  # Cambia 'Tu_Columna_Fecha' y 'Tu_Columna_Precio' por los nombres exactos
  mutate(date = as.Date(date))

# Convertir la tasa anual del T-Bill a una base diaria
tbill_data <- tbill_data %>%
  mutate(Daily_TBill = (1 + PX_LAST / 100)^(1 / 252) - 1)

# Guardar el archivo en formato CSV
write.csv(tbill_data, file = paste0(getwd(), "/data_clean/tbill_3m_daily.csv"), row.names = FALSE)

# Mostrar los primeros registros para verificar
head(tbill_data)

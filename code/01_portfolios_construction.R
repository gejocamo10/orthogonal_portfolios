library(tidyverse)

# Definir la ruta del archivo de trabajo
# ruta_archivo <- "C:/Users/geral/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios/"
ruta_archivo <- "C:/Users/gj.camposm/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios/"
setwd(ruta_archivo)

# Cargar los tres archivos .RData
load("code/data_orthogonal_from_eem_to_fem.RData")
resultados_por_etf_1 <- resultados_por_etf

load("code/data_orthogonal_ieem.RData")
resultados_por_etf_2 <- resultados_por_etf

load("code/data_orthogonal_psrm.RData")
resultados_por_etf_3 <- resultados_por_etf

# Unir todas las listas en una sola
resultados_por_etf_completo <- c(resultados_por_etf_1, resultados_por_etf_2, resultados_por_etf_3)

# Cargar los datos del 3-month T-Bill en base diaria
tbill_data <- read.csv("data_clean/tbill_3m_daily.csv")

# Función para clasificar los activos en los portafolios por tamaño y valoración
clasificar_activos <- function(market_cap_df, price_to_book_df) {
  p10 <- map_dbl(1:nrow(market_cap_df), function(i) quantile(as.numeric(market_cap_df[i, ]), 0.10, na.rm = TRUE))
  p90 <- map_dbl(1:nrow(market_cap_df), function(i) quantile(as.numeric(market_cap_df[i, ]), 0.90, na.rm = TRUE))
  q1 <- map_dbl(1:nrow(price_to_book_df), function(i) quantile(as.numeric(price_to_book_df[i, ]), 0.25, na.rm = TRUE))
  q3 <- map_dbl(1:nrow(price_to_book_df), function(i) quantile(as.numeric(price_to_book_df[i, ]), 0.75, na.rm = TRUE))
  
  clasificacion <- list()
  
  for (i in 1:nrow(market_cap_df)) {
    size_class <- ifelse(as.numeric(market_cap_df[i, ]) <= p10[i], 'small',
                         ifelse(as.numeric(market_cap_df[i, ]) >= p90[i], 'big', NA))
    value_class <- ifelse(as.numeric(price_to_book_df[i, ]) <= q1[i], 'value',
                          ifelse(as.numeric(price_to_book_df[i, ]) >= q3[i], 'growth', 'neutral'))
    
    clasificacion[[i]] <- data.frame(
      Ticker = colnames(market_cap_df),
      Size = size_class,
      Value = value_class,
      Classification = paste(size_class, value_class, sep = "_")
    )
  }
  
  return(clasificacion)
}

# Función para filtrar combinaciones válidas de clasificación
filtrar_combinaciones_validas <- function(clasificacion_df) {
  clasificacion_df %>% filter(!is.na(Size) & !is.na(Value))
}

# Función para calcular los retornos promedio por cada clasificación y ajustar por exceso de retorno
calcular_retornos_promedio_con_exceso <- function(precios_df, clasificacion_df, tbill_df) {
  retornos_por_portafolio <- list()
  
  for (i in 1:length(clasificacion_df)) {
    clasificacion_filtrada <- filtrar_combinaciones_validas(clasificacion_df[[i]])
    clases <- unique(clasificacion_filtrada$Classification)
    retornos_periodo <- list()
    
    for (clase in clases) {
      tickers_clase <- clasificacion_filtrada %>%
        filter(Classification == clase) %>%
        pull(Ticker) %>%
        setdiff('date')
      
      precios_clase <- precios_df %>% select(date, one_of(tickers_clase))
      
      retornos_clase <- precios_clase %>%
        mutate(across(!date, ~ log(.x) - lag(log(.x)), .names = "ret_{col}"))
      
      retornos_promedio <- rowMeans(select(retornos_clase, starts_with("ret_")), na.rm = TRUE)
      
      # Unir retornos promedio con T-Bill en base diaria
      retornos_promedio <- merge(data.frame(date = precios_df$date, retornos_promedio), tbill_df, by = "date", all.x = TRUE)
      
      # Restar la tasa del T-Bill para obtener el exceso de retorno
      retornos_exceso <- retornos_promedio$retornos_promedio - retornos_promedio$Daily_TBill
      
      retornos_periodo[[clase]] <- retornos_exceso
    }
    
    retornos_por_portafolio[[i]] <- retornos_periodo
  }
  
  return(retornos_por_portafolio)
}

# Función para consolidar los retornos en un solo dataframe ordenado por fecha
consolidar_retornos_por_etf <- function(retornos_por_portafolio, precios_df) {
  fechas <- precios_df$date
  retornos_df <- data.frame(date = fechas)
  
  portafolios <- c("big_growth", "big_neutral", "big_value", 
                   "small_growth", "small_neutral", "small_value")
  
  for (i in 1:length(retornos_por_portafolio)) {
    for (portafolio in portafolios) {
      if (!is.null(retornos_por_portafolio[[i]][[portafolio]])) {
        retornos_df[[portafolio]] <- retornos_por_portafolio[[i]][[portafolio]]
      } else {
        retornos_df[[portafolio]] <- NA
      }
    }
  }
  
  retornos_df <- retornos_df %>% arrange(date)
  return(retornos_df)
}

# Aplicar las funciones a los ETFs
portafolios_retorno_df <- list()

for (etf in names(resultados_por_etf_completo)) {
  etf_data <- resultados_por_etf_completo[[etf]]
  df_etf <- data.frame()
  
  for (periodo in names(etf_data)) {
    cat("estamos en ", periodo, "\n")
    precios <- etf_data[[periodo]]$precios
    market_cap <- etf_data[[periodo]]$market_cap
    price_to_book <- etf_data[[periodo]]$price_to_book
    
    clasificacion <- clasificar_activos(market_cap, price_to_book)
    
    retornos_promedio_con_exceso <- calcular_retornos_promedio_con_exceso(precios, clasificacion, tbill_data)
    
    df_periodo <- consolidar_retornos_por_etf(retornos_promedio_con_exceso, precios)
    
    df_etf <- bind_rows(df_etf, df_periodo)
  }
  
  portafolios_retorno_df[[etf]] <- df_etf
}

# Exportar los retornos con exceso a CSV
nombres_portafolios_modificados <- c("EEM.US", "EEMO.US", "EEMV.US", "FEM.US", "IEEM.LN", "PSRM.LN")
for (i in 1:length(nombres_portafolios_modificados)) {
  nuevo_nombre <- paste0(nombres_portafolios_modificados[i], "_risk_ports_excess_clean")
  
  write.csv(portafolios_retorno_df[[i]] %>% na.omit(), 
            file = paste0("data_clean/", nuevo_nombre, ".csv"),
            row.names = FALSE)
}

# Guardar el entorno de trabajo
save.image(file = "code/data_clean_environment.RData")

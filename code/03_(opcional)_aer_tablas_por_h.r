# Librerías necesarias
library(tidyverse)

# Ruta principal donde están los archivos
ruta_principal <- "C:/Users/YOGA/Desktop/repositories/research/orthogonal_portfolios/data_simulation"

# Parámetros definidos
etfs <- c("eem", "eemo", "eemv", "fem", "ieem", "psrm", "vwo")
distribuciones <- c("copula", "gauss", "t")
h_valores <- seq(100, 500, by = 50)

# Loop para procesar cada combinación distribucion y h_valor
for (dist in distribuciones) {
  for (h in h_valores) {

    resultados <- data.frame()

    for (etf in etfs) {

      # Construir ruta del archivo actual
      archivo_actual <- paste0(ruta_principal, "/retornos_", dist, "_", etf, "_", h, "_portfolio_profits.csv")

      # Leer archivo actual y eliminar columnas sin nombre
      datos_actual <- read_csv(archivo_actual, show_col_types = FALSE) %>%
        select(-starts_with("..."))

      # Calcular promedios de cada columna y multiplicar por 100
      promedios <- datos_actual %>%
        summarise(across(everything(), ~ mean(.x) * 100)) %>%
        mutate(etf = etf) %>%
        relocate(etf)

      # Combinar los promedios para cada ETF
      resultados <- bind_rows(resultados, promedios)
    }

    # Guardar resultados en archivo CSV con nombre según ejemplo dado
    archivo_resultado <- paste0(ruta_principal, "/retornos_", dist, "_h_", h, ".csv")

    write_csv(resultados, archivo_resultado)
  }
}
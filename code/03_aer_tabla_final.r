# Librerías necesarias
library(tidyverse)

# Ruta principal donde están los archivos
ruta_principal <- "C:/Users/YOGA/Desktop/repositories/research/orthogonal_portfolios/data_simulation"

# Definir los parámetros
distribuciones <- c("copula", "gauss", "t")
h_valores <- seq(100, 500, by = 50)
etfs <- c("eem", "eemo", "eemv", "fem", "ieem", "psrm", "vwo")

# Crear un dataframe vacío para almacenar los resultados
df_final <- data.frame()

# Loop para cargar todos los archivos y combinarlos
for (dist in distribuciones) {
  for (h in h_valores) {
    
    # Crear la ruta del archivo actual
    archivo_actual <- paste0(ruta_principal, "/retornos_", dist, "_h_", h, ".csv")
    
    # Leer archivo csv actual
    datos_actual <- read_csv(archivo_actual, show_col_types = FALSE)
    
    # Añadir columnas de etf, distribucion y h_valor
    datos_actual <- datos_actual %>%
      mutate(
        etf = etfs,
        distribucion = dist,
        h_valor = h
      ) %>%
      relocate(etf, distribucion, h_valor)
    
    # Agregar los datos al dataframe final
    df_final <- bind_rows(df_final, datos_actual)
  }
}

# Resultado
print(head(df_final))

archivo_consolidado <- paste0(ruta_principal, "/retornos_consolidado.csv")
write_csv(df_final, archivo_consolidado)
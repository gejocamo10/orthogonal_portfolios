setwd("C:/Users/YOGA/Desktop/repositories/research/orthogonal_portfolios")

library(ggplot2)
library(tidyverse)
library(gridExtra)  # Para dividir los gráficos en bloques

# Cargar los environments y asignar el final_list correspondiente a cada uno
load("code/environment_gauss.RData")
final_list_gauss <- final_list  # Guardar el final_list del environment gauss

load("code/environment_t.RData")
final_list_t <- final_list  # Guardar el final_list del environment t

load("code/environment_copula.RData")
final_list_copula <- final_list  # Guardar el final_list del environment copula

# Definir una lista de final_lists y nombres para cada environment
final_lists <- list(gauss = final_list_gauss, t = final_list_t, copula = final_list_copula)
env_names <- c("gauss","t", "copula")

# Inicializamos una lista para almacenar los gráficos de cada environment
all_plots <- list()

# Iteramos sobre cada environment cargado
for (env in env_names) {
  # Cargar la lista final de cada environment
  final_list <- final_lists[[env]]
  
  # Crear el dataframe combinado para cada environment
  combined_df <- data.frame()
  for (i in seq_along(final_list)) {
    df <- as.data.frame(final_list[[i]][, 1:5])
    df$t_value <- h_vector[i]  # Usar el h_vector específico
    df$etf <- etfs
    combined_df <- bind_rows(combined_df, df)
  }

  # Generar los gráficos para todos los ETFs
  etf_plots <- list()
  for (etf_value in etfs) {
    reshaped_df <- combined_df %>%
      filter(etf == etf_value, t_value %in% seq(100, 500, 50)) %>%
      pivot_longer(cols = -c(t_value, etf), names_to = "strategy", values_to = "value") %>%
      pivot_wider(names_from = t_value, values_from = value)
    
    long_df <- reshaped_df[, -1] %>%
      pivot_longer(cols = -strategy, names_to = "t_value", values_to = "value") %>%
      mutate(t_value = as.numeric(t_value))  # Convertir t_value a numérico
    
    variable_order <- long_df %>%
      group_by(strategy) %>%
      summarize(avg_value = mean(value, na.rm = TRUE)) %>%
      arrange(avg_value)
    
    long_df <- long_df %>%
      mutate(variable = factor(strategy, levels = variable_order$strategy)) %>%
      mutate(t_value_factor = factor(t_value, levels = sort(unique(t_value))))
    
    # Crear el gráfico para cada ETF
    etf_plot <- ggplot(long_df, aes(x = t_value, y = value, color = strategy)) +
      geom_line() +
      labs(title = paste0(etf_value, " - ", env), x = "h_value", y = "performance") +
      theme_minimal()
    
    etf_plots[[etf_value]] <- etf_plot
  }
  
  # Dividir los gráficos en 2 bloques
  block1 <- grid.arrange(grobs = etf_plots[1:4], ncol = 2)
  block2 <- grid.arrange(grobs = etf_plots[5:length(etfs)], ncol = 2)
  all_blocks <- grid.arrange(grobs = etf_plots, ncol = 2)
  
  # Guardar los gráficos en la lista
  all_plots[[env]] <- list(block1, block2, all_blocks)
}

# Mostrar los gráficos finales
# Para gauss:
grid.arrange(all_plots[["gauss"]][[3]])
# Para t:
grid.arrange(all_plots[["t"]][[3]])
# Para copula:
grid.arrange(all_plots[["copula"]][[3]])

# Guardar los gráficos para gauss
ggsave(filename = "figures/gauss_block1.png", plot = all_plots[["gauss"]][[1]], width = 10, height = 6)
ggsave(filename = "figures/gauss_block2.png", plot = all_plots[["gauss"]][[2]], width = 10, height = 8)
ggsave(filename = "figures/gauss.png", plot = grid.arrange(all_plots[["gauss"]][[3]]), width = 10, height = 8)
# Guardar los gráficos para t
ggsave(filename = "figures/t_block1.png", plot = all_plots[["t"]][[1]], width = 10, height = 8)
ggsave(filename = "figures/t_block2.png", plot = all_plots[["t"]][[2]], width = 10, height = 8)
ggsave(filename = "figures/t.png", plot = grid.arrange(all_plots[["t"]][[3]]), width = 10, height = 8)
# Guardar los gráficos para copula
ggsave(filename = "figures/copula_block1.png", plot = all_plots[["copula"]][[1]], width = 10, height = 8)
ggsave(filename = "figures/copula_block2.png", plot = all_plots[["copula"]][[2]], width = 10, height = 8)
ggsave(filename = "figures/copula.png", plot = grid.arrange(all_plots[["copula"]][[3]]), width = 10, height = 8)


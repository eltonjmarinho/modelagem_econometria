# views/plot_view.R
# -----------------------------------------------------------------------------
# Este script contém todas as funções para a visualização de dados e resultados,
# como gráficos da série temporal, ACF/PACF, e diagnósticos de resíduos.
# -----------------------------------------------------------------------------
library(ggplot2)

# Função para plotar a série temporal
plot_time_series <- function(data, title = "Série Temporal") {
  ggplot(data, aes(x = refdate, y = settle)) +
    geom_line() +
    labs(title = title,
         x = "Data",
         y = "Preço de Liquidação") +
    theme_minimal()
}

# Função para plotar ACF e PACF lado a lado
plot_acf_pacf <- function(time_series, title_suffix = "") {
  # Configura o layout para 2 gráficos em uma linha
  par(mfrow = c(1, 2))
  
  # Plota o ACF
  acf(time_series, main = paste("ACF", title_suffix))
  
  # Plota o PACF
  pacf(time_series, main = paste("PACF", title_suffix))
  
  # Restaura o layout de gráfico padrão
  par(mfrow = c(1, 1))
}

# Função para plotar os diagnósticos de resíduos
plot_residual_diagnostics <- function(model_name, model_object) {
  
  message(paste("\n3. Gráficos de Diagnóstico para o Modelo:", model_name))
  
  # A função checkresiduals faz um ótimo trabalho plotando os resíduos,
  # o ACF dos resíduos e um histograma.
  checkresiduals(model_object)
}

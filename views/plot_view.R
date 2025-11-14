# views/plot_view.R
# -----------------------------------------------------------------------------
# Este script contém todas as funções para a visualização de dados e resultados,
# como gráficos da série temporal, ACF/PACF, e diagnósticos de resíduos.
# -----------------------------------------------------------------------------
library(ggplot2)

# Função para plotar a série temporal e salvar em arquivo
plot_time_series <- function(data, title = "Série Temporal", filename) {
  p <- ggplot(data, aes(x = date, y = price)) +
    geom_line() +
    labs(title = title,
         x = "Data",
         y = "Preço") +
    theme_minimal()
  
  # Salva o gráfico em arquivo
  ggsave(filename, plot = p, width = 10, height = 6)
  message(paste("Gráfico da série temporal salvo em:", filename))
  
  # Exibe o gráfico na sessão interativa
  print(p)
}

# Função para plotar ACF e PACF lado a lado e salvar em arquivo
plot_acf_pacf <- function(time_series, title_suffix = "", filename) {
  # Abre o dispositivo gráfico PNG
  png(filename, width = 800, height = 400)
  
  # Configura o layout para 2 gráficos em uma linha
  par(mfrow = c(1, 2))
  
  # Plota o ACF
  acf(time_series, main = paste("ACF", title_suffix))
  
  # Plota o PACF
  pacf(time_series, main = paste("PACF", title_suffix))
  
  # Fecha o dispositivo, salvando o arquivo
  dev.off()
  
  # Restaura o layout de gráfico padrão para a sessão interativa
  par(mfrow = c(1, 1))
  
  message(paste("Gráficos ACF/PACF salvos em:", filename))
}

# Função para plotar os diagnósticos de resíduos e salvar em arquivo
plot_residual_diagnostics <- function(model_name, model_object, filename) {
  # Abre o dispositivo gráfico PNG
  png(filename, width = 800, height = 600)
  
  message(paste("\n3. Gerando gráficos de diagnóstico para o Modelo:", model_name))
  
  # A função checkresiduals plota os resíduos, o ACF e o histograma
  checkresiduals(model_object)
  
  # Fecha o dispositivo, salvando o arquivo
  dev.off()
  
  message(paste("Gráficos de diagnóstico salvos em:", filename))
}

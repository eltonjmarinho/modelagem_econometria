# views/plot_view.R
# -----------------------------------------------------------------------------
# Este script contém todas as funções para a visualização de dados e resultados,
# como gráficos da série temporal, ACF/PACF, e diagnósticos de resíduos.
# -----------------------------------------------------------------------------
library(ggplot2)
library(plotly)
library(htmlwidgets)

# Função para plotar a série temporal e salvar em arquivo interativo
plot_time_series <- function(data, title = "Série Temporal", filename) {
  p <- ggplot(data, aes(x = date, y = price, text = paste("Data:", date, "<br>Preço:", price))) +
    geom_line() +
    labs(title = title, x = "Data", y = "Preço") +
    theme_minimal()
  
  ip <- ggplotly(p, tooltip = "text")
  
  saveWidget(ip, filename, selfcontained = FALSE)
  message(paste("Gráfico interativo da série temporal salvo em:", filename))
  
  print(ip)
}

# Função para plotar ACF e PACF interativos e salvar em arquivo
plot_acf_pacf <- function(time_series, title_suffix = "", filename) {
  p_acf <- ggAcf(time_series) + labs(title = paste("ACF", title_suffix))
  p_pacf <- ggPacf(time_series) + labs(title = paste("PACF", title_suffix))
  
  ip <- subplot(ggplotly(p_acf), ggplotly(p_pacf), nrows = 1, titleX = TRUE, titleY = TRUE) %>%
    layout(title = paste("ACF & PACF", title_suffix))
    
  saveWidget(ip, filename, selfcontained = FALSE)
  message(paste("Gráficos ACF/PACF interativos salvos em:", filename))
  
  print(ip)
}

# Função para plotar os diagnósticos de resíduos interativos e salvar
plot_residual_diagnostics <- function(model_name, model_object, filename) {
  message(paste("\n3. Gerando gráficos de diagnóstico interativos para o Modelo:", model_name))
  
  res <- residuals(model_object)
  res_df <- data.frame(residuals = as.vector(res), time = seq_along(res))
  
  # 1. Gráfico de resíduos ao longo do tempo
  p_res <- ggplot(res_df, aes(x = time, y = residuals)) +
    geom_line() +
    geom_point() +
    labs(title = "Resíduos ao longo do tempo", x = "Tempo", y = "Resíduos") +
    theme_minimal()
    
  # 2. Gráfico ACF dos resíduos
  p_acf <- ggAcf(res) + labs(title = "ACF dos Resíduos")
  
  # 3. Histograma dos resíduos
  p_hist <- ggplot(res_df, aes(x = residuals)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue") +
    geom_density(color = "red") +
    labs(title = "Histograma dos Resíduos", x = "Resíduos", y = "Densidade") +
    theme_minimal()
    
  # Combina os gráficos em um único widget interativo
  ip <- subplot(
    ggplotly(p_res),
    subplot(ggplotly(p_acf), ggplotly(p_hist), nrows = 1, titleX = TRUE),
    nrows = 2,
    heights = c(0.5, 0.5),
    titleY = TRUE
  ) %>% layout(title = paste("Diagnóstico de Resíduos para", model_name))
  
  saveWidget(ip, filename, selfcontained = FALSE)
  message(paste("Gráficos de diagnóstico interativos salvos em:", filename))
  
  print(ip)
}

# Função para plotar e salvar a previsão interativa de um modelo
plot_forecast <- function(model, h = 30, filename) {
  fc <- forecast(model, h = h)
  
  p <- autoplot(fc) +
    labs(
      title = paste("Previsão do Modelo", model$method),
      subtitle = paste("Previsão para os próximos", h, "períodos"),
      x = "Tempo",
      y = "Preço"
    ) +
    theme_minimal()
  
  ip <- ggplotly(p)
  
  saveWidget(ip, filename, selfcontained = FALSE)
  message(paste("Gráfico de previsão interativo salvo em:", filename))
  
  print(ip)
}

# views/plot_view.R
# -----------------------------------------------------------------------------
# Este script contém todas as funções para a visualização de dados e resultados,
# como gráficos da série temporal, ACF/PACF, e diagnósticos de resíduos.
# -----------------------------------------------------------------------------
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(scales) # for date_format

# Função para plotar a série temporal e retornar o objeto plotly
plot_time_series <- function(data, title = "Série Temporal") {
  p <- ggplot(data, aes(x = date, y = price)) +
    geom_line() +
    labs(title = title, x = "Ano", y = "Preço") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_minimal()
  
  ggplotly(p)
}

# Função para plotar ACF e PACF interativos e retornar o objeto plotly
plot_acf_pacf <- function(time_series, title_suffix = "") {
  p_acf <- ggAcf(time_series) + labs(title = paste("ACF", title_suffix))
  p_pacf <- ggPacf(time_series) + labs(title = paste("PACF", title_suffix))
  
  subplot(ggplotly(p_acf), ggplotly(p_pacf), nrows = 1, titleX = TRUE, titleY = TRUE) %>%
    layout(title = paste("ACF & PACF", title_suffix))
}

# Função para plotar os diagnósticos de resíduos interativos e retornar o objeto plotly
plot_residual_diagnostics <- function(model_name, model_object, data) {
  res <- residuals(model_object)
  
  # Ajusta os dados se o número de resíduos for menor (devido à diferenciação)
  if (length(res) < nrow(data)) {
    data <- data[(nrow(data) - length(res) + 1):nrow(data), ]
  }
  
  res_df <- data.frame(residuals = as.vector(res), date = data$date)
  
  # 1. Gráfico de resíduos ao longo do tempo
  p_res <- ggplot(res_df, aes(x = date, y = residuals)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 0.8) +
    labs(title = "Resíduos ao longo do tempo", x = "Ano", y = "Resíduos") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
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
  subplot(
    ggplotly(p_res),
    subplot(ggplotly(p_acf), ggplotly(p_hist), nrows = 1, titleX = TRUE),
    nrows = 2,
    heights = c(0.5, 0.5),
    titleY = TRUE
  ) %>% layout(title = paste("Diagnóstico de Resíduos para", model_name))
}

# Função para plotar a previsão interativa de um modelo e retornar o objeto plotly
plot_forecast <- function(model, data, h = 30) {
  fc <- forecast(model, h = h)
  
  # Dados originais
  df_orig <- data.frame(
    date = data$date,
    price = as.vector(model$x)
  )
  
  # Dados previstos
  last_date <- data$date[nrow(data)]
  future_dates <- seq(from = last_date + 1, by = "day", length.out = h)
  
  df_fc <- data.frame(
    date = future_dates,
    point_forecast = as.vector(fc$mean),
    lower_80 = fc$lower[, 1],
    upper_80 = fc$upper[, 1],
    lower_95 = fc$lower[, 2],
    upper_95 = fc$upper[, 2]
  )
  
  p <- ggplot() +
    geom_line(data = df_orig, aes(x = date, y = price), color = "black") +
    geom_line(data = df_fc, aes(x = date, y = point_forecast), color = "blue") +
    geom_ribbon(data = df_fc, aes(x = date, ymin = lower_95, ymax = upper_95), fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
    geom_ribbon(data = df_fc, aes(x = date, ymin = lower_80, ymax = upper_80), fill = "blue", alpha = 0.4, inherit.aes = FALSE) +
    labs(
      title = paste("Previsão do Modelo", model$method),
      subtitle = paste("Previsão para os próximos", h, "períodos"),
      x = "Ano",
      y = "Preço"
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_minimal()
    
  ggplotly(p)
}

# Função para salvar uma lista de elementos (gráficos, texto) em um único HTML
save_report_as_html <- function(items, filename) {
  # Converte todos os itens para tags HTML
  html_items <- lapply(items, function(item) {
    if (inherits(item, "htmlwidget")) {
      # Para gráficos plotly/htmlwidget
      tags$div(style = "width:100%; height:auto; margin-bottom: 20px;", item)
    } else if (is.character(item)) {
      # Para texto (que pode conter HTML)
      tags$div(style = "width:100%; margin-bottom: 20px;", HTML(item))
    } else {
      # Para outros tipos de tags htmltools
      item
    }
  })
  
  # Cria o corpo do HTML com todos os itens
  doc <- tags$html(
    tags$head(
      tags$title("Relatório de Análise de Séries Temporais")
    ),
    tags$body(
      h1("Relatório de Análise de Séries Temporais - Preços do Café"),
      html_items
    )
  )
  
  save_html(doc, file = filename, background = "white", libdir = "lib")
  
  message(paste("Relatório HTML completo salvo em:", filename))
}

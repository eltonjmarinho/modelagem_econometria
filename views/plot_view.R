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

# Função para salvar uma lista de elementos (gráficos, texto) em um único HTML com design aprimorado
save_report_as_html <- function(items, filename) {
  
  # CSS para um design moderno e profissional
  css <- "
    @import url('https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap');
    
    body {
      font-family: 'Lato', sans-serif;
      background-color: #f4f4f9;
      color: #333;
      margin: 0;
      padding: 20px;
      line-height: 1.6;
    }
    .container {
      max-width: 1200px;
      margin: 0 auto;
      background-color: #ffffff;
      padding: 20px 40px;
      border-radius: 10px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.05);
    }
    h1, h2, h3 {
      color: #2c3e50;
      border-bottom: 2px solid #e0e0e0;
      padding-bottom: 10px;
      margin-top: 30px;
    }
    h1 {
      font-size: 2.5em;
      text-align: center;
      border-bottom: none;
      margin-bottom: 20px;
    }
    h2 {
      font-size: 2em;
    }
    h3 {
      font-size: 1.5em;
      border-bottom: 1px solid #e0e0e0;
    }
    .card {
      background-color: #fdfdfd;
      border: 1px solid #e0e0e0;
      border-radius: 8px;
      padding: 20px;
      margin-top: 20px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.03);
      overflow: hidden; /* Para conter os gráficos */
    }
    pre {
      background-color: #ecf0f1;
      padding: 15px;
      border-radius: 5px;
      white-space: pre-wrap;
      word-wrap: break-word;
      font-family: monospace;
      color: #2c3e50;
    }
    .plotly {
        width: 100% !important;
    }
  "
  
  # Converte todos os itens para tags HTML
  html_items <- lapply(items, function(item) {
    # Apenas envolve widgets e texto de teste (que são strings HTML) em cards
    if (inherits(item, "htmlwidget") || (is.character(item) && grepl("<h4>", item))) {
      tags$div(class = "card", {
        if (inherits(item, "htmlwidget")) item else HTML(item)
      })
    } else {
      # Deixa os outros elementos (como h2, h3, p) como estão
      if (is.character(item)) HTML(item) else item
    }
  })
  
  # Cria o corpo do HTML com todos os itens
  doc <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$title("Relatório de Análise de Séries Temporais"),
      tags$style(HTML(css))
    ),
    tags$body(
      tags$div(class = "container",
        tags$h1("Relatório de Análise de Séries Temporais - Preços do Café"),
        html_items
      )
    )
  )
  
  save_html(doc, file = filename, background = "#f4f4f9", libdir = "lib")
  
  message(paste("Relatório HTML completo salvo em:", filename))
}

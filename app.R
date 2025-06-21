library(shiny)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(PerformanceAnalytics)
library(readxl)
library(ggcorrplot)

# ---- Dados ----
load("dados.RData")

# ---- Modelo ----
modelo <- lm(NUMDEFECTS ~ COUPLING_BETWEEN_OBJECTS, data = dados)

# ---- Gráficos pré-gerados ----

# 1. Histograma
grafico_histograma <- dados %>%
  pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

# 2. Boxplot
grafico_boxplot <- dados %>%
  pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

# 3. Dispersão com linha de regressão
grafico_dispersao <- ggplot(dados, aes(x = COUPLING_BETWEEN_OBJECTS, y = NUMDEFECTS)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()

# 4. Matriz de correlação
matriz_cor <- cor(dados %>% select(where(is.numeric)), use = "complete.obs")
grafico_correlacao <- ggcorrplot(matriz_cor, lab = TRUE, colors = c("red", "white", "blue"))

# ---- Interface ----
ui <- fluidPage(
  titlePanel("Previsão de Defeitos em Classes de Software"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("coupling", "Informe o Coupling Between Objects:", value = 0),
      actionButton("prever", "Prever NUMDEFECTS"),
      hr(),
      h4("Selecione o gráfico para visualizar:"),
      selectInput("grafico", "Gráfico:",
                  choices = c("Histograma", "Boxplot", "Dispersão", "Correlação"))
    ),
    
    mainPanel(
      h3("Resultado da Previsão:"),
      verbatimTextOutput("resultado"),
      
      h3("Gráfico Selecionado:"),
      plotOutput("grafico_output")
    )
  )
)

# ---- Servidor ----
server <- function(input, output) {
  # Previsão
  observeEvent(input$prever, {
    pred <- predict(modelo, newdata = data.frame(COUPLING_BETWEEN_OBJECTS = input$coupling))
    output$resultado <- renderPrint({
      paste("Previsão de NUMDEFECTS:", round(pred, 2))
    })
  })
  
  # Gráficos
  output$grafico_output <- renderPlot({
    if (input$grafico == "Histograma") {
      grafico_histograma
    } else if (input$grafico == "Boxplot") {
      grafico_boxplot
    } else if (input$grafico == "Dispersão") {
      grafico_dispersao
    } else if (input$grafico == "Correlação") {
      grafico_correlacao
    }
  })
}

# ---- Executa o App ----
shinyApp(ui = ui, server = server)
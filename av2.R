library(readxl)
library(tidyverse)
library(ggpubr)
library(PerformanceAnalytics)
library(plumber)


# Carregar o dataset
dados <- read_excel("dataset_KC1_classlevel_numdefect.xlsx")

# ✅ Estatísticas descritivas
resumo_estat <- dados %>%
  summarise(across(where(is.numeric), list(
    media = ~mean(., na.rm = TRUE),
    mediana = ~median(., na.rm = TRUE),
    desvio = ~sd(., na.rm = TRUE),
    minimo = ~min(., na.rm = TRUE),
    maximo = ~max(., na.rm = TRUE),
    amplitude = ~max(., na.rm = TRUE) - min(., na.rm = TRUE)
  )))

print(resumo_estat)

# ✅ Moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda_resultado <- sapply(dados %>% select(where(is.numeric)), moda)
print(moda_resultado)

# ✅ Histogramas com densidade
histograma <- dados %>%
  pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

print(histograma)

# ✅ Boxplots
boxplot <- dados %>%
  pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

print(boxplot)

# ✅ Teste de normalidade
normalidade <- apply(dados %>% select(where(is.numeric)), 2, function(x) shapiro.test(x)$p.value)
print(normalidade)

# ✅ Matriz de correlação
chart.Correlation(dados %>% select(where(is.numeric)), histogram = TRUE, pch = 19)

# ✅ Regressão linear simples
modelo <- lm(NUMDEFECTS ~ COUPLING_BETWEEN_OBJECTS, data = dados)
print(summary(modelo))

# ✅ Diagnóstico dos resíduos
par(mfrow = c(2, 2))
plot(modelo)


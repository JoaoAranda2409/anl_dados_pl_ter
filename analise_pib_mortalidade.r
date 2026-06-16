library("readxl")
# Analise: desenvolvimento socioeconomico e saude nas UFs do Brasil
#
# Fontes publicas:
# - IBGE/SIDRA tabela 5938: PIB das Unidades da Federacao.
# - IBGE/SIDRA tabela 7358: populacao projetada.
# - IBGE/SIDRA tabela 7362: esperanca de vida e mortalidade infantil.


ano_analise <- 2023
ano_categoria_sidra <- 49039
dir_resultados <- "resultados"

dir.create(dir_resultados, showWarnings = FALSE, recursive = TRUE)

baixar_sidra <- function(url) {
  dados <- jsonlite::fromJSON(url, flatten = TRUE)
  dados <- as.data.frame(dados, stringsAsFactors = FALSE)

  dados[-1, , drop = FALSE]
}

numero_sidra <- function(x) {
  as.numeric(gsub(",", ".", x, fixed = TRUE))
}

coeficientes_modelo <- function(modelo, nome_modelo) {
  coefs <- as.data.frame(summary(modelo)$coefficients)
  data.frame(
    modelo = nome_modelo,
    termo = row.names(coefs),
    estimativa = coefs[, "Estimate"],
    erro_padrao = coefs[, "Std. Error"],
    estatistica_t = coefs[, "t value"],
    p_valor = coefs[, "Pr(>|t|)"],
    row.names = NULL
  )
}

estatisticas_modelo <- function(modelo, nome_modelo) {
  resumo <- summary(modelo)
  data.frame(
    modelo = nome_modelo,
    r_quadrado = resumo$r.squared,
    r_quadrado_ajustado = resumo$adj.r.squared,
    erro_padrao_residual = resumo$sigma,
    n_observacoes = length(modelo$fitted.values)
  )
}

salvar_grafico <- function(base, y, y_label, titulo, arquivo, cor) {
  caminho <- file.path(dir_resultados, arquivo)

  png(caminho, width = 1200, height = 800, res = 140)
  par(mar = c(5, 5, 4, 2))

  plot(
    base$pib_per_capita,
    base[[y]],
    log = "x",
    pch = 19,
    col = cor,
    xlab = "PIB per capita (R$, escala log)",
    ylab = y_label,
    main = titulo
  )

  text(
    base$pib_per_capita,
    base[[y]],
    labels = base$uf_codigo,
    pos = 3,
    cex = 0.75
  )

  dados_grafico <- data.frame(
    x = base$pib_per_capita,
    y = base[[y]]
  )
  modelo <- lm(y ~ log(x), data = dados_grafico)
  eixo_x <- seq(min(base$pib_per_capita), max(base$pib_per_capita), length.out = 200)
  previsao <- predict(
    modelo,
    newdata = data.frame(x = eixo_x),
    type = "response"
  )

  lines(eixo_x, previsao, col = "black", lwd = 2)
  mtext("Fonte: IBGE/SIDRA, tabelas 5938, 7358 e 7362.", side = 1, line = 4, cex = 0.8)

  dev.off()
  caminho
}

url_pib <- paste0(
  "https://servicodados.ibge.gov.br/api/v3/agregados/5938/",
  "periodos/", ano_analise, "/variaveis/37",
  "?localidades=N3%5Ball%5D&view=flat"
)

url_populacao <- paste0(
  "https://servicodados.ibge.gov.br/api/v3/agregados/7358/",
  "periodos/2018/variaveis/606",
  "?localidades=N3%5Ball%5D",
  "&classificacao=2%5B6794%5D%7C287%5B100362%5D%7C1933%5B",
  ano_categoria_sidra,
  "%5D&view=flat"
)

url_saude <- paste0(
  "https://servicodados.ibge.gov.br/api/v3/agregados/7362/",
  "periodos/2018/variaveis/all",
  "?localidades=N3%5Ball%5D",
  "&classificacao=2%5B6794%5D%7C1933%5B",
  ano_categoria_sidra,
  "%5D&view=flat"
)

pib_bruto <- baixar_sidra(url_pib)
populacao_bruta <- baixar_sidra(url_populacao)
saude_bruta <- baixar_sidra(url_saude)

pib <- data.frame(
  uf_codigo = pib_bruto$D1C,
  uf = pib_bruto$D1N,
  ano = as.integer(pib_bruto$D2N),
  pib_mil_reais = numero_sidra(pib_bruto$V),
  stringsAsFactors = FALSE
)
pib$pib_reais <- pib$pib_mil_reais * 1000

populacao <- data.frame(
  uf_codigo = populacao_bruta$D1C,
  populacao = numero_sidra(populacao_bruta$V),
  stringsAsFactors = FALSE
)

vida <- saude_bruta[saude_bruta$D3C == "2503", c("D1C", "V")]
names(vida) <- c("uf_codigo", "esperanca_vida")
vida$esperanca_vida <- numero_sidra(vida$esperanca_vida)

mortalidade <- saude_bruta[saude_bruta$D3C == "1940", c("D1C", "V")]
names(mortalidade) <- c("uf_codigo", "mortalidade_infantil")
mortalidade$mortalidade_infantil <- numero_sidra(mortalidade$mortalidade_infantil)

base_modelo <- merge(pib, populacao, by = "uf_codigo")
base_modelo <- merge(base_modelo, vida, by = "uf_codigo")
base_modelo <- merge(base_modelo, mortalidade, by = "uf_codigo")

base_modelo$pib_per_capita <- base_modelo$pib_reais / base_modelo$populacao
base_modelo$log_pib_per_capita <- log(base_modelo$pib_per_capita)
base_modelo <- base_modelo[order(base_modelo$pib_per_capita, decreasing = TRUE), ]
row.names(base_modelo) <- NULL

modelo_vida <- lm(esperanca_vida ~ log_pib_per_capita, data = base_modelo)
modelo_mortalidade <- lm(mortalidade_infantil ~ log_pib_per_capita, data = base_modelo)

resumo_modelos <- rbind(
  coeficientes_modelo(modelo_vida, "Esperanca de vida"),
  coeficientes_modelo(modelo_mortalidade, "Mortalidade infantil")
)

estatisticas_modelos <- rbind(
  estatisticas_modelo(modelo_vida, "Esperanca de vida"),
  estatisticas_modelo(modelo_mortalidade, "Mortalidade infantil")
)

write.csv(
  base_modelo,
  file.path(dir_resultados, "base_saude_desenvolvimento_uf.csv"),
  row.names = FALSE
)

write.csv(
  resumo_modelos,
  file.path(dir_resultados, "coeficientes_modelos.csv"),
  row.names = FALSE
)

write.csv(
  estatisticas_modelos,
  file.path(dir_resultados, "estatisticas_modelos.csv"),
  row.names = FALSE
)

grafico_vida <- salvar_grafico(
  base = base_modelo,
  y = "esperanca_vida",
  y_label = "Esperanca de vida ao nascer (anos)",
  titulo = paste("PIB per capita e esperanca de vida nas UFs brasileiras -", ano_analise),
  arquivo = "pib_per_capita_esperanca_vida.png",
  cor = "#1F77B4"
)

grafico_mortalidade <- salvar_grafico(
  base = base_modelo,
  y = "mortalidade_infantil",
  y_label = "Mortalidade infantil por mil nascidos vivos",
  titulo = paste("PIB per capita e mortalidade infantil nas UFs brasileiras -", ano_analise),
  arquivo = "pib_per_capita_mortalidade_infantil.png",
  cor = "#D62728"
)

cat("\nBase final:\n")
print(base_modelo)

cat("\nModelo 1: esperanca de vida ~ log(PIB per capita)\n")
print(summary(modelo_vida))

cat("\nModelo 2: mortalidade infantil ~ log(PIB per capita)\n")
print(summary(modelo_mortalidade))

cat("\nArquivos gerados:\n")
cat("- ", normalizePath(file.path(dir_resultados, "base_saude_desenvolvimento_uf.csv")), "\n", sep = "")
cat("- ", normalizePath(file.path(dir_resultados, "coeficientes_modelos.csv")), "\n", sep = "")
cat("- ", normalizePath(file.path(dir_resultados, "estatisticas_modelos.csv")), "\n", sep = "")
cat("- ", normalizePath(grafico_vida), "\n", sep = "")
cat("- ", normalizePath(grafico_mortalidade), "\n", sep = "")
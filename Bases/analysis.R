# =============================================================================
# Analise de acidentes de transito - Porto Alegre (2009-2014)
# =============================================================================

library(arules)

# -- Configuracao -------------------------------------------------------------
ANOS <- 2009:2014
DATA_DIR <- "data"
IMG_DIR <- "images"

colunas_veiculos <- c("AUTO", "TAXI", "LOTACAO", "ONIBUS_URB", "ONIBUS_INT",
                      "CAMINHAO", "MOTO", "CARROCA", "BICICLETA", "OUTRO")
colunas_vitimas <- c("FATAIS", "MORTES", "MORTE_POST", "FERIDOS")

# -- Carga dos dados ----------------------------------------------------------
carregar_acidentes <- function(ano) {
  arquivo <- file.path(DATA_DIR, paste0("acidentes-", ano, ".csv"))
  df <- read.csv(arquivo, header = TRUE, sep = ";")
  df$ANO <- ano
  df
}

# Carrega todos os anos e empilha num unico dataframe
lista <- lapply(ANOS, carregar_acidentes)
acidentes <- do.call(rbind, lista)

cat("Total de registros carregados:", nrow(acidentes), "\n")

# -- Resumo por ano -----------------------------------------------------------
resumo <- do.call(rbind, lapply(ANOS, function(ano) {
  df <- acidentes[acidentes$ANO == ano, ]
  data.frame(
    ANO            = ano,
    TOTAL_ACIDENTES = nrow(df),
    TOTAL_MORTES    = sum(df$MORTES, na.rm = TRUE) + sum(df$MORTE_POST, na.rm = TRUE),
    TOTAL_FERIDOS   = sum(df$FERIDOS, na.rm = TRUE),
    ACIDENTES_FATAIS = sum(as.numeric(df$FATAIS) >= 1, na.rm = TRUE),
    RANGE_MORTES     = paste(range(df$MORTES, na.rm = TRUE), collapse = "-"),
    RANGE_MORTE_POST = paste(range(df$MORTE_POST, na.rm = TRUE), collapse = "-"),
    RANGE_FATAIS     = paste(range(df$FATAIS, na.rm = TRUE), collapse = "-")
  )
}))

print(resumo)

# -- Histogramas: horarios de acidentes ----------------------------------------
jpeg(file.path(IMG_DIR, "horas_acidentes.jpg"),
     width = 800, height = 800, quality = 100)
par(mfrow = c(3, 2))
for (ano in rev(ANOS)) {
  df <- acidentes[acidentes$ANO == ano, ]
  hist(as.numeric(df$FX_HORA), breaks = 24,
       main = ano, xlab = "Horario do acidente",
       ylab = "Frequencia", col = "steelblue")
}
dev.off()

# -- Histogramas: meses de acidentes -------------------------------------------
jpeg(file.path(IMG_DIR, "meses_acidentes.jpg"),
     width = 800, height = 800, quality = 100)
par(mfrow = c(3, 2))
for (ano in rev(ANOS)) {
  df <- acidentes[acidentes$ANO == ano, ]
  hist(as.numeric(df$MES), breaks = 12,
       main = ano, xlab = "Mes do acidente",
       ylab = "Frequencia", col = "steelblue")
}
dev.off()

# -- Correlacoes: veiculos x vitimas (acidentes fatais) ------------------------
fatais <- acidentes[acidentes$FATAIS != 0, c(colunas_veiculos, colunas_vitimas)]

correlacoes <- sapply(colunas_veiculos, function(veiculo) {
  sapply(colunas_vitimas, function(vitima) {
    cor(fatais[[vitima]], fatais[[veiculo]], use = "complete.obs")
  })
})

cat("\nCorrelacoes (veiculos x vitimas em acidentes fatais):\n")
print(round(correlacoes, 3))

# -- Apriori: regras de associacao em acidentes fatais -------------------------
colunas_apriori <- c("NOITE_DIA", "LOCAL", "DIA_SEM", "LOG1",
                     "TIPO_ACID", "TEMPO", "REGIAO")

fatais_cat <- acidentes[acidentes$FATAIS != 0, colunas_apriori]
fatais_cat[] <- lapply(fatais_cat, as.factor)

# Regras para dia/noite
rules_noitedia <- apriori(
  fatais_cat,
  parameter  = list(minlen = 2, supp = 0.005, conf = 0.8),
  appearance = list(rhs = c("NOITE_DIA=NOITE", "NOITE_DIA=DIA"), default = "lhs"),
  control    = list(verbose = FALSE)
)
cat("\nRegras Apriori - Noite/Dia:\n")
inspect(rules_noitedia)

# Regras para tipo de acidente
rules_tipo <- apriori(
  fatais_cat,
  parameter  = list(minlen = 2, supp = 0.01, conf = 0.8),
  appearance = list(
    rhs = c("TIPO_ACID=ATROPELAMENTO", "TIPO_ACID=CHOQUE", "TIPO_ACID=ALBAROAMENTO"),
    default = "lhs"
  ),
  control = list(verbose = FALSE)
)
cat("\nRegras Apriori - Tipo de acidente:\n")
inspect(rules_tipo)

#-------------------------------------------------------------------------------
# COMPARAÇÃO DOS INDICADORES — POF 2002-2003, POF 2008-2009,
# PNS 2013 E PNS 2019
# TCC Ciências Sociais
# Felipe Casula, 2026
#-------------------------------------------------------------------------------
#
# Este script não recalcula nenhum indicador. Ele:
#   1. lê os CSVs produzidos pelos quatro scripts analíticos;
#   2. verifica se todas as ondas, sexos e faixas etárias estão presentes;
#   3. reúne os resultados em tabelas históricas;
#   4. cria gráficos comparáveis com título, legenda e fontes.
#
# Execute este arquivo somente depois de concluir os quatro scripts:
#   indicadores_pof_2002_2003_bootstrap.R
#   indicadores_pof_2008_2009_bootstrap.R
#   indicadores_pns_2013_bootstrap.R
#   indicadores_pns_2019_bootstrap.R
#-------------------------------------------------------------------------------

rm(list = ls(all = TRUE))

#--- 0. Bibliotecas, caminhos e definições ------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  janitor,
  scales,
  here
)

pasta_pof02 <- here::here(
  "dados", "resultados", "pof_2002_2003"
)
pasta_pof08 <- here::here(
  "dados", "resultados", "pof_2008_2009"
)
pasta_pns13 <- here::here(
  "dados", "resultados", "pns_2013"
)
pasta_pns19 <- here::here(
  "dados", "resultados", "pns_2019"
)

pasta_resultados <- here::here(
  "dados", "resultados", "comparacao_4_ondas"
)
pasta_graficos <- here::here(
  "graficos", "comparacao_4_ondas"
)

dir.create(pasta_resultados, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)

faixas_idade <- c(
  "20-29", "30-39", "40-49", "50-59",
  "60-69", "70-79", "80+"
)

ordem_pesquisas <- c(
  "POF 2002-2003",
  "POF 2008-2009",
  "PNS 2013",
  "PNS 2019"
)

arquivos_esperados <- c(
  file.path(pasta_pof02, "05_paf_por_sexo_e_idade.csv"),
  file.path(pasta_pof02, "06_paf_sintetica_por_sexo.csv"),
  file.path(pasta_pof02, "07_mortes_evitaveis_por_sexo_e_idade.csv"),
  file.path(pasta_pof02, "08_mortes_evitaveis_por_sexo.csv"),
  file.path(pasta_pof02, "09_pgev20_por_sexo.csv"),

  file.path(pasta_pof08, "05_paf_por_sexo_e_idade.csv"),
  file.path(pasta_pof08, "06_paf_sintetica_por_sexo.csv"),
  file.path(pasta_pof08, "07_mortes_evitaveis_por_sexo_e_idade.csv"),
  file.path(pasta_pof08, "08_mortes_evitaveis_por_sexo.csv"),
  file.path(pasta_pof08, "09_pgev20_por_sexo.csv"),

  file.path(pasta_pns13, "05_paf_por_sexo_e_idade.csv"),
  file.path(pasta_pns13, "06_paf_sintetica_por_sexo.csv"),
  file.path(pasta_pns13, "07_mortes_evitaveis_por_sexo_e_idade.csv"),
  file.path(pasta_pns13, "08_mortes_evitaveis_por_sexo.csv"),
  file.path(pasta_pns13, "09_pgev20_por_sexo.csv"),

  file.path(pasta_pns19, "05_paf_por_sexo_e_idade.csv"),
  file.path(pasta_pns19, "06_paf_sintetica_por_sexo.csv"),
  file.path(pasta_pns19, "07_mortes_evitaveis_por_sexo_e_idade.csv"),
  file.path(pasta_pns19, "08_mortes_evitaveis_por_sexo.csv"),
  file.path(pasta_pns19, "09_pgev20_por_sexo.csv")
)

if (any(!file.exists(arquivos_esperados))) {
  stop(
    "Execute primeiro os quatro scripts analíticos. Arquivos ausentes:\n",
    paste(arquivos_esperados[!file.exists(arquivos_esperados)], collapse = "\n")
  )
}

#--- 1. Ler os resultados de cada pesquisa ------------------------------------

message("\n[1/6] Ler os resultados das quatro ondas")

# POF 2002-2003
paf_idade_pof02 <- readr::read_csv(
  file.path(pasta_pof02, "05_paf_por_sexo_e_idade.csv"),
  show_col_types = FALSE
)
paf_sintetica_pof02 <- readr::read_csv(
  file.path(pasta_pof02, "06_paf_sintetica_por_sexo.csv"),
  show_col_types = FALSE
)
mortes_idade_pof02 <- readr::read_csv(
  file.path(pasta_pof02, "07_mortes_evitaveis_por_sexo_e_idade.csv"),
  show_col_types = FALSE
)
mortes_total_pof02 <- readr::read_csv(
  file.path(pasta_pof02, "08_mortes_evitaveis_por_sexo.csv"),
  show_col_types = FALSE
)
pgev20_pof02 <- readr::read_csv(
  file.path(pasta_pof02, "09_pgev20_por_sexo.csv"),
  show_col_types = FALSE
)

# POF 2008-2009
paf_idade_pof08 <- readr::read_csv(
  file.path(pasta_pof08, "05_paf_por_sexo_e_idade.csv"),
  show_col_types = FALSE
)
paf_sintetica_pof08 <- readr::read_csv(
  file.path(pasta_pof08, "06_paf_sintetica_por_sexo.csv"),
  show_col_types = FALSE
)
mortes_idade_pof08 <- readr::read_csv(
  file.path(pasta_pof08, "07_mortes_evitaveis_por_sexo_e_idade.csv"),
  show_col_types = FALSE
)
mortes_total_pof08 <- readr::read_csv(
  file.path(pasta_pof08, "08_mortes_evitaveis_por_sexo.csv"),
  show_col_types = FALSE
)
pgev20_pof08 <- readr::read_csv(
  file.path(pasta_pof08, "09_pgev20_por_sexo.csv"),
  show_col_types = FALSE
)

# PNS 2013
paf_idade_pns13 <- readr::read_csv(
  file.path(pasta_pns13, "05_paf_por_sexo_e_idade.csv"),
  show_col_types = FALSE
)
paf_sintetica_pns13 <- readr::read_csv(
  file.path(pasta_pns13, "06_paf_sintetica_por_sexo.csv"),
  show_col_types = FALSE
)
mortes_idade_pns13 <- readr::read_csv(
  file.path(pasta_pns13, "07_mortes_evitaveis_por_sexo_e_idade.csv"),
  show_col_types = FALSE
)
mortes_total_pns13 <- readr::read_csv(
  file.path(pasta_pns13, "08_mortes_evitaveis_por_sexo.csv"),
  show_col_types = FALSE
)
pgev20_pns13 <- readr::read_csv(
  file.path(pasta_pns13, "09_pgev20_por_sexo.csv"),
  show_col_types = FALSE
)

# PNS 2019
paf_idade_pns19 <- readr::read_csv(
  file.path(pasta_pns19, "05_paf_por_sexo_e_idade.csv"),
  show_col_types = FALSE
)
paf_sintetica_pns19 <- readr::read_csv(
  file.path(pasta_pns19, "06_paf_sintetica_por_sexo.csv"),
  show_col_types = FALSE
)
mortes_idade_pns19 <- readr::read_csv(
  file.path(pasta_pns19, "07_mortes_evitaveis_por_sexo_e_idade.csv"),
  show_col_types = FALSE
)
mortes_total_pns19 <- readr::read_csv(
  file.path(pasta_pns19, "08_mortes_evitaveis_por_sexo.csv"),
  show_col_types = FALSE
)
pgev20_pns19 <- readr::read_csv(
  file.path(pasta_pns19, "09_pgev20_por_sexo.csv"),
  show_col_types = FALSE
)

#--- 2. Reunir e verificar as tabelas históricas ------------------------------

message("[2/6] Reunir e verificar as tabelas históricas")

paf_idade_4_ondas <- bind_rows(
  paf_idade_pof02,
  paf_idade_pof08,
  paf_idade_pns13,
  paf_idade_pns19
) |>
  mutate(
    ano_mortalidade = as.integer(ano_mortalidade),
    pesquisa = factor(pesquisa, levels = ordem_pesquisas, ordered = TRUE),
    sexo = factor(sexo, levels = c("M", "F")),
    sexo_rotulo = factor(
      sexo_rotulo,
      levels = c("Homens", "Mulheres"),
      ordered = TRUE
    ),
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    )
  ) |>
  arrange(pesquisa, sexo, faixa_idade)

paf_sintetica_4_ondas <- bind_rows(
  paf_sintetica_pof02,
  paf_sintetica_pof08,
  paf_sintetica_pns13,
  paf_sintetica_pns19
) |>
  mutate(
    ano_mortalidade = as.integer(ano_mortalidade),
    pesquisa = factor(pesquisa, levels = ordem_pesquisas, ordered = TRUE),
    sexo = factor(sexo, levels = c("M", "F")),
    sexo_rotulo = factor(
      sexo_rotulo,
      levels = c("Homens", "Mulheres"),
      ordered = TRUE
    )
  ) |>
  arrange(pesquisa, sexo)

mortes_idade_4_ondas <- bind_rows(
  mortes_idade_pof02,
  mortes_idade_pof08,
  mortes_idade_pns13,
  mortes_idade_pns19
) |>
  mutate(
    ano_mortalidade = as.integer(ano_mortalidade),
    pesquisa = factor(pesquisa, levels = ordem_pesquisas, ordered = TRUE),
    sexo = factor(sexo, levels = c("M", "F")),
    sexo_rotulo = factor(
      sexo_rotulo,
      levels = c("Homens", "Mulheres"),
      ordered = TRUE
    ),
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    )
  ) |>
  arrange(pesquisa, sexo, faixa_idade)

mortes_total_4_ondas <- bind_rows(
  mortes_total_pof02,
  mortes_total_pof08,
  mortes_total_pns13,
  mortes_total_pns19
) |>
  mutate(
    ano_mortalidade = as.integer(ano_mortalidade),
    pesquisa = factor(pesquisa, levels = ordem_pesquisas, ordered = TRUE),
    sexo = factor(sexo, levels = c("M", "F")),
    sexo_rotulo = factor(
      sexo_rotulo,
      levels = c("Homens", "Mulheres"),
      ordered = TRUE
    )
  ) |>
  arrange(pesquisa, sexo)

pgev20_4_ondas <- bind_rows(
  pgev20_pof02,
  pgev20_pof08,
  pgev20_pns13,
  pgev20_pns19
) |>
  mutate(
    ano_mortalidade = as.integer(ano_mortalidade),
    pesquisa = factor(pesquisa, levels = ordem_pesquisas, ordered = TRUE),
    sexo = factor(sexo, levels = c("M", "F")),
    sexo_rotulo = factor(
      sexo_rotulo,
      levels = c("Homens", "Mulheres"),
      ordered = TRUE
    )
  ) |>
  arrange(pesquisa, sexo)

if (nrow(paf_idade_4_ondas) != 4L * 2L * length(faixas_idade)) {
  stop("A tabela histórica de PAF por idade não possui as 56 células esperadas.")
}

if (nrow(mortes_idade_4_ondas) != 4L * 2L * length(faixas_idade)) {
  stop("A tabela histórica de mortes por idade não possui as 56 células esperadas.")
}

if (nrow(paf_sintetica_4_ondas) != 8L ||
    nrow(mortes_total_4_ondas) != 8L ||
    nrow(pgev20_4_ondas) != 8L) {
  stop("Um resultado agregado não possui as oito combinações pesquisa x sexo.")
}

if (any(!is.finite(paf_idade_4_ondas$estimativa)) ||
    any(!is.finite(paf_sintetica_4_ondas$estimativa)) ||
    any(!is.finite(mortes_idade_4_ondas$estimativa)) ||
    any(!is.finite(mortes_total_4_ondas$estimativa)) ||
    any(!is.finite(pgev20_4_ondas$estimativa))) {
  stop("Há estimativas não finitas nos resultados consolidados.")
}

#--- 3. Registrar fontes e decisões metodológicas -----------------------------

message("[3/6] Registrar as fontes em formato ABNT")

referencias_ondas <- tribble(
  ~pesquisa, ~ano_mortalidade, ~fonte_exposicao, ~fonte_obitos, ~criterio_obitos, ~fonte_tabua_vida, ~metodo_bootstrap,
  "POF 2002-2003", 2003L,
  "IBGE. Pesquisa de Orçamentos Familiares 2002-2003: microdados. Rio de Janeiro: IBGE.",
  "BRASIL. Ministério da Saúde. Sistema de Informações sobre Mortalidade (SIM): óbitos por ocorrência, 2003. Brasília, DF: Ministério da Saúde.",
  "Óbitos por ocorrência; sexo ignorado não redistribuído.",
  "IBGE. Tábua completa de mortalidade para o Brasil, 2003. Rio de Janeiro: IBGE.",
  "Rao-Wu-Yue-Beaumont, 1.000 réplicas; IC percentil de 95%.",

  "POF 2008-2009", 2009L,
  "IBGE. Pesquisa de Orçamentos Familiares 2008-2009: microdados. Rio de Janeiro: IBGE.",
  "BRASIL. Ministério da Saúde. Sistema de Informações sobre Mortalidade (SIM): óbitos por ocorrência, 2009. Brasília, DF: Ministério da Saúde.",
  "Óbitos por ocorrência; sexo ignorado não redistribuído.",
  "IBGE. Tábua completa de mortalidade para o Brasil, 2009. Rio de Janeiro: IBGE.",
  "Bootstrap generalizado de Beaumont e Patak, variância por conglomerado último, 1.000 réplicas; IC percentil de 95%.",

  "PNS 2013", 2013L,
  "IBGE. Pesquisa Nacional de Saúde 2013: microdados. Rio de Janeiro: IBGE.",
  "BRASIL. Ministério da Saúde. Sistema de Informações sobre Mortalidade (SIM): óbitos por ocorrência, 2013. Brasília, DF: Ministério da Saúde.",
  "Óbitos por ocorrência; sexo ignorado não redistribuído.",
  "IBGE. Tábua completa de mortalidade para o Brasil, 2013. Rio de Janeiro: IBGE.",
  "Rao-Wu-Yue-Beaumont, 1.000 réplicas; IC percentil de 95%.",

  "PNS 2019", 2019L,
  "IBGE. Pesquisa Nacional de Saúde 2019: microdados. Rio de Janeiro: IBGE.",
  "BRASIL. Ministério da Saúde. Sistema de Informações sobre Mortalidade (SIM): óbitos por ocorrência, 2019. Brasília, DF: Ministério da Saúde.",
  "Óbitos por ocorrência; sexo ignorado não redistribuído.",
  "IBGE. Tábua completa de mortalidade para o Brasil, 2019. Rio de Janeiro: IBGE.",
  "Bootstrap generalizado de Beaumont e Patak, variância por conglomerado último, 1.000 réplicas; IC percentil de 95%."
) |>
  mutate(
    fonte_riscos_relativos = paste0(
      "DI ANGELANTONIO, E. et al. Body-mass index and all-cause mortality: ",
      "individual-participant-data meta-analysis of 239 prospective studies ",
      "in four continents. The Lancet, v. 388, p. 776-786, 2016. ",
      "DOI: 10.1016/S0140-6736(16)30175-1."
    ),
    cenario = "Eliminação do IMC >= 25 kg/m²",
    incerteza_rr = "Não propagada; medidas centrais fixas.",
    incerteza_mortalidade = "Não propagada; tábua de vida e óbitos do SIM fixos."
  )

#--- 4. Salvar as tabelas históricas ------------------------------------------

message("[4/6] Salvar as tabelas históricas")

readr::write_csv(
  referencias_ondas,
  file.path(pasta_resultados, "00_referencias_e_metodos.csv")
)

readr::write_csv(
  paf_idade_4_ondas,
  file.path(pasta_resultados, "01_paf_por_sexo_e_idade_4_ondas.csv")
)

readr::write_csv(
  paf_sintetica_4_ondas,
  file.path(pasta_resultados, "02_paf_sintetica_por_sexo_4_ondas.csv")
)

readr::write_csv(
  mortes_idade_4_ondas,
  file.path(
    pasta_resultados,
    "03_mortes_evitaveis_por_sexo_e_idade_4_ondas.csv"
  )
)

readr::write_csv(
  mortes_total_4_ondas,
  file.path(pasta_resultados, "04_mortes_evitaveis_por_sexo_4_ondas.csv")
)

readr::write_csv(
  pgev20_4_ondas,
  file.path(pasta_resultados, "05_pgev20_por_sexo_4_ondas.csv")
)

#--- 5. Definir a apresentação gráfica ----------------------------------------

message("[5/6] Construir os gráficos comparativos")

cores_ondas <- c(
  "POF 2002-2003" = "#264653",
  "POF 2008-2009" = "#2A9D8F",
  "PNS 2013" = "#E9C46A",
  "PNS 2019" = "#C14953"
)

formas_ondas <- c(
  "POF 2002-2003" = 16,
  "POF 2008-2009" = 17,
  "PNS 2013" = 15,
  "PNS 2019" = 18
)

cores_sexo <- c(
  "Homens" = "#1F4E79",
  "Mulheres" = "#8A3B5C"
)

tema_abnt <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 8)),
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
    axis.title = element_text(size = 10),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 12, 10, 10)
  )

fonte_exposicao_rr <- paste0(
  "Fonte: IBGE, POF 2002-2003 e 2008-2009; IBGE, PNS 2013 e 2019; ",
  "Di Angelantonio et al. (2016). Elaboração própria."
)

fonte_tabua_rr <- paste0(
  "Fonte: IBGE, POF 2002-2003 e 2008-2009; IBGE, PNS 2013 e 2019; ",
  "IBGE, Tábuas Completas de Mortalidade; Di Angelantonio et al. ",
  "(2016). Elaboração própria."
)

fonte_sim_rr <- paste0(
  "Fonte: IBGE, POF 2002-2003 e 2008-2009; IBGE, PNS 2013 e 2019; ",
  "MS/SVSA/CGIAE, SIM, óbitos por ocorrência; Di Angelantonio et al. ",
  "(2016). Elaboração própria."
)

grafico_paf_idade <- ggplot(
  paf_idade_4_ondas,
  aes(
    x = faixa_idade,
    y = estimativa_pct,
    color = pesquisa,
    shape = pesquisa,
    group = pesquisa
  )
) +
  geom_errorbar(
    aes(ymin = ic_inferior_pct, ymax = ic_superior_pct),
    width = 0.10,
    linewidth = 0.35,
    alpha = 0.65
  ) +
  geom_line(linewidth = 0.72) +
  geom_point(size = 2.0) +
  facet_wrap(~sexo_rotulo, ncol = 1) +
  scale_color_manual(values = cores_ondas) +
  scale_shape_manual(values = formas_ondas) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", suffix = "%", accuracy = 0.1)
  ) +
  labs(
    title = "Fração atribuível populacional por idade e sexo",
    subtitle = "Brasil, quatro ondas; IC 95% obtido pelas réplicas bootstrap",
    x = "Grupo etário",
    y = "Fração atribuível populacional",
    color = "Pesquisa",
    shape = "Pesquisa",
    caption = fonte_exposicao_rr
  ) +
  tema_abnt

grafico_paf_sintetica <- ggplot(
  paf_sintetica_4_ondas,
  aes(
    x = ano_mortalidade,
    y = estimativa_pct,
    color = sexo_rotulo,
    group = sexo_rotulo
  )
) +
  geom_errorbar(
    aes(ymin = ic_inferior_pct, ymax = ic_superior_pct),
    width = 0.32,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.3) +
  scale_color_manual(values = cores_sexo) +
  scale_x_continuous(breaks = c(2003, 2009, 2013, 2019)) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", suffix = "%", accuracy = 0.1)
  ) +
  labs(
    title = "Evolução da fração atribuível populacional sintética",
    subtitle = "Ponderação pela distribuição de óbitos da tábua de vida de cada ano",
    x = "Ano de referência da mortalidade",
    y = "Fração atribuível populacional",
    color = "Sexo",
    caption = fonte_tabua_rr
  ) +
  tema_abnt

grafico_mortes_idade <- ggplot(
  mortes_idade_4_ondas,
  aes(
    x = faixa_idade,
    y = estimativa,
    color = pesquisa,
    shape = pesquisa,
    group = pesquisa
  )
) +
  geom_errorbar(
    aes(ymin = ic_inferior, ymax = ic_superior),
    width = 0.10,
    linewidth = 0.35,
    alpha = 0.65
  ) +
  geom_line(linewidth = 0.72) +
  geom_point(size = 2.0) +
  facet_wrap(~sexo_rotulo, ncol = 1, scales = "free_y") +
  scale_color_manual(values = cores_ondas) +
  scale_shape_manual(values = formas_ondas) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 1)
  ) +
  labs(
    title = "Número absoluto de mortes evitáveis por idade e sexo",
    subtitle = "PAF da célula multiplicada pelos óbitos do SIM na mesma célula; IC 95% bootstrap",
    x = "Grupo etário",
    y = "Número de mortes evitáveis",
    color = "Pesquisa",
    shape = "Pesquisa",
    caption = fonte_sim_rr
  ) +
  tema_abnt

grafico_mortes_total <- ggplot(
  mortes_total_4_ondas,
  aes(
    x = ano_mortalidade,
    y = estimativa,
    color = sexo_rotulo,
    group = sexo_rotulo
  )
) +
  geom_errorbar(
    aes(ymin = ic_inferior, ymax = ic_superior),
    width = 0.32,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.3) +
  scale_color_manual(values = cores_sexo) +
  scale_x_continuous(breaks = c(2003, 2009, 2013, 2019)) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 1)
  ) +
  labs(
    title = "Evolução do número absoluto de mortes evitáveis",
    subtitle = "Brasil, população de 20 anos ou mais; IC 95% bootstrap",
    x = "Ano de referência da mortalidade",
    y = "Número de mortes evitáveis",
    color = "Sexo",
    caption = fonte_sim_rr
  ) +
  tema_abnt

grafico_pgev20 <- ggplot(
  pgev20_4_ondas,
  aes(
    x = ano_mortalidade,
    y = estimativa,
    color = sexo_rotulo,
    group = sexo_rotulo
  )
) +
  geom_errorbar(
    aes(ymin = ic_inferior, ymax = ic_superior),
    width = 0.32,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.3) +
  scale_color_manual(values = cores_sexo) +
  scale_x_continuous(breaks = c(2003, 2009, 2013, 2019)) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", accuracy = 0.01)
  ) +
  labs(
    title = "Evolução do potencial ganho em expectativa de vida aos 20 anos",
    subtitle = "Contrafactual de eliminação do IMC >= 25 kg/m²; IC 95% bootstrap",
    x = "Ano de referência da mortalidade",
    y = "Ganho em anos de vida",
    color = "Sexo",
    caption = fonte_tabua_rr
  ) +
  tema_abnt

#--- 6. Salvar os gráficos -----------------------------------------------------

message("[6/6] Salvar os gráficos comparativos")

ggsave(
  file.path(pasta_graficos, "01_paf_por_idade_e_sexo_4_ondas.png"),
  grafico_paf_idade,
  width = 8.27,
  height = 8.0,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "01_paf_por_idade_e_sexo_4_ondas.pdf"),
  grafico_paf_idade,
  width = 8.27,
  height = 8.0,
  units = "in"
)

ggsave(
  file.path(pasta_graficos, "02_paf_sintetica_4_ondas.png"),
  grafico_paf_sintetica,
  width = 8.27,
  height = 5.5,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "02_paf_sintetica_4_ondas.pdf"),
  grafico_paf_sintetica,
  width = 8.27,
  height = 5.5,
  units = "in"
)

ggsave(
  file.path(pasta_graficos, "03_mortes_por_idade_e_sexo_4_ondas.png"),
  grafico_mortes_idade,
  width = 8.27,
  height = 8.0,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "03_mortes_por_idade_e_sexo_4_ondas.pdf"),
  grafico_mortes_idade,
  width = 8.27,
  height = 8.0,
  units = "in"
)

ggsave(
  file.path(pasta_graficos, "04_mortes_evitaveis_4_ondas.png"),
  grafico_mortes_total,
  width = 8.27,
  height = 5.5,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "04_mortes_evitaveis_4_ondas.pdf"),
  grafico_mortes_total,
  width = 8.27,
  height = 5.5,
  units = "in"
)

ggsave(
  file.path(pasta_graficos, "05_pgev20_4_ondas.png"),
  grafico_pgev20,
  width = 8.27,
  height = 5.5,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "05_pgev20_4_ondas.pdf"),
  grafico_pgev20,
  width = 8.27,
  height = 5.5,
  units = "in"
)

message(
  "\nComparação concluída.",
  "\nTabelas: ", pasta_resultados,
  "\nGráficos: ", pasta_graficos
)

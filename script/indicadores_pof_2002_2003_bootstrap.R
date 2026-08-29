#-------------------------------------------------------------------------------
# PAF, MORTES EVITÁVEIS ABSOLUTAS E PGEV20 — POF 2002-2003
# TCC Ciências Sociais
# Felipe Casula, 2026
#-------------------------------------------------------------------------------
#
# Objetivo
#   Estimar, separadamente para homens e mulheres de 20 anos ou mais:
#     1. a PAF por grupo etário decenal;
#     2. a PAF sintética, ponderada pelos óbitos da tábua de vida do IBGE;
#     3. o número absoluto de mortes evitáveis por grupo etário e agregado;
#     4. o potencial ganho em expectativa de vida aos 20 anos (PGEV20).
#
# Incerteza
#   A incerteza vem exclusivamente do desenho amostral da POF 2002-2003.
#   São criadas 1.000 réplicas Rao-Wu-Yue-Beaumont. Os intervalos de 95%
#   correspondem aos percentis 2,5 e 97,5 das estimativas replicadas.
#   Os riscos relativos, a tábua de vida e os óbitos do SIM são fixos.
#
# Bases de mortalidade
#   A Tábua de Vida do IBGE de 2003 fornece mx, ax e ndx para a PAF sintética e
#   para o PGEV20. O arquivo sim_2003.csv fornece as contagens absolutas de
#   óbitos por ocorrência. Em cada célula sexo x idade, mortes evitáveis =
#   PAF específica x número absoluto de óbitos do SIM.
#
# Contrafactual
#   Eliminação do excesso de peso: toda a população com IMC >= 25 kg/m² é
#   deslocada para a categoria de IMC normal. A prevalência de baixo peso é
#   preservada.
#
# Observação sobre os riscos relativos
#   O arquivo df_RR.csv fornecido contém riscos categoriais centrais, mas não
#   possui uma coluna de sexo. Por isso, a mesma tabela de RR é aplicada aos
#   dois sexos. A separação por sexo permanece na exposição, na mortalidade e
#   na tábua de vida. RR_ci_l e RR_ci_u não entram em nenhum cálculo.
#-------------------------------------------------------------------------------

rm(list = ls(all = TRUE))

#--- 0. Bibliotecas e opções ---------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  survey,
  svrep,
  tidyverse,
  janitor,
  scales,
  here
)

options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE,
  scipen = 999
)

caminho_pof <- here::here(
  "dados", "raw", "pof", "T_MORADOR_2002-2003.TXT"
)

# O arquivo original do IBGE costuma ser extraído com este nome mais curto.
if (!file.exists(caminho_pof)) {
  caminho_pof <- here::here(
    "dados", "raw", "pof", "T_MORADOR.TXT"
  )
}

caminho_rr <- here::here(
  "dados", "raw", "riscos_relativos", "df_RR.csv"
)

caminho_tabua <- here::here(
  "dados", "raw", "tabuas_de_vida", "tabuas_de_vida.csv"
)

caminho_sim <- here::here(
  "dados", "dados_obitos", "sim_2003.csv"
)

pasta_resultados <- here::here(
  "dados", "resultados", "pof_2002_2003"
)
pasta_graficos <- here::here(
  "graficos", "pof_2002_2003"
)

dir.create(pasta_resultados, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)

arquivos_obrigatorios <- c(caminho_pof, caminho_rr, caminho_tabua, caminho_sim)

if (any(!file.exists(arquivos_obrigatorios))) {
  stop(
    "Arquivos obrigatórios não encontrados:\n",
    paste(arquivos_obrigatorios[!file.exists(arquivos_obrigatorios)], collapse = "\n")
  )
}

faixas_idade <- c(
  "20-29", "30-39", "40-49", "50-59",
  "60-69", "70-79", "80+"
)

categorias_imc <- c(
  "baixo", "normal", "sobrepeso",
  "obesidade1", "obesidade2", "obesidade3"
)

numero_replicas <- 1000L
semente_bootstrap <- 20262003L

#--- 1. Ler e tratar a POF 2002-2003 ------------------------------------------

message("\n[1/9] Ler e tratar a antropometria da POF 2002-2003")

# Arquivo utilizado: T_MORADOR.TXT
#
# Posições no arquivo de largura fixa:
#   3-4     Unidade da Federação
#   5-7     Número sequencial do setor
#   8       Dígito verificador do setor
#   12-13   Estrato geográfico
#   14-24   Fator de expansão inicial
#   25-35   Fator de expansão final
#   40      Sexo
#   49-51   Idade
#   116-120 Peso imputado, em kg
#   121-125 Altura imputada, em cm

pof2002_bruta <- readr::read_fwf(
  file = caminho_pof,
  col_positions = readr::fwf_positions(
    start = c(3, 5, 8, 12, 14, 25, 40, 49, 116, 121),
    end = c(4, 7, 8, 13, 24, 35, 40, 51, 120, 125),
    col_names = c(
      "uf",
      "sequencial_setor",
      "dv_setor",
      "estrato_pof",
      "peso_inicial",
      "peso_final",
      "sexo_codigo",
      "idade",
      "peso_kg",
      "altura_cm"
    )
  ),
  col_types = readr::cols(.default = readr::col_character()),
  trim_ws = TRUE,
  progress = TRUE
)

pof2002 <- pof2002_bruta |>
  transmute(
    # O setor é a unidade primária de amostragem nesta declaração.
    # UF + setor + dígito tornam seu identificador nacionalmente único.
    upa = paste0(uf, sequencial_setor, dv_setor),

    # O código do estrato geográfico também é combinado com a UF para impedir
    # que códigos iguais de unidades da Federação diferentes sejam confundidos.
    estrato = paste0(uf, "_", estrato_pof),

    # O segundo fator de expansão é o peso final fornecido pelo IBGE.
    peso_amostral = readr::parse_double(peso_final),
    idade = readr::parse_integer(idade),
    sexo = case_when(
      sexo_codigo == "1" ~ "M",
      sexo_codigo == "2" ~ "F",
      TRUE ~ NA_character_
    ),
    peso_kg = readr::parse_double(peso_kg),
    altura_cm = readr::parse_double(altura_cm)
  ) |>
  mutate(
    altura_m = altura_cm / 100,
    imc = peso_kg / (altura_m^2),

    # O arquivo não permite identificar diretamente todas as gestantes.
    # Mantém-se, portanto, a mesma decisão do script de prevalência fornecido.
    antropometria_valida =
      is.finite(peso_kg) & peso_kg > 0 &
      is.finite(altura_cm) & altura_cm > 0 &
      is.finite(imc) & imc >= 15 & imc < 60,

    faixa_idade = cut(
      idade,
      breaks = c(20, 30, 40, 50, 60, 70, 80, Inf),
      labels = faixas_idade,
      right = FALSE,
      ordered_result = TRUE
    ),

    # As seis categorias são usadas somente nos indicadores de impacto.
    # As prevalências descritivas de sobrepeso e obesidade continuam binárias
    # no script próprio de prevalências.
    categoria_imc = case_when(
      antropometria_valida & imc < 18.5 ~ "baixo",
      antropometria_valida & imc < 25.0 ~ "normal",
      antropometria_valida & imc < 30.0 ~ "sobrepeso",
      antropometria_valida & imc < 35.0 ~ "obesidade1",
      antropometria_valida & imc < 40.0 ~ "obesidade2",
      antropometria_valida & imc >= 40.0 ~ "obesidade3",
      TRUE ~ NA_character_
    ),
    sexo = factor(sexo, levels = c("M", "F")),
    categoria_imc = factor(
      categoria_imc,
      levels = categorias_imc,
      ordered = TRUE
    )
  ) |>
  filter(
    is.finite(peso_amostral),
    peso_amostral > 0
  )

#--- 2. Declarar o desenho amostral -------------------------------------------

message("[2/9] Declarar o objeto survey com o peso final da POF")

# A declaração ocorre antes do recorte etário e antropométrico. O recorte é
# aplicado em seguida como domínio, mantendo a estrutura do desenho declarada.
des_pof02 <- survey::svydesign(
  ids = ~upa,
  strata = ~estrato,
  weights = ~peso_amostral,
  data = pof2002,
  nest = TRUE
)

des_pof02 <- subset(
  des_pof02,
  idade >= 20 &
    !is.na(sexo) &
    !is.na(faixa_idade) &
    antropometria_valida &
    !is.na(categoria_imc)
)

celulas_pof <- with(
  des_pof02$variables,
  table(sexo, faixa_idade, categoria_imc)
)

if (any(apply(celulas_pof, c(1, 2), sum) == 0)) {
  stop("A POF possui pelo menos uma célula sexo x idade sem observações válidas.")
}

perfil_amostral <- tibble(
  pesquisa = "POF 2002-2003",
  pessoas_analisadas = nrow(des_pof02$variables),
  estratos = n_distinct(des_pof02$variables$estrato),
  upas = n_distinct(des_pof02$variables$upa),
  estratos_com_upa_solitaria = des_pof02$variables |>
    distinct(estrato, upa) |>
    count(estrato) |>
    summarise(total = sum(n == 1L)) |>
    pull(total),
  graus_de_liberdade = survey::degf(des_pof02),
  peso_final_minimo = min(weights(des_pof02)),
  peso_final_maximo = max(weights(des_pof02))
)

if (perfil_amostral$estratos_com_upa_solitaria > 0) {
  stop(
    "O domínio analisado contém estrato com UPA solitária. ",
    "Revise a declaração antes de construir as réplicas RWYB."
  )
}

readr::write_csv(
  perfil_amostral,
  file.path(pasta_resultados, "00_perfil_amostral.csv")
)

print(perfil_amostral)

#--- 3. Ler os riscos relativos centrais --------------------------------------

message("[3/9] Ler os riscos relativos categoriais centrais")

rr <- readr::read_delim(
  caminho_rr,
  delim = ";",
  locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  transmute(
    categoria_imc = as.character(categoria_imc),
    rr = as.numeric(rr)
  )

if (!setequal(rr$categoria_imc, categorias_imc)) {
  stop(
    "df_RR.csv precisa conter exatamente: ",
    paste(categorias_imc, collapse = ", ")
  )
}

if (any(!is.finite(rr$rr)) || any(rr$rr <= 0)) {
  stop("Todos os riscos relativos centrais precisam ser positivos e finitos.")
}

if (rr$rr[rr$categoria_imc == "normal"] != 1) {
  stop("A categoria 'normal' precisa ser a referência, com RR igual a 1.")
}

rr <- rr |>
  mutate(
    categoria_imc = factor(
      categoria_imc,
      levels = categorias_imc,
      ordered = TRUE
    )
  ) |>
  arrange(categoria_imc)

readr::write_csv(
  rr,
  file.path(pasta_resultados, "01_riscos_relativos_centrais_utilizados.csv")
)

#--- 4. Preparar a tábua de vida de 2003 --------------------------------------

message("[4/9] Agregar a tábua de vida do IBGE em intervalos decenais")

tabua_2003 <- readr::read_delim(
  caminho_tabua,
  delim = ";",
  col_types = readr::cols(.default = readr::col_character()),
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  transmute(
    idade = readr::parse_integer(idade),
    sexo_original = trimws(sexo),
    ano = readr::parse_integer(ano),
    codigo_local = readr::parse_integer(cod),
    sigla = trimws(sigla),
    local = trimws(local),
    lx = readr::parse_number(
      lx,
      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    ),
    ndx = readr::parse_number(
      ndx,
      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    ),
    n_lx = readr::parse_number(
      n_lx,
      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    ),
    ex_publicada = readr::parse_number(
      ex,
      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    )
  ) |>
  filter(
    ano == 2003L,
    codigo_local == 0L,
    sigla == "BR",
    sexo_original %in% c("Homens", "Mulheres"),
    idade >= 20L
  ) |>
  mutate(
    sexo = case_when(
      sexo_original == "Homens" ~ "M",
      sexo_original == "Mulheres" ~ "F"
    ),
    faixa_idade = case_when(
      idade >= 20 & idade < 30 ~ "20-29",
      idade >= 30 & idade < 40 ~ "30-39",
      idade >= 40 & idade < 50 ~ "40-49",
      idade >= 50 & idade < 60 ~ "50-59",
      idade >= 60 & idade < 70 ~ "60-69",
      idade >= 70 & idade < 80 ~ "70-79",
      idade >= 80 ~ "80+"
    ),
    sexo = factor(sexo, levels = c("M", "F")),
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    )
  ) |>
  arrange(sexo, idade)

if (nrow(tabua_2003) == 0) {
  stop("A tábua de vida não contém Brasil, ano 2003, homens e mulheres.")
}

# Para cada intervalo decenal fechado:
#   ndx = lx no início menos lx no início do intervalo seguinte;
#   ndx_publicado_soma = soma dos ndx quinquenais, mantida para auditoria;
#   nLx = soma dos anos-pessoa vividos nos dois intervalos;
#   mx  = ndx / nLx;
#   ax  = [nLx - 10 * (lx - ndx)] / ndx.
# Para 80+, ndx = lx, mx = lx / nLx e qx = 1.

tabua_2003_10anos <- tabua_2003 |>
  group_by(sexo, faixa_idade) |>
  arrange(idade, .by_group = TRUE) |>
  summarise(
    idade_inicial = min(idade),
    lx = first(lx),
    ndx_publicado_soma = sum(ndx),
    n_lx = sum(n_lx),
    .groups = "drop"
  ) |>
  arrange(sexo, faixa_idade) |>
  group_by(sexo) |>
  mutate(
    intervalo = if_else(as.character(faixa_idade) == "80+", Inf, 10),
    lx_intervalo_seguinte = lead(lx),
    # Nos intervalos fechados, ndx é obtido pela diferença entre sobreviventes
    # de duas idades exatas. No intervalo aberto, ndx = lx. Isso garante o
    # fechamento da coorte apesar do arredondamento das colunas publicadas.
    ndx = if_else(
      as.character(faixa_idade) == "80+",
      lx,
      lx - lx_intervalo_seguinte
    ),
    mx = if_else(
      as.character(faixa_idade) == "80+",
      lx / n_lx,
      ndx / n_lx
    ),
    ax = if_else(
      as.character(faixa_idade) == "80+",
      NA_real_,
      (n_lx - 10 * (lx - ndx)) / ndx
    ),
    qx = if_else(
      as.character(faixa_idade) == "80+",
      1,
      (10 * mx) / (1 + (10 - ax) * mx)
    )
  ) |>
  ungroup() |>
  arrange(sexo, faixa_idade)

if (nrow(tabua_2003_10anos) != 2L * length(faixas_idade)) {
  stop("A tábua de vida não produziu as 14 células sexo x faixa etária.")
}

if (any(!is.finite(tabua_2003_10anos$mx)) ||
    any(tabua_2003_10anos$mx <= 0) ||
    any(tabua_2003_10anos$qx < 0 | tabua_2003_10anos$qx > 1)) {
  stop("A agregação da tábua de vida produziu funções biométricas inválidas.")
}

fechamento_coorte <- tabua_2003_10anos |>
  group_by(sexo) |>
  summarise(
    sobreviventes_aos_20 = first(lx),
    obitos_sinteticos_20_mais = sum(ndx),
    diferenca = obitos_sinteticos_20_mais - sobreviventes_aos_20,
    .groups = "drop"
  )

if (any(abs(fechamento_coorte$diferenca) > 1e-8)) {
  stop("A distribuição de óbitos sintéticos não fecha com lx aos 20 anos.")
}

readr::write_csv(
  tabua_2003_10anos,
  file.path(pasta_resultados, "02_tabua_vida_2003_decenal.csv")
)

# Reconstrução explícita da tábua observada dos homens, com radix 100.000.
tabua_homens_observada <- tabua_2003_10anos |>
  filter(sexo == "M") |>
  arrange(faixa_idade)

tabua_homens_observada$lx_reconstruido <- numeric(nrow(tabua_homens_observada))
tabua_homens_observada$lx_reconstruido[1] <- 100000

for (i in 2:nrow(tabua_homens_observada)) {
  tabua_homens_observada$lx_reconstruido[i] <-
    tabua_homens_observada$lx_reconstruido[i - 1] *
    (1 - tabua_homens_observada$qx[i - 1])
}

tabua_homens_observada <- tabua_homens_observada |>
  mutate(
    dx_reconstruido = lx_reconstruido * qx,
    lx_anos_pessoa = if_else(
      as.character(faixa_idade) == "80+",
      lx_reconstruido / mx,
      10 * (lx_reconstruido - dx_reconstruido) + ax * dx_reconstruido
    ),
    tx_reconstruido = rev(cumsum(rev(lx_anos_pessoa))),
    ex_reconstruida = tx_reconstruido / lx_reconstruido
  )

e20_observada_homens <- tabua_homens_observada$ex_reconstruida[1]

# Reconstrução explícita da tábua observada das mulheres.
tabua_mulheres_observada <- tabua_2003_10anos |>
  filter(sexo == "F") |>
  arrange(faixa_idade)

tabua_mulheres_observada$lx_reconstruido <- numeric(nrow(tabua_mulheres_observada))
tabua_mulheres_observada$lx_reconstruido[1] <- 100000

for (i in 2:nrow(tabua_mulheres_observada)) {
  tabua_mulheres_observada$lx_reconstruido[i] <-
    tabua_mulheres_observada$lx_reconstruido[i - 1] *
    (1 - tabua_mulheres_observada$qx[i - 1])
}

tabua_mulheres_observada <- tabua_mulheres_observada |>
  mutate(
    dx_reconstruido = lx_reconstruido * qx,
    lx_anos_pessoa = if_else(
      as.character(faixa_idade) == "80+",
      lx_reconstruido / mx,
      10 * (lx_reconstruido - dx_reconstruido) + ax * dx_reconstruido
    ),
    tx_reconstruido = rev(cumsum(rev(lx_anos_pessoa))),
    ex_reconstruida = tx_reconstruido / lx_reconstruido
  )

e20_observada_mulheres <- tabua_mulheres_observada$ex_reconstruida[1]

#--- 5. Ler os óbitos absolutos do SIM ----------------------------------------

message("[5/9] Ler e validar os óbitos absolutos do SIM em 2003")

# O CSV foi emitido pelo Portal do SIM com três linhas de identificação antes
# do cabeçalho. A codificação original é ISO-8859-1. São lidas somente as 14
# linhas da tabela; as notas bibliográficas ao final não entram como dados.
sim_2003_bruto <- readr::read_delim(
  caminho_sim,
  delim = ";",
  skip = 3,
  n_max = 14,
  locale = readr::locale(encoding = "ISO-8859-1"),
  na = c("-", ""),
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  janitor::clean_names()

sim_2003 <- sim_2003_bruto |>
  filter(
    faixa_etaria %in% c(
      "20 a 29 anos", "30 a 39 anos", "40 a 49 anos",
      "50 a 59 anos", "60 a 69 anos", "70 a 79 anos",
      "80 anos e mais"
    )
  ) |>
  mutate(
    faixa_idade = case_when(
      faixa_etaria == "20 a 29 anos" ~ "20-29",
      faixa_etaria == "30 a 39 anos" ~ "30-39",
      faixa_etaria == "40 a 49 anos" ~ "40-49",
      faixa_etaria == "50 a 59 anos" ~ "50-59",
      faixa_etaria == "60 a 69 anos" ~ "60-69",
      faixa_etaria == "70 a 79 anos" ~ "70-79",
      faixa_etaria == "80 anos e mais" ~ "80+"
    ),
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    ),
    masc = as.numeric(masc),
    fem = as.numeric(fem),
    ign = tidyr::replace_na(as.numeric(ign), 0)
  ) |>
  arrange(faixa_idade)

# A separação por sexo é feita explicitamente. Óbitos com sexo ignorado são
# preservados na auditoria, mas não são redistribuídos entre homens e mulheres.
obitos_2003 <- bind_rows(
  sim_2003 |>
    transmute(sexo = "M", faixa_idade, obitos = masc),
  sim_2003 |>
    transmute(sexo = "F", faixa_idade, obitos = fem)
) |>
  mutate(
    sexo = factor(sexo, levels = c("M", "F")),
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    )
  ) |>
  arrange(sexo, faixa_idade)

if (nrow(obitos_2003) != 2L * length(faixas_idade) ||
    any(!is.finite(obitos_2003$obitos)) ||
    any(obitos_2003$obitos < 0)) {
  stop("A tabela do SIM não produziu as 14 células sexo x faixa etária.")
}

auditoria_sim_2003 <- sim_2003 |>
  summarise(
    ano = 2003L,
    criterio = "Óbitos por ocorrência",
    obitos_masculinos_20_mais = sum(masc),
    obitos_femininos_20_mais = sum(fem),
    obitos_sexo_ignorado_20_mais = sum(ign),
    ignorados_redistribuidos = FALSE,
    fonte = "MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM"
  )

readr::write_csv(
  obitos_2003,
  file.path(pasta_resultados, "03_obitos_absolutos_sim_2003.csv")
)

readr::write_csv(
  auditoria_sim_2003,
  file.path(pasta_resultados, "03b_auditoria_obitos_sim_2003.csv")
)

#--- 6. Distribuição do IMC e estimativas pontuais ----------------------------

message("[6/9] Calcular a distribuição do IMC com os pesos finais")

# Esta tabela não substitui o script de prevalências. Ela documenta as seis
# parcelas da distribuição que entram no cálculo da PAF.
distribuicao_imc_ponto <- des_pof02$variables |>
  mutate(peso_analise = as.numeric(weights(des_pof02))) |>
  group_by(sexo, faixa_idade, categoria_imc) |>
  summarise(total_ponderado = sum(peso_analise), .groups = "drop") |>
  tidyr::complete(
    sexo = factor(c("M", "F"), levels = c("M", "F")),
    faixa_idade = factor(
      faixas_idade,
      levels = faixas_idade,
      ordered = TRUE
    ),
    categoria_imc = factor(
      categorias_imc,
      levels = categorias_imc,
      ordered = TRUE
    ),
    fill = list(total_ponderado = 0)
  ) |>
  group_by(sexo, faixa_idade) |>
  mutate(
    populacao_celula = sum(total_ponderado),
    proporcao = total_ponderado / populacao_celula
  ) |>
  ungroup() |>
  arrange(sexo, faixa_idade, categoria_imc)

if (any(abs(
  distribuicao_imc_ponto |>
    group_by(sexo, faixa_idade) |>
    summarise(soma = sum(proporcao), .groups = "drop") |>
    pull(soma) - 1
) > 1e-10)) {
  stop("As proporções de IMC não somam 1 em todas as células.")
}

readr::write_csv(
  distribuicao_imc_ponto,
  file.path(pasta_resultados, "04_distribuicao_imc_usada_na_paf.csv")
)

#--- 7. Bootstrap Rao-Wu-Yue-Beaumont -----------------------------------------

message(
  "[7/9] Criar ", numero_replicas,
  " réplicas Rao-Wu-Yue-Beaumont e recalcular todos os indicadores"
)

set.seed(semente_bootstrap)

des_pof02_bootstrap <- svrep::as_bootstrap_design(
  design = des_pof02,
  type = "Rao-Wu-Yue-Beaumont",
  replicates = numero_replicas,
  compress = TRUE,
  mse = TRUE
)

# withReplicates fornece, em cada repetição, os pesos de análise apropriados.
# O bloco abaixo é deliberadamente específico para a POF 2002-2003: todas as
# etapas substantivas aparecem na ordem em que são calculadas.

ajuste_bootstrap <- survey::withReplicates(
  design = des_pof02_bootstrap,
  theta = function(pesos, dados) {

    # A. Distribuição das seis categorias de IMC em cada sexo e faixa etária.
    distribuicao_imc <- dados |>
      mutate(peso_bootstrap = as.numeric(pesos)) |>
      group_by(sexo, faixa_idade, categoria_imc) |>
      summarise(
        total_ponderado = sum(peso_bootstrap),
        .groups = "drop"
      ) |>
      tidyr::complete(
        sexo = factor(c("M", "F"), levels = c("M", "F")),
        faixa_idade = factor(
          faixas_idade,
          levels = faixas_idade,
          ordered = TRUE
        ),
        categoria_imc = factor(
          categorias_imc,
          levels = categorias_imc,
          ordered = TRUE
        ),
        fill = list(total_ponderado = 0)
      ) |>
      group_by(sexo, faixa_idade) |>
      mutate(
        total_celula = sum(total_ponderado),
        proporcao = total_ponderado / total_celula
      ) |>
      ungroup() |>
      left_join(rr, by = "categoria_imc") |>
      arrange(sexo, faixa_idade, categoria_imc)

    # B. PAF por sexo e faixa etária.
    #
    # Risco médio observado: soma(p_k * RR_k).
    # Risco médio contrafactual: o baixo peso é preservado; todas as pessoas
    # com IMC >= 25 são transferidas para a categoria normal (RR = 1).
    # PAF = 1 - risco médio contrafactual / risco médio observado.
    paf_idade <- distribuicao_imc |>
      group_by(sexo, faixa_idade) |>
      summarise(
        risco_medio_observado = sum(proporcao * rr),
        proporcao_baixo_peso = sum(
          proporcao[as.character(categoria_imc) == "baixo"]
        ),
        rr_baixo_peso = rr[as.character(categoria_imc) == "baixo"][1],
        rr_normal = rr[as.character(categoria_imc) == "normal"][1],
        risco_medio_contrafactual =
          proporcao_baixo_peso * rr_baixo_peso +
          (1 - proporcao_baixo_peso) * rr_normal,
        paf = 1 - risco_medio_contrafactual / risco_medio_observado,
        .groups = "drop"
      ) |>
      mutate(paf = pmin(pmax(paf, 0), 1)) |>
      arrange(sexo, faixa_idade)

    # C. PAF sintética ponderada pelos ndx da tábua de vida do IBGE.
    paf_sintetica <- paf_idade |>
      left_join(
        tabua_2003_10anos |>
          select(sexo, faixa_idade, ndx),
        by = c("sexo", "faixa_idade")
      ) |>
      group_by(sexo) |>
      summarise(
        paf = sum(paf * ndx) / sum(ndx),
        .groups = "drop"
      ) |>
      arrange(sexo)

    # D. Mortes evitáveis absolutas.
    # Cada número absoluto do SIM é multiplicado pela PAF da mesma célula.
    mortes_evitaveis_idade <- paf_idade |>
      select(sexo, faixa_idade, paf) |>
      left_join(obitos_2003, by = c("sexo", "faixa_idade")) |>
      mutate(mortes_evitaveis = paf * obitos) |>
      arrange(sexo, faixa_idade)

    mortes_evitaveis_sexo <- mortes_evitaveis_idade |>
      group_by(sexo) |>
      summarise(
        mortes_evitaveis = sum(mortes_evitaveis),
        .groups = "drop"
      ) |>
      arrange(sexo)

    # E. Tábua contrafactual dos homens.
    # A PAF específica reduz mx em cada idade: mx* = mx * (1 - PAF).
    tabua_homens_cf <- tabua_2003_10anos |>
      filter(sexo == "M") |>
      left_join(
        paf_idade |>
          filter(sexo == "M") |>
          select(faixa_idade, paf),
        by = "faixa_idade"
      ) |>
      arrange(faixa_idade) |>
      mutate(
        mx_contrafactual = mx * (1 - paf),
        qx_contrafactual = if_else(
          as.character(faixa_idade) == "80+",
          1,
          (10 * mx_contrafactual) /
            (1 + (10 - ax) * mx_contrafactual)
        )
      )

    tabua_homens_cf$lx_contrafactual <- numeric(nrow(tabua_homens_cf))
    tabua_homens_cf$lx_contrafactual[1] <- 100000

    for (i in 2:nrow(tabua_homens_cf)) {
      tabua_homens_cf$lx_contrafactual[i] <-
        tabua_homens_cf$lx_contrafactual[i - 1] *
        (1 - tabua_homens_cf$qx_contrafactual[i - 1])
    }

    tabua_homens_cf <- tabua_homens_cf |>
      mutate(
        dx_contrafactual = lx_contrafactual * qx_contrafactual,
        lx_anos_pessoa_contrafactual = if_else(
          as.character(faixa_idade) == "80+",
          lx_contrafactual / mx_contrafactual,
          10 * (lx_contrafactual - dx_contrafactual) +
            ax * dx_contrafactual
        ),
        tx_contrafactual = rev(cumsum(rev(lx_anos_pessoa_contrafactual))),
        ex_contrafactual = tx_contrafactual / lx_contrafactual
      )

    e20_contrafactual_homens <- tabua_homens_cf$ex_contrafactual[1]
    pgev20_homens <- e20_contrafactual_homens - e20_observada_homens

    # F. Tábua contrafactual das mulheres.
    tabua_mulheres_cf <- tabua_2003_10anos |>
      filter(sexo == "F") |>
      left_join(
        paf_idade |>
          filter(sexo == "F") |>
          select(faixa_idade, paf),
        by = "faixa_idade"
      ) |>
      arrange(faixa_idade) |>
      mutate(
        mx_contrafactual = mx * (1 - paf),
        qx_contrafactual = if_else(
          as.character(faixa_idade) == "80+",
          1,
          (10 * mx_contrafactual) /
            (1 + (10 - ax) * mx_contrafactual)
        )
      )

    tabua_mulheres_cf$lx_contrafactual <- numeric(nrow(tabua_mulheres_cf))
    tabua_mulheres_cf$lx_contrafactual[1] <- 100000

    for (i in 2:nrow(tabua_mulheres_cf)) {
      tabua_mulheres_cf$lx_contrafactual[i] <-
        tabua_mulheres_cf$lx_contrafactual[i - 1] *
        (1 - tabua_mulheres_cf$qx_contrafactual[i - 1])
    }

    tabua_mulheres_cf <- tabua_mulheres_cf |>
      mutate(
        dx_contrafactual = lx_contrafactual * qx_contrafactual,
        lx_anos_pessoa_contrafactual = if_else(
          as.character(faixa_idade) == "80+",
          lx_contrafactual / mx_contrafactual,
          10 * (lx_contrafactual - dx_contrafactual) +
            ax * dx_contrafactual
        ),
        tx_contrafactual = rev(cumsum(rev(lx_anos_pessoa_contrafactual))),
        ex_contrafactual = tx_contrafactual / lx_contrafactual
      )

    e20_contrafactual_mulheres <- tabua_mulheres_cf$ex_contrafactual[1]
    pgev20_mulheres <- e20_contrafactual_mulheres - e20_observada_mulheres

    # G. Vetor de resultados. A ordem e os nomes permanecem idênticos em todas
    # as réplicas, o que permite formar os intervalos percentis depois.
    resultados <- c(
      stats::setNames(
        paf_idade$paf,
        paste(
          "paf_idade",
          as.character(paf_idade$sexo),
          as.character(paf_idade$faixa_idade),
          sep = "|"
        )
      ),
      stats::setNames(
        paf_sintetica$paf,
        paste(
          "paf_sintetica",
          as.character(paf_sintetica$sexo),
          "20+",
          sep = "|"
        )
      ),
      stats::setNames(
        mortes_evitaveis_idade$mortes_evitaveis,
        paste(
          "mortes_evitaveis_idade",
          as.character(mortes_evitaveis_idade$sexo),
          as.character(mortes_evitaveis_idade$faixa_idade),
          sep = "|"
        )
      ),
      stats::setNames(
        mortes_evitaveis_sexo$mortes_evitaveis,
        paste(
          "mortes_evitaveis_total",
          as.character(mortes_evitaveis_sexo$sexo),
          "20+",
          sep = "|"
        )
      ),
      "pgev20|M|20" = pgev20_homens,
      "pgev20|F|20" = pgev20_mulheres
    )

    if (any(!is.finite(resultados))) {
      stop("Uma réplica produziu indicador não finito.")
    }

    resultados
  },
  return.replicates = TRUE
)

estimativas_pontuais <- as.numeric(ajuste_bootstrap$theta)
names(estimativas_pontuais) <- names(ajuste_bootstrap$theta)

replicas_indicadores <- as.matrix(ajuste_bootstrap$replicates)

if (ncol(replicas_indicadores) != length(estimativas_pontuais) &&
    nrow(replicas_indicadores) == length(estimativas_pontuais)) {
  replicas_indicadores <- t(replicas_indicadores)
}

if (ncol(replicas_indicadores) != length(estimativas_pontuais)) {
  stop("A matriz de resultados bootstrap possui dimensão inesperada.")
}

colnames(replicas_indicadores) <- names(estimativas_pontuais)

limites_inferiores <- apply(
  replicas_indicadores,
  2,
  quantile,
  probs = 0.025,
  na.rm = TRUE,
  names = FALSE,
  type = 8
)

limites_superiores <- apply(
  replicas_indicadores,
  2,
  quantile,
  probs = 0.975,
  na.rm = TRUE,
  names = FALSE,
  type = 8
)

# withReplicates armazena a matriz de variância no atributo "var" de theta.
# Nesta forma de retorno, theta é um vetor numérico comum; por isso, não se deve
# chamar SE(theta), pois SE tentaria aplicar vcov() diretamente ao vetor.
matriz_variancia_bootstrap <- attr(ajuste_bootstrap$theta, "var")

if (is.null(matriz_variancia_bootstrap)) {
  stop("withReplicates não devolveu a matriz de variância esperada.")
}

matriz_variancia_bootstrap <- as.matrix(matriz_variancia_bootstrap)

if (nrow(matriz_variancia_bootstrap) != length(estimativas_pontuais) ||
    ncol(matriz_variancia_bootstrap) != length(estimativas_pontuais)) {
  stop("A matriz de variância bootstrap possui dimensão inesperada.")
}

# O erro-padrão respeita os fatores de escala do desenho replicado. Os limites
# do IC continuam sendo os percentis empíricos das 1.000 réplicas.
erros_padrao_bootstrap <- sqrt(
  pmax(diag(matriz_variancia_bootstrap), 0)
)

replicas_validas <- colSums(is.finite(replicas_indicadores))

resultados <- tibble(
  chave = names(estimativas_pontuais),
  estimativa = estimativas_pontuais,
  erro_padrao_bootstrap = as.numeric(erros_padrao_bootstrap),
  ic_inferior = as.numeric(limites_inferiores),
  ic_superior = as.numeric(limites_superiores),
  replicas_validas = as.integer(replicas_validas)
) |>
  tidyr::separate_wider_delim(
    chave,
    delim = "|",
    names = c("indicador", "sexo", "faixa_idade")
  ) |>
  mutate(
    pesquisa = "POF 2002-2003",
    ano_mortalidade = 2003L,
    cenario = "Eliminação do IMC >= 25 kg/m²",
    metodo_bootstrap = "Rao-Wu-Yue-Beaumont",
    replicas_solicitadas = numero_replicas,
    metodo_ic = "Percentis 2,5 e 97,5 das réplicas",
    nivel_ic = 0.95,
    coeficiente_variacao_pct = if_else(
      estimativa == 0,
      NA_real_,
      100 * erro_padrao_bootstrap / abs(estimativa)
    ),
    sexo_rotulo = recode(sexo, M = "Homens", F = "Mulheres"),
    faixa_idade = factor(
      faixa_idade,
      levels = c(faixas_idade, "20+", "20"),
      ordered = TRUE
    ),
    .before = 1
  ) |>
  arrange(indicador, sexo, faixa_idade)

if (any(resultados$replicas_validas < 0.95 * numero_replicas)) {
  stop("Algum indicador possui menos de 95% das réplicas válidas.")
}

#--- 8. Separar e salvar os resultados ----------------------------------------

message("[8/9] Salvar os resultados em CSV e as réplicas em RDS")

paf_idade <- resultados |>
  filter(indicador == "paf_idade") |>
  mutate(
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior
  )

paf_sintetica <- resultados |>
  filter(indicador == "paf_sintetica") |>
  mutate(
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    ponderacao = "ndx da tábua de vida do IBGE de 2003"
  )

mortes_evitaveis_idade <- resultados |>
  filter(indicador == "mortes_evitaveis_idade") |>
  mutate(
    unidade = "Número absoluto de mortes",
    base_mortalidade = "SIM 2003 - óbitos por ocorrência"
  )

mortes_evitaveis_total <- resultados |>
  filter(indicador == "mortes_evitaveis_total") |>
  mutate(
    unidade = "Número absoluto de mortes",
    base_mortalidade = "SIM 2003 - óbitos por ocorrência"
  )

pgev20 <- resultados |>
  filter(indicador == "pgev20") |>
  mutate(
    e20_observada = case_when(
      sexo == "M" ~ e20_observada_homens,
      sexo == "F" ~ e20_observada_mulheres
    ),
    e20_contrafactual = e20_observada + estimativa,
    e20_contrafactual_ic_inferior = e20_observada + ic_inferior,
    e20_contrafactual_ic_superior = e20_observada + ic_superior
  )

readr::write_csv(
  paf_idade,
  file.path(pasta_resultados, "05_paf_por_sexo_e_idade.csv")
)

readr::write_csv(
  paf_sintetica,
  file.path(pasta_resultados, "06_paf_sintetica_por_sexo.csv")
)

readr::write_csv(
  mortes_evitaveis_idade,
  file.path(pasta_resultados, "07_mortes_evitaveis_por_sexo_e_idade.csv")
)

readr::write_csv(
  mortes_evitaveis_total,
  file.path(pasta_resultados, "08_mortes_evitaveis_por_sexo.csv")
)

readr::write_csv(
  pgev20,
  file.path(pasta_resultados, "09_pgev20_por_sexo.csv")
)

auditoria_bootstrap <- resultados |>
  summarise(
    pesquisa = first(pesquisa),
    metodo = first(metodo_bootstrap),
    semente = semente_bootstrap,
    replicas_solicitadas = numero_replicas,
    menor_numero_replicas_validas = min(replicas_validas),
    maior_numero_replicas_validas = max(replicas_validas),
    numero_indicadores = n(),
    rr_sorteado = FALSE,
    mortalidade_sorteada = FALSE,
    base_mortalidade_absoluta = "SIM 2003 - óbitos por ocorrência",
    base_mortalidade_pgev = "Tábua de Vida do IBGE de 2003",
    unidade_obitos = "Número absoluto de mortes",
    comparacao_linearizacao = FALSE,
    intervalo = "Percentis 2,5 e 97,5"
  )

readr::write_csv(
  auditoria_bootstrap,
  file.path(pasta_resultados, "10_auditoria_bootstrap.csv")
)

saveRDS(
  list(
    estimativas_pontuais = estimativas_pontuais,
    replicas = replicas_indicadores,
    metodo = "Rao-Wu-Yue-Beaumont",
    semente = semente_bootstrap
  ),
  file.path(pasta_resultados, "11_replicas_dos_indicadores.rds"),
  compress = "xz"
)

#--- 9. Gráficos com apresentação acadêmica -----------------------------------

message("[9/9] Criar e salvar os gráficos")

cores_sexo <- c("M" = "#1F4E79", "F" = "#8A3B5C")

tema_abnt <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 8)),
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
    axis.title = element_text(size = 10),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 12, 10, 10)
  )

grafico_paf_idade <- ggplot(
  paf_idade,
  aes(
    x = faixa_idade,
    y = estimativa_pct,
    color = sexo,
    group = sexo
  )
) +
  geom_errorbar(
    aes(ymin = ic_inferior_pct, ymax = ic_superior_pct),
    width = 0.14,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_color_manual(
    values = cores_sexo,
    breaks = c("M", "F"),
    labels = c("Homens", "Mulheres")
  ) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", suffix = "%", accuracy = 0.1)
  ) +
  labs(
    title = "Fração atribuível populacional por idade e sexo",
    subtitle = paste0(
      "POF 2002-2003; contrafactual de eliminação do IMC >= 25 kg/m²; ",
      "IC 95% bootstrap"
    ),
    x = "Grupo etário",
    y = "Fração atribuível populacional",
    color = "Sexo",
    caption = paste0(
      "Fonte: IBGE, POF 2002-2003; Global BMI Mortality Collaboration (2016), ",
      "conforme riscos relativos informados em df_RR.csv. ",
      "Elaboração própria."
    )
  ) +
  tema_abnt

grafico_paf_sintetica <- ggplot(
  paf_sintetica,
  aes(x = sexo_rotulo, y = estimativa_pct, fill = sexo)
) +
  geom_col(width = 0.58) +
  geom_errorbar(
    aes(ymin = ic_inferior_pct, ymax = ic_superior_pct),
    width = 0.12,
    linewidth = 0.6
  ) +
  scale_fill_manual(values = cores_sexo, guide = "none") +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", suffix = "%", accuracy = 0.1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Fração atribuível populacional sintética por sexo",
    subtitle = "Ponderação pelos óbitos da tábua de vida do IBGE de 2003; IC 95% bootstrap",
    x = NULL,
    y = "Fração atribuível populacional",
    caption = paste0(
      "Fonte: IBGE, POF 2002-2003 e Tábua de Vida 2003; ",
      "Global BMI Mortality Collaboration (2016), conforme riscos relativos ",
      "informados em df_RR.csv. Elaboração própria."
    )
  ) +
  tema_abnt

grafico_mortes_idade <- ggplot(
  mortes_evitaveis_idade,
  aes(
    x = faixa_idade,
    y = estimativa,
    color = sexo,
    group = sexo
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(ymin = ic_inferior, ymax = ic_superior),
    width = 0.16,
    linewidth = 0.5
  ) +
  scale_color_manual(
    values = cores_sexo,
    breaks = c("M", "F"),
    labels = c("Homens", "Mulheres")
  ) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 1)
  ) +
  labs(
    title = "Número absoluto de mortes evitáveis por idade e sexo",
    subtitle = paste0(
      "Brasil, 2003; contrafactual de eliminação do IMC >= 25 kg/m²; ",
      "IC 95% bootstrap"
    ),
    x = "Grupo etário",
    y = "Número de mortes evitáveis",
    color = "Sexo",
    caption = paste0(
      "Fonte: IBGE, POF 2002-2003; MS/SVSA/CGIAE, SIM 2003, ",
      "óbitos por ocorrência; ",
      "Global BMI Mortality Collaboration (2016), conforme riscos relativos ",
      "informados em df_RR.csv. Elaboração própria."
    )
  ) +
  tema_abnt

grafico_mortes_total <- ggplot(
  mortes_evitaveis_total,
  aes(x = sexo_rotulo, y = estimativa, fill = sexo)
) +
  geom_col(width = 0.58) +
  geom_errorbar(
    aes(ymin = ic_inferior, ymax = ic_superior),
    width = 0.12,
    linewidth = 0.6
  ) +
  scale_fill_manual(values = cores_sexo, guide = "none") +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Número absoluto de mortes evitáveis por sexo",
    subtitle = paste0(
      "Brasil, população de 20 anos ou mais, 2003; ",
      "IC 95% bootstrap"
    ),
    x = NULL,
    y = "Número de mortes evitáveis",
    caption = paste0(
      "Fonte: IBGE, POF 2002-2003; MS/SVSA/CGIAE, SIM 2003, ",
      "óbitos por ocorrência; ",
      "Global BMI Mortality Collaboration (2016), conforme riscos relativos ",
      "informados em df_RR.csv. Elaboração própria."
    )
  ) +
  tema_abnt

grafico_pgev20 <- ggplot(
  pgev20,
  aes(x = sexo_rotulo, y = estimativa, fill = sexo)
) +
  geom_col(width = 0.58) +
  geom_errorbar(
    aes(ymin = ic_inferior, ymax = ic_superior),
    width = 0.12,
    linewidth = 0.6
  ) +
  scale_fill_manual(values = cores_sexo, guide = "none") +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", accuracy = 0.01),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Potencial ganho em expectativa de vida aos 20 anos",
    subtitle = paste0(
      "POF 2002-2003 e Tábua de Vida 2003; eliminação do IMC >= 25 kg/m²; ",
      "IC 95% bootstrap"
    ),
    x = NULL,
    y = "Ganho em anos de vida",
    caption = paste0(
      "Fonte: IBGE, POF 2002-2003 e Tábua de Vida 2003; ",
      "Global BMI Mortality Collaboration (2016), conforme riscos relativos ",
      "informados em df_RR.csv. Elaboração própria."
    )
  ) +
  tema_abnt

ggsave(
  file.path(pasta_graficos, "01_paf_por_idade_e_sexo.png"),
  grafico_paf_idade,
  width = 8.27,
  height = 5.6,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "01_paf_por_idade_e_sexo.pdf"),
  grafico_paf_idade,
  width = 8.27,
  height = 5.6,
  units = "in"
)

ggsave(
  file.path(pasta_graficos, "02_paf_sintetica_por_sexo.png"),
  grafico_paf_sintetica,
  width = 7.1,
  height = 5.2,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "02_paf_sintetica_por_sexo.pdf"),
  grafico_paf_sintetica,
  width = 7.1,
  height = 5.2,
  units = "in"
)

ggsave(
  file.path(pasta_graficos, "03_mortes_evitaveis_por_idade_e_sexo.png"),
  grafico_mortes_idade,
  width = 8.27,
  height = 5.6,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "03_mortes_evitaveis_por_idade_e_sexo.pdf"),
  grafico_mortes_idade,
  width = 8.27,
  height = 5.6,
  units = "in"
)

ggsave(
  file.path(pasta_graficos, "04_mortes_evitaveis_por_sexo.png"),
  grafico_mortes_total,
  width = 7.1,
  height = 5.2,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "04_mortes_evitaveis_por_sexo.pdf"),
  grafico_mortes_total,
  width = 7.1,
  height = 5.2,
  units = "in"
)

ggsave(
  file.path(pasta_graficos, "05_pgev20_por_sexo.png"),
  grafico_pgev20,
  width = 7.1,
  height = 5.2,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  file.path(pasta_graficos, "05_pgev20_por_sexo.pdf"),
  grafico_pgev20,
  width = 7.1,
  height = 5.2,
  units = "in"
)

message(
  "\nAnálise concluída.",
  "\nResultados: ", pasta_resultados,
  "\nGráficos: ", pasta_graficos
)


rm(list = ls(all = TRUE))

# ----------------------  Carregando bibliotecas --------------------
library(tidyverse)
library(readxl)
library(janitor)

# ----------------------  Setar diretório base --------------------

base_path <- "D:/TCC/tcc_demografia_2026/2a_versao"

# ----------------------  Funções de Suporte (Construção de Tabua de Vida) --------------------

build_life_table <- function(df, qvar){
  df <- df %>% arrange(faixa_idade) %>% mutate(qx = .data[[qvar]])
  lx <- numeric(nrow(df))
  lx[1] <- 100000
  for(i in 2:nrow(df)){ lx[i] <- lx[i-1] * (1 - df$qx[i-1]) }
  df$lx <- lx
  df
}

calc_life_metrics <- function(df, mx_col){
  df <- df %>%
    mutate(
      ndx_calc = lx * qx,
      Lx = ifelse(faixa_idade == "80+", lx / .data[[mx_col]], 5 * lx - 2.5 * ndx_calc)
    ) %>%
    arrange(desc(faixa_idade)) %>%
    mutate(Tx = cumsum(Lx), ex = Tx / lx) %>%
    arrange(faixa_idade)
  df
}


# ----------------------  Carregando dados --------------------
pns_2019 <- read.csv(paste0(base_path, "/dados_amostrais_tratados/grupos_imc_por_sexo_e_regiao_2019_2.0.csv")) %>%
  select(-X, -ci_l, -ci_u)

ibge_lt <- read.csv2(paste0(base_path, "/dados_populacionais/tabuas_de_vida.csv")) %>%
  clean_names()

df_rr <- read.csv2(paste0(base_path, "/dados_populacionais/df_RR.csv")) %>%
  select(categoria_imc, RR)

# ----------------------  Preparando Tabua de Vida --------------------

lt_2019 <- ibge_lt %>%
  filter(local == 'Brasil', 
         ano == "2019", 
         idade >= 20) 

lt_2019 <- lt_2019 %>%
  mutate(
    faixa_idade = case_when(
      idade < 25  ~ "20-24",
      idade < 30  ~ "25-29",
      idade < 35  ~ "30-34",
      idade < 40  ~ "35-39",
      idade < 45  ~ "40-44",
      idade < 50  ~ "45-49",
      idade < 55  ~ "50-54",
      idade < 60  ~ "55-59",
      idade < 65  ~ "60-64",
      idade < 70  ~ "65-69",
      idade < 75  ~ "70-74",
      idade < 80  ~ "75-79",
      idade < 85  ~ "80-84",
      idade < 90  ~ "85-89",
      idade >= 90 ~ "90+"
    )
  ) 

df_lt_2019 <- lt_2019 %>%
  mutate(
    n_mx = as.numeric(n_mx),
    lx   = as.numeric(lx)
  ) %>%
  group_by(sexo, faixa_idade) %>%
  summarise(
    mx = weighted.mean(n_mx, lx), 
    ndx = sum(as.numeric(ndx)),
    .groups = "drop"
  ) %>%
  filter(sexo != "Ambos") %>%
  mutate(
    sexo = case_when(
      sexo == "Mulheres" ~ "F",
      sexo == "Homens"   ~ "M"
    )
  )

# ----------------------  Join e Rebalanceamento de Cenários --------------------

base_modelo <- pns_2019 %>%
  filter(!faixa_idade %in% c("15-19", "90+")) %>%
  left_join(df_lt_2019, by = c("sexo", "faixa_idade")) %>%
  left_join(df_rr, by = "categoria_imc") %>%
  group_by(sexo, faixa_idade) %>%
  ungroup() %>%
  mutate(
    categoria_imc = factor(categoria_imc, 
                           levels = c("baixo", "normal", "sobrepeso", "obesidade1", "obesidade2", "obesidade3"),
                           ordered = TRUE)
  ) %>%
  arrange(sexo, faixa_idade, categoria_imc)

# ----------------------  Calculando Hazard Ratio por grupo de IMC  ------------

base_modelo <- base_modelo %>%
  group_by(sexo, faixa_idade) %>%
  mutate(
    D = sum(prop * RR),     # denominador
    m0 = mx / D,            # risco basal
    mx_imc = m0 * RR        # risco por categoria
  ) %>%
  ungroup()

# ----------------------  Ajuste das proporções por grupo (Cenário Contrafactual)  ------------

base_modelo <- base_modelo %>%
  group_by(sexo, faixa_idade) %>%
  mutate(
    prop_cf = case_when(
      categoria_imc == "baixo" ~ prop,  # keep 'baixo' as is
      categoria_imc == "normal" ~ prop + sum(prop[categoria_imc %in% c("sobrepeso", "obesidade1", "obesidade2", "obesidade3")]), 
      TRUE ~ 0  # set others to 0
    )
  ) %>%
  ungroup()

# ----------------------  Calculando PAF  --------------

paf_base <- base_modelo %>%
  group_by(sexo, faixa_idade) %>%
  summarise(
    paf = sum((prop * mx_imc)-(prop_cf * mx_imc)) / sum(prop * mx_imc),
    .groups = "drop"
  )

paf_sint <- paf_base %>%
  left_join(
    df_lt_2019 %>% select(sexo, faixa_idade, ndx),
    by = c("sexo", "faixa_idade")
  ) %>%
  group_by(sexo) %>%
  summarise(
    paf_sint = sum(paf * ndx) / sum(ndx),
    .groups = "drop"
  )

# ----------------------  Cenário Contrafactual  --------------

cf_df <- df_lt_2019 %>%
  left_join(paf_base, by = c("sexo", "faixa_idade")) %>%
  group_by(sexo, faixa_idade)%>%
  summarise(
    mx_ideal = mx*(1-paf),
    .groups = "drop"
  )


lt_cf <- df_lt_2019 %>%
  left_join(cf_df, by = c("sexo", "faixa_idade")) %>% 
  select(-ndx)

lt_cf <- lt_cf %>%
  mutate(mx_ideal = case_when(faixa_idade == "90+" ~ mx,
                              TRUE ~ mx_ideal))

lt_cf <- lt_cf %>%
  mutate(
    n = ifelse(faixa_idade == "90+", NA_real_, 5),
    ax = ifelse(faixa_idade == "90+", NA, 2.5),
    nqx = (n * mx) / (1 + (n - ax) * mx),
    nqx_ideal = (n * mx_ideal) / (1 + (n - ax) * mx_ideal),
    nqx = ifelse(faixa_idade == "90+", 1, nqx),
    nqx_ideal = ifelse(faixa_idade == "90+", 1, nqx_ideal)
  )

# Função lx
build_life_table <- function(df, qvar){
  df <- df %>%
    arrange(faixa_idade) %>%
    mutate(qx = .data[[qvar]])
  lx <- numeric(nrow(df))
  lx[1] <- 100000
  for(i in 2:nrow(df)){
    lx[i] <- lx[i-1] * (1 - df$qx[i-1])
  }
  df$lx <- lx
  df
}

# CORREÇÃO DA FUNÇÃO: Adicionado o parâmetro mx_col
calc_life_metrics <- function(df, mx_col){
  df <- df %>%
    mutate(
      ndx = lx * qx,
      Lx = ifelse(
        faixa_idade == "80+",
        lx / .data[[mx_col]], # CORREÇÃO: Usa a coluna correta de mx do cenário
        5 * lx - 2.5 * ndx
      )
    ) %>%
    arrange(desc(faixa_idade)) %>%
    mutate(
      Tx = cumsum(Lx),
      ex = Tx / lx
    ) %>%
    arrange(faixa_idade)
  df
}

# Aplicando funções construindo lx
lt_obs <- lt_cf %>%
  group_by(sexo) %>%
  group_modify(~ build_life_table(.x, "nqx"))

lt_ideal <- lt_cf %>%
  group_by(sexo) %>%
  group_modify(~ build_life_table(.x, "nqx_ideal"))

#  Passando o nome da coluna de mx respectiva
lt_obs <- lt_obs %>% 
  group_by(sexo) %>% 
  group_modify(~ calc_life_metrics(.x, "mx"))

lt_ideal <- lt_ideal %>% 
  group_by(sexo) %>% 
  group_modify(~ calc_life_metrics(.x, "mx_ideal"))

# Ganho em expectativa de vida
ev_gain <- lt_obs %>%
  filter(faixa_idade == "20-24") %>%
  select(sexo, e_obs = ex) %>%
  left_join(
    lt_ideal %>%
      filter(faixa_idade == "20-24") %>%
      select(sexo, e_ideal = ex),
    by = c("sexo")
  ) %>%
  mutate(ganho = e_ideal - e_obs)

#--------------- Número Absoluto de Mortes Evitaveis -------------

obitos_2019 <- read_csv(paste0(base_path, "/dados_populacionais/obitos_por_sexo_2019.csv"))

df_obitos_2019 <- obitos_2019 %>%
  mutate(SEXO = case_when(SEXO == 1 ~ 'M',
                          SEXO == 2 ~ 'F',
                          TRUE ~ 'OUTRO')) %>%
  filter(SEXO != 'OUTRO', AgeGroup != 'Outros') %>%
  rename(sexo = SEXO,
         faixa_idade = AgeGroup,
         obitos = Deaths)

mortes_evitaveis <- df_obitos_2019 %>%
  left_join(paf_base, by = c("sexo", "faixa_idade")) %>%
  mutate(
    mortes_evitaveis = obitos*paf)


resumo_sexo <- mortes_evitaveis %>%
  group_by(sexo) %>%
  summarise(
    total_mortes = sum(obitos, na.rm = TRUE),
    total_evitaveis = sum(mortes_evitaveis, na.rm = TRUE)
  )

# Variáveis por sexo
mortes_M <- resumo_sexo$total_mortes[resumo_sexo$sexo == "M"]
mortes_F <- resumo_sexo$total_mortes[resumo_sexo$sexo == "F"]

evitaveis_M <- resumo_sexo$total_evitaveis[resumo_sexo$sexo == "M"]
evitaveis_F <- resumo_sexo$total_evitaveis[resumo_sexo$sexo == "F"]

percent_evitaveis <- mortes_evitaveis %>%
  group_by(sexo) %>%
  summarise(
    pct_evitaveis_M = evitaveis_M/ mortes_M,
    pct_evitaveis_F = evitaveis_F/ mortes_F,
    .groups = "drop"
  )

#--------------- Salvar Resultados -------------

# Exportação
write.csv(paf_base, paste0(base_path, "/analise_contrafactual/output/resultados/paf_2019.csv"))
write.csv(paf_sint, paste0(base_path, "/analise_contrafactual/output/resultados/paf_sint_2019.csv"))
write.csv(ev_gain, paste0(base_path, "/analise_contrafactual/output/resultados/e_gain_2019.csv"))
write.csv(mortes_evitaveis, paste0(base_path, "/analise_contrafactual/output/resultados/mortes_evitaveis_2019.csv"))



#=============================================
# APLICAÇÃO DE MÉTODO DE KITAGAWA PARA TAXAS ESPECIFICAS DE MORTALIDADE  
# CONSIDERANDO GRUPOS DE IMC
#
# O objetivo dessa análise é medir/buscar medir como a mudança na composição
# da população por grupos de IMC impactou na evolução das TEM entre 2013 e 2019

rm(list = ls(all = TRUE))

# ----------------------  Carregando bibliotecas --------------------
library(tidyverse)
library(readxl)
library(janitor)
library(writexl) 

# ----------------------  Carregando dados --------------------

base_path <- "D:/TCC/tcc_demografia_2026/2a_versao"

pns_2013 <- read.csv(paste0(base_path, "/dados_amostrais_tratados/grupos_imc_por_sexo_e_regiao_2013_2.0.csv")) %>%
  select(-X) %>% mutate(ano = as.character("2013")) %>%
  select(-ci_l, -ci_u)

pns_2019 <- read.csv(paste0(base_path, "/dados_amostrais_tratados/grupos_imc_por_sexo_e_regiao_2019_2.0.csv")) %>%
  select(-X) %>% mutate(ano = as.character("2019"))%>%
  select(-ci_l, -ci_u)

ibge_lt <- read.csv2(paste0(base_path, "/dados_populacionais/tabuas_de_vida.csv")) %>%
  clean_names() %>% mutate(ano = as.character(ano))

df_rr <- read.csv2(paste0(base_path, "/dados_populacionais/df_RR.csv")) %>%
  select(categoria_imc, RR)

pns_data <- bind_rows(pns_2013, pns_2019)

# ----------------------  Preparando Tabua de Vida --------------------

ibge_lt <- ibge_lt %>%
  filter(local == 'Brasil', 
         ano %in% c("2013", "2019"),
         idade >= 20) 

ibge_lt <- ibge_lt %>%
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

ibge_lt <- ibge_lt %>%
  mutate(
    n_mx = as.numeric(n_mx),
    lx   = as.numeric(lx)
  ) %>%
  group_by(ano,sexo, faixa_idade) %>%
  summarise(
    mx = weighted.mean(n_mx, lx), 
 #   ndx = sum(as.numeric(ndx)),
    .groups = "drop"
  ) %>%
  filter(sexo != "Ambos") %>%
  mutate(
    sexo = case_when(
      sexo == "Mulheres" ~ "F",
      sexo == "Homens"   ~ "M"
    )
  ) 

# ----- Preparando modelo (ano|regiao|sexo|idade|grupo_imc|prop|mx_imc)  -------


base_modelo <- pns_data %>%
  left_join(
    ibge_lt,
    by = c("sexo", "faixa_idade", "ano")
  ) %>%
  left_join(df_rr, by = "categoria_imc") %>%
  mutate(
    categoria_imc = factor(
      categoria_imc,
      levels = c("baixo", "normal", "sobrepeso", "obesidade1", "obesidade2", "obesidade3"),
      ordered = TRUE
    )
  ) %>%
  arrange(sexo, faixa_idade, categoria_imc) %>%
  filter(!faixa_idade %in% c("15-19", "90+")) 
  


base_modelo <- base_modelo %>%
  group_by(ano, sexo, faixa_idade, mx) %>%
  mutate(
    D = sum(prop * RR),     # denominador
    m0 = mx / D * 100000,            # risco basal
    mx_imc = m0 * RR      # risco por categoria
  ) %>%
  ungroup() %>%
  select(ano, sexo, faixa_idade, categoria_imc, prop, mx, mx_imc) %>%
  arrange(ano, sexo, faixa_idade) 


glimpse(base_modelo)


# -------------------------------------------------------------------------
# MÉTODO DE KITAGAWA: FOCO NO GANHO (2013 - 2019)
# -------------------------------------------------------------------------

options(scipen = 999) # Desativa notação científica

# 1. Preparação dos dados Wide
kitagawa_wide <- base_modelo %>%
  pivot_wider(
    names_from = ano, 
    values_from = c(prop, mx, mx_imc),
    names_sep = "_"
  )

# 2. Decomposição Padronizada 
kitagawa_final <- kitagawa_wide %>%
  group_by(sexo, faixa_idade) %>%
  summarise(

    tem_2013 = first(mx_2013) * 100000,
    tem_2019 = first(mx_2019) * 100000,
    delta_tem = tem_2019 - tem_2013, 
    
    # EFEITO COMPOSIÇÃO (Mudança na estrutura de IMC)
    # (P_2013 - P_2019) * M_média
    # Se o peso aumentou (P_2019 > P_2013), este valor será NEGATIVO ("perda de ganho")
    efeito_composicao = sum(
      (prop_2019 - prop_2013) * ((mx_imc_2019 + mx_imc_2013) / 2), #eu deveria fazer dois somatórios separados aqui?
      na.rm = TRUE
    ),
    
    # EFEITO TAXA (Mudança nos riscos basais)
    # (M_2013 - M_2019) * P_médio
    # Se o risco caiu (M_2013 > M_2019), este valor será POSITIVO (contribuição para o ganho)
    efeito_taxa = sum(
      (mx_imc_2019 - mx_imc_2013) * ((prop_2019 + prop_2013) / 2), 
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  # 3. Cálculo de Contribuição Percentual e Erro
  mutate(
    pct_composicao = (efeito_composicao / delta_tem),
    pct_taxa       = (efeito_taxa / delta_tem),
    erro_estimacao = delta_tem - (efeito_composicao + efeito_taxa)
    ) %>%
  # Formatação para 4 casas decimais
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Exibição dos resultados
view(kitagawa_final)

#--------------- Salvar Resultados -------------

write.csv(kitagawa_final, paste0(base_path, "/analise_contrafactual/output/resultados/kitagawa.csv"))

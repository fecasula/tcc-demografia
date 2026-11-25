montar_graficos_onda <- function(resultados, pesquisa, ano, largura_barras = 0.58) {
  cores_sexo <- c("M" = "#1F4E79", "F" = "#8A3B5C")

  tema_abnt <- tema_figura_abnt()

  grafico_paf_idade <- ggplot(
    resultados$paf_idade,
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
        sprintf("%s; contrafactual de eliminação do IMC >= 25 kg/m²; ", pesquisa),
        "IC 95% bootstrap"
      ),
      x = "Grupo etário",
      y = "FAP",
      color = "Sexo",
      caption = paste0(
        sprintf("Fonte: IBGE, %s; DYNAMO-HIA ", pesquisa),
        "(Lobstein; Jackson-Leach, 2010), riscos relativos por sexo e idade. ",
        "Elaboração própria."
      )
    ) +
    tema_abnt

  grafico_paf_sintetica <- ggplot(
    resultados$paf_sintetica,
    aes(x = sexo_rotulo, y = estimativa_pct, fill = sexo)
  ) +
    geom_col(width = largura_barras) +
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
      subtitle = sprintf("Ponderação pelos óbitos da tábua de vida do IBGE de %s; IC 95%% bootstrap", ano),
      x = NULL,
      y = "FAP",
      caption = paste0(
        sprintf("Fonte: IBGE, %s e Tábua de Vida %s; ", pesquisa, ano),
        "DYNAMO-HIA (Lobstein; Jackson-Leach, 2010), riscos relativos por ",
        "sexo e idade. Elaboração própria."
      )
    ) +
    tema_abnt

  grafico_mortes_idade <- ggplot(
    resultados$mortes_evitaveis_idade,
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
        sprintf("Brasil, %s; contrafactual de eliminação do IMC >= 25 kg/m²; ", ano),
        "IC 95% bootstrap"
      ),
      x = "Grupo etário",
      y = "Número de mortes evitáveis",
      color = "Sexo",
      caption = paste0(
        sprintf("Fonte: IBGE, %s; MS/SVSA/CGIAE, SIM %s, ", pesquisa, ano),
        "óbitos por ocorrência; ",
        "DYNAMO-HIA (Lobstein; Jackson-Leach, 2010), riscos relativos por ",
        "sexo e idade. Elaboração própria."
      )
    ) +
    tema_abnt

  grafico_mortes_total <- ggplot(
    resultados$mortes_evitaveis_total,
    aes(x = sexo_rotulo, y = estimativa, fill = sexo)
  ) +
    geom_col(width = largura_barras) +
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
        sprintf("Brasil, população de 20 anos ou mais, %s; ", ano),
        "IC 95% bootstrap"
      ),
      x = NULL,
      y = "Número de mortes evitáveis",
      caption = paste0(
        sprintf("Fonte: IBGE, %s; MS/SVSA/CGIAE, SIM %s, ", pesquisa, ano),
        "óbitos por ocorrência; ",
        "DYNAMO-HIA (Lobstein; Jackson-Leach, 2010), riscos relativos por ",
        "sexo e idade. Elaboração própria."
      )
    ) +
    tema_abnt

  grafico_pgev20 <- ggplot(
    resultados$pgev20,
    aes(x = sexo_rotulo, y = estimativa, fill = sexo)
  ) +
    geom_col(width = largura_barras) +
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
        sprintf("%s e Tábua de Vida %s; eliminação do IMC >= 25 kg/m²; ", pesquisa, ano),
        "IC 95% bootstrap"
      ),
      x = NULL,
      y = "Ganho em anos de vida",
      caption = paste0(
        sprintf("Fonte: IBGE, %s e Tábua de Vida %s; ", pesquisa, ano),
        "DYNAMO-HIA (Lobstein; Jackson-Leach, 2010), riscos relativos por ",
        "sexo e idade. Elaboração própria."
      )
    ) +
    tema_abnt

  list(
    "01_paf_por_idade_e_sexo" = grafico_paf_idade,
    "02_paf_sintetica_por_sexo" = grafico_paf_sintetica,
    "03_mortes_evitaveis_por_idade_e_sexo" = grafico_mortes_idade,
    "04_mortes_evitaveis_por_sexo" = grafico_mortes_total,
    "05_pgev20_por_sexo" = grafico_pgev20
  )
}

montar_graficos_comparacao <- function(resultados) {
  cores_sexo <- c(
    "Homens" = "#1F4E79",
    "Mulheres" = "#8A3B5C"
  )

  tema_abnt <- tema_figura_abnt()

  fonte_exposicao_rr <- paste0(
    "Fonte: IBGE, POF 2002-2003 e 2008-2009; IBGE, PNS 2013 e 2019; ",
    "DYNAMO-HIA (Lobstein; Jackson-Leach, 2010), riscos relativos por ",
    "sexo e idade. Elaboração própria."
  )

  fonte_tabua_rr <- paste0(
    "Fonte: IBGE, POF 2002-2003 e 2008-2009; IBGE, PNS 2013 e 2019; ",
    "IBGE, Tábuas Completas de Mortalidade; DYNAMO-HIA (Lobstein; ",
    "Jackson-Leach, 2010), riscos relativos por sexo e idade. ",
    "Elaboração própria."
  )

  fonte_sim_rr <- paste0(
    "Fonte: IBGE, POF 2002-2003 e 2008-2009; IBGE, PNS 2013 e 2019; ",
    "MS/SVSA/CGIAE, SIM, óbitos por ocorrência; DYNAMO-HIA ",
    "(Lobstein; Jackson-Leach, 2010), riscos relativos por sexo e idade. ",
    "Elaboração própria."
  )

  grafico_paf_idade <- ggplot(
    resultados$paf_idade,
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
      linewidth = 0.5,
      alpha = 0.85
    ) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.5) +
    facet_wrap(~sexo_rotulo, ncol = 1) +
    scale_color_manual(values = cores_pesquisas_abnt) +
    scale_shape_manual(values = formas_pesquisas_abnt) +
    scale_y_continuous(
      labels = label_number(decimal.mark = ",", suffix = "%", accuracy = 0.1)
    ) +
    labs(
      title = "Fração atribuível populacional por idade e sexo",
      subtitle = paste0(
        "Brasil, quatro ondas; RR por sexo e idade; IC 95% obtido pelas ",
        "réplicas bootstrap"
      ),
      x = "Grupo etário",
      y = "FAP",
      color = "Pesquisa",
      shape = "Pesquisa",
      caption = fonte_exposicao_rr
    ) +
    tema_abnt

  grafico_paf_sintetica <- ggplot(
    resultados$paf_sintetica,
    aes(
      x = ano_mortalidade,
      y = estimativa_pct,
      color = sexo_rotulo,
      shape = sexo_rotulo,
      group = sexo_rotulo
    )
  ) +
    geom_errorbar(
      aes(ymin = ic_inferior_pct, ymax = ic_superior_pct),
      width = 0.32,
      linewidth = 0.5
    ) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.8) +
    scale_color_manual(values = cores_sexo) +
    scale_shape_manual(values = c("Homens" = 16, "Mulheres" = 17)) +
    scale_x_continuous(breaks = c(2003, 2009, 2013, 2019)) +
    scale_y_continuous(
      labels = label_number(decimal.mark = ",", suffix = "%", accuracy = 0.1)
    ) +
    labs(
      title = "Evolução da fração atribuível populacional sintética",
      subtitle = "Ponderação pela distribuição de óbitos da tábua de vida de cada ano",
      x = "Ano de referência",
      y = "FAP",
      color = "Sexo",
      shape = "Sexo",
      caption = fonte_tabua_rr
    ) +
    tema_abnt

  grafico_mortes_idade <- ggplot(
    resultados$mortes_evitaveis_idade,
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
      linewidth = 0.5,
      alpha = 0.85
    ) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.5) +
    facet_wrap(~sexo_rotulo, ncol = 1, scales = "free_y") +
    scale_color_manual(values = cores_pesquisas_abnt) +
    scale_shape_manual(values = formas_pesquisas_abnt) +
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
    resultados$mortes_evitaveis_total,
    aes(
      x = ano_mortalidade,
      y = estimativa,
      color = sexo_rotulo,
      shape = sexo_rotulo,
      group = sexo_rotulo
    )
  ) +
    geom_errorbar(
      aes(ymin = ic_inferior, ymax = ic_superior),
      width = 0.32,
      linewidth = 0.5
    ) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.8) +
    scale_color_manual(values = cores_sexo) +
    scale_shape_manual(values = c("Homens" = 16, "Mulheres" = 17)) +
    scale_x_continuous(breaks = c(2003, 2009, 2013, 2019)) +
    scale_y_continuous(
      labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 1)
    ) +
    labs(
      title = "Evolução do número absoluto de mortes evitáveis",
      subtitle = "Brasil, população de 20 anos ou mais; IC 95% bootstrap",
      x = "Ano de referência",
      y = "Número de mortes evitáveis",
      color = "Sexo",
      shape = "Sexo",
      caption = fonte_sim_rr
    ) +
    tema_abnt

  grafico_pgev20 <- ggplot(
    resultados$pgev20,
    aes(
      x = ano_mortalidade,
      y = estimativa,
      color = sexo_rotulo,
      shape = sexo_rotulo,
      group = sexo_rotulo
    )
  ) +
    geom_errorbar(
      aes(ymin = ic_inferior, ymax = ic_superior),
      width = 0.32,
      linewidth = 0.5
    ) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.8) +
    scale_color_manual(values = cores_sexo) +
    scale_shape_manual(values = c("Homens" = 16, "Mulheres" = 17)) +
    scale_x_continuous(breaks = c(2003, 2009, 2013, 2019)) +
    scale_y_continuous(
      labels = label_number(decimal.mark = ",", accuracy = 0.01)
    ) +
    labs(
      title = "Evolução do potencial ganho em expectativa de vida aos 20 anos",
      subtitle = "Contrafactual de eliminação do IMC >= 25 kg/m²; IC 95% bootstrap",
      x = "Ano de referência",
      y = "Ganho em anos de vida",
      color = "Sexo",
      shape = "Sexo",
      caption = fonte_tabua_rr
    ) +
    tema_abnt

  list(
    "01_paf_por_idade_e_sexo_4_ondas" = grafico_paf_idade,
    "02_paf_sintetica_4_ondas" = grafico_paf_sintetica,
    "03_mortes_por_idade_e_sexo_4_ondas" = grafico_mortes_idade,
    "04_mortes_evitaveis_4_ondas" = grafico_mortes_total,
    "05_pgev20_4_ondas" = grafico_pgev20
  )
}

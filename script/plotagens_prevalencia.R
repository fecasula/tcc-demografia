plotar_prevalencia_idade <- function(dados) {
  ggplot(
    dados,
    aes(
      x = faixa_idade,
      y = estimativa,
      color = pesquisa,
      shape = pesquisa,
      group = pesquisa
    )
  ) +
    geom_linerange(
      aes(
        ymin = ic_inferior,
        ymax = ic_superior
      ),
      linewidth = 0.44,
      alpha = 0.72
    ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.15) +
    facet_grid(
      rows = vars(indicador),
      cols = vars(sexo_rotulo),
      switch = "y"
    ) +
    scale_color_manual(values = cores_pesquisas_abnt) +
    scale_shape_manual(values = formas_pesquisas_abnt) +
    scale_y_continuous(
      labels = scales::label_percent(
        accuracy = 1,
        decimal.mark = ","
      ),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title = "Prevalências antropométricas por idade e sexo",
      subtitle = paste(
        "Brasil, POF 2002-2003 e 2008-2009;",
        "PNS 2013 e 2019 — intervalos de confiança de 95%"
      ),
      x = "Grupo etário",
      y = "Prevalência estimada",
      color = "Pesquisa",
      shape = "Pesquisa",
      caption = paste(
        "Fonte: IBGE, Pesquisa de Orçamentos Familiares 2002-2003 e",
        "2008-2009; Pesquisa Nacional de Saúde 2013 e 2019.",
        "\nNota: sobrepeso = 25 ≤ IMC < 30 kg/m²; excesso de peso = IMC ≥ 25 kg/m²",
        "e inclui as pessoas com obesidade; obesidade = IMC ≥ 30 kg/m².",
        "\nElaboração própria."
      )
    ) +
    tema_figura_abnt() +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y.left = element_text(angle = 90, size = 9.5)
    )
}

plotar_prevalencia_agregada <- function(dados, padronizada = FALSE) {
  cores_sexo <- c(
    "Homens" = "#28587B",
    "Mulheres" = "#A23B72"
  )

  rotulos <- if (padronizada) {
    labs(
      title = paste(
        "Evolução das prevalências antropométricas",
        "padronizadas por idade"
      ),
      subtitle = paste(
        "População de 20 anos ou mais; estrutura etária fixa da",
        "população brasileira projetada para 2019"
      ),
      x = "Ano de referência",
      y = "Prevalência padronizada",
      color = "Sexo",
      shape = "Sexo",
      caption = paste(
        "Fonte: IBGE, Pesquisa de Orçamentos Familiares 2002-2003 e",
        "2008-2009; Pesquisa Nacional de Saúde 2013 e 2019;",
        "Projeções da População, revisão 2024.",
        "\nNota: sobrepeso = 25 ≤ IMC < 30 kg/m²; excesso de peso = IMC ≥ 25 kg/m²",
        "e inclui as pessoas com obesidade; obesidade = IMC ≥ 30 kg/m².",
        "Padrão de 2019 formado pela população de ambos os sexos;",
        "padronização direta com survey::svystandardize();",
        "IC 95% logit calculado no desenho padronizado.",
        "\nElaboração própria."
      )
    )
  } else {
    labs(
      title = "Evolução das prevalências antropométricas",
      subtitle = paste(
        "Estimativas agregadas por sexo, população de 20 anos ou mais;",
        "intervalos de confiança de 95%"
      ),
      x = "Ano de referência",
      y = "Prevalência estimada",
      color = "Sexo",
      shape = "Sexo",
      caption = paste(
        "Fonte: IBGE, Pesquisa de Orçamentos Familiares 2002-2003 e",
        "2008-2009; Pesquisa Nacional de Saúde 2013 e 2019.",
        "\nNota: sobrepeso = 25 ≤ IMC < 30 kg/m²; excesso de peso = IMC ≥ 25 kg/m²",
        "e inclui as pessoas com obesidade; obesidade = IMC ≥ 30 kg/m².",
        "Estimativas ainda não padronizadas por idade.",
        "\nElaboração própria."
      )
    )
  }

  ggplot(
    dados,
    aes(
      x = pesquisa,
      y = estimativa,
      color = sexo_rotulo,
      shape = sexo_rotulo,
      group = sexo_rotulo
    )
  ) +
    geom_errorbar(
      aes(
        ymin = ic_inferior,
        ymax = ic_superior
      ),
      width = 0.08,
      linewidth = 0.55,
      position = position_dodge(width = 0.25)
    ) +
    geom_line(
      linewidth = 0.75,
      position = position_dodge(width = 0.25)
    ) +
    geom_point(
      size = 2.8,
      position = position_dodge(width = 0.25)
    ) +
    facet_wrap(
      vars(indicador),
      ncol = 1,
      scales = "free_y"
    ) +
    scale_color_manual(
      values = cores_sexo
    ) +
    scale_shape_manual(
      values = c(
        "Homens" = 16,
        "Mulheres" = 17
      )
    ) +
    scale_y_continuous(
      labels = scales::label_percent(
        accuracy = 1,
        decimal.mark = ","
      ),
      expand = expansion(mult = c(0.03, 0.10))
    ) +
    scale_x_discrete(labels = c(
      "POF 2002-2003" = "2003", "POF 2008-2009" = "2009",
      "PNS 2013" = "2013", "PNS 2019" = "2019"
    )) +
    rotulos +
    tema_figura_abnt()
}

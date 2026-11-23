# tcc-demografia

Código e resultados da análise estatística do TCC de Felipe Casula, *A obesidade sob uma perspectiva demográfica*. O estudo examina a evolução do excesso de peso e sua relação com a mortalidade da população brasileira de 20 anos ou mais, por sexo e grupo etário, de 2003 a 2019. [Site do TCC](https://fecasula.github.io/tcc-site/)

## Análise estatística

**Fontes.** A exposição antropométrica vem dos microdados das POF 2002–2003 e 2008–2009 e das PNS 2013 e 2019. A análise de mortalidade combina os óbitos por ocorrência do SIM, as Tábuas Completas de Mortalidade do IBGE e os riscos relativos de mortalidade por todas as causas do projeto DYNAMO-HIA, diferenciados por sexo, idade e categoria de IMC.

**Prevalências.** As estimativas de sobrepeso (25 ≤ IMC < 30), excesso de peso (IMC ≥ 25) e obesidade (IMC ≥ 30) usam os pesos, estratos e unidades primárias de amostragem de cada inquérito. A variância é calculada por linearização de Taylor, com intervalos de confiança de 95% pela transformação logit. A padronização direta aplica a mesma estrutura etária aos dois sexos e às quatro ondas, usando a população brasileira de 2019 na revisão de 2024 das projeções do IBGE.

**Cenário contrafactual.** O estudo estima a mortalidade que seria observada se as pessoas com IMC ≥ 25 kg/m² tivessem o risco de referência do grupo com IMC < 25 kg/m². A partir desse cenário, calcula quatro indicadores:

- **Fração atribuível populacional (FAP)** por sexo e grupo etário, correspondente à redução proporcional da taxa de mortalidade no cenário.
- **FAP sintética por sexo**, ponderada pela distribuição etária dos óbitos da tábua de vida.
- **Óbitos potencialmente evitáveis**, obtidos ao aplicar a FAP de cada grupo aos óbitos observados no SIM.
- **Ganho potencial em expectativa de vida aos 20 anos (PGEV20)**, definido como a diferença entre a expectativa de vida contrafactual e a observada.

**Incerteza.** Os indicadores contrafactuais são recalculados em 1.000 réplicas do desenho amostral, com IC de 95% pelos percentis de 2,5% e 97,5%. A POF 2002–2003 e a PNS 2013 usam o bootstrap de Rao–Wu–Yue–Beaumont, enquanto a POF 2008–2009 e a PNS 2019 usam o bootstrap generalizado de Beaumont e Patak. Os intervalos refletem a incerteza amostral das prevalências. Os riscos relativos, os óbitos do SIM e as tábuas de mortalidade são tratados como fixos.

### Resultados em destaque

| Estimativa pontual | Homens, 2003 | Homens, 2019 | Mulheres, 2003 | Mulheres, 2019 |
| --- | ---: | ---: | ---: | ---: |
| Prevalência de obesidade padronizada por idade | 9,0% | 22,3% | 13,6% | 29,9% |
| Ganho potencial em expectativa de vida aos 20 anos | 1,47 ano | 2,50 anos | 1,49 ano | 2,12 anos |

Os resultados intermediários e os intervalos de confiança estão nas tabelas de [prevalências padronizadas](dados/output/prevalencias/prevalencias_padronizadas_etaria_2019.csv) e de [ganho potencial em expectativa de vida](dados/resultados/comparacao_4_ondas/05_pgev20_por_sexo_4_ondas.csv).

### Onde encontrar os arquivos

| Caminho | Conteúdo |
| --- | --- |
| [`script/`](script/) | Estimação e padronização das prevalências, bootstrap por pesquisa e comparação das quatro ondas |
| [`dados/raw/`](dados/raw/) e [`dados/dados_obitos/`](dados/dados_obitos/) | Microdados, projeções, tábuas de vida, parâmetros de risco e registros do SIM |
| [`dados/output/prevalencias/`](dados/output/prevalencias/) | Tabelas de prevalência por sexo e idade, estimativas agregadas e padronização etária |
| [`dados/resultados/`](dados/resultados/) | Indicadores contrafactuais por pesquisa, auditorias do bootstrap e tabelas comparativas |

## Figuras para o TCC

Execute os scripts na raiz do projeto com as dependências R instaladas, incluindo `ggplot2` 3.5 ou posterior e suporte a Cairo no R:

```sh
Rscript script/padronizacao_etaria_2019.R
Rscript script/grafico_diferenca_prevalecia_entre_sexos.R
Rscript script/comparar_indicadores_4_ondas.R
```

A padronização executa a estimação de prevalência na mesma sessão quando necessário. A comparação usa as tabelas das quatro pesquisas em `dados/resultados/`. Os scripts de bootstrap individuais também usam o tema de exportação compartilhado.

Os PDFs vetoriais e os PNGs foram preparados para uso no documento A4. As figuras usam 16 cm e 600 dpi, com exceção da Figura 2, que mantém o formato panorâmico da versão original para reunir os 12 painéis. Insira as figuras na largura útil do texto, mantendo a proporção. Use no documento o título acima da imagem e a nota e a fonte abaixo.

| Figura no TCC | Arquivo PDF |
| --- | --- |
| 1 | [Prevalências por idade e sexo](output/graficos_prevalencia/prevalencia_por_idade_sexo.pdf) |
| 2 | [Diferenças por sexo em 12 painéis](output/graficos_prevalencia/diferenca_prevalencias_antropometricas_por_sexo.pdf) |
| 3 | [Prevalências padronizadas por idade](output/graficos_prevalencia/prevalencia_padronizada_estrutura_2019.pdf) |
| 5 | [FAP por idade e sexo](graficos/comparacao_4_ondas/01_paf_por_idade_e_sexo_4_ondas.pdf) |
| 6 | [FAP sintética](graficos/comparacao_4_ondas/02_paf_sintetica_4_ondas.pdf) |
| 7 | [Mortes evitáveis por idade e sexo](graficos/comparacao_4_ondas/03_mortes_por_idade_e_sexo_4_ondas.pdf) |
| 8 | [Total de mortes evitáveis](graficos/comparacao_4_ondas/04_mortes_evitaveis_4_ondas.pdf) |
| 9 | [Ganho potencial em expectativa de vida aos 20 anos](graficos/comparacao_4_ondas/05_pgev20_4_ondas.pdf) |

## Site Quarto

```sh
cd site
quarto render
```

Use `quarto preview` no diretório `site/` para pré-visualizar alterações. A capa, o Resumo e o Abstract estão em [index.qmd](site/index.qmd). As demais seções estão em páginas Quarto próprias, e as tabelas, equações e listas reproduzidas do PDF em `site/assets/documento/`. O site estático é gerado em `site/_site/`. O PDF para download está em `site/assets/tcc-felipe-casula.pdf`.

Quando a apresentação estiver pronta, salve o arquivo como `site/assets/apresentacao.pptx` e renderize de novo. O botão passa de **Em preparação** para um link para o arquivo.

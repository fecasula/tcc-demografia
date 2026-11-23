# tcc-demografia

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

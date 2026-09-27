<p align="center"><img src="www/img/logo_app.png" width="120" alt="Logo do Plota"></p>

# Plota

<p>
  <a href="https://github.com/Marlenildo/plota/releases"><img alt="Versão" src="https://img.shields.io/github/v/release/Marlenildo/plota?label=vers%C3%A3o&color=2a5c92"></a>
  <a href="LICENSE"><img alt="Licença MIT" src="https://img.shields.io/badge/licen%C3%A7a-MIT-4d965d"></a>
  <img alt="R >= 4.1" src="https://img.shields.io/badge/R-%E2%89%A5%204.1-173b5b">
</p>

**Gráficos elegantes para a pesquisa agronômica** — barras, linhas, interação, regressão e boxplot
com fonte, cores e tamanho sob medida, agrupados em painéis e prontos para artigos, teses e apresentações.

Aplicativo [Shiny](https://shiny.posit.co/) da mesma família do Ranova, do Croma e do Calibra.

## Funcionalidades

- **Dados**: cole do Excel, envie `.xlsx`, `.xls` ou `.csv`, ou use os exemplos. Vários conjuntos de dados
  na mesma sessão, editáveis numa planilha (vírgula decimal aceita).
- **10 tipos de gráfico**: barras, barras horizontais, empilhadas (valores ou 100%), linhas, médias com erro,
  dispersão com regressão, boxplot, violino, histograma e densidade.
- **Estatística**: média, mediana ou soma; erro-padrão, desvio-padrão ou IC 95%; letras de teste de médias
  vindas de uma coluna; regressão linear, quadrática, cúbica, logarítmica ou LOESS, com equação e R² no gráfico
  ou na legenda e dose de máxima eficiência na quadrática.
- **Interação e agrupamento**: um fator no eixo X e outro em cor; divisão em painéis (facetas) por um terceiro.
- **Aparência**: paletas prontas (inclusive tons de cinza e Okabe-Ito para daltônicos) ou a cor de cada nível,
  contorno e espessuras, opacidade, formas e tracejados por grupo, 5 fontes, tamanho e cor do texto,
  4 temas, grade, marcas dos eixos para dentro, posição da legenda, limites e casas decimais do eixo.
- **Expoentes e índices**: escreva `kg ha^-1` ou `CO_2` e o gráfico mostra kg ha<sup>−1</sup> e CO<sub>2</sub>.
- **Vários gráficos**: crie, duplique, exclua e aplique o estilo de um gráfico a todos com um clique.
- **Painel**: junte gráficos em uma figura com letras (A, B, C / (a), (b)...) e legenda comum.
- **Exportação**: TIFF (LZW, formato padrão, o mais pedido pelas revistas), PNG, JPEG, PDF, SVG e EPS; qualquer resolução (72 a 2400 dpi) e tamanho em
  cm, mm, polegadas ou pixels, com tamanhos prontos para artigo e slide. A prévia é desenhada no mesmo
  tamanho da exportação: o que se vê é o que se baixa.
- **Projeto**: salve dados e gráficos em um arquivo `.plota` no seu computador e abra depois.

Os dados ficam apenas na sessão aberta: nada é gravado em banco de dados, arquivos ou cookies.

## Como executar

Requer R 4.1 ou superior.

```r
install.packages(c("shiny", "ggplot2", "patchwork", "showtext", "rhandsontable",
                   "readxl", "colourpicker", "scales", "jsonlite"))
shiny::runApp()
```

Ou direto do GitHub:

```r
shiny::runGitHub("plota", "Marlenildo")
```

## Fontes

As fontes ficam em `fonts/` e são desenhadas com o `showtext`, então o resultado é o mesmo na tela e em
qualquer formato, em qualquer servidor: Liberation Sans (métricas do Arial), Liberation Serif (métricas do
Times New Roman), PT Sans, Poppins e Crimson Text, todas sob a SIL Open Font License (licenças na pasta).
Nos formatos vetoriais (PDF, SVG, EPS), o texto sai convertido em curvas.

## Publicação (Posit Connect Cloud)

O repositório inclui um `manifest.json` para publicar direto do GitHub em
[connect.posit.cloud](https://connect.posit.cloud): **Publish → Shiny → repositório `Marlenildo/plota`,
branch `main`, arquivo `app.R`**. Para regerar o manifest depois de mudar dependências ou arquivos:

```r
rsconnect::writeManifest(appFiles = c("app.R", "global.R", "ui.R", "server.R", "DESCRIPTION",
                                      list.files("www", recursive = TRUE, full.names = TRUE),
                                      list.files("fonts", full.names = TRUE)))
```

## Estrutura

- `app.R`: ponto de entrada · `ui.R`: interface · `server.R`: estado e lógica · `global.R`: dados, estatística,
  construção dos gráficos, painel e exportação
- `fonts/`: fontes livres · `www/`: estilos e imagens · `scripts/gerar_logo_app.R`: gera a logo do app
- `DESCRIPTION`: versão e dependências · `CHANGELOG.md`: histórico · `CITATION.cff`: citação · `LICENSE`: licença MIT

## Licença

MIT © Marlenildo. As fontes em `fonts/` seguem as próprias licenças (SIL Open Font License 1.1).

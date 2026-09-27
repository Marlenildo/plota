# Plota — Gráficos elegantes para a pesquisa agronômica. Ponto de entrada da aplicação.
source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)

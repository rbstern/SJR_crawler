# Etapa 1: baixa os currículos Lattes dos docentes e roda o scriptLattes.
#
# Uso, a partir da raiz do repositório:
#   Rscript lattes/01_baixar.R
#
# Cria lattes/execucoes/AAAA-MM-DD/ com:
#   docentes.csv   planilha de entrada já validada (nome, id_lattes), lida pela etapa 2
#   cache/         HTML de cada CV, baixado pelo robô do scriptLattes
#   scriptlattes/  saída do scriptLattes: relatório HTML e json/, com um arquivo por docente
#   logs/          saída do Python: um arquivo por download e um da compilação
#
# Os CVs são baixados um a um, antes da compilação: o scriptLattes aborta o grupo
# inteiro quando um download falha, e assim uma falha afeta só aquele docente.
suppressPackageStartupMessages(library(tidyverse))
source("lattes/config.R")

# Lê e valida a planilha de docentes. Devolve tibble (nome, id_lattes).
le_docentes <- function(path) {
  bruto <- if (str_detect(path, regex("\\.xlsx?$", ignore_case = TRUE))) {
    readxl::read_excel(path, col_types = "text")
  } else {
    read_csv(path, col_types = cols(.default = col_character()))
  }
  names(bruto) <- str_to_lower(str_squish(names(bruto)))
  faltam <- setdiff(c("nome", "lattes"), names(bruto))
  if (length(faltam) > 0) {
    stop("A planilha ", path, " precisa das colunas: ", str_flatten_comma(faltam))
  }

  docentes <- bruto %>%
    transmute(nome = str_squish(nome), lattes = str_squish(lattes)) %>%
    filter(!is.na(nome) | !is.na(lattes)) %>% # ignora linhas em branco
    mutate(id_lattes = str_extract(lattes, "(?<!\\d)\\d{16}(?!\\d)"))

  invalidos <- docentes %>% filter(is.na(nome) | is.na(id_lattes))
  if (nrow(invalidos) > 0) {
    stop("Linhas sem nome ou sem ID Lattes de 16 dígitos na planilha:\n",
         str_flatten(str_c("  ", invalidos$nome, " | ", invalidos$lattes), "\n"))
  }
  for (col in c("nome", "id_lattes")) {
    rep <- docentes[[col]][duplicated(docentes[[col]])]
    if (length(rep) > 0) stop("Valores repetidos na coluna ", col, ": ", str_flatten_comma(rep))
  }
  select(docentes, nome, id_lattes)
}

# Roda o Python do scriptLattes a partir do diretório dele (o robô procura o
# chromedriver no diretório corrente), com stdout e stderr no arquivo_log.
roda_scriptlattes <- function(args, arquivo_log, timeout = 0) {
  antigo <- setwd(path.expand(SCRIPTLATTES_DIR))
  on.exit(setwd(antigo))
  suppressWarnings(system2(SCRIPTLATTES_PYTHON, args, stdout = arquivo_log,
                           stderr = arquivo_log, timeout = timeout))
}

# Baixa um CV com o robô do próprio scriptLattes. Devolve TRUE se o HTML salvo
# é de fato um currículo.
baixa_cv <- function(id, dir_cache, arquivo_log) {
  destino <- file.path(dir_cache, id)
  codigo <- str_c("import sys; from scriptLattes.baixaLattes import baixaCVLattes; ",
                  "baixaCVLattes(sys.argv[1], sys.argv[2])")
  roda_scriptlattes(c("-c", shQuote(codigo), id, shQuote(dir_cache)), arquivo_log, timeout = 600)
  ok <- file.exists(destino) &&
    str_detect(read_file(destino), fixed("Última atualização do currículo"))
  if (!ok && file.exists(destino)) file.remove(destino) # nova tentativa na próxima execução
  ok
}

# ---------------------------------------------------------------------------
docentes <- le_docentes(PLANILHA_DOCENTES)
message(nrow(docentes), " docentes em ", PLANILHA_DOCENTES)

dir_scriptlattes <- path.expand(SCRIPTLATTES_DIR)
if (!file.exists(file.path(dir_scriptlattes, "scriptLattes.py"))) {
  stop("scriptLattes não encontrado em ", dir_scriptlattes, " (ajuste SCRIPTLATTES_DIR em lattes/config.R)")
}
if (!file.exists(file.path(dir_scriptlattes, "chromedriver"))) {
  stop("chromedriver ausente em ", dir_scriptlattes, ": rode \"make install\" lá dentro")
}
if (!file.exists(SCRIPTLATTES_PYTHON)) SCRIPTLATTES_PYTHON <- Sys.which("python3")

dir_execucao <- file.path(DIR_EXECUCOES, format(Sys.Date(), "%Y-%m-%d"))
dir.create(dir_execucao, recursive = TRUE, showWarnings = FALSE)
dir_execucao <- normalizePath(dir_execucao)
dir_cache <- file.path(dir_execucao, "cache")
dir_saida <- file.path(dir_execucao, "scriptlattes")
dir_logs <- file.path(dir_execucao, "logs")
walk(c(dir_cache, dir_logs), dir.create, showWarnings = FALSE)
write_csv(docentes, file.path(dir_execucao, "docentes.csv"))

# Download, um docente por vez (os já presentes no cache de hoje são pulados)
docentes$baixado <- FALSE
for (i in seq_len(nrow(docentes))) {
  id <- docentes$id_lattes[i]
  rotulo <- sprintf("[%d/%d] %s", i, nrow(docentes), docentes$nome[i])
  if (file.exists(file.path(dir_cache, id))) {
    docentes$baixado[i] <- TRUE
    message(rotulo, ": já no cache")
    next
  }
  docentes$baixado[i] <- baixa_cv(id, dir_cache, file.path(dir_logs, str_c(id, ".log")))
  message(rotulo, if (docentes$baixado[i]) ": ok" else ": FALHOU")
}
baixados <- filter(docentes, baixado)
if (nrow(baixados) == 0) stop("Nenhum CV foi baixado; veja os logs em ", dir_logs)

# Compilação: o scriptLattes lê os CVs do cache e gera o relatório e os JSONs.
# A saída é refeita do zero para não sobrar JSON de uma execução anterior do mesmo dia.
unlink(dir_saida, recursive = TRUE)
arquivo_list <- file.path(dir_execucao, "docentes.list")
writeLines(str_c(baixados$id_lattes, " , ", str_remove_all(baixados$nome, "[,;#\t]")),
           arquivo_list)
arquivo_config <- file.path(dir_execucao, "scriptlattes.config")
writeLines(c(
  "global-nome_do_grupo = Departamento",
  str_c("global-arquivo_de_entrada = ", arquivo_list),
  str_c("global-diretorio_de_saida = ", dir_saida, "/"),
  str_c("global-diretorio_de_armazenamento_de_cvs = ", dir_cache, "/"),
  "global-itens_desde_o_ano = 1900",
  str_c("global-itens_ate_o_ano = ", as.integer(format(Sys.Date(), "%Y")) + 1),
  "grafo-mostrar_grafo_de_colaboracoes = nao"
), arquivo_config)

message("Compilando com o scriptLattes...")
log_compilacao <- file.path(dir_logs, "scriptlattes.log")
status <- roda_scriptlattes(c("scriptLattes.py", shQuote(arquivo_config)), log_compilacao)
n_json <- length(list.files(file.path(dir_saida, "json"), "\\.json$"))
if (status != 0 || n_json != nrow(baixados)) {
  stop("O scriptLattes terminou com erro (", n_json, " JSONs para ", nrow(baixados),
       " CVs). Veja ", log_compilacao)
}

message("\n", nrow(baixados), " de ", nrow(docentes), " CVs processados em ", dir_execucao)
falhas <- filter(docentes, !baixado)
if (nrow(falhas) > 0) {
  message("Não foi possível baixar (confira o ID e rode de novo; os logs estão em ",
          dir_logs, "):\n", str_flatten(str_c("  ", falhas$nome, " (", falhas$id_lattes, ")"), "\n"))
}
message("Próximo passo: Rscript lattes/02_produtos.R")

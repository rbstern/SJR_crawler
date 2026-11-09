# Etapa 2: monta as planilhas a partir dos JSONs gerados pelo scriptLattes.
#
# Uso, a partir da raiz do repositório:
#   Rscript lattes/02_produtos.R                              # execução mais recente
#   Rscript lattes/02_produtos.R lattes/execucoes/2026-11-09  # execução específica
#
# Gera, no diretório da execução:
#   produto1_atualizacao_lattes.xlsx  uma linha por docente, com a data da
#                                     última atualização do CV
#   produto2_artigos.xlsx             aba "artigos": uma linha por artigo em
#                                     periódico e uma coluna 0/1 por docente;
#                                     abas de conferência "verificar_coautoria"
#                                     e "fusoes" (ver abaixo)
#
# Na coluna de um docente, 1 = o artigo consta no Lattes desse docente; a
# coluna fica em branco se o CV do docente não foi baixado.
#
# O mesmo artigo aparece no Lattes de cada coautor, às vezes com pequenas
# diferenças. Dois registros são tratados como o mesmo artigo quando têm o
# mesmo DOI, ou quando os anos diferem em no máximo 1 e os títulos são
# similares pelo critério do scriptLattes (após normalização: similaridade de
# Levenshtein >= 0.93 ou distância <= 5, ou um título contido no outro quando
# ambos têm 50+ caracteres). Registros com DOIs diferentes não são unidos
# pelo título.
#
# Abas de conferência:
#   verificar_coautoria  o docente aparece na lista de autores (por um dos seus
#                        "nomes em citações" do Lattes), mas o artigo não está
#                        no Lattes dele: possível omissão no CV
#   fusoes               artigos montados a partir de registros com títulos ou
#                        DOIs diferentes, para conferir a deduplicação
suppressPackageStartupMessages(library(tidyverse))
source("lattes/config.R")

args <- commandArgs(trailingOnly = TRUE)
dir_execucao <- if (length(args) > 0) {
  args[1]
} else {
  execucoes <- sort(list.dirs(DIR_EXECUCOES, recursive = FALSE))
  if (length(execucoes) == 0) {
    stop("Nenhuma execução em ", DIR_EXECUCOES, "; rode antes lattes/01_baixar.R")
  }
  last(execucoes)
}
data_execucao <- coalesce(lubridate::ymd(basename(dir_execucao), quiet = TRUE),
                          Sys.Date())

# ---------------------------------------------------------------------------
# Leitura
# ---------------------------------------------------------------------------
docentes <- read_csv(file.path(dir_execucao, "docentes.csv"),
                     col_types = cols(.default = col_character()))

arquivos_json <- list.files(file.path(dir_execucao, "scriptlattes", "json"),
                            "\\.json$", full.names = TRUE)
if (length(arquivos_json) == 0) {
  stop("Nenhum JSON do scriptLattes em ", dir_execucao, "; rode antes lattes/01_baixar.R")
}
cvs_json <- map(arquivos_json, jsonlite::fromJSON)

vazio_para_na <- function(x) na_if(str_squish(as.character(x)), "")

cvs <- map_dfr(cvs_json, function(cv) {
  info <- cv$informacoes_pessoais
  tibble(
    id_lattes = info$id_lattes,
    nome_lattes = info$nome_completo %||% NA_character_,
    nome_citacoes = info$nome_citacoes %||% NA_character_,
    atualizacao_cv = info$atualizacao_cv %||% NA_character_
  )
}) %>%
  mutate(across(everything(), vazio_para_na))

# Um registro = um artigo no Lattes de um docente
registros <- map_dfr(cvs_json, function(cv) {
  artigos <- cv$producao_bibliografica$artigos_periodicos
  if (length(artigos) == 0) return(NULL)
  as_tibble(artigos) %>%
    transmute(id_lattes = cv$informacoes_pessoais$id_lattes,
              titulo, ano, revista, issn, doi, autores)
}) %>%
  mutate(across(everything(), vazio_para_na),
         ano = suppressWarnings(as.integer(ano))) %>%
  semi_join(docentes, by = "id_lattes")
if (nrow(registros) == 0) stop("Nenhum artigo encontrado nos JSONs de ", dir_execucao)

# ---------------------------------------------------------------------------
# Deduplicação
# ---------------------------------------------------------------------------
normaliza_titulo <- function(x) {
  x %>%
    coalesce("") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

# O campo DOI do Lattes às vezes traz outra coisa (ex.: um link genérico do
# Scopus, igual para artigos diferentes); só vale o que tem a forma de um DOI.
normaliza_doi <- function(x) {
  x %>%
    str_to_lower() %>%
    str_extract("10\\.\\d{4,9}/\\S+") %>%
    coalesce("")
}

# Matriz lógica length(a) x length(b): a[i] e b[j] são o mesmo título?
titulos_similares <- function(a, b) {
  dist <- adist(a, b)
  la <- matrix(nchar(a), length(a), length(b))
  lb <- matrix(nchar(b), length(a), length(b), byrow = TRUE)
  maior <- pmax(la, lb)
  menor <- pmin(la, lb)
  similar <- menor >= 10 & (1 - dist / maior >= 0.93 | dist <= 5)
  # Um título contido no outro: só é possível se a distância for a diferença
  # de tamanho, então o teste exato fica restrito a esses pares.
  cand <- which(!similar & menor >= 50 & dist == abs(la - lb), arr.ind = TRUE)
  if (nrow(cand) > 0) {
    ta <- a[cand[, 1]]
    tb <- b[cand[, 2]]
    similar[cand] <- stringi::stri_detect_fixed(tb, ta) |
      stringi::stri_detect_fixed(ta, tb)
  }
  similar
}

# Pares (i, j) de registros com títulos similares e anos a no máximo 1 de
# distância. Registros sem ano são comparados com todos.
pares_por_titulo <- function(titulo_norm, ano) {
  pares <- list()
  compara <- function(ia, ib) {
    p <- which(titulos_similares(titulo_norm[ia], titulo_norm[ib]), arr.ind = TRUE)
    cbind(ia[p[, 1]], ib[p[, 2]])
  }
  for (a in sort(unique(na.omit(ano)))) {
    pares[[length(pares) + 1]] <- compara(which(ano == a), which(ano %in% c(a, a + 1)))
  }
  sem_ano <- which(is.na(ano))
  if (length(sem_ano) > 0) {
    pares[[length(pares) + 1]] <- compara(sem_ano, seq_along(ano))
  }
  pares <- do.call(rbind, pares)
  pares[pares[, 1] != pares[, 2], , drop = FALSE]
}

# Pares de registros com o mesmo DOI
pares_por_doi <- function(doi_norm) {
  com_doi <- which(doi_norm != "")
  grupos <- split(com_doi, doi_norm[com_doi])
  grupos <- grupos[lengths(grupos) > 1]
  do.call(rbind, c(list(matrix(integer(0), 0, 2)),
                   map(grupos, ~ cbind(.x[1], .x[-1]))))
}

# Componentes conexas (union-find): devolve o grupo de cada um dos n registros
componentes <- function(n, pares) {
  pai <- seq_len(n)
  raiz <- function(x) {
    while (pai[x] != x) x <- pai[x]
    x
  }
  for (k in seq_len(nrow(pares))) {
    r1 <- raiz(pares[k, 1])
    r2 <- raiz(pares[k, 2])
    if (r1 != r2) pai[max(r1, r2)] <- min(r1, r2)
  }
  vapply(seq_len(n), raiz, integer(1))
}

registros <- registros %>%
  mutate(titulo_norm = normaliza_titulo(titulo), doi_norm = normaliza_doi(doi))

pares_titulo <- pares_por_titulo(registros$titulo_norm, registros$ano)
doi_i <- registros$doi_norm[pares_titulo[, 1]]
doi_j <- registros$doi_norm[pares_titulo[, 2]]
pares_titulo <- pares_titulo[doi_i == "" | doi_j == "" | doi_i == doi_j, , drop = FALSE]

registros$grupo <- componentes(
  nrow(registros), rbind(pares_titulo, pares_por_doi(registros$doi_norm))
)

# Cada artigo é representado pelo registro mais completo do seu grupo
sem_periodo <- is.null(ANO_INICIAL) && is.null(ANO_FINAL)
artigos <- registros %>%
  arrange(grupo, doi_norm == "", is.na(issn), desc(nchar(coalesce(titulo, "")))) %>%
  distinct(grupo, .keep_all = TRUE) %>%
  # com período definido, artigos sem ano ficam de fora
  filter(sem_periodo |
           between(ano, ANO_INICIAL %||% -Inf, ANO_FINAL %||% Inf) %in% TRUE) %>%
  arrange(desc(ano), titulo_norm) %>%
  mutate(id_artigo = row_number(), doi = na_if(doi_norm, "")) %>%
  select(id_artigo, grupo, titulo, ano, revista, issn, doi, autores)

# ---------------------------------------------------------------------------
# Produto 1: data da última atualização do Lattes
# ---------------------------------------------------------------------------
produto1 <- docentes %>%
  left_join(cvs, by = "id_lattes") %>%
  mutate(
    ultima_atualizacao = lubridate::dmy(atualizacao_cv, quiet = TRUE),
    dias_sem_atualizar = as.integer(data_execucao - ultima_atualizacao),
    situacao = case_when(
      is.na(nome_lattes) ~ "CV não baixado",
      is.na(ultima_atualizacao) ~ "data não encontrada no CV",
      TRUE ~ "ok"
    ),
    url = str_c("http://lattes.cnpq.br/", id_lattes)
  ) %>%
  select(docente = nome, id_lattes, nome_no_lattes = nome_lattes,
         ultima_atualizacao, dias_sem_atualizar, situacao, url)

# ---------------------------------------------------------------------------
# Produto 2: artigos x docentes
# ---------------------------------------------------------------------------
autoria <- registros %>% distinct(grupo, id_lattes)

matriz <- autoria %>%
  mutate(v = 1L) %>%
  pivot_wider(names_from = id_lattes, values_from = v, values_fill = 0L)
for (id in setdiff(docentes$id_lattes, names(matriz))) matriz[[id]] <- 0L
# Docente sem CV baixado: coluna em branco (desconhecido), e não 0
for (id in setdiff(docentes$id_lattes, cvs$id_lattes)) matriz[[id]] <- NA_integer_
matriz <- matriz %>%
  select(grupo, all_of(docentes$id_lattes)) %>%
  rename(all_of(set_names(docentes$id_lattes, docentes$nome)))

produto2 <- artigos %>%
  left_join(count(autoria, grupo, name = "n_docentes"), by = "grupo") %>%
  left_join(matriz, by = "grupo")

# Docente na lista de autores (por um dos seus nomes em citações), mas sem o
# artigo no próprio Lattes
normaliza_nome <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z,]+", " ") %>%
    str_replace_all(" ?, ?", ", ") %>%
    str_squish()
}

variantes <- cvs %>%
  select(id_lattes, nome_citacoes) %>%
  filter(!is.na(nome_citacoes)) %>%
  separate_longer_delim(nome_citacoes, ";") %>%
  mutate(autor = normaliza_nome(nome_citacoes)) %>%
  filter(str_detect(autor, "[A-Z]+[ ,]+[A-Z]")) %>% # pelo menos 2 partes
  distinct(id_lattes, autor)

verificar <- registros %>%
  select(grupo, autores) %>%
  filter(!is.na(autores)) %>%
  separate_longer_delim(autores, ";") %>%
  mutate(nome_na_lista = str_squish(autores), autor = normaliza_nome(autores)) %>%
  inner_join(variantes, by = "autor", relationship = "many-to-many") %>%
  anti_join(autoria, by = c("grupo", "id_lattes")) %>%
  distinct(grupo, id_lattes, .keep_all = TRUE) %>%
  inner_join(artigos, by = "grupo") %>%
  left_join(select(docentes, id_lattes, docente = nome), by = "id_lattes") %>%
  arrange(id_artigo, docente) %>%
  select(id_artigo, docente, nome_na_lista, titulo, ano, revista, doi)

fusoes <- registros %>%
  group_by(grupo) %>%
  filter(n_distinct(titulo_norm) > 1 | n_distinct(doi_norm[doi_norm != ""]) > 1) %>%
  ungroup() %>%
  inner_join(select(artigos, grupo, id_artigo), by = "grupo") %>%
  left_join(select(docentes, id_lattes, docente = nome), by = "id_lattes") %>%
  arrange(id_artigo, docente) %>%
  select(id_artigo, docente, titulo_registrado = titulo, ano, revista,
         doi_registrado = doi)

# ---------------------------------------------------------------------------
# Planilhas
# ---------------------------------------------------------------------------
options(openxlsx.dateFormat = "dd/mm/yyyy")
estilo_cabecalho <- openxlsx::createStyle(
  textDecoration = "bold", fgFill = "#DDE3EA", border = "bottom",
  valign = "center", wrapText = TRUE
)

# Aba com cabeçalho fixo; as colunas antes de col_fixa também ficam fixas
adiciona_aba <- function(wb, aba, dados, larguras, col_fixa = 1) {
  openxlsx::addWorksheet(wb, aba)
  openxlsx::writeData(wb, aba, dados, headerStyle = estilo_cabecalho, withFilter = TRUE)
  openxlsx::freezePane(wb, aba, firstActiveRow = 2, firstActiveCol = col_fixa)
  openxlsx::setColWidths(wb, aba, seq_along(larguras), larguras)
}

# Produto 1
wb1 <- openxlsx::createWorkbook()
class(produto1$url) <- "hyperlink"
adiciona_aba(wb1, "docentes", produto1, c(40, 19, 40, 18, 18, 24, 40))
arquivo1 <- file.path(dir_execucao, "produto1_atualizacao_lattes.xlsx")
openxlsx::saveWorkbook(wb1, arquivo1, overwrite = TRUE)

# Produto 2
wb2 <- openxlsx::createWorkbook()
saida2 <- select(produto2, -grupo)
col_docentes <- which(names(saida2) %in% docentes$nome)
adiciona_aba(wb2, "artigos", saida2,
             c(9, 70, 6, 40, 11, 30, 50, 11, rep(4.5, length(col_docentes))),
             col_fixa = 3)
openxlsx::addStyle(wb2, "artigos", rows = 1, cols = col_docentes, stack = TRUE,
                   style = openxlsx::createStyle(textRotation = 90, halign = "center"))
openxlsx::setRowHeights(wb2, "artigos", rows = 1, heights = 160)
if (nrow(saida2) > 0) {
  linhas <- seq_len(nrow(saida2)) + 1
  openxlsx::addStyle(wb2, "artigos", rows = linhas, cols = col_docentes,
                     gridExpand = TRUE, style = openxlsx::createStyle(halign = "center"))
  openxlsx::conditionalFormatting(
    wb2, "artigos", cols = col_docentes, rows = linhas, rule = "==1",
    style = openxlsx::createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
  )
}
adiciona_aba(wb2, "verificar_coautoria", verificar, c(9, 30, 30, 70, 6, 40, 30))
adiciona_aba(wb2, "fusoes", fusoes, c(9, 30, 70, 6, 40, 40))
arquivo2 <- file.path(dir_execucao, "produto2_artigos.xlsx")
openxlsx::saveWorkbook(wb2, arquivo2, overwrite = TRUE)

message(
  "Produto 1: ", arquivo1, "\n",
  "  ", sum(produto1$situacao == "ok"), " de ", nrow(produto1),
  " docentes com data de atualização\n",
  "Produto 2: ", arquivo2, "\n",
  "  ", nrow(artigos), " artigos (", nrow(registros), " registros nos CVs); ",
  nrow(verificar), " possíveis omissões; ",
  n_distinct(fusoes$id_artigo), " artigos com registros divergentes unidos"
)

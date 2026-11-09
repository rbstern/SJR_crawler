# Parâmetros da análise da produção do Departamento a partir do Lattes.
# Os caminhos são relativos à raiz do repositório: rode os scripts a partir dela.

# Planilha de entrada (.csv ou .xlsx): uma linha por docente, colunas
#   nome   - nome do docente (vira o nome da coluna no produto 2)
#   lattes - URL do currículo (http://lattes.cnpq.br/0123456789012345) ou o ID de 16 dígitos.
# Prefira a URL: aberto no Excel, um ID numérico de 16 dígitos é arredondado
# (o Excel guarda só 15 dígitos significativos) e o ID fica corrompido.
PLANILHA_DOCENTES <- "lattes/docentes.csv"

# Instalação do scriptLattes: git clone https://github.com/jpmenachalco/scriptLattes
# seguido de "make install" dentro do diretório clonado (requer Google Chrome ou Chromium).
# A variável de ambiente SCRIPTLATTES_DIR, se definida, tem precedência.
SCRIPTLATTES_DIR <- Sys.getenv("SCRIPTLATTES_DIR", unset = "~/scriptLattes")

# Python usado para rodar o scriptLattes. Por padrão, o venv criado pelo "make install";
# se ele não existir, o python3 do sistema. A variável de ambiente SCRIPTLATTES_PYTHON
# tem precedência sobre ambos.
SCRIPTLATTES_PYTHON <- Sys.getenv(
  "SCRIPTLATTES_PYTHON",
  unset = file.path(path.expand(SCRIPTLATTES_DIR), "venv", "bin", "python")
)

# Cada execução fica em DIR_EXECUCOES/AAAA-MM-DD/. Rodar de novo no mesmo dia
# reaproveita os CVs já baixados; num dia novo, tudo é baixado de novo (assim a
# data de última atualização reflete o Lattes do dia).
DIR_EXECUCOES <- "lattes/execucoes"

# Período dos artigos no produto 2 (NULL = sem limite).
ANO_INICIAL <- NULL
ANO_FINAL <- NULL

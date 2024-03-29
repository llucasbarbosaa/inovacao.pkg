library(logr)

#' Log Evento - Mensagem Única
#'
#' @param caminho Caminho onde o log deve ser salvo - string
#' @param msg Mensagem que deve ser salva no arquivo log - string
#' Default: ""
#' @param script Nome do script onde a função foi invocada. - string
#' Esse parâmetro serve para compor o nome do arquivo log que será gerado
#' Default: ""
#'
#' @return A função salva um arquivo log com cabeçalho e com a mensagem escolhida
#' O nome do arqivo gerado é composto da data e hora em que a função foi invocada e do parâmetro script
#' Nome do arquivo log gerado: aammdd-hhmmss-script.log
#'
#' @examples
#'
log_evento <- function(caminho, msg = "", script = ""){

# Obter a data atual
data_atual <- Sys.Date()
hora_atual <- format(Sys.time(), "%H%M%S")
data_formatada <- format(data_atual, "%y%m%d")
timestamp <- paste0(data_formatada, "-", hora_atual)

# Gera o nome do arquivo e o caminho final do aqruivo
file_name <- sprintf("%s-%s.log", timestamp, script)
file_path <- sprintf("%s%s", caminho, file_name)

# Abre log
lf <- logr::log_open(file_path)

# Envia a mensagem msg para o log
logr::log_print(msg)

# Fecha log
logr::log_close()

return <- sprintf("Log de %s salvo em %s", script, file_path)

return(return)
}

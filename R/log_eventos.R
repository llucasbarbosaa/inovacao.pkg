library(logr)
library(scriptName)

#' Log Eventos - Múltipas Mensagens
#'
#' @param caminho Caminho onde o log deve ser salvo - string
#' @param msgs Mensagens que devem ser salvas no arquivo log - list
#' Default: ""
#' @param script Nome do script onde a função foi invocada. - string
#' Esse parâmetro serve para compor o nome do arquivo log que será gerado
#' Default: ""
#'
#' @return A função salva um arquivo log com cabeçalho e com as mensagens escolhidas
#' O nome do arqivo gerado é composto da data e hora em que a função foi invocada e do parâmetro script
#' Nome do arquivo log gerado: aammdd-hhmmss-script.log
#'
#' @examples
#'
log_eventos <- function(caminho, msgs = c(""), script = ""){

  # Obter a data atual
  data_atual <- Sys.Date()
  hora_atual <- format(Sys.time(), "%H%M%S")
  data_formatada <- format(data_atual, "%y%m%d")
  timestamp <- paste0(data_formatada, "-", hora_atual)

  # Gera o nome do arquivo e o caminho final do aqruivo
  file_name <- sprintf("%s-%s.log", timestamp, script)
  file_path <- sprintf("%s%s", caminho, file_name)

  # Abre log
  lf <- log_open(file_path)

  # Envia a mensagem msg para o log
  for (m in (1:length(msgs))) {
    log_print(msgs[m])
  }

  # Fecha log
  log_close()

  return <- sprintf("Log de %s salvo em %s", script, file_path)

  return(return)
}

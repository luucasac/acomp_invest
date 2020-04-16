###############################################################################
############################## FUNCOES ARQUIVOS ###############################
###############################################################################

caminhoCarteira <- function(nome_carteira) {
  
  file <- paste0(
    "../bases/"
    # "./bases/"
    ,nome_carteira
    ,"/"
  )
  
  return(file)
}

salvarRDS.carteira <- function(df, nome_carteira, tp) {
  
  file <- paste0(
    caminhoCarteira(nome_carteira)
    ,tp
    ,".rds"
  )
  
  saveRDS(object = df, file = file)
  
  return("Concluido!")
}

carregarRDS.carteira <- function(tp, nome_carteira) {
  
  file <- paste0(
    caminhoCarteira(nome_carteira)
    ,tp
    ,".rds"
  )
  
  r <- readRDS(file = file)
  
  return(r)
}

criarCarteira <- function(nome_carteira) {
  
  carteira_path <- caminhoCarteira(nome_carteira)
  
  dir.create(
    path = carteira_path
    ,recursive = TRUE
  )
  
  operacoes <- data.frame(
    "cod_tit" = c("")
    ,"nome_tit" = c("")
    ,"cod_tit_yahoo" = c("")
    ,"dt_compra" = c("")
    ,"vl_compra" = c("")
    ,"qtd_cotas" = c("")
    ,"tp_invest" = c("")
    ,"vl_atual" = c("")
    ,"ult_atualizacao" = c("")
    ,"rendimento" = c("")
    ,"var_dia" = c("")
    ,stringsAsFactors = FALSE
  )
  
  carteira <- data.frame(
    "cod_tit" = c("")
    ,"nome_tit" = c("")
    ,"tp_tit" = c("")
    ,"vl_compra" = c("")
    ,"vl_atual" = c("")
    ,"var_dia" = c("")
    ,"qtd_cotas" = c("")
    ,"rendimento" = c("")
    ,stringsAsFactors = FALSE
  )
  
  series_historicas <- list()
  
  # log_carteira <- data.frame(
  #   "nome_carteira" = c(nome_carteira)
  #   ,
  #   ,stringsAsFactors = FALSE
  # )
  
  salvarRDS.carteira(series_historicas, nome_carteira, "series_historicas")
  salvarRDS.carteira(carteira, nome_carteira, "carteira")
  salvarRDS.carteira(operacoes, nome_carteira, "operacoes")
  
  return("Concluido!")
  
}

salvarCarteira <- function(nome_carteira, obj.reac) {
  
  salvarRDS.carteira(
    df = obj.reac$carteira.arquivos$carteira
    ,nome_carteira = nome_carteira
    ,tp = "carteira"
  )
  
  salvarRDS.carteira(
    df = obj.reac$carteira.arquivos$operacoes
    ,nome_carteira = nome_carteira
    ,tp = "operacoes"
  )
  
  salvarRDS.carteira(
    df = obj.reac$carteira.arquivos$series_historicas
    ,nome_carteira = nome_carteira
    ,tp = "series_historicas"
  )
  
  return("Salvo com sucesso!")
}
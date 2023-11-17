

get_acf <- function(dados, lag_max, coluna){
  dados_filtrados = dados %>% select({{coluna}})
  return(dados_filtrados %>% ACF(dados_filtrados,lag_max = lag_max))
}

sem <-function(x){lubridate::semester(x,with_year=TRUE)}


dados_filtrados_tempo <- function(dados, granularidade = "ano") {
  data <- dados %>% 
    filter(data_bo_reg != -1) %>% 
    mutate(data_bo_reg = ymd(data_bo_reg)) %>%  
    filter(year(data_bo_reg) >= 2003) %>% 
    filter(year(data_bo_reg) <= 2022)
  
  index_col <- switch(granularidade,
                      "ano" = year(data$data_bo_reg),
                      "sem" = semester(data$data_bo_reg),
                      "tri" = yearquarter(data$data_bo_reg),
                      "mes" = yearmonth(data$data_bo_reg),
                      "semana" = yearweek(data$data_bo_reg)
  )
  
  data %>% 
    select(data_bo_reg) %>% 
    mutate(granularidade = index_col) %>% 
    group_by(granularidade) %>% 
    summarise(total = n()) %>% 
    as_tsibble(index = granularidade)
  
  
}


########################

get_transformacoes <- function(series_temporal, var = "total") {
  
  lambda <- series_temporal %>% 
    features(total, features = guerrero)  %>% 
    pull(lambda_guerrero)
  
  series_temporal <- series_temporal %>%
    mutate(log = if (var == "log") log(total) else NULL,
           sqrt = if (var == "sqrt") sqrt(total) else NULL,
           inversa = if (var == "inversa") 1/total else NULL,
           box_cox = if (var == "box_cox") box_cox(total, lambda) else NULL)

  
  return(series_temporal)
}





get_difference <- function(serie_temporal, atraso, ordem, nome, var) {
  serie_temporal <- serie_temporal %>%
    mutate({{nome}} := !!sym(var) - lag(!!sym(var), atraso, default = NA))
  return(serie_temporal)
}
#####
teste_adf <- function(serie_temporal,var){
  adf_test  =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) %>% adf.test()
  print(adf_test)
  
}

teste_kpss <- function(serie_temporal,var){
  serie_temporal %>% features({{var}}, unitroot_kpss) }

teste_kpss_numero_diff <- function(serie_temporal,var){
  serie_temporal %>% features({{var}}, unitroot_ndiffs ) }


teste_kpss_season_diff <- function(serie_temporal,var){
  serie_temporal %>% features({{var}}, unitroot_nsdiffs ) }



teste_cox_stuart <- function(serie_temporal,var){
  cox_stuart =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) %>% cs.test()
  print(cox_stuart)
  
}

teste_kendall<- function(serie_temporal,var){
  kendall =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) %>% mk.test()
  print(kendall)
  
}


teste_WAVK <- function(serie_temporal,var){
  serie =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) 
  
  WAVK = notrend_test(serie, test = "WAVK", factor.length = "adaptive.selection")
  
  print(WAVK)}

teste_seasonal <- function(serie_temporal,var  ,teste = "combined"){
  frequencia  = serie_temporal %>% pull(index_time) %>% guess_frequency()
  sesonal =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) %>% isSeasonal(test = teste,freq = frequencia)
  print(sesonal)}

####
#teste <- dados_filtrados_tempo(homicidioDoloso_parcial, "mes")
#teste
#teste2 <- teste_kpss(teste, total)
#teste2
#
#teste3 <- teste_kpss_numero_diff(teste, total)
#teste3
#
#
#teste4 <- teste_kpss_season_diff(teste, total)
#teste4
#
#teste5 <- teste_cox_stuart(teste,total)
#teste5
#
#teste6 <- teste_kendall(teste,total)
#
#teste7<- teste_WAVK(teste,total)
#teste7
#
#teste8<- teste_seasonal(teste,total)

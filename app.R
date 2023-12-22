# Bibliotecas
library(shiny)
library(data.table)
library(shinyjs)
library(future)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(tidyverse)
library(fpp3)
library(readxl)
library(plotly)
library(knitr)
library(kableExtra)
library(readr)
library(ggpubr)
library(forecast)
library(tseries)
library(glue)
library(trend)
library(aTSA)
library(funtimes)
library(seasonal)
library(seastests)
library(future.apply)
library(shinycssloaders)
library(shinyalert)
library(zoo)
library(fabletools)
plan(multisession)


#install.packages("tseries",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("glue",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("trend",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("aTSA",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("forecast",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("funtimes",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")



vetor <- c(
  "id_bo"                  
  ,"id_laudo"     
  ,"datahora_iml_reg"         
  ,"index"                  
  ,"data_bo_reg"            
  ,"id_dp_reg"                  
  ,"data_ocorr"             
  ,"hora_ocorr"              
  ,"num_logradouro_ocorr"   
  ,"latitude_ocorr"         
  ,"longitude_ocorr"    
  ,"datahora_inicio_bo"          
  ,"datahora_bo_reg"     
  ,"marca_veiculo"          
  ,"cor_veiculo"            
  ,"cidade_veiculo"         
  ,"uf_veiculo"             
  ,"ano_fab_veiculo"        
  ,"ano_mod_veiculo"     
  ,"V1"                  
  ,"qtde_celular","marca_celular",
  "logradouro_ocorr")



# Vetor de interesse
vetor_interesse <- c("nome_dp_circ", "causa_morte_ocorr", "conclusao_ocorr", "idade_vitima",
                     "nome_dp_reg", "nome_municipio_circ", "nome_municipio_reg",
                     "descricao_local_ocorr", "vitima_fatal_ocorr", "tipo_pessoa",
                     "sexo_pessoa", "idade_pessoa", "cor_pele_pessoa", "profissao_pessoa",
                     "autoria_bo", "solucao_bo", "bairro_ocorr", "cidade_ocorr",
                     "flagrante_ocorr", "especie_ocorr", "status_ocorr", "desdobramento_ocorr",
                     "natureza_jur_ocorr", "tipo_veiculo", "coorp_envolvida_ocorr",
                     "coorp_situacao_ocorr", "descricao_conduta_ocorr",
                     "natureza_apurada_ocorr")


dicionario <- setNames(gsub("_", " ", gsub("ocorr", "", trimws(vetor_interesse))), vetor_interesse)

dicionario2 <- tibble(
  "Homicídio" = c('2017/01','2022/12'),
  "Feminicídio" = c('2015/04','2022/12'),
  "Latrocínio" = c('2018/01','2022/12'),
  "Lesão Corporal Seguida de Morte" = c('2016/09', '2022/12'),
  "Morte Decorrente de Intervenção Policial" =  c('2013/01', '2022/12'),
  "Morte Suspeita" = c('2013/01', '2022/12'),
  "Roubo de Celular" = c('2010/01', '2022/12'),
  "Roubo de Veículo" = c('2003/01' , '2022/12'),
  "Furto de Celular" = c('2010/01', '2022/12'),
  "Furto de Veículo" = c('2003/01' , '2022/12'),
  "IML (Instituto Médico Legal)" = c('2013/01', '2022/12'),
  "Dados Criminais" = c('2022/01' , '2022/12')
)


j <- c("-1","0", "1", "2", "3", "4", "5",
       "6", "7", "8", "9", "10", "11",
       "12", "13", "14", "15", "16", "17",
       "18", "19", "20", "21", "22", "23",
       "24", "25", "26", "27", "28", "29",
       "30", "31", "32", "33", "34", "35",
       "36", "37", "38", "39", "40", "41",
       "42", "43", "44", "45", "46", "47",
       "48", "49", "50", "51", "52", "53",
       "54", "56", "57", "58", "59", "60",
       "61", "62", "63", "64", "65", "66",
       "67", "68", "69", "70", "71", "72",
       "73", "74", "75", "76", "77", "78",
       "79", "80", "81", "84", "85", "86",
       "87", "88", "89", "90", "91", "92",
       "93", "95", "96", "98", "99", "100",
       "101", "102", "103", "105", "106", "108",
       "109", "110", "111", "112", "113", "116",
       "117", "119", "120", "121", "122", "123",
       "124", "125", "126", "129", "130", "131",
       "132", "133", "135", "136", "137", "138",
       "139", "140", "142", "146", "147", "150",
       "152", "154", "156", "157", "159", "160",
       "162", "165", "166", "169", "170", "171",
       "173", "176", "178", "180", "181", "182",
       "191", "195", "197", "198", "200", "203",
       "206", "208", "210", "213", "214", "216",
       "217", "218", "223", "240", "248", "249",
       "250", "254", "255", "256", "258", "259",
       "264", "265", "270", "274", "278", "280",
       "282", "286", "288", "293", "300", "303",
       "304", "310", "316", "319", "320", "340",
       "360", "361", "364", "373", "399", "400",
       "405", "417", "424", "426", "433", "470",
       "480", "498", "500", "501", "522", "542",
       "547", "560", "570", "600", "647", "649",
       "651", "697", "699", "700", "710", "749",
       "750", "798", "800", "860", "899", "900",
       "961", "1000", "1006", "1007", "1011", "1080",
       "1100", "1119", "1150", "1161", "1179", "1188",
       "1270", "1353", "1354", "1355", "1357", "1359",
       "1367", "1382", "1440", "1500", "1597", "1693",
       "1783", "1815", "1818", "1845", "1918", "1941",
       "1951", "1959", "1966", "1970", "1974", "1999",
       "2000", "2240", "2996", "3500", "3531", "3581",
       "4000", "4204", "6000", "9736", "9999", "17731",
       "1353133", "1358692", "9999999", "99999999", "135401508"
)


l<- c("-1","0", "1", "2", "3", "4", "5", "6",
      "7", "8", "9", "10", "11", "12", "13",
      "14", "15", "16", "17", "18", "19", "20",
      "21", "22", "23", "24", "25", "26", "27",
      "28", "29", "30", "31", "32", "33", "34",
      "35", "36", "37", "38", "39", "40", "41",
      "42", "43", "44", "45", "46", "47", "48",
      "49", "50", "51", "52", "53", "54", "55",
      "56", "57", "58", "59", "60", "61", "62",
      "63", "64", "65", "66", "67", "68", "69",
      "70", "71", "72", "73", "74", "75", "76",
      "77", "78", "79", "80", "81", "82", "83",
      "84", "85", "87", "88", "89", "90", "91",
      "92", "93", "94", "96", "97", "98", "99",
      "100", "101", "102", "103", "104", "106", "107",
      "109", "110", "111", "112", "113", "114", "115",
      "116", "117", "118", "119", "120", "121", "123",
      "124", "125", "126", "127", "128", "129", "130",
      "131", "132", "133", "134", "135", "137", "138",
      "140", "141", "142", "143", "144", "146", "148",
      "149", "150", "152", "153", "155", "156", "157",
      "159", "160", "162", "163", "166", "167", "168",
      "170", "171", "174", "175", "176", "178", "179",
      "180", "181", "182", "183", "184", "185", "186",
      "189", "190", "191", "192", "193", "195", "196",
      "198", "199", "200", "201", "204", "207", "210",
      "212", "213", "218", "219", "220", "221", "222",
      "226", "228", "229", "230", "231", "233", "239",
      "240", "249", "250", "251", "253", "256", "257",
      "258", "260", "264", "265", "266", "274", "280",
      "291", "297", "298", "300", "301", "305", "308",
      "309", "311", "314", "329", "330", "346", "348",
      "350", "370", "376", "378", "380", "384", "424",
      "433", "483", "486", "488", "492", "498", "499",
      "500", "542", "547", "551", "557", "600", "612",
      "621", "629", "660", "670", "672", "688", "700",
      "720", "730", "750", "751", "768", "779", "785",
      "799", "800", "804", "815", "850", "880", "899",
      "930", "938", "949", "999", "1000", "1011", "1050",
      "1080", "1100", "1111", "1115", "1117", "1119", "1145",
      "1169", "1197", "1210", "1213", "1281", "1300", "1351",
      "1353", "1354", "1355", "1358", "1404", "1470", "1500",
      "1513", "1530", "1550", "1553", "1560", "1612", "1615",
      "1650", "1654", "1665", "1677", "1679", "1707", "1729",
      "1733", "1741", "1777", "1786", "1798", "1800", "1804",
      "1808", "1819", "1854", "1867", "1871", "1880", "1883",
      "1906", "1920", "1957", "1965", "1966", "1968", "1971",
      "1980", "1981", "1989", "1990", "1996", "1999", "2000",
      "2199", "2500", "2751", "2800", "2944", "3000", "3100",
      "3465", "3551", "4400", "5000", "9413", "9579", "9675",
      "9844", "9860", "9999", "168455", "14728864")



Side_hom <- 
  fluidPage(
    #h3("Filtro dos dados"),
    # br(),
    dateRangeInput('dateRange2',
                   label = "Filtrar por data",
                   start = "2017-01-01", end = "2022-12-31",
                   min = "2017-01-01", max = "2022-12-31",
                   separator = " - ", format = "dd/mm/yy",
                   startview = 'year', language = 'pt-BR', weekstart = 1
    ),
    pickerInput(
      inputId = "cor_pele",
      label = "Cor da pele", 
      choices = c("amarela","branca","ignorada","não informado", "outros", "parda", "preta", "vermelha"),
      selected = c("amarela","branca","ignorada","não informado", "outros", "parda", "preta", "vermelha"),
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE
    ),
    pickerInput(
      inputId = "sexo",
      label = "Sexo", 
      choices = c("feminino","indefinido","masculino"),
      selected = c("feminino","indefinido","masculino"),
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE
    ),
    sliderInput("idade_slider", "Filtrar por Idade:",
                min = -1, max = 97, value = c(-1, 97)),
    actionButton("aplicar","Modificar os dados"),
    actionButton("reset","Não usar Filtro"))




Side_fem <- 
  fluidPage(
    
    
    dateRangeInput('dateRange2',
                   label = "Filtrar por data",
                   start = "2015-04-12", end = "2022-12-31",
                   min = "2015-04-12", max = "2022-12-31",
                   separator = " - ", format = "dd/mm/yy",
                   startview = 'year', language = 'pt-BR', weekstart = 1),
    pickerInput(
      inputId = "cor_pele",
      label = "Cor da pele", 
      choices = c("amarela","branca","ignorada","outros","parda","preta"),
      selected = c("amarela","branca","ignorada","outros","parda","preta"),
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE),
    sliderInput("idade_slider", "Filtrar por Idade:", min = -1, max = 87, value = c(-1,87)),
    actionButton("aplicar","Modificar os dados"),
    actionButton("reset","Não usar Filtro"))  



Side_lat <-  fluidPage( 
  
  
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2018-01-03", end = "2022-12-31",
                 min = "2018-01-03", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1),
  pickerInput(inputId = "cor_pele",
              label = "Cor da pele", 
              choices = c("amarela","branca","não informado","parda","preta","vermelha"),
              selected = c("amarela","branca","não informado","parda","preta","vermelha"),
              options = list(
                `actions-box` = TRUE), 
              multiple = TRUE),
  pickerInput(inputId = "sexo",
              label = "Sexo", 
              choices = c("feminino","indefinido","masculino"),
              selected = c("feminino","indefinido","masculino"),
              options = list(
                `actions-box` = TRUE), 
              multiple = TRUE),
  sliderInput("idade_slider", "Filtrar por Idade:",
              min = -1, max = 118, value = c(-1, 118)),
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

Side_lcs <- fluidPage(
  
  
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2016-09-14", end = "2022-12-31",
                 min = "2016-09-14", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1),
  pickerInput(
    inputId = "cor_pele",
    label = "Cor da pele", 
    choices = c("amarela","branca","ignorada","não informado","outros","parda","preta"),
    selected = c("amarela","branca","ignorada","não informado","outros","parda","preta"),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE),
  pickerInput(
    inputId = "sexo",
    label = "Sexo", 
    choices = c("feminino","indefinido","masculino"),
    selected = c("feminino","indefinido","masculino"),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  ),
  sliderInput("idade_slider", "Filtrar por Idade:",
              min = -1, max = 88, value = c(-1, 88)),
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  



Side_mdi <- fluidPage(
  
  
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2013-01-01", end = "2022-12-31",
                 min = "2013-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1),
  pickerInput(
    inputId = "cor_pele",
    label = "Cor da pele", 
    choices = c("amarela","branca","ignorada","não informado","outros","parda","preta"),
    selected = c("amarela","branca","ignorada","não informado","outros","parda","preta"),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE),
  pickerInput(
    inputId = "sexo",
    label = "Sexo", 
    choices = c("feminino","indefinido","masculino","não informado"),
    selected = c("feminino","indefinido","masculino","não informado"),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE),
  sliderInput("idade_slider", "Filtrar por Idade:",
              min = -1, max = 77, value = c(-1, 77)),
  
  fluidRow(
    column(7,pickerInput(
      inputId = "corp",
      label = "Situação do policial", 
      choices = c("folga","serviço"),
      selected = c("folga","serviço"),
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE
    ))),
  
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

Side_mor <- fluidPage(
  
  
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2013-01-01", end = "2022-12-31",
                 min = "2013-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1),
  pickerInput(
    inputId = "descr_local",
    label = "Descrição do local de ocorrência", 
    choices = c("area não ocupada","centro comerc./empresarial","comércio e serviços","condominio comercial","condominio residencial","entidade assistencial","escritório","estabelecimento bancário","estabelecimento de ensino","estabelecimento industrial","estabelecimento prisional","estacionamento particular","estrada de ferro","favela","garagem coletiva de prédio","garagem ou abrigo de residência","hospedagem","internet","lazer e recreação","local clandestino/ilegal","outros","repartição pública","residência","restaurante e afins","rodovia/estrada","saúde","serviços e bens públicos","shopping center","sindicato","templo e afins","terminal/estação","unidade rural","veículo em movimento","via pública"),
    selected = c("area não ocupada","centro comerc./empresarial","comércio e serviços","condominio comercial","condominio residencial","entidade assistencial","escritório","estabelecimento bancário","estabelecimento de ensino","estabelecimento industrial","estabelecimento prisional","estacionamento particular","estrada de ferro","favela","garagem coletiva de prédio","garagem ou abrigo de residência","hospedagem","internet","lazer e recreação","local clandestino/ilegal","outros","repartição pública","residência","restaurante e afins","rodovia/estrada","saúde","serviços e bens públicos","shopping center","sindicato","templo e afins","terminal/estação","unidade rural","veículo em movimento","via pública"),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE),
  pickerInput(
    inputId = "desdobramento",
    label = "Desdobramento da ocorrência", 
    choices = c(
      "i - encontro de cadáver sem lesões aparentes",
      "ii - dúvidas razoáveis quanto a suicídio ou morte provocada",
      "iii - morte acidental",
      "iv - morte súbita ou natural",
      "não informado"
    ),
    selected = c(
      "i - encontro de cadáver sem lesões aparentes",
      "ii - dúvidas razoáveis quanto a suicídio ou morte provocada",
      "iii - morte acidental",
      "iv - morte súbita ou natural",
      "não informado"
    ),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  ),
  sliderInput("idade_slider", "Filtrar por Idade:",
              min = -1, max = 123, value = c(-1,123)),
  
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

Side_iml <- fluidPage(
  
  
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2013-01-01", end = "2022-12-31",
                 min = "2013-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1 ),
  sliderTextInput("idade_slider2", "Filtrar por Idade:",
                  choices = as.character(c(-1:110,2015,2016)), selected = as.character(c(-1:110,2015,2016))[c(1,114)]  ),
  
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

Side_dad <- fluidPage(dateRangeInput('dateRange2',
                                     label = "Filtrar por data",
                                     start = "2022-01-01", end = "2022-12-31",
                                     min = "2022-01-01", max = "2022-12-31",
                                     separator = " - ", format = "dd/mm/yy",
                                     startview = 'year', language = 'pt-BR', weekstart = 1),
                      pickerInput(
                        inputId = "natureza",
                        label = "Natureza da ocorrência", 
                        choices = c(
                          "estupro",
                          "estupro de vulnerável",
                          "furto - outros",
                          "furto de carga",
                          "furto de veículo",
                          "homicídio doloso",
                          "homicídio doloso em estab. prisional",
                          "homicídio doloso por acidente de trânsito",
                          "latrocínio",
                          "lesão corporal seguida de morte",
                          "roubo - outros",
                          "roubo a banco",
                          "roubo de carga",
                          "roubo de veículo"
                        ),
                        selected = c(
                          "estupro",
                          "estupro de vulnerável",
                          "furto - outros",
                          "furto de carga",
                          "furto de veículo",
                          "homicídio doloso",
                          "homicídio doloso em estab. prisional",
                          "homicídio doloso por acidente de trânsito",
                          "latrocínio",
                          "lesão corporal seguida de morte",
                          "roubo - outros",
                          "roubo a banco",
                          "roubo de carga",
                          "roubo de veículo"
                        ),
                        options = list(
                          `actions-box` = TRUE), 
                        multiple = TRUE
                      ),
                      
                      pickerInput(
                        inputId = "descr_local",
                        label = "Descrição do local da ocorrência", 
                        choices = c(
                          "area não ocupada",
                          "centro comerc./empresarial",
                          "comércio e serviços",
                          "condominio comercial",
                          "condominio residencial",
                          "entidade assistencial",
                          "escritório",
                          "estabelecimento bancário",
                          "estabelecimento comercial",
                          "estabelecimento de ensino",
                          "estabelecimento industrial",
                          "estabelecimento prisional",
                          "estacionamento com vigilância",
                          "estacionamento particular",
                          "estacionamento público",
                          "estrada de ferro",
                          "favela",
                          "garagem coletiva de prédio",
                          "garagem ou abrigo de residência",
                          "hospedagem",
                          "internet",
                          "lazer e recreação",
                          "local clandestino/ilegal",
                          "não informado",
                          "outros",
                          "repartição pública",
                          "residência",
                          "restaurante e afins",
                          "rodovia/estrada",
                          "saúde",
                          "serviços e bens públicos",
                          "shopping center",
                          "sindicato",
                          "templo e afins",
                          "terminal/estação",
                          "unidade rural",
                          "veículo em movimento",
                          "via pública"
                        ),
                        selected = c(
                          "area não ocupada",
                          "centro comerc./empresarial",
                          "comércio e serviços",
                          "condominio comercial",
                          "condominio residencial",
                          "entidade assistencial",
                          "escritório",
                          "estabelecimento bancário",
                          "estabelecimento comercial",
                          "estabelecimento de ensino",
                          "estabelecimento industrial",
                          "estabelecimento prisional",
                          "estacionamento com vigilância",
                          "estacionamento particular",
                          "estacionamento público",
                          "estrada de ferro",
                          "favela",
                          "garagem coletiva de prédio",
                          "garagem ou abrigo de residência",
                          "hospedagem",
                          "internet",
                          "lazer e recreação",
                          "local clandestino/ilegal",
                          "não informado",
                          "outros",
                          "repartição pública",
                          "residência",
                          "restaurante e afins",
                          "rodovia/estrada",
                          "saúde",
                          "serviços e bens públicos",
                          "shopping center",
                          "sindicato",
                          "templo e afins",
                          "terminal/estação",
                          "unidade rural",
                          "veículo em movimento",
                          "via pública"
                        ),
                        options = list(
                          `actions-box` = TRUE), 
                        multiple = TRUE
                      ),
                      
                      actionButton("aplicar","Modificar os dados"),
                      actionButton("reset","Não usar Filtro"))  


Side_rouc <-  fluidPage(
  
  
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2010-01-01", end = "2022-12-31",
                 min = "2010-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1),
  
  
  sliderTextInput("quantidade_celular", "Filtrar por Idade:",
                  choices = l, selected = l[c(1,356)] ),
  
  
  pickerInput(
    inputId = "descr_local",
    label = "Descrição do local", 
    choices = c(
      "area não ocupada","carro forte", "centro comerc./empresarial", "comércio e serviços",
      "condominio residencial", "condominio comercial" , "entidade assistencial",
      "escritório", "estabelecimento bancário", "estabelecimento de ensino",
      "estabelecimento industrial", "estabelecimento prisional",
      "estacionamento com vigilância", "estacionamento particular",
      "estacionamento público", "estrada de ferro", "favela",
      "garagem coletiva de prédio", "garagem ou abrigo de residência", "hospedagem",
      "internet", "lazer e recreação", "local clandestino/ilegal", "não informado",
      "outros", "repartição pública", "residência", "restaurante e afins",
      "rodovia/estrada", "saúde", "serviços e bens públicos", "shopping center",
      "sindicato", "templo e afins", "terminal/estação", "unidade rural",
      "veículo em movimento", "via pública"
    ),
    selected = c(
      "area não ocupada","carro forte", "centro comerc./empresarial", "comércio e serviços",
      "condominio residencial", "condominio comercial", "entidade assistencial",
      "escritório", "estabelecimento bancário", "estabelecimento de ensino",
      "estabelecimento industrial", "estabelecimento prisional",
      "estacionamento com vigilância", "estacionamento particular",
      "estacionamento público", "estrada de ferro", "favela",
      "garagem coletiva de prédio", "garagem ou abrigo de residência", "hospedagem",
      "internet", "lazer e recreação", "local clandestino/ilegal", "não informado",
      "outros", "repartição pública", "residência", "restaurante e afins",
      "rodovia/estrada", "saúde", "serviços e bens públicos", "shopping center",
      "sindicato", "templo e afins", "terminal/estação", "unidade rural",
      "veículo em movimento", "via pública"
    ),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  ),
  
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

Side_furc <- fluidPage(
  
  
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2010-01-01", end = "2022-12-31",
                 min = "2010-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1),
  
  sliderTextInput("quantidade_celular", "Filtrar por Idade:",
                  choices = j, selected = j[c(1,282)] ),
  
  pickerInput(
    inputId = "descr_local",
    label = "Descrição do local", 
    choices = c(
      "area não ocupada", "carro forte", "centro comerc./empresarial", 
      "comércio e serviços", "condominio comercial", "condominio residencial", 
      "entidade assistencial", "escritório", "estabelecimento bancário", 
      "estabelecimento de ensino", "estabelecimento industrial", 
      "estabelecimento prisional", "estacionamento com vigilância", 
      "estacionamento particular", "estacionamento público", "estrada de ferro", 
      "favela", "garagem coletiva de prédio", "garagem ou abrigo de residência", 
      "hospedagem", "internet", "lazer e recreação", "local clandestino/ilegal", 
      "outros", "repartição pública", "residência", "restaurante e afins", 
      "rodovia/estrada", "saúde", "serviços e bens públicos", 
      "shopping center", "sindicato", "templo e afins", "terminal/estação", 
      "unidade rural", "veículo em movimento", "via pública"
    ),
    selected = c(
      "area não ocupada", "carro forte", "centro comerc./empresarial", 
      "comércio e serviços", "condominio comercial", "condominio residencial", 
      "entidade assistencial", "escritório", "estabelecimento bancário", 
      "estabelecimento de ensino", "estabelecimento industrial", 
      "estabelecimento prisional", "estacionamento com vigilância", 
      "estacionamento particular", "estacionamento público", "estrada de ferro", 
      "favela", "garagem coletiva de prédio", "garagem ou abrigo de residência", 
      "hospedagem", "internet", "lazer e recreação", "local clandestino/ilegal", 
      "outros", "repartição pública", "residência", "restaurante e afins", 
      "rodovia/estrada", "saúde", "serviços e bens públicos", 
      "shopping center", "sindicato", "templo e afins", "terminal/estação", 
      "unidade rural", "veículo em movimento", "via pública"
    ),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  ),
  
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

Side_rouv <- fluidPage(
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2003-01-01", end = "2022-12-31",
                 min = "2003-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1
  ),
  pickerInput(
    inputId = "tipo_veiculo",
    label = "Tipo de veiculo", 
    choices =c("automovel"         
               ,"bicicleta"        
               ,"bonde"             
               ,"caminhão"         
               ,"caminhão trator"   
               ,"caminhonete"      
               ,"camioneta"         
               ,"carro de mâo"     
               ,"carroça"           
               ,"chassi-plataforma"
               ,"ciclomoto"         
               ,"inexist."         
               ,"micro-onibus"      
               ,"motociclo"        
               ,"motoneta"          
               ,"motor casa"       
               ,"não informado"     
               ,"onibus"           
               ,"quadriciclo"       
               ,"reboque"          
               ,"semi-reboque"      
               ,"side-car"         
               ,"trator esteiras"   
               ,"trator misto"     
               ,"trator rodas"      
               ,"triciclo"         
               ,"utilitário"),
    selected = c("automovel"         
                 ,"bicicleta"        
                 ,"bonde"             
                 ,"caminhão"         
                 ,"caminhão trator"   
                 ,"caminhonete"      
                 ,"camioneta"         
                 ,"carro de mâo"     
                 ,"carroça"           
                 ,"chassi-plataforma"
                 ,"ciclomoto"         
                 ,"inexist."         
                 ,"micro-onibus"      
                 ,"motociclo"        
                 ,"motoneta"          
                 ,"motor casa"       
                 ,"não informado"     
                 ,"onibus"           
                 ,"quadriciclo"       
                 ,"reboque"          
                 ,"semi-reboque"      
                 ,"side-car"         
                 ,"trator esteiras"   
                 ,"trator misto"     
                 ,"trator rodas"      
                 ,"triciclo"         
                 ,"utilitário"),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  ),
  
  pickerInput(
    inputId = "ano_veiculo",
    label = "Ano do veiculo", 
    choices = c(
      "0", "10", "1000", "1007", "1010", "1011", "1014", "1034", "1039", "1111",
      "1112", "1113", "113", "1180", "12", "120", "1200", "1212", "124", "125",
      "1313", "1314", "1316", "1318", "1367", "1414", "1415", "1418", "150", "1501",
      "1513", "1517", "1519", "160", "1600", "1617", "1618", "1620", "1634", "1718",
      "1720", "180", "1900", "1914", "1938", "1959", "1960", "1961", "1962", "1963",
      "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973",
      "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983",
      "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993",
      "1994", "1995", "1996", "1997", "1998", "1999", "200", "2000", "2001", "2002",
      "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
      "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
      "2023", "2033", "205", "2055", "206", "2066", "207", "208", "209", "2099",
      "2102", "2107", "211", "2112", "2113", "2122", "217", "219", "2204", "2206",
      "2209", "2220", "230", "2301", "2322", "2422", "2423", "2425", "2428", "2429",
      "250", "265", "275", "283", "2912", "2913", "292", "2924", "300", "315", "323",
      "4", "4000", "412", "416", "4275", "4283", "4292", "5", "50", "5049", "506",
      "521", "5310", "580", "60", "600", "608", "65", "6997", "7", "7180", "750",
      "8", "802", "809", "85", "9", "910", "915", "93", "97", "98", "não informado"
    ),
    selected = c(
      "0", "10", "1000", "1007", "1010", "1011", "1014", "1034", "1039", "1111",
      "1112", "1113", "113", "1180", "12", "120", "1200", "1212", "124", "125",
      "1313", "1314", "1316", "1318", "1367", "1414", "1415", "1418", "150", "1501",
      "1513", "1517", "1519", "160", "1600", "1617", "1618", "1620", "1634", "1718",
      "1720", "180", "1900", "1914", "1938", "1959", "1960", "1961", "1962", "1963",
      "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973",
      "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983",
      "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993",
      "1994", "1995", "1996", "1997", "1998", "1999", "200", "2000", "2001", "2002",
      "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
      "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
      "2023", "2033", "205", "2055", "206", "2066", "207", "208", "209", "2099",
      "2102", "2107", "211", "2112", "2113", "2122", "217", "219", "2204", "2206",
      "2209", "2220", "230", "2301", "2322", "2422", "2423", "2425", "2428", "2429",
      "250", "265", "275", "283", "2912", "2913", "292", "2924", "300", "315", "323",
      "4", "4000", "412", "416", "4275", "4283", "4292", "5", "50", "5049", "506",
      "521", "5310", "580", "60", "600", "608", "65", "6997", "7", "7180", "750",
      "8", "802", "809", "85", "9", "910", "915", "93", "97", "98", "não informado"
    ),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  ),
  
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  
Side_furv <- fluidPage(
  
  
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2003-01-01", end = "2022-12-31",
                 min = "2003-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1
  ),
  pickerInput(
    inputId = "tipo_veiculo",
    label = "Tipo de veiculo", 
    choices = c("automovel"         
                ,"bicicleta"        
                ,"caminhão"          
                ,"caminhão trator"  
                ,"caminhonete"       
                ,"camioneta"        
                ,"carroça"           
                ,"charrete"         
                ,"chassi-plataforma" 
                ,"ciclomoto"        
                ,"inexist."          
                ,"micro-onibus"     
                ,"motociclo"         
                ,"motoneta"         
                ,"motor casa"        
                ,"não informado"    
                ,"onibus"            
                ,"quadriciclo"      
                ,"reboque"           
                ,"semi-reboque"     
                ,"side-car"          
                ,"trator esteiras"  
                ,"trator misto"      
                ,"trator rodas"     
                ,"triciclo"          
                ,"utilitário"),
    selected = c("automovel"         
                 ,"bicicleta"        
                 ,"caminhão"          
                 ,"caminhão trator"  
                 ,"caminhonete"       
                 ,"camioneta"        
                 ,"carroça"           
                 ,"charrete"         
                 ,"chassi-plataforma" 
                 ,"ciclomoto"        
                 ,"inexist."          
                 ,"micro-onibus"     
                 ,"motociclo"         
                 ,"motoneta"         
                 ,"motor casa"        
                 ,"não informado"    
                 ,"onibus"            
                 ,"quadriciclo"      
                 ,"reboque"           
                 ,"semi-reboque"     
                 ,"side-car"          
                 ,"trator esteiras"  
                 ,"trator misto"      
                 ,"trator rodas"     
                 ,"triciclo"          
                 ,"utilitário"),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  ),
  
  pickerInput(
    inputId = "ano_veiculo",
    label = "Ano do veiculo", 
    choices = c(
      -1, 1892, 1910, 1911, 1912, 1914, 1928, 1929, 1932, 1939, 1940,
      1944, 1945, 1946, 1948, 1949, 1950, 1951, 1952, 1954, 1955, 1957,
      1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968,
      1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979,
      1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
      1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
      2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
      2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
      2090
    ),
    selected = c(
      -1, 1892, 1910, 1911, 1912, 1914, 1928, 1929, 1932, 1939, 1940,
      1944, 1945, 1946, 1948, 1949, 1950, 1951, 1952, 1954, 1955, 1957,
      1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968,
      1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979,
      1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
      1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
      2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
      2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
      2090
    ),
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  ),
  
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

aplicarEstilosAoGrafico <- function(graficoId) {
  runjs(sprintf('
    var graficoDiv = document.getElementById("%s");
    graficoDiv.style.borderRadius = "50px";  // ou qualquer valor desejado
    graficoDiv.style.boxShadow = "8px 8px 16px #888888";
  ', graficoId))
}

get_acf <- function(dados, lag_max, coluna, tipo = "correlation"){
  
  dados_filtrados = dados %>% select({{coluna}})
  
  return(dados_filtrados %>% ACF(lag_max = lag_max,type = tipo))
}

dados_filtrados_tempo <- function(dados, granularidade = "ano",base) {
  filtro_1 <- dicionario2 %>% select({{base}}) %>% slice(1) %>% pull() %>% yearmonth()
  filtro_2 <-  dicionario2 %>% select({{base}}) %>% slice(2) %>% pull() %>% yearmonth()
  
  data <- dados %>% 
    filter(data_bo_reg != -1) %>% 
    mutate(data_bo_reg = ymd(data_bo_reg)) %>%  
    filter(yearmonth(data_bo_reg) >= filtro_1) %>% 
    filter(yearmonth(data_bo_reg) <= filtro_2)
  
  if(granularidade == "sem"){
    semestre_data = data %>%  select(data_bo_reg) %>% map( ~ semester(.x, with_year  = TRUE)) %>% unlist() %>% yearmon() %>% yearmonth()
    serie_semestre <- data %>% mutate(granularidade = semestre_data) %>%  group_by(granularidade)%>% summarise( total = n())
    
    invervalo_semestre = new_interval(month = 6)
    
    
    serie <-
      serie_semestre %>%
      build_tsibble(
        index = granularidade,
        interval = invervalo_semestre
      ) 
    
    
    
  }else{
    index_col <- switch(granularidade,
                        "ano" = year(data$data_bo_reg),
                        "tri" = yearquarter(data$data_bo_reg),
                        "mes" = yearmonth(data$data_bo_reg),
                        "semana" = yearweek(data$data_bo_reg)
    )
    
    serie <- data %>% 
      select(data_bo_reg) %>% 
      mutate(granularidade = index_col) %>% 
      group_by(granularidade) %>% 
      summarise(total = n()) %>% 
      as_tsibble(index = granularidade)
  }
  
  colnames(serie)[2] <- "total"
  serie <- serie %>% fill_gaps() %>% replace_na(list(total= 0))
  
  return(serie)
}




########################

get_transformacoes <- function(series_temporal, var = "total") {
  if(var == "box_cox"){
    lambda <- series_temporal %>% 
      features(total, features = guerrero)  %>% 
      pull(lambda_guerrero)}
  
  series_temporal <- series_temporal %>%
    mutate(log = if (var == "log") log(total) else NULL,
           sqrt = if (var == "sqrt") sqrt(total) else NULL,
           inversa = if (var == "inversa") 1/total else NULL,
           box_cox = if (var == "box_cox") box_cox(total, lambda) else NULL)
  if (var != "total"){
    series_temporal <- series_temporal[,-2]}
  
  return(series_temporal)
}





diff <- function(serie,lag_ , dif){
  serie_modificada = serie %>% mutate(transformacao = difference(!!sym(colnames(serie)[2]), lag = lag_ , differences = dif))
  if(colnames(serie_modificada)[2] == "transformacao"){
    return(serie_modificada)}
  else{
    return(serie_modificada %>% select(-2))}
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
  frequencia  = serie_temporal %>% pull(granularidade) %>% guess_frequency()
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




get_acf_sig <- function(serie_temporal, ci  = 0.95,var){
  lag_max = serie_temporal %>% pull(total) %>% length()
  
  ci = qnorm((1+0.95)/2)/sqrt(lag_max) # calcula o intervalo de confiança
  
  # autocorrelacao
  not_sig = !get_acf(serie_temporal,lag_max,var) %>% pull(acf) %>% between(-ci,ci) # calcula quem passou do intervalo de confiança
  sequencia = rle(not_sig) # faz o Run-length encoding (RLE) do vetor not_sig calcula o tamanho das sequencias de true e false
  
  #candidatos_MA = cumsum(sequencia$lengths)  %>% tibble() %>% slice( which(sequencia$values == TRUE) ) %>% pull() # candidatos 
  
  # autocorrelacao parcial
  
  not_sig_partial =  !get_acf(serie_temporal,lag_max,var,"partial") %>% pull(acf) %>% between(-ci,ci)
  sequencia_partial = rle(not_sig_partial) 
  
  #candidatos_AR = cumsum(sequencia_partial$lengths)  %>% tibble() %>% slice( which(sequencia_partial$values == TRUE) ) %>% pull()
  
  
  # suguestão para o user
  if(length(sequencia$lengths) == 2){
    modelo = glue('MA{sequencia$lengths[1]}')
    p = 0
    q = sequencia$lengths[1]
    
  } else if(length(sequencia_partial$lengths) == 2){
    modelo = glue('AR{sequencia_partial$lengths[1]}')
    q = 0
    p = sequencia$lengths[1]
    
  }else if(length(sequencia$lengths) >= 2 | length(sequencia_partial$lengths) >= 2){
    
    modelo = glue('ARMA({sequencia_partial$lengths[1]},{sequencia$lengths[1]})')
    q = sequencia$lengths[1]
    p = sequencia_partial$lengths[1]
  } else{ modelo = 'não foi possível selecionar um modelo'}
  
  # ajuste o modelo
  
  formula = as.formula(glue('{var} ~  0 + pdq({p},0,{q}) + PDQ(0,0,0)'))
  formula_arima = as.formula(glue('{var} ~  0 + PDQ(0,0,0)'))
  formula_PQD= as.formula(glue('{var} ~  0'))
  
  
  fit <- serie_temporal %>% 
    model(modelo_sugerido = ARIMA(formula),
          modelo_arima = ARIMA(formula_arima, stepwise = FALSE),
          modelo_arima_default = ARIMA(formula_PQD, stepwise = FALSE))
  
  
  
  return(fit)
  
  
}


plot_raiz <- function(fit,modelo){
  plot  = fit %>% select({{modelo}}) %>% pull() %>% gg_arma() + theme_pubclean() + ggtitle(glue('{modelo}')) + theme(plot.title = element_text(hjust = 0.5))
  return(plot)
  
}



plot_res <- function(fit,modelo){
  plot  = fit %>% select({{modelo}}) %>% pull() %>% gg_tsresiduals() + ggtitle(glue('{modelo}'))
  return(plot)
  
  
}



plot_predicao <- function(fit,modelo,h,serie){
  fit %>% select({{modelo}}) %>%  pull() %>% forecast::forecast(h = {{h}}) %>% autoplot(serie) +  theme_pubclean()
}



plot_report <- function(fit,modelo){
  fit %>% select({{modelo}}) %>% pull() %>% report() 
}


teste_ljung_box <- function(objeto_fit,modelo){
  modelo_selecionado <- objeto_fit %>% select({{modelo}}) %>% pull() %>% pull()
  p = modelo_selecionado[[1]]$fit$spec$p
  q = modelo_selecionado[[1]]$fit$spec$q
  P = modelo_selecionado[[1]]$fit$spec$P
  Q = modelo_selecionado[[1]]$fit$spec$Q
  freq = modelo_selecionado[[1]]$fit$spec$period
  df = p +q + P + Q
  objeto_fit %>% select({{modelo}}) %>% pull() %>% augment() %>% features(.innov, ljung_box, lag = 2*freq, dof = df)
  
  
  
}



######



get_pqd <- function(serie_temporal,d,ci  = 0.95){
  # intervalo de conf
  
  lag_max = serie_temporal %>% pull(transformacao) %>% length()
  if (d != 0){
    serie_temporal <- serie_temporal %>% mutate(transformacao = difference(transformacao, lag = 1 , differences = d))
  }
  
  ci = qnorm((1+0.95)/2)/sqrt(lag_max) 
  
  
  # autocorrelacao
  
  acf = get_acf(serie_temporal,lag_max,"transformacao") %>% pull(acf)
  not_sig = !acf %>%  between(-ci,ci) # calcula quem passou do intervalo de confiança
  sequencia = rle(not_sig) 
  q = sequencia$lengths[1]
  acf_truncado_acf <- acf[q:length(acf)]
  sinais_acf =  (acf_truncado_acf - lag(acf_truncado_acf))  %>% na.remove()
  sinais_obs_acf <- sum(sinais_acf > 0)
  p_valor_acf <- binom.test(sinais_obs_acf,length(sinais_acf))$p.value
  
  
  
  
  # auto autocorrelacao parcial
  pacf = get_acf(serie_temporal,lag_max,"transformacao", "partial") %>% pull(acf)
  not_sig_partial =  !pacf %>% between(-ci,ci)
  sequencia_partial = rle(not_sig_partial) 
  p = sequencia_partial$lengths[1]
  pacf_truncado_acf <- pacf[p:length(pacf)]
  sinais_pacf =  (pacf_truncado_acf - lag(pacf_truncado_acf))  %>% na.remove()
  sinais_obs_pacf <- sum(sinais_pacf > 0)
  p_valor_pacf <- binom.test(sinais_obs_pacf,length(sinais_pacf))$p.value
  
  arma = (p > 0) & (q > 0)
  if(arma){
    truncar = abs(p - q)
    pacf_truncado_arima <- pacf[truncar:length(pacf)]
    
    sinais_pacf_arima =  (pacf_truncado_arima - lag(pacf_truncado_arima))  %>% na.remove()
    sinais_obs_pacf_arima <- sum(sinais_pacf_arima > 0)
    p_valor_pacf_arima <- binom.test(sinais_obs_pacf_arima,length(sinais_pacf_arima))$p.value
    
    
    acf_truncado_arima <- acf[truncar:length(acf)]
    sinais_acf_arima =  (acf_truncado_arima - lag(acf_truncado_arima))  %>% na.remove()
    sinais_obs_acf_arima <- sum(sinais_acf_arima > 0)
    p_valor_acf_arima <- binom.test(sinais_obs_acf_arima,length(sinais_acf_arima))$p.value
  }
  
  
  
  
  
  
  # medias moveis
  if ((which(sequencia$values)[1] == 1) & (p_valor_pacf > 0.05) & !arma) {
    p = 0
    modelo = glue('pdq(0,{d},{q})')
  } else if ((which(sequencia_partial$values)[1] == 1) & (p_valor_acf > 0.05) & !arma) {
    q = 0
    modelo = glue('pdq({p},{d},0)')
  } else if (arma & (p_valor_pacf_arima > 0.05)) {
    modelo = glue('pdq({p},{d},{q})')
  } else {
    modelo = 'Não é possível encontrar um modelo'
  }
  
  ruido_branco = (p == 0) & (q==0)
  return(list('modelo' = modelo,
              'ruido_branco' = ruido_branco))
  
}









get_transformacoes_modelo <- function(serie,tipo_transformacao = "identidade"){
  if(tipo_transformacao == "box_cox"){lambda = serie %>% features(!!sym(colnames(serie)[2]), features = guerrero) %>% pull(lambda_guerrero)}
  serie_modificada = switch(tipo_transformacao,
                            
                            identidade = {
                              serie %>%
                                mutate(transformacao = !!sym(colnames(serie)[2]))
                            },
                            
                            
                            log = {
                              serie %>%
                                mutate(transformacao = log(!!sym(colnames(serie)[2])))
                            },
                            sqrt = {
                              serie %>%
                                mutate(transformacao = sqrt(!!sym(colnames(serie)[2])))
                            },
                            inversa = {
                              serie %>%
                                mutate(transformacao = 1/!!sym(colnames(serie)[2]))
                            },
                            box_cox = {
                              serie %>%
                                mutate(transformacao = box_cox(!!sym(colnames(serie)[2]), lambda))
                            },
  )
  return(serie_modificada)      
}




#<ARIMA(0,1,1)(1,0,1)[12]>
#PDQ

fit_serie <- function(serie,tipo_transformacao = 'identidade', d,dif ='1:2' , seasonal_dif = '0:1' ,p = '0:5',q = '0:5',P = '0:2',Q = '0:2',lag_seasonal_dif = "automatico",modelos){
  serie_modificada = get_transformacoes_modelo(serie,tipo_transformacao)
  
  
  pqd_modelo_sugerido =  get_pqd(serie_modificada,d=d)['modelo']
  if(pqd_modelo_sugerido['ruido_branco'] == "Verifique se a série é ruído branco")
  {return(123)}
  
  else{
    
    
    
    if(lag_seasonal_dif == "automatico"){lag_seasonal_dif =  serie_modificada %>% pull(granularidade) %>% guess_frequency() %>% floor() }
    
    
    
    if(tipo_transformacao == "box_cox"){
      lambda = serie %>% features(!!sym(colnames(serie)[2]), features = guerrero) %>% pull(lambda_guerrero)
      string_box_cox = glue('fabletools::box_cox(total, {lambda})')
    }
    
    # Ajuste dos modelos
    
    string_transformacao = switch(tipo_transformacao,
                                  identidade = 'total',
                                  box_cox = string_box_cox,
                                  sqrt = 'sqrt(total)',
                                  inversa = '1/(total + 1)',
                                  log = 'log(total + 1 )')
    
    #usuario
    
    PQD = ifelse(seasonal_dif == 0,'PDQ(0,0,0)', glue('PDQ({P},{seasonal_dif},{Q}, period = {lag_seasonal_dif})'))
    string_modelo_usuario= glue('0 + pdq({p},{dif},{q}) + {PQD}')
    formula_usuario = as.formula(paste(string_transformacao,'~',string_modelo_usuario))
    
    
    # modelo sugerido
    
    string_modelo_sugerido = glue('0 + {pqd_modelo_sugerido} + PDQ(0,0,0)')
    formula_modelo_sugerido = as.formula(paste(string_transformacao,'~',string_modelo_sugerido))
    
    
    # SARIMA
    
    formula_SARIMA = as.formula(glue('{string_transformacao} ~ 0'))
    
    
    # ARMA
    string_modelo_ARMA = glue('0 + pdq(1:5,0,1:5) + PDQ(0,0,0)')
    formula_modelo_ARMA= as.formula(paste(string_transformacao,'~',string_modelo_ARMA))
    
    # ARIMA
    
    string_modelo_ARIMA = glue('0 + pdq(1:5,1:2,1:5) + PDQ(0,0,0)')
    formula_modelo_ARIMA= as.formula(paste(string_transformacao,'~',string_modelo_ARIMA))
    
    
    fit_ARMA<- if ('arma' %in% modelos) {
      serie %>%
        model(arma = ARIMA(formula_modelo_ARMA, stepwise = FALSE))
    } else {
      NA
    }
    
    fit_ARIMA<- if ('arima' %in% modelos) {
      
      serie %>%
        model(arima = ARIMA(formula_modelo_ARIMA, stepwise = FALSE))
    } else {
      NA
    }
    
    
    
    fit_usuario <- if ('usuario' %in% modelos) {
      serie %>%
        model(usuario = ARIMA(formula_usuario, stepwise = FALSE))
    } else {
      NA
    }
    
    fit_ingenuo <- if ('modelo_pia' %in% modelos & pqd_modelo_sugerido != "Não é possível encontrar um modelo") {
      serie %>%
        model(modelo_pia = ARIMA(formula_modelo_sugerido, stepwise = FALSE))
    } else if ('modelo_pia' %in% modelos & pqd_modelo_sugerido == "Não é possível encontrar um modelo") {
      'Não foi possível encontrar um modelo'
    } else if (!('modelo_pia' %in% modelos)) {
      NA
    }
    
    fit_AUTO_sarima <- if ('auto_sarima' %in% modelos) {
      serie %>%
        model(auto_sarima = ARIMA(formula_SARIMA, stepwise = FALSE))
    } else {
      NA
    }
    
    
    
    
    fit = tibble(
      usuario = fit_usuario,
      modelo_pia = fit_ingenuo,
      arma = fit_ARMA,
      auto_sarima = fit_AUTO_sarima,
      arima = fit_ARIMA,
      
      .name_repair = c('minimal')
    )
    
    # para o relatório
    fit <- fit %>% select(!where(is.na))
    
    return(fit)
    
    
    # para o relatorio
    
  }
  
  
}

ruido_branco_func <- function(serie_temporal,ci = 0.95){
  
  lag_max = serie_temporal %>% pull(!!sym(colnames(serie_temporal)[2])) %>% length()
  ci = qnorm((1+0.95)/2)/sqrt(lag_max)
  acf = get_acf(serie_temporal,lag_max,colnames(serie_temporal)[2]) %>% pull(acf)
  
  not_sig = !acf %>%  between(-ci,ci) 
  
  
  ruido_branco = (TRUE %in% not_sig)
  
  
  return(!ruido_branco)
}

shapiro_test <- function(objeto_fit,modelo){
  objeto_fit %>% select({{modelo}}) %>% pull() %>%  augment() %>% pull(.innov) %>% shapiro.test()
}

check_model_2 <- function(objeto_fit,modelo){
  nulo <- objeto_fit %>% select({{modelo}}) %>% pull() %>% pull()
  return(class(nulo[[1]]$fit) == "null_mdl")
}


# Ui
ui <- dashboardPage(
  dashboardHeader(title = "Exemplo Shiny Dashboard",disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(
      useShinyjs(),
      tags$style(
        HTML(
          "
        .shiny-notification {height: 100px; width: 300px; font-size: 25px; color: black}
        
        
        .nav-tabs-custom {
          margin-bottom: 20px;
          background: #fff;
          box-shadow: 10px 10px 5px rgba(0,0,0,.1);
          border-top-left-radius: 20px; border-top-right-radius: 20px;
        border-bottom-left-radius: 20px; border-bottom-right-radius: 20px;
        }
         
         .nav-tabs-custom>.tab-content {
          border-bottom-right-radius: 20px;
          border-bottom-left-radius: 20px;
          }
    
        .box-header { color: white; border-top-left-radius: 20px; border-top-right-radius: 20px;
        border-bottom-left-radius: 20px; border-bottom-right-radius: 20px; background-color: #11104d; }
        
        
        .box {
          position: relative;
          border-radius: 20px;
          background: #fff;
          border-top: 3px solid #11104d;
          margin-bottom: 20px;
          width: 100%;
          box-shadow: 5px 5px 5px rgba(0,0,0,.1);
          }
        
        #filtro_box > .box-header { color: black; border-top-left-radius: 20px; border-top-right-radius: 20px;
        border-bottom-left-radius: 20px; border-bottom-right-radius: 20px; background-color: #FFFFFF; }
        
        
        #controls {
         position: absolute;
         top: 200px; /* Posição inicial no eixo Y */
         left: 1150px; /* Posição inicial no eixo X */
         width: 250px; /* Largura do painel */
         background-color: rgba(255, 255, 255, 0.25); /* Fundo semi-transparente */
         border: 1px solid #000000; /* Borda do painel */
         padding: 10px; /* Espaçamento interno */
          }
        .box-title {
            text-align: center;
        }
       
    .ul li.custom-tab1 {
      width: 150px;
    }
    
    .ul li.custom-tab2 {
      width: 250px;
    }'
    
       .meu-codigo {
        display: block;
        padding: 9.5px;
        margin: 0 0 10px;
        margin-top: 10px;
        font-size: 13px;
        line-height: 1.6; 
        word-break: break-all;
        word-wrap: break-word;
        white-space: pre-wrap;
        background-color: #F5F5F5;
        border: 1px solid rgba(0, 0, 0, 0.15);
        border-radius: 4px;
        font-family: Arial, sans-serif; 
      }
      .meu-codigo ul {
        margin-top: 10px; 
        list-style-type: disc; 
        padding-left: 20px; 
        margin-bottom: 10px; /* Reduz o espaçamento abaixo da lista */
      }
      .meu-codigo ul li {
        font-size: 14px; 
        margin-bottom: 5px; /* Reduz o espaçamento entre os itens da lista */
      }
      .meu-codigo p {
        margin-top: 5px; /* Reduz o espaçamento acima dos parágrafos */
        margin-bottom: 5px; /* Reduz o espaçamento abaixo dos parágrafos */
      }
        
        
      hr {border-top: 1px solid #000000;}
      .custom-button {
      width: 150px;
      height: 40px;
      margin-bottom: 15px;
      background-color: #11104d;
      color: white;
      border: 2px solid #000000;
      border-radius: 5px;
      padding: 10px;
      font-size: 15px;
      font-family: inherit;
      cursor: pointer;
      }
      
      html, body, .container.fluid {
        width: 100vw;
        min-height: 100vh;
        
      }
      
     
      
      .page-layout {
        display: grid;
        grid-template-columns: minmax(0, 1fr);
        grid-template-rows: 80px 1fr;
        grid-template-areas:
            'header'
            'main ';
        height: 100vh;
        width: 100%;
        max-width: 100vw;
        overflow-x: hidden;
      }
      
      .page-header {
        grid-area: header;
      }
      .page-main {
        grid-area: main;
        transition: opacity .5s ease;
        visibility: none;
        z-index: -1;
        opacity: 0;
      }
      
      .main-layout {
        display: grid;
        grid-template-columns: minmax(0, 1fr);
        grid-template-rows: auto minmax(0, 1fr);
        grid-template-areas:
          'sidebar'
          'main';
        height: 100%;
        width: 100%;
        padding: 20px;
        gap: 20px;
      }
      @media only screen and (min-width: 800px) {
        .main-layout {
          grid-template-columns: minmax(0, 350px) minmax(0, 1fr);
          grid-template-rows: minmax(0, 1fr);
          grid-template-areas:
            'sidebar main';
        }
      }
      
      .card-style {
        border: 1px solid #eaeaea;
        padding: 20px;
        border-radius: 5px;
        box-shadow: rgb(0 0 0 / 10%) 3px 8px 12px;
        overflow: hidden;
      }
      .no-padding {
        padding: 0;
      }
      
      .main-main {
        grid-area: main;
      }
      .main-sidebar {
        grid-area: sidebar;
      }
      
      #map-map {
        width: 100% !important;
        height: 100% !important;
      }
      
      .navbar-top {
        display: grid;
        grid-template-columns: 140px 1fr auto 1fr 150px 40px;
        grid-template-rows: minmax(0, 1fr);
        grid-template-areas:
          'logo ... menu ... dataset info';
        height: 100%;
        width: 100%;
        padding: 20px;
        gap: 20px;
        
      }
      @media only screen and (max-width: 600px) {
        .navbar-top {
          grid-template-columns: 140px 1fr 1fr 85px;
          grid-template-rows: minmax(0, 1fr) minmax(0, 1fr);
          grid-template-areas:
            'logo ...   ...     info'
            'menu menu  dataset dataset';
          padding: 0;
          gap: 0;
        }
      }
      
      .navbar-top {
        background-color: #11104d;
        }
      
      .navigation-logo {
        grid-area: logo;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .navigation-menu {
        grid-area: menu;
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 10px;
      }
      @media only screen and (max-width: 600px) {
        .navigation-menu {
          justify-content: flex-start;
        }
      }
      
      .navigation-dataset {
        grid-area: dataset;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .navigation-dataset .form-group {
        margin: 0;
      }
      
      .navigation-info {
        grid-area: info;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      
      #MAPA {height: calc(100vh - 80px) !important;}
      
      
      
      .sidebarPanel {
      background-color: #FFC0CB
      }
      
      
      .tabbable > .nav > li > a{
      background-color:#FFFFFF;
      color: black
      }
      
      body {background-color: #FFFFFF;}
     
      
      "
        )
        
      ),
      tags$script(
        '
      Shiny.addCustomMessageHandler("scrollCallback",
        function(color) {
          var objDiv = document.getElementById("teste_scroll2");
          objDiv.scrollTop = objDiv.scrollHeight;
        }
      );'
      ),
      
      # Cabeçalho da página
      tags$div(
        # Logotipo
        tags$div(
          tags$a(
            shiny::imageOutput("image",  width = "60px", height = "60px")  # Exibe a imagem renderizada pelo servidor
          ),
          id = "logo-top",
          class = "navigation-logo"
        ),
        
        # Botões de navegação
        tags$div(
          tags$div(
            actionButton(
              inputId = "inicio",
              label = "Início",
              class = "navbar-button",
              icon = icon("home"),
              value = 1
            ),
            id = "div-navbar-inicio"
          ),
          
          tags$div(
            actionButton(
              inputId = "consulta",
              label = "Dados",
              class = "navbar-button",
              icon = icon("table")
            ),
            id = "div-navbar-como-usar"
          ),
          tags$div(
            actionButton(
              inputId = "map",
              label = "Mapa",
              class = "navbar-button",
              icon = icon("map")
            ),
            id = "div-navbar-map"
          ),
          tags$div(
            actionButton(
              inputId = "series",
              label = "Séries Temporais",
              class = "navbar-button",
              icon = icon("clock")
            ),
            id = "div-navbar-series"
          ),
          tags$div(
            actionButton(
              inputId = "decomp",
              label = "Modelo",
              class = "navbar-button",
              icon = icon("warning")
            ),
            id = "div-navbar-pred"
          ),
          
          id = "div-navbar-tabs",
          class = "navigation-menu"
        ),
        # Ícone de informações  
        tags$div(
          actionButton(
            inputId = "info",
            label = NULL,
            icon = icon("info-sign", lib = "glyphicon"),
            class = "navbar-info"
          ),
          class = "navigation-info"
        ),
        
        id = "div-navbar",
        class = "navbar-top page-header card-style"
      ),
      div(id="scroll_teste2",
          uiOutput("conteudo"))
    )
    
  )
)



#Server


server <- function(input, output, session) {
  #### Variaveis reativas (variaveis globais que podem trocar de valores com a interação do usuário)
  estado <- reactiveVal("inicio")
  tema <- reactiveVal(NULL)
  dado1 <- reactiveVal(NULL)
  dado2 <- reactiveVal(NULL)
  dado3 <- reactiveVal(NULL)
  dado4 <- reactiveVal(NULL)
  dado5 <- reactiveVal(NULL)
  dado6 <- reactiveVal(NULL)
  dado7 <- reactiveVal(NULL)
  dado8 <- reactiveVal(NULL)
  dado9 <- reactiveVal(NULL)
  dado10 <- reactiveVal(NULL)
  dado11 <- reactiveVal(NULL)
  dado12 <- reactiveVal(NULL)
  
  top_info <- reactiveVal(NULL)
  
  dado_real <- reactiveVal(NULL)
  dado_filtrado <- reactiveVal(NULL)
  fit <- reactiveVal(NULL)
  
  Side1 <- reactiveVal(NULL)
  m <- reactiveVal(NULL)
  
  startApp <- reactiveVal(TRUE)
  
  lista <- reactiveVal(list("inicial"))
  Lista_Trans <- reactiveVal(list(NULL))
  Lista_Serie <- reactiveVal(list(NULL))
  Lista_atributo <- reactiveVal(NULL)
  Lista_Tempo <- reactiveVal(c("semanal","mensal","trimestral","anual"))
  Lista_Diff <- reactiveVal(list(NULL))
  Serie_Atual <- reactiveVal(NULL)
  Serie_Atual_Modelo <- reactiveVal(NULL)
  Serie_Tempo <- reactiveVal(NULL)
  Serie_Transformacao  <- reactiveVal(NULL)
  Granularidade  <- reactiveVal(NULL)
  Graph_Tempo <- reactiveVal(NULL)
  lista_atributos_decomp <- reactiveVal(NULL)
  
  ###reativas da série -- armazenar infos ###
  armazem_granularidade <- reactiveVal(NULL)
  armazem_transformacao <- reactiveVal(NULL)
  armazem_diff1 <- reactiveVal(NULL)
  armazem_lag1 <- reactiveVal(NULL)
  armazem_ordem1 <- reactiveVal(NULL)
  armazem_diff2 <- reactiveVal(NULL)
  armazem_lag2 <- reactiveVal(NULL)
  armazem_ordem2 <- reactiveVal(NULL)
  armazem_serie_original <- reactiveVal(NULL)
  
  ############# Logo da plataforma
  output$image <- shiny::renderImage({
    filename <- "ssp.png"  # Caminho para a imagem
    list(src = filename, contentType = "image/png")
  })
  ################## Desabilitando os botões de navegação
  # shinyjs::disable("exp")
  # shinyjs::disable("diference")
  shinyjs::disable("consulta")
  shinyjs::disable("map")
  shinyjs::disable("series")
  shinyjs::disable("decomp")
  
  ##### oBServes inciaiis
  
  
  observe({
    dado_filtrado(dado_real())
  })
  
  observeEvent(input$button1, {
    estado("consulta")
    shinyalert(
      title = "Aviso!",
      text = "Modificações dos dados serão propagados para todas as análises.
            Para retornar e utilizar os dados originais clique em *Não usar Filtro ",
      type = "info",
      confirmButtonCol = "#11104d")
  })
  
  
  
  criarObservador <- function(inputId, estado) {
    observeEvent(input[[inputId]], {
      estado(inputId)
    })
  }
  
  criarObservador2 <- function(inputId, tema) {
    observeEvent(input[[inputId]], {
      tema(inputId)
      shinyjs::enable("consulta")
      shinyjs::enable("map")
      shinyjs::enable("series")
      ##################### Inicio 
      
      
      
      output$inicial2 <- renderUI({ 
        div(id="teste_scroll",
            fluidPage(
              fluidRow(
                column(10, align = "center", h1(paste0(tema()), style = "text-align: center;")),
                actionButton("button1", "Visualizar dados", class = "custom-button")
              ),
              
              if (tema() == "IML (Instituto Médico Legal)") {
                shinyjs::disable("map")
                Side1(Side_iml)
                
                if (is.null(dado7())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 3)
                  dado7(fread("iml_parcial.csv"))
                  dado7(dado7() %>% mutate(data_ocorr = as.Date(datahora_iml_reg)))
                  dado7(dado7() %>% mutate(data_bo_reg = as.Date(datahora_iml_reg),
                                           idade_pessoa = idade_vitima))
                }
                dado_real(dado7())
                box(width = NULL,
                    fluidPage(
                      HTML("Os dados apresentam informações básicas sobre todas as 
                               entradas de óbitos no IML desde janeiro de 2013 a dezembro de 2022, quando foi implantado no Estado, pela Superintendência da Polícia 
                               Técnico-Científica, o serviço digital Gestão de Laudos (GDL). (Fonte: GDL da SPTC).")
                    ) )
                
              }
              
              
              else if (tema() == "Latrocínio") {
                Side1(Side_lat)
                
                
                if (is.null(dado3())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 1)
                  dado3(fread("latrocinio_parcial.csv"))
                }
                
                dado_real(dado3())
                box(width = NULL,
                    fluidPage(
                      HTML("
                  <p>O latrocínio é um crime hediondo descrito pelo parágrafo 3 do artigo 157 do código penal, é definido como a subtração de coisa móvel alheia mediante grave ameaça ou violência a pessoa, em que da violência resultar lesão corporal grave ou morte, sem agravantes ou alterações de pena descritas.</p>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de janeiro de 2018 a dezembro de 2022.</p>
                ") ) )
              }
              
              
              else if (tema() == "Lesão Corporal Seguida de Morte") {
                Side1(Side_lcs)
                
                if (is.null(dado4())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 1)
                  dado4(fread("lesaoCSM_parcial.csv"))
                }
                
                dado_real(dado4())
                box(width = NULL,
                    fluidPage(
                      HTML("
                  <p>A lesão corporal seguida de morte é descrita pelo parágrafo 3 do artigo 129 do código penal, definida pela ofensa à integridade corporal ou saúde de outrem tendo por resultado a morte, com as circunstâncias evidenciando que o agente não quis o resultado, nem assumiu o risco de produzi-lo.</p>
                  <p>É considerado agravado quando:</p>
                  <ul>
                    <li>Se a lesão for praticada contra ascendente, descendente, irmão, cônjuge ou companheiro, ou com quem conviva ou tenha convivido, ou, ainda, prevalecendo-se o agente das relações domésticas, de coabitação ou de hospitalidade</li>
                    <li>Se a lesão for praticada contra autoridade ou agente descrito nos arts. 142 e 144 da Constituição Federal, integrantes do sistema prisional e da Força Nacional de Segurança Pública, no exercício da função ou em decorrência dela, ou contra seu cônjuge, companheiro ou parente consanguíneo até terceiro grau, em razão dessa condição, a pena é aumentada de um a dois terços</li>
                    <li>Se a lesão for praticada contra a mulher, por razões da condição do sexo feminino</li>
                  </ul>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de setembro de 2016 a dezembro de 2022</p>
                ") )  )
              }
              
              
              else if (tema() == "Roubo de Veículo") {
                Side1(Side_rouv)
                
                
                if (is.null(dado11())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 10)
                  dado11(fread("rouboVeiculo_parcial.csv"))
                  dado11(  dado11() %>%
                             mutate(data_bo_reg = as.Date(datahora_bo_reg),
                                    nome_municipio_circ=cidade_ocorr))
                }
                
                dado_real(dado11())
                
                box(width = NULL,
                    fluidPage(
                      HTML("
                  <p>O roubo é descrito no artigo 157 do Código Penal, é caracterizado pela subtração de bem material mediante grave ameaça ou violência, é agravado quando:</p>
                  <ul>
                    <li>Há o concurso de duas ou mais pessoas</li>
                    <li>A vítima está em serviço de transporte de valores e o agente conhece tal circunstância</li>
                    <li>A subtração for de veículo automotor que venha a ser transportado para outro Estado ou para o exterior</li>
                    <li>O agente mantém a vítima em seu poder, restringindo sua liberdade</li>
                    <li>A subtração for de substâncias explosivas ou de acessórios que, conjunta ou isoladamente, possibilitem sua fabricação, montagem ou emprego</li>
                    <li>A violência ou grave ameaça é exercida com emprego de arma branca ou arma de fogo</li>
                    <li>Há destruição ou rompimento de obstáculo mediante o emprego de explosivo ou de artefato análogo que cause perigo comum</li>
                  </ul>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de janeiro de 2003 a dezembro de 2022 </p>
                ") ) )
                
                
                
              }
              
              
              else if (tema() == "Furto de Veículo") {
                Side1(Side_furv)
                
                if (is.null(dado12())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 10)
                  dado12(fread("furtoVeiculo_parcial.csv"))
                  dado12(  dado12() %>%
                             mutate(data_bo_reg = as.Date(datahora_bo_reg),
                                    nome_municipio_circ=cidade_ocorr))
                  
                }
                
                dado_real(dado12())
                box(width = NULL,
                    fluidPage(
                      HTML("
                  <p>O furto é descrito no artigo 155 do Código Penal, é caracterizado pela subtração de bem material alheio (destacando-se a ausência de ameaça grave ou violência), é agravado se:</p>
                  <ul>
                    <li>O crime é praticado durante o repouso noturno</li>
                    <li>O crime é cometido com destruição ou rompimento de obstáculo à subtração da coisa</li>
                    <li>O crime é cometido com abuso de confiança, ou mediante fraude, escalada ou destreza</li>
                    <li>O crime é cometido com emprego de chave falsa</li>
                    <li>O crime é cometido mediante concurso de duas ou mais pessoas</li>
                    <li>Houver emprego de explosivo ou de artefato análogo que cause perigo comum</li>
                    <li>O furto mediante fraude é cometido por meio de dispositivo eletrônico ou informático, conectado ou não à rede de computadores, com ou sem a violação de mecanismo de segurança ou a utilização de programa malicioso, ou por qualquer outro meio fraudulento análogo</li>
                    <li>O crime é praticado mediante a utilização de servidor mantido fora do território nacional</li>
                    <li>O crime é praticado contra idoso ou vulnerável</li>
                    <li>A subtração for de veículo automotor que venha a ser transportado para outro Estado ou para o exterior</li>
                    <li>A subtração for de semovente domesticável de produção, ainda que abatido ou dividido em partes no local da subtração</li>
                    <li>A subtração for de substâncias explosivas ou de acessórios que, conjunta ou isoladamente, possibilitem sua fabricação, montagem ou emprego</li>
                  </ul>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de janeiro de 2003 a dezembro de 2022</p>
                ") ))
                
                
              }
              
              
              else if (tema() == "Morte Decorrente de Intervenção Policial") {
                Side1(Side_mdi)
                
                if (is.null(dado5())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 1)
                  dado5(fread("morteDIP_parcial.csv"))
                }
                
                dado_real(dado5())
                
                box(width = NULL,
                    fluidPage(
                      HTML("
                  <p>A morte decorrente de intervenção policial é um crime militar impróprio, tipificado no artigo 205 artigo 9º, inciso II, alínea b da Constituição Federal de 1988, caracterizado pelos abusos cometidos pelas forças policiais.</p>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a janeira de 2013 a dezembro de 2022</p>
                ")  ) )
              }
              
              
              else if (tema() == "Morte Suspeita") {
                Side1(Side_mor)
                
                if (is.null(dado6())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 2)
                  dado6(fread("morteSuspeita_parcial.csv"))
                  dado6(dado6() %>% mutate(data_bo_reg = as.Date(datahora_bo_reg)))
                }
                
                dado_real(dado6())
                box(width = NULL,
                    fluidPage(
                      HTML("
                  <p>A morte suspeita é definida nos incisos I a IV, do Artigo 2º, da Portaria DGP nº 14/2005, descrita como:</p>
                  <ol>
                    <li>Encontro de cadáver sem lesões aparentes: Encontro de cadáver, ou parte relevante deste, em qualquer estágio de decomposição, no qual existam lesões aparentes ou quaisquer outras circunstâncias que, mesmo indiciariamente, apontem para a produção violenta da morte</li>
                    <li>Dúvidas razoáveis quanto a suicídio ou morte provocada: Morte violenta em que subsistam dúvidas razoáveis quanto a tratar-se de suicídio ou morte provocada por outrem</li>
                    <li>Morte acidental: Morte não natural onde existam indícios de causação acidental do evento exclusivamente por ato não intencional da própria vítima</li>
                    <li>Morte súbita e natural: Morte súbita, sem causa determinante aparente, ocorrida de modo imprevisto, com a vítima fora do respectivo domicílio e sem a assistência de médico, familiar ou responsável</li>
                  </ol>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a janeiro de 2013 a dezembro de 2022</p>
                ")))
                
              }
              
              
              else if (tema() == "Dados Criminais") {
                Side1(Side_dad)
                
                if (is.null(dado8())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 4)
                  dado8(fread("dadosCriminais_parcial.csv", header = TRUE))
                }
                
                dado_real(dado8())
                
                box(width = NULL,
                    fluidPage(
                      HTML("Contém todos os dados de boletins de ocorrência, dos temas presentes, do ano de 2022.")  )
                    
                )
              }
              
              else if (tema() == "Roubo de Celular") {
                Side1(Side_rouc)
                
                if (is.null(dado9())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 20)
                  dado9(fread("rouboCelular_parcial.csv"))
                  k <- dado9()
                  k$qtde_celular <- ifelse(k$qtde_celular == "não informado",-1,k$qtde_celular)
                  dado9(k)
                  dado9(  dado9() %>%
                            mutate(data_bo_reg = as.Date(datahora_bo_reg),
                                   nome_municipio_circ=cidade_ocorr,
                                   qtde_celular = as.integer(qtde_celular)
                            ))
                  
                }
                
                dado_real(dado9())
                box(width = NULL,
                    fluidPage(
                      HTML("
                  <p>O roubo é descrito no artigo 157 do Código Penal, é caracterizado pela subtração de bem material mediante grave ameaça ou violência, é agravado quando:</p>
                  <ul>
                    <li>Há o concurso de duas ou mais pessoas</li>
                    <li>A vítima está em serviço de transporte de valores e o agente conhece tal circunstância</li>
                    <li>A subtração for de veículo automotor que venha a ser transportado para outro Estado ou para o exterior</li>
                    <li>O agente mantém a vítima em seu poder, restringindo sua liberdade</li>
                    <li>A subtração for de substâncias explosivas ou de acessórios que, conjunta ou isoladamente, possibilitem sua fabricação, montagem ou emprego</li>
                    <li>A violência ou grave ameaça é exercida com emprego de arma branca ou arma de fogo</li>
                    <li>Há destruição ou rompimento de obstáculo mediante o emprego de explosivo ou de artefato análogo que cause perigo comum</li>
                  </ul>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de janeiro de 2010 a dezembro de 2022</p>
                ")   ))
                
              }
              
              
              else if (tema() == "Furto de Celular") {
                Side1(Side_furc)
                column(8,
                       textOutput("furto_texto")
                )
                
                if (is.null(dado10())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 20)
                  dado10(fread("furtoCelular_parcial.csv"))
                  k <- dado10()
                  k$qtde_celular <- ifelse(k$qtde_celular == "não informado",-1,k$qtde_celular)
                  dado10(k)
                  dado10(  dado10() %>%
                             mutate(qtde_celular = as.integer(qtde_celular),
                                    data_bo_reg = as.Date(datahora_bo_reg),
                                    nome_municipio_circ=cidade_ocorr))
                  
                }
                
                dado_real(dado10())
                
                output$furto_texto <- renderUI({
                  box(width = NULL,
                      fluidPage(
                        
                        HTML("
                  <p>O furto é descrito no artigo 155 do Código Penal, é caracterizado pela subtração de bem material alheio (destacando-se a ausência de ameaça grave ou violência), é agravado se:</p>
                  <ul>
                    <li>O crime é praticado durante o repouso noturno</li>
                    <li>O crime é cometido com destruição ou rompimento de obstáculo à subtração da coisa</li>
                    <li>O crime é cometido com abuso de confiança, ou mediante fraude, escalada ou destreza</li>
                    <li>O crime é cometido com emprego de chave falsa</li>
                    <li>O crime é cometido mediante concurso de duas ou mais pessoas</li>
                    <li>Houver emprego de explosivo ou de artefato análogo que cause perigo comum</li>
                    <li>O furto mediante fraude é cometido por meio de dispositivo eletrônico ou informático, conectado ou não à rede de computadores, com ou sem a violação de mecanismo de segurança ou a utilização de programa malicioso, ou por qualquer outro meio fraudulento análogo</li>
                    <li>O crime é praticado mediante a utilização de servidor mantido fora do território nacional</li>
                    <li>O crime é praticado contra idoso ou vulnerável</li>
                    <li>A subtração for de veículo automotor que venha a ser transportado para outro Estado ou para o exterior</li>
                    <li>A subtração for de semovente domesticável de produção, ainda que abatido ou dividido em partes no local da subtração</li>
                    <li>A subtração for de substâncias explosivas ou de acessórios que, conjunta ou isoladamente, possibilitem sua fabricação, montagem ou emprego</li>
                  </ul>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de janeiro de 2010 a dezembro de 2022</p>
                "))
                      
                  )
                })
                
              }
              
              
              else if (tema() == "Homicídio") {
                Side1(Side_hom)
                
                
                if (is.null(dado1())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 1)
                  dado1(fread("homicidioDoloso_parcial.csv"))
                }
                
                dado_real(dado1())
                box(width = NULL,
                    fluidPage(
                      
                      HTML("
                <p>O homicídio doloso é descrito pelo artigo 121 do Código Penal, além de suas qualificações e alterações de pena. O crime é descrito pelo assassinato intencional de uma pessoa física por outra, constando agravantes tais como:</p>
                <ul>
                  <li>Motivação fútil</li>
                  <li>Mediante paga ou promessa de recompensa, com emprego de veneno, fogo, explosivo, asfixia, tortura ou outro meio insidioso ou cruel, ou de que possa resultar perigo comum</li>
                  <li>À traição, de emboscada, ou mediante dissimulação ou outro recurso que dificulte ou torne impossível a defesa do ofendido</li>
                  <li>Para assegurar a execução, a ocultação, a impunidade ou vantagem de outro crime</li>
                  <li>Contra a mulher por razões da condição de sexo feminino (feminicídio)</li>
                  <li>Contra autoridade ou agente descrito nos artigos 142 e 144 da Constituição Federal, integrantes do sistema prisional e da Força Nacional de Segurança Pública, no exercício da função ou em decorrência dela, ou contra seu cônjuge, companheiro ou parente consanguíneo até terceiro grau, em razão dessa condição</li>
                  <li>Com emprego de arma de fogo de uso restrito ou proibido</li>
                  <li>Contra menor de 14 anos</li>
                </ul>
                <p>A base traz algumas dessas qualificações incluindo feminicídio, trazendo boletins de janeiro de 2017 a dezembro de 2022.</p>
              ")
                      
                    )
                    
                )
                
                
              }
              
              
              
              else if (tema() == "Feminicídio") {
                Side1(Side_fem)
                
                
                if (is.null(dado2())) {
                  showNotification("Carregando os dados. Aguarde!",type = "message",duration = 1)
                  dado2(fread("feminicidio_parcial.csv"))
                  dado2(dado2() %>% mutate(data_bo_reg = as.Date(datahora_bo_reg)))
                }
                
                dado_real(dado2())
                box(width = NULL,
                    fluidPage(
                      
                      HTML("
                  <p>O feminicídio, tal qual o homicídio, é descrito pelo artigo 121 do Código Penal, é definido como o assassinato de uma mulher em razão das condições do sexo feminino, de forma que o homicídio torna-se feminicídio quando, em conjunto ao assassinato, há violência doméstica ou familiar e menosprezo ou discriminação à condição da mulher, tendo como agravantes quando o crime é praticado:</p>
                  <ul>
                    <li>Durante a gestação ou nos 3 meses posteriores ao parto</li>
                    <li>Contra pessoa menor de 14 (catorze) anos, maior de 60 (sessenta) anos, com deficiência ou portadora de doenças degenerativas que acarretem condição limitante ou de vulnerabilidade física ou mental</li>
                    <li>Na presença física ou virtual de descendente ou de ascendente da vítima</li>
                    <li>Em descumprimento das medidas protetivas de urgência previstas nos incisos I, II e III do caput do art. 22 da Lei nº 11.340, de 7 de agosto de 2006</li>
                  </ul>
                  <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de abril de 2015 a dezembro de 2022.</p>
                "))          )
                
                
                
              }
              
              
            )
        )
      })
    })
  }
  ###################### Observadores para o servidor saber onde está e onde é foi clicado
  
  criarObservador("inicio", estado)
  criarObservador("series", estado)
  criarObservador("map", estado)
  criarObservador("diference", estado)
  criarObservador("info", estado)
  criarObservador("exp", estado)
  criarObservador("consulta", estado)
  criarObservador("decomp", estado)
  
  
  
  criarObservador2("Homicídio", tema)
  criarObservador2("Feminicídio", tema)
  criarObservador2("Latrocínio", tema)
  criarObservador2("Lesão Corporal Seguida de Morte", tema)
  criarObservador2("Morte Decorrente de Intervenção Policial", tema)
  criarObservador2("Morte Suspeita", tema)
  criarObservador2("IML (Instituto Médico Legal)", tema)
  criarObservador2("Dados Criminais", tema)
  criarObservador2("Roubo de Celular", tema)
  criarObservador2("Furto de Celular", tema)
  criarObservador2("Roubo de Veículo", tema)
  criarObservador2("Furto de Veículo", tema)
  
  
  
  render_initial_content <- function(){
    fluidPage(
      h2("Escolha um tema"),
      br(),
      fluidRow(
        column(width = 3,
               box(
                 title = "Crimes contra a vida",
                 width = NULL,
                 div(
                   tags$ul(
                     lapply(c("Homicídio", "Feminicídio", "Latrocínio", "Lesão Corporal Seguida de Morte"), function(crime) {
                       tags$li(actionLink(crime, crime, class = "action-button-scroll"))
                     })
                   )
                 )
               )
        ),
        column(width = 3,
               box(
                 title = "Crimes contra o patrimonio",
                 width = NULL,
                 div(
                   tags$ul(
                     lapply(c("Roubo de Celular", "Furto de Celular", "Roubo de Veículo", "Furto de Veículo"), function(crime) {
                       tags$li(actionLink(crime, crime, class = "action-button-scroll"))
                     })
                   )
                 )
               )
        ),
        column(width = 3,
               box(
                 title = "Intervenção Policial",
                 width = NULL,
                 div(
                   tags$ul(
                     lapply(c("Morte Decorrente de Intervenção Policial"), function(crime) {
                       tags$li(actionLink(crime, crime, class = "action-button-scroll"))
                     })
                   )
                 )
               )
        ),
        column(width = 3,
               box(
                 title = "Outros",
                 width = NULL,
                 div(
                   tags$ul(
                     lapply(c("Morte Suspeita", "IML (Instituto Médico Legal)", "Dados Criminais"), function(crime) {
                       tags$li(actionLink(crime, crime, class = "action-button-scroll"))
                     })
                   )
                 )
               )
        )
        
      ))
    
  }
  
  observe({
    shinyjs::runjs('
          $(".action-button-scroll").click(function() {
            var target = $(this).parent().next();
            $("html, body").animate({
              scrollTop: target.offset().top
            }, 500);
          });
        ')
  })
  
  output$inicial <- renderUI({
    render_initial_content()
  })
  
  observeEvent(input$inicio,{
    output$inicial <- renderUI ({
      render_initial_content()
      
    })
  })
  
  observeEvent(input$tes,{
    output$inicial <- renderUI ({
      render_initial_content()
      
    })
  })
  
  observe({
    # Verifica se é o início da aplicação
    if (is.null(input$inicio) && startApp()) {
      # Aciona a lógica desejada
      print("Botão Clicado ao iniciar a aplicação")
      # Atualiza a variável reativa para evitar repetições
      startApp(FALSE)
    }
  })
  
  observe(input$inicio)
  
  
  ############### O que é mostrado para cada 
  
  output$conteudo <- renderUI({
    conteudo <- switch(estado(),
                       "inicio" = {
                         fluidPage(
                           fluidRow(
                             h1("Painel Interativo de Análises (PIA)"),
                             br(),
                             column(width = 4,
                                    
                                    box(title = "Sobre",
                                        width=NULL,
                                        solidHeader = TRUE,
                                        header = tags$div( class = "box-header"),
                                        "O PIA é o resultado de um projeto em grupo da disciplina de Séries Temporais. 
                                                Esse painel foi desenvolvido com base nos dados fornecidos pela Secretaria de
                                                Segurança Pública do Estado de São Paulo ao longo dos anos.")),
                             column(width =4,
                                    box(title = "Objetivos",
                                        width = NULL,
                                        "Essa plataforma visa tornar acessível a análise de dados, permitindo que pessoas de diferentes
                                                campos e níveis explorem os temas de maneira intuitiva e personalizada. Como resultado
                                                a teoria é transportada para a prática.")),
                             column(width = 4,
                                    box(title = "Ferramentas",
                                        width = NULL,
                                        "Aqui você encontra ferramentas que permitem uma análise exploratória por meio de gráficos, visualizações
                                                espaciais, além de conter todo um aparato relacionado a séries temporais permitindo não só a visualização de diferentes maneiras de uma série temporal como também a criação de modelos de predição "))
                           ),
                           fluidRow(
                             uiOutput("inicial"),
                             uiOutput("inicial2"))
                         )
                         
                       },
                       
                       "series" = {
                         fluidPage(
                           fluidRow(
                             column(2,
                                    verticalLayout(
                                      h1(tema()),
                                      #top_info(),
                                      
                                      box(width = NULL,
                                          title= "Opções",
                                          
                                          pickerInput(
                                            inputId = "cont_filtro",
                                            label = "Selecione o tempo",
                                            choices = Lista_Tempo(),
                                            options = list(
                                              `actions-box` = TRUE),
                                            multiple = FALSE 
                                          ),
                                          pickerInput(
                                            inputId = "cont_trans",
                                            label = "Selecione a transformação da série",
                                            choices = c("identidade","log","sqrt","inversa","box_cox"),
                                            options = list(
                                              `actions-box` = TRUE),
                                            multiple = FALSE, 
                                            selected = "total"  
                                          )),
                                      
                                      box(width = NULL,
                                          title="Diferenciação",
                                          materialSwitch(
                                            inputId = "Id077",
                                            label = "Diferenciar", 
                                            value = FALSE,
                                            status = "primary"
                                          ),
                                          fluidRow(
                                            column(6,numericInput("serie_ordem", "Ordem", 1, min = 1, max = 2, step = NA))),
                                      ),
                                      box(width = NULL,
                                          title = "Diferenciação Sazonal",
                                          materialSwitch(
                                            inputId = "Id078",
                                            label = "Diferenciar", 
                                            value = FALSE,
                                            status = "primary"
                                          ),
                                          fluidRow(column(6,numericInput("serie_ordem2", "Ordem", 1, min = 1, max = 2, step = NA)),
                                                   column(6,numericInput("serie_lag2", "Lag Sazonal", 1, min = 1, max = 99, step = NA)))
                                          
                                      )
                                      
                                    )),
                             column(8,
                                    tabBox(width = NULL,
                                           id = "tabs",
                                           tabPanel("Gráfico no Tempo",
                                                    withSpinner(
                                                      plotlyOutput("cont",height = "600px"), type = 6),
                                           ),
                                           tabPanel("Gráfico de Sazonalidade",
                                                    withSpinner(
                                                      plotlyOutput("cont2",height = "600px"), type = 6),
                                           ),
                                           tabPanel("Gráfico de Defasagens",
                                                    withSpinner(
                                                      plotlyOutput("cont3",height = "600px"), type =6),
                                           ),
                                           tabPanel("Autocorrelação",
                                                    value = "autoc",
                                                    withSpinner(
                                                      plotlyOutput("auto1"),type = 6),
                                                    hr(),
                                                    withSpinner(
                                                      plotlyOutput("auto2"),type = 6)),
                                           
                                           tabPanel("Decomposição",
                                                    withSpinner(
                                                      plotlyOutput("decomp_serie",height = "600px"),type = 6)))),
                             column(2,
                                    verticalLayout(
                                      box(width = NULL,
                                          title = "Testes",
                                          div(
                                            tags$ul(
                                              lapply(c("ADF","KPSS","Cox_Stuart","Kendall","Sazonal"), function(bolinha){
                                                tags$li(actionLink(bolinha, bolinha))
                                              })
                                            ))),
                                      box(width = NULL,
                                          title = "Modelo",
                                          div(tags$ul(
                                            tags$li(
                                              actionLink("gerar_modelo","Criação de Modelos")))))
                                      
                                    ))
                           ))
                       },
                       
                       "map" = {
                         fluidRow(
                           box(width = 12,
                               withSpinner(
                                 leafletOutput("MAPA"),type = 6)
                           )
                         ) },
                       
                       "decomp" = {
                         fluidPage(
                           
                           fluidRow(
                             column(2,
                                    verticalLayout(
                                      h1(tema()),
                                      
                                      box(width = NULL,
                                          title = "Modelos",
                                          materialSwitch(
                                            inputId = "Modelo_Usuario",
                                            label = "Modelo Usuário", 
                                            value = FALSE,
                                            status = "primary"),
                                          h5("Intervalos de busca"),
                                          fluidRow(column(6,sliderInput("pzin", "p", value = c(0,5), min = 0, max = 5, step = NA,ticks = FALSE)),
                                                   column(6,sliderInput("pzao", "P", value = c(0,5), min = 0, max = 5, step = NA,ticks = FALSE))),
                                          fluidRow(column(6,sliderInput("qzin", "q", value = c(0,5), min = 0, max = 5, step = NA,ticks = FALSE)),
                                                   column(6,sliderInput("qzao", "Q", value = c(0,5), min = 0, max = 5, step = NA,ticks = FALSE))),
                                          fluidRow(column(6,sliderInput("dzin", "d", value = c(0,2), min = 0, max = 2, step = NA,ticks = FALSE)),
                                                   column(6,sliderInput("dzao", "D", value = c(0,2), min = 0, max = 2, step = NA,ticks = FALSE))),
                                          
                                          materialSwitch(
                                            inputId = "Modelo_Ingenuo",
                                            label = "Modelo Pia", 
                                            value = FALSE,
                                            status = "primary"
                                          ),
                                          
                                          materialSwitch(
                                            inputId = "Modelo_Sarima",
                                            label = "Modelo Auto SARIMA", 
                                            value = FALSE,
                                            status = "primary"
                                          ),
                                          
                                          materialSwitch(
                                            inputId = "Modelo_Arma",
                                            label = "Modelo Auto Arma", 
                                            value = FALSE,
                                            status = "primary"
                                          ),
                                          
                                          materialSwitch(
                                            inputId = "Modelo_Arima",
                                            label = "Modelo Auto Arima", 
                                            value = FALSE,
                                            status = "primary"
                                          ),
                                          actionButton("sugerido","Gerar Modelos"),
                                          pickerInput(
                                            inputId = "model",
                                            label = "Selecione o modelo",
                                            choices = NULL,
                                            options = list(
                                              `actions-box` = TRUE),
                                            multiple = FALSE 
                                          )
                                      ))),
                             column(8,
                                    tabBox( width = NULL,
                                            tabPanel("modelos",
                                                     withSpinner(
                                                       verbatimTextOutput("summary2"),type = 6)),
                                            
                                            tabPanel("Raízes características",
                                                     withSpinner(
                                                       plotOutput("plot1", height = "600px"),type = 6)),
                                            
                                            tabPanel("Resíduos",
                                                     withSpinner(
                                                       plotOutput("plot2", height = "600px"),type = 6)),
                                            
                                            tabPanel("Predição",
                                                     withSpinner(
                                                       plotOutput("plot3", height = "600px"),type = 6)),
                                            
                                            tabPanel("Summary",
                                                     withSpinner(
                                                       verbatimTextOutput("plot4"),type = 6)),
                                            
                                    )),
                             column(2,
                                    verticalLayout(
                                      box(width = NULL,
                                          title ="Predição",
                                          column(6,
                                                 numericInput("lag_pred","Horizonte", value = 12, min = 0, max = 52))),
                                      box(width = NULL,
                                          title = "Testes",
                                          div(tags$ul(tags$li(actionLink("test7","Ljung Box")),
                                                      tags$li(actionLink("test8","Shapiro wilk"))))),
                                      
                                      box(width = NULL, title = "Relatório",
                                          div(tags$ul(tags$li(
                                            actionLink("teste1","Gerar Relatório")))))
                                    ))
                             
                             
                           ))
                       },
                       
                       "consulta" = {
                         fluidPage(
                           
                           fluidRow(
                             column(3,
                                    verticalLayout(
                                      top_info(),
                                      hr(),
                                      box(
                                        width = NULL,
                                        tabsetPanel(width = NULL,
                                                    tabPanel(
                                                      'Gráficos',
                                                      fluidPage(
                                                        br(),
                                                        pickerInput(
                                                          inputId = "Var1",
                                                          label = "Selecione a Variável 1:",
                                                          choices = setdiff(colnames(dado_real()),vetor),
                                                          options = list(
                                                            `actions-box` = TRUE),
                                                          selected = "cor_pele_pessoa"
                                                          
                                                        ),
                                                        pickerInput(
                                                          inputId = "Var2",
                                                          label = "Selecione a Variável 2:",
                                                          choices = c("Nenhuma",setdiff(colnames(dado_real()),vetor)),
                                                          options = list(
                                                            `actions-box` = TRUE)
                                                          
                                                        )
                                                        
                                                      )  
                                                    ),
                                                    tabPanel("Tabela",
                                                             fluidPage(
                                                               pickerInput(
                                                                 inputId = "colunas_selecionadas",
                                                                 label = "Selecione as colunas:",
                                                                 choices = colnames(dado_real()),
                                                                 options = list(
                                                                   `actions-box` = TRUE),
                                                                 multiple = TRUE, 
                                                                 selected = colnames(dado_real())  
                                                               ),
                                                               
                                                               downloadButton("download_full","Download tema.csv"),
                                                               
                                                               
                                                               downloadButton("download_filtrado","Download tema_filtrado.csv")
                                                               
                                                             )  )
                                                    
                                        )
                                      ))),
                             
                             column(9,
                                    tabBox(id = "tab_consulta",
                                           width = NULL,
                                           tabPanel("Gráficos",
                                                    withSpinner(
                                                      plotlyOutput("graficos", height = "600px"),type = 6)
                                                    
                                           ),
                                           tabPanel("Tabela",
                                                    withSpinner(
                                                      DTOutput("tabela_1"), type = 6))
                                    )
                             )
                             
                             
                             
                           ),
                         )
                         
                       },
                       "exp" = {
                         fluidPage(
                           h2("Nada por enquanto")
                         )
                         
                       },
                       fluidRow(
                         column(12, h1("Integrantes do projeto")),
                         fluidRow(
                           column(
                             2,
                             offset = 1,
                             box(
                               width = NULL,
                               title = "Anderson Lavinsky",
                               img(src = "https://i.postimg.cc/FsCwfjxy/Vo.jpg", width = 200, height = 200),
                               
                               HTML('<i class="fab fa-github"></i>'),
                               a("GitHub", href = "link_do_github_integrante1"),
                               br(),
                               HTML('<i class="fab fa-linkedin"></i>'),
                               a("LinkedIn", href = "link_do_linkedin_integrante1"),
                               h4("Maiores contribuições:"),
                               h6("Pré-processamento dos dados")
                               
                             )
                           ),
                           column(
                             2,
                             box(
                               width = NULL,
                               title = "Gabriel de Almeida",
                               img(src = "https://moinhoglobo.com.br/wp-content/uploads/2016/03/44-p%C3%A3o-de-queijo-768x432.jpg", width = 200, height = 200),
                               
                               HTML('<i class="fab fa-github"></i>'),
                               a("GitHub", href = "link_do_github_integrante2"),
                               br(),
                               HTML('<i class="fab fa-linkedin"></i>'),
                               a("LinkedIn", href = "link_do_linkedin_integrante2"),
                               h4("Maiores contribuições:"),
                               h6("Pré-processamento dos dados")
                             )
                           ),
                           column(
                             2,
                             box(
                               width = NULL,
                               title = "Pedro Henrique Freitas Maiorano",
                               img(src = "https://media.licdn.com/dms/image/D4D03AQF1PYImCh6rPw/profile-displayphoto-shrink_800_800/0/1689926093491?e=1708560000&v=beta&t=CC_rE7Sw5a2uY0uh6vXcoXCZwT_W3MdAH7gSWwZaDN8", width = 200, height = 200),
                               
                               HTML('<i class="fab fa-github"></i>'),
                               a("GitHub", href = "link_do_github_integrante3"),
                               br(),
                               HTML('<i class="fab fa-linkedin"></i>'),
                               a("LinkedIn", href = "link_do_linkedin_integrante3"),
                               h4("Maiores contribuições:"),
                               h6("Análise Exploratória"),
                               h6("Mapa"),
                               h6("Criação e desenvolvimento da plataforma"),
                               h6("Criação das ferramentas da plataforma de séries temporais"),
                               h6("Criação das ferramentas de modelo")
                             )
                           ),
                           column(
                             2,
                             box(
                               width = NULL,
                               title = "Vinicius Teixeira",
                               img(src = "https://i.postimg.cc/153L5WDR/vinicius.jpg", width = 200, height = 200),
                               
                               HTML('<i class="fab fa-github"></i>'),
                               a("GitHub", href = "link_do_github_integrante4"),
                               br(),
                               HTML('<i class="fab fa-linkedin"></i>'),
                               a("LinkedIn", href = "link_do_linkedin_integrante4"),
                               h4("Maiores contribuições:"),
                               h6("Pré-processamento dos dados"),
                               h6("Pré-processamento dos dados")
                             )
                           ),
                           column(
                             2,
                             box(
                               width = NULL,
                               title = "Yan Kohler",
                               img(src = "https://i.postimg.cc/GmBVvHMZ/yan.jpg", width = 200, height = 200),
                               
                               HTML('<i class="fab fa-github"></i>'),
                               a("GitHub", href = "link_do_github_integrante5"),
                               br(),
                               HTML('<i class="fab fa-linkedin"></i>'),
                               a("LinkedIn", href = "link_do_linkedin_integrante5"),
                               h4("Maiores contribuições:"),
                               h6("Pré-processamento dos dados")
                             )
                           )
                         )
                       )
    )
    
    
    return(conteudo)
  })
  
  top_info <- reactive({
    fluidPage(
      h1(tema()),
      box(id="filtro_box",width = NULL,title = "Filtro dos dados", collapsible = TRUE,
          header = tags$div( class = "box-header"),
          collapsed = TRUE, Side1()))
  })
  observeEvent(input$toggle_sidebar, {
    if (shinyjs::hasClass("sidebarPanelId", "collapsed")) {
      shinyjs::removeClass("sidebarPanelId", "collapsed")
    } else {
      shinyjs::addClass("sidebarPanelId", "collapsed")
    }
  })
  
  
  modal_state <- reactiveVal(FALSE)
  
  observeEvent(input$consulta, {
    
    shinyalert(
      title = "Aviso!",
      text = "Modificações dos dados serão propagados para todas as análises.
          Para retornar e utilizar os dados originais clique em *Não usar Filtro ",
      type = "info",
      confirmButtonCol = "#11104d")
    
  })
  
  
  observeEvent(input$close_popup, {
    removeModal()
    modal_state(FALSE)
  })
  
  
  
  observeEvent(input$aplicar, {
    ##limpar
    faixa_etaria <- NULL
    data <- NULL
    cor <- NULL
    tveiculo <- NULL
    anov <- NULL
    qtd_cel <- NULL
    descr_local <- NULL
    natureza <- NULL
    sexo <- NULL
    corp <- NULL
    ##
    faixa_etaria <- input$idade_slider
    idade1 <- as.integer((input$idade_slider2)[1])
    idade2 <- as.integer((input$idade_slider2)[2])
    data <- input$dateRange2
    cor <- input$cor_pele
    tveiculo <- input$tipo_veiculo
    anov <- input$ano_veiculo
    qtd_cel1 <- as.integer((input$quantidade_celular)[1])
    qtd_cel2 <- as.integer((input$quantidade_celular)[2])
    descr_local <- input$descr_local
    natureza <- input$natureza  
    sexo <- input$sexo
    corp <- input$corp 
    
    desdobramento <- input$desdobramento
    
    if (!is.null(faixa_etaria) || !is.null(data) || !is.null(cor) || !is.null(tveiculo) || !is.null(anov) || !is.null(qtd_cel) || !is.null(descr_local) || !is.null(natureza) || !is.null(sexo) || !is.null(desdobramento) || !is.null(corp)) {
      
      filtro <- TRUE
      
      if (!is.null(data)) {
        filtro <- filtro & (dado_real()$data_bo_reg >= data[1] & dado_real()$data_bo_reg <= data[2])
      }
      
      if(tema()== "Homicídio" | tema()=="Feminicídio" | tema()=="Latrocínio" | tema()=="Lesão Corporal Seguida de Morte" | tema()=="Morte Decorrente de Intervenção Policial"){
        if (!is.null(cor)) {
          filtro <- filtro & (dado_real()$cor_pele %in% cor)
        }}
      
      if(tema()== "Homicídio" | tema()=="Feminicídio" | tema()=="Latrocínio" | tema()=="Lesão Corporal Seguida de Morte" | tema()=="Morte Decorrente de Intervenção Policial" | tema()=="Morte Suspeita"){
        if (!is.null(faixa_etaria)) {
          filtro <- filtro & (dado_real()$idade_pessoa >= faixa_etaria[1] & dado_real()$idade_pessoa <= faixa_etaria[2])
        }}
      
      if(tema()== "IML (Instituto Médico Legal)"){
        if (!is.null(idade1) & !is.null(idade2) ) {
          filtro <- filtro & (dado_real()$idade_pessoa >= idade1 & dado_real()$idade_pessoa <= idade2)
        }}
      
      if (tema()== "Homicídio"| tema()== "Latrocínio" | tema()== "Lesão Corporal Seguida de Morte" | tema()=="Morte Decorrente de Intervenção Policial" ) {
        if (!is.null(sexo)) {
          filtro <- filtro & (dado_real()$sexo_pessoa %in% sexo)
        }}
      
      if(tema()== "Morte Decorrente de Intervenção Policial"){
        if (!is.null(corp)) {
          filtro <- filtro & (dado_real()$coorp_situacao_ocorr %in% corp)
        }}
      
      if(tema()=="Morte Suspeita"){
        if (!is.null(desdobramento)) {
          filtro <- filtro & (dado_real()$desdobramento_ocorr %in% desdobramento)
        }}
      
      if(tema()=="Roubo de Veículo" | tema() == "Furto de Veículo" ){
        if (!is.null(tveiculo)) {
          filtro <- filtro & (dado_real()$tipo_veiculo %in% tveiculo)
        }
        
        if (!is.null(anov)) {
          filtro <- filtro & (dado_real()$ano_mod_veiculo %in% anov)
        }}
      
      if(tema()=="Roubo de Celular" | tema()=="Furto de Celular"){
        if (!is.null(qtd_cel1) & !is.null(qtd_cel2)) {
          filtro <- filtro & (dado_real()$qtde_celular >= qtd_cel1 & dado_real()$qtde_celular <= qtd_cel2)
        }}
      
      if(tema()=="Roubo de Celular" | tema()=="Furto de Celular" | tema()=="Morte Suspeita" | tema()=="Dados Criminais"){
        if (!is.null(descr_local)) {
          filtro <- filtro & (dado_real()$descricao_local_ocorr %in% descr_local)
        }}
      
      if(tema()=="Dados Criminais"){
        if (!is.null(natureza)) {
          filtro <- filtro & (dado_real()$natureza_apurada_ocorr %in% natureza)
        }}
      
      
      dado_filtrado(dado_real()[filtro, ])
      
      faixa_etaria <- NULL
      data <- NULL
      cor <- NULL
      tveiculo <- NULL
      anov <- NULL
      qtd_cel <- NULL
      descr_local <- NULL
      natureza <- NULL
      sexo <- NULL
      corp <- NULL
      
    } 
  })
  
  observeEvent(input$reset,{
    
    dado_filtrado(dado_real())
    
  })
  
  output$graficos <- renderPlotly({
    var1 <- input$Var1
    var2 <- input$Var2
    dic_var1 <- dicionario[var1]
    dic_var2 <- dicionario[var2]
    Graph <- NULL
    if (var2 == "Nenhuma"){
      if (class(dado_filtrado()[[var1]]) == "character"){
        
        dados_grafico <- dado_filtrado() %>%
          count(!!sym(var1), name = "contagem") %>%
          top_n(10, wt = contagem)
        
        Graph <-  ggplot(dados_grafico, aes(x = contagem, y = !!sym(var1))) +
          geom_bar(stat = "identity", fill = "skyblue") +
          geom_text(aes(label = contagem), vjust = -0.5) +
          labs(title = "Contagem de Categorias (Top 10)", x = "Contagem", y = "Categoria")
        
        Graph <- ggplotly(Graph) %>% layout(height = 600)
        
        
        
        
      } else{
        Graph <- ggplot(dado_filtrado(), aes(x = !!sym(var1))) +
          geom_histogram(fill = "skyblue", color = "black", bins = 10, alpha = 0.7) +
          geom_density(color = "red") +
          labs(title = "Distribuição da Variável Contínua", x = "Valor", y = "Frequência")
        Graph <- ggplotly(Graph)
        
        Graph <- Graph %>% layout(height = 600)
        
        return(Graph)
        
      }
      
    } else{
      
      if (class(dado_filtrado()[[var1]]) == "character"){
        if (class(dado_filtrado()[[var2]]) == "character"){ 
          
          count_var1 <- dado_filtrado() %>% count(!!sym(var1), sort = TRUE)
          count_var2 <- dado_filtrado() %>% count(!!sym(var2), sort = TRUE)
          
          # Selecione as top 10 de var1 e top 5 de var2
          top_var1 <- count_var1 %>% slice(1:10)
          top_var2 <- count_var2 %>% slice(1:5)
          
          result <- dado_filtrado() %>%
            filter(!!sym(var1) %in% top_var1[[var1]], !!sym(var2) %in% top_var2[[var2]])
          
          
          
          Graph <- ggplot(result, aes(y = !!sym(var1), fill = !!sym(var2))) +
            geom_bar(position = "dodge", stat = "count", width = 0.7) +
            labs(title = paste0("Contagem de Combinações de ",dic_var1," e ",dic_var2),
                 x = "Contagem",
                 y = dic_var1,
                 fill = dic_var2) +
            theme_minimal()
          
          Graph <- ggplotly(Graph)
          
          Graph <- Graph %>% layout(height = 600)
          
          return(Graph)
          
          
          
        }
        else{
          top_categorias <- dado_filtrado() %>%
            count(!!sym(var1), sort = TRUE) %>%
            slice(1:10)
          
          dados_filtrados <- dado_filtrado() %>%
            filter(!!sym(var1) %in% top_categorias[[var1]])
          
          # Boxplot para cada categoria
          Graph <- ggplot(dados_filtrados, aes(x = !!sym(var1), y = !!sym(var2))) +
            geom_boxplot() +
            labs(title = "Boxplot para as 10 Categorias Mais Recorrentes",
                 x = "Categoria",
                 y = "Valor Numérico") +
            theme_minimal()
          
          Graph <- ggplotly(Graph)
          
          Graph <- Graph %>% layout(height = 600)
          
          return(Graph)
          
          
        }
      }else{
        if (class(dado_filtrado()[[var2]]) == "character"){ 
          top_categorias <- dado_filtrado() %>%
            count(!!sym(var2), sort = TRUE) %>%
            slice(1:10)
          
          dados_filtrados <- dado_filtrado() %>%
            filter(!!sym(var2) %in% top_categorias[[var2]])
          
          # Boxplot para cada categoria
          Graph <- ggplot(dados_filtrados, aes(x = !!sym(var2), y = !!sym(var1))) +
            geom_boxplot() +
            labs(title = "Boxplot para as 10 Categorias Mais Recorrentes",
                 x = "Categoria",
                 y = "Valor Numérico") +
            theme_minimal()
          
          Graph <- ggplotly(Graph)
          
          Graph <- Graph %>% layout(height = 600)
          
          return(Graph)
          
        }else{
          Graph <- ggplot(dado_filtrado(), aes(x = !!sym(var1), y = !!sym(var2))) +
            geom_point() +
            labs(title = "Scatter Plot entre Duas Variáveis Contínuas",
                 x = "Variável Contínua 1",
                 y = "Variável Contínua 2") +
            theme_minimal()
          Graph <- ggplotly(Graph)
          Graph <- Graph %>% layout(height = 600)
          
          return(Graph)
          
        }
        
        
      }
      
    }})
  
  output$tabela_1 <- renderDataTable({
    if (!is.null(dado_real())) {
      colunas_selecionadas <- input$colunas_selecionadas
      dados <- dado_filtrado()
      
      if (length(colunas_selecionadas) > 0) {
        dados <- dados[, ..colunas_selecionadas, drop = FALSE]
      }
      
      datatable(dados, 
                options = list(
                  paging = TRUE,
                  pageLength = 10,
                  lengthMenu = c(10, 25, 50),
                  searching = TRUE,
                  ordering = TRUE,
                  responsive = TRUE,
                  scrollX = TRUE
                ),
                rownames = FALSE,
                class = 'display'
      )}})
  
  observeEvent(input$inicio, {
    output$MAPA <- renderLeaflet({})
    
  })
  
  observeEvent(input$map, {
    output$MAPA <- renderLeaflet({
      df2 <- dado_filtrado()
      df2$longitude_ocorr <- as.numeric(df2$longitude_ocorr)
      df2$latitude_ocorr <- as.numeric(df2$latitude_ocorr)
      
      df2$longitude_ocorr <- ifelse(abs(df2$longitude_ocorr) > 180, NA, df2$longitude_ocorr)
      df2$latitude_ocorr <- ifelse(abs(df2$latitude_ocorr) > 90, NA, df2$latitude_ocorr)
      df2 <- na.omit(df2)
      
      
      nocorr <- nrow(df2)  # Conta o número de ocorrências após limpar os dados
      
      m(leaflet(df2) %>%
          addTiles() %>%
          setView(lng = -46.5, lat = -23.5, zoom = 7) %>%
          
          addCircleMarkers(
            ~longitude_ocorr, ~latitude_ocorr,
            radius = 7,
            color = "blue",
            popup = ~paste("Location: ", nome_municipio_circ),
            label = ~nome_municipio_circ,
            clusterOptions = markerClusterOptions()
          ) 
        #addLegend(position = "bottomright", pal = "YlGnBu", values = c(nocorr / 10, nocorr / 5, nocorr / 4, nocorr / 2))
      )
      m() 
    })
  })
  
  output$serie_ano <- renderPlotly({
    Graph <- NULL
    serie_de_ano <- dados_filtrados_tempo(dado_filtrado())
    
    Graph <- autoplot(serie_de_ano, .vars = total) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste('Número de ocorrências por ano')) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly_chart <- ggplotly(Graph)
    
    plotly_chart <- plotly_chart 
    
    plotly_chart
  })
  
  
  ##Download
  output$download_full <- downloadHandler(
    filename = function() {
      paste(tema(), ".csv", sep = "")
    },
    content = function(file) {
      dados <- dado_real()
      write.csv(dados, file, row.names = FALSE)
    },
    contentType = "text/csv" 
  )
  
  output$download_filtrado <- downloadHandler(
    filename = function() {
      paste(tema(), "_filtrado.csv", sep = "")
    },
    content = function(file) {
      colunas_selecionadas <- input$colunas_selecionadas
      dados <- dado_filtrado()
      if (length(colunas_selecionadas) > 0) {
        dados <- dados[, ..colunas_selecionadas, drop = FALSE]
      }
      write.csv(dados, file, row.names = FALSE)
    }
  )
  
  
  
  
  observeEvent(input$cont_trans,{
    transformacao <- input$cont_trans
    
    if (transformacao == "identidade") {
      Serie_Transformacao("total")
      
    } else if (transformacao == "log") {
      Serie_Transformacao("log")
      
    } else if (transformacao == "sqrt") {
      Serie_Transformacao("sqrt")
      
    } else if (transformacao == "inversa") {
      Serie_Transformacao("inversa")
      
    } else if (transformacao == "box_cox") {
      Serie_Transformacao("box_cox")
      
    }
    
  })
  
  observeEvent(input$cont_filtro,{
    tempo <- input$cont_filtro
    Granularidade(tempo)
    
    if (tempo == "semanal") {
      Serie_Tempo("semana")
      Graph_Tempo("Semana")
      
    } else if (tempo == "mensal") {
      Serie_Tempo("mes")
      Graph_Tempo("Mês")
      
    } else if (tempo == "trimestral") {
      Serie_Tempo("tri")
      Graph_Tempo("Trimestre")
      
    } else if (tempo == "semestral") {
      Serie_Tempo("sem")
      Graph_Tempo("Semestre")
      
    } else if (tempo == "anual") {
      Serie_Tempo("ano")
      Graph_Tempo("Ano")
      
    }
    
  })
  ### Observes do armazem!!!
  observeEvent(input$cont_filtro,{
    if (!is.null(input$cont_filtro)){
      tempo <- input$cont_filtro
      if (tempo == "semanal") {
        armazem_granularidade("semana")
        
        
      } else if (tempo == "mensal") {
        armazem_granularidade("mes")
        
        
      } else if (tempo == "trimestral") {
        armazem_granularidade("tri")
        
        
      } else if (tempo == "semestral") {
        armazem_granularidade("sem")
        
        
      } else if (tempo == "anual") {
        armazem_granularidade("ano")
        
        
      }
      
    }
  })
  
  observeEvent(input$cont_trans,{
    if (!is.null(input$cont_trans)){
      armazem_transformacao(input$cont_trans)
    }
  })
  
  observeEvent(input$Id077,{
    if (!is.null(input$Id077)){
      armazem_diff1(input$Id077)
    }
  })
  
  
  
  observeEvent(input$serie_ordem,{
    if (!is.null(input$serie_ordem)){
      armazem_ordem1(input$serie_ordem)
    }
  })
  
  observeEvent(input$Id078,{
    if (!is.null(input$Id078)){
      armazem_diff2(input$Id078)
    }
  })
  
  observeEvent(input$serie_lag2,{
    if (!is.null(input$serie_lag2)){
      armazem_lag2(input$serie_lag2)
    }
  })
  
  observeEvent(input$serie_ordem,{
    if (!is.null(input$serie_ordem)){
      armazem_ordem2(input$serie_ordem)
    }
  })
  ################################################################################################      
  Serie_Atual <- reactive({
    if(!is.null(Serie_Tempo()) && !is.null(Serie_Transformacao())){
      serie <- dados_filtrados_tempo(dado_filtrado(),Serie_Tempo(),tema())
      
      if(nrow(serie) <=1){
        shinyalert("Poucas observações",text ="Dados insuficientes para a construção da série temporal com esse período de tempo")
      }else{
        armazem_serie_original(serie)
        serie <- get_transformacoes(serie,Serie_Transformacao())
        
        
        if(input$Id077 == TRUE){
          lag <- 1
          ordem <- input$serie_ordem
          
          serie <- diff(serie,lag,ordem)}
        
        if(input$Id078 == TRUE){
          
          lag2 <- input$serie_lag2
          ordem2 <- input$serie_ordem2
          
          serie <- diff(serie,lag2,ordem2)
        }
        
        colnames(serie)[2] <- "total"
        
        serie <- serie %>% fill_gaps() %>% replace_na(list(total= 0))
        
        serie
      }}
  })
  
  
  
  output$cont <- renderPlotly({
    Graph <- NULL
    transformacao <- Serie_Transformacao()
    Graph <- autoplot(Serie_Atual(), .vars = !!sym(colnames(Serie_Atual())[2]) ) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste('Número de ocorrências registradas por ', Graph_Tempo())) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly_chart <- ggplotly(Graph)
    
    plotly_chart <- plotly_chart %>% layout(height = 600)
    
    plotly_chart
    
  })
  
  
  output$cont2 <- renderPlotly({
    tempo <- input$cont_filtro
    if (tempo == "anual"){
      shinyalert(text="Para visualizar o gráfico, é necessário uma granularidade inferior a anual.")
      Graph <- NULL
      Graph
    }else{
      
      Serie <- Serie_Atual()
      
      colnames(Serie)[2] <- "total"
      
      Graph <- NULL
      
      Graph <- Serie %>% fill_gaps() %>% replace_na(list(total= 0)) %>%  gg_season() + xlab(paste(Graph_Tempo())) + theme_pubclean()
      
      plotly_chart <- ggplotly(Graph)
      
      plotly_chart <- plotly_chart %>% layout(height = 600)
      
      plotly_chart}
  })
  
  output$cont3 <- renderPlotly({
    Graph <- NULL
    
    Graph <- Serie_Atual() %>% gg_lag(y = !!sym(colnames(Serie_Atual())[2]), lags = 1:12) + xlab('lag') + theme_classic()
    
    plotly_chart <- ggplotly(Graph)
    
    #plotly_chart <- plotly_chart %>% layout(height = 700)
    
    plotly_chart
  })
  
  
  output$auto1 <- renderPlotly({
    Graph <- NULL
    serie <- Serie_Atual()
    colnames(serie)[2] <- "total"
    
    lag_max = serie %>% pull(total) %>% length()
    
    Graph <- autoplot(serie %>% ACF(lag_max = lag_max )) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste("Autocorrelação")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly_chart <- ggplotly(Graph)
    
    #plotly_chart <- plotly_chart %>% layout(height = 600)
    
    plotly_chart 
    
  })
  
  output$auto2 <- renderPlotly({
    Graph <- NULL
    serie <- Serie_Atual()
    colnames(serie)[2] <- "total"
    
    lag_max = serie %>% pull(total) %>% length()
    
    Graph <- autoplot(serie %>% PACF(lag_max = lag_max)) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste("Autocorrelação Parcial")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly_chart <- ggplotly(Graph)
    
    #plotly_chart <- plotly_chart %>% layout(height = 600)
    
    plotly_chart 
    
  })
  
  
  
  
  output$summary <- renderTable({
    Serie_Atual()})
  
  output$cont_diff <- renderPlotly({
    
    name_serie <- input$serie_escolha_teste
    
    k <- Lista_Serie()[[name_serie]]
    
    j <- input$Col_diff2
    Graph <- NULL
    
    Graph <- autoplot(k, .vars = !!sym(j) ) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste('Número de ocorrências por ', Graph_Tempo())) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly_chart <- ggplotly(Graph)
    
    plotly_chart <- plotly_chart %>% layout(height = 600)
    
    plotly_chart
  })
  
  
  observeEvent(input$test7,{
    if (is.null(fit())){
      showNotification("Nenhum modelo gerado",type = "error")
    }else{
      fit <- fit()
      
      resultado_summary <- teste_ljung_box(fit,input$model)
      
      texto_summary <- capture.output(print(resultado_summary))
      
      modalContent <- modalDialog(
        title = "Teste Ljung Box",
        verbatimTextOutput("summary_output"),  # Exibindo o texto do summary
        footer = tagList(
          actionButton("close_popup", "Fechar")
        )
      )
      
      
      output$summary_output <- renderPrint({
        cat(texto_summary, sep = "\n")
      })
      
      showModal(modalContent)
      
      modal_state(TRUE)}
  })
  
  
  observeEvent(input$test8,{
    if (is.null(fit())){
      showNotification("Nenhum modelo gerado",type = "error")
    }else{
      fit <- fit()
      
      resultado_summary <- shapiro_test(fit,input$model)
      
      texto_summary <- capture.output(print(resultado_summary))
      
      modalContent <- modalDialog(
        title = "Teste Shapiro wilk",
        verbatimTextOutput("summary_output"),  # Exibindo o texto do summary
        footer = tagList(
          actionButton("close_popup", "Fechar")
        )
      )
      
      
      output$summary_output <- renderPrint({
        cat(texto_summary, sep = "\n")
      })
      
      showModal(modalContent)
      
      modal_state(TRUE)}
  })
  
  
  
  
  observeEvent(input$ADF,{
    serie <- Serie_Atual()
    
    resultado_summary <- teste_adf(serie,colnames(serie[2]))
    
    texto_summary <- capture.output(print(resultado_summary))
    
    modalContent <- modalDialog(
      title = "Teste ADF",
      verbatimTextOutput("summary_output"),  # Exibindo o texto do summary
      footer = tagList(
        actionButton("close_popup", "Fechar")
      )
    )
    
    # Definindo o output no modal
    output$summary_output <- renderPrint({
      cat(texto_summary, sep = "\n")
    })
    
    showModal(modalContent)
    
    modal_state(TRUE)
  })
  
  observeEvent(input$Sazonal,{
    serie <- Serie_Atual()
    
    resultado_summary <- teste_seasonal(serie,colnames(serie[2]))
    
    texto_summary <- capture.output(print(resultado_summary))
    
    modalContent <- modalDialog(
      title = "Teste ADF",
      verbatimTextOutput("summary_output"),  # Exibindo o texto do summary
      footer = tagList(
        actionButton("close_popup", "Fechar")
      )
    )
    
    # Definindo o output no modal
    output$summary_output <- renderPrint({
      cat(texto_summary, sep = "\n")
    })
    
    showModal(modalContent)
    
    modal_state(TRUE)
  })
  
  observeEvent(input$Kendall,{
    serie <- Serie_Atual()
    
    resultado_summary <- teste_kendall(serie,colnames(serie[2]))
    
    texto_summary <- capture.output(print(resultado_summary))
    
    modalContent <- modalDialog(
      title = "Teste Kendall",
      verbatimTextOutput("summary_output"),  # Exibindo o texto do summary
      footer = tagList(
        actionButton("close_popup", "Fechar")
      )
    )
    
    # Definindo o output no modal
    output$summary_output <- renderPrint({
      cat(texto_summary, sep = "\n")
    })
    
    showModal(modalContent)
    
    modal_state(TRUE)
  })
  
  observeEvent(input$Cox_Stuart,{
    serie <- Serie_Atual()
    
    resultado_summary <- teste_cox_stuart(serie,colnames(serie[2]))
    
    texto_summary <- capture.output(print(resultado_summary))
    
    modalContent <- modalDialog(
      title = "Teste Cox and Stuart",
      verbatimTextOutput("summary_output"),  # Exibindo o texto do summary
      footer = tagList(
        actionButton("close_popup", "Fechar")
      )
    )
    
    # Definindo o output no modal
    output$summary_output <- renderPrint({
      cat(texto_summary, sep = "\n")
    })
    
    showModal(modalContent)
    
    modal_state(TRUE)
  })
  
  
  observeEvent(input$KPSS,{
    serie <- Serie_Atual()
    
    kpss1 <- teste_kpss(serie, !!sym(colnames(serie)[2]))
    #kpss2 <- teste_kpss_numero_diff(serie, !!sym(colnames(serie)[2]))
    #kpss3 <- teste_kpss_season_diff(serie, !!sym(colnames(serie)[2]))
    
    texto_summary  <- capture.output(print(kpss1))
    #texto_summary2 <- capture.output(print(kpss2))
    #texto_summary3 <- capture.output(print(kpss3))
    
    modalContent <- modalDialog(
      title = "Teste ADF",
      verbatimTextOutput("summary_output"),  # Exibindo o texto do summary
      footer = tagList(
        actionButton("close_popup", "Fechar")
      )
    )
    
    # Definindo o output no modal
    output$summary_output <- renderPrint({
      cat(texto_summary, sep = "\n")
      #cat(texto_summary2, sep = "\n")
      #cat(texto_summary3, sep = "\n")
    })
    
    showModal(modalContent)
    
    modal_state(TRUE)
  })
  
  output$decomp_serie <- renderPlotly({
    Graph <- NULL
    
    
    k <- Serie_Atual()
    
    Graph <- k %>% na.omit() %>%  
      model(
        STL(!!sym(colnames(k)[2]) ~ trend() +
              season(),
            robust = TRUE)) %>% 
      components() 
    
    grafico <- autoplot(Graph)
    
    plotly_chart <- ggplotly(grafico)
    
    #plotly_chart <- plotly_chart %>% layout(width = 800, height = 600)
    
    plotly_chart
  })
  
  
  
  
  
  observeEvent(input$sugerido,{
    
    SERIE <- armazem_serie_original()
    
    teste <- ruido_branco_func(SERIE)
    
    if (teste){
      shinyalert(title="Alerta",text = "A série é ruído branco, não é possível gerar modelos"
                 ,type = "warning", confirmButtonCol = "#11104d")
    }else{
      
      showNotification("Geração de modelos iniciada",type = "warning")
      
      vetor2 <- c()
      
      modelo_usuario <- input$Modelo_Usuario
      
      modelo_ingenuo <- input$Modelo_Ingenuo
      
      modelo_sarima <- input$Modelo_Sarima
      
      modelo_arma <- input$Modelo_Arma
      
      modelo_arima <- input$Modelo_Arima
      
      if (modelo_usuario){
        vetor2 <- append(vetor2,"usuario")
      }
      
      if (modelo_ingenuo){
        vetor2 <- append(vetor2,"modelo_pia")
      }
      
      if (modelo_sarima){
        vetor2 <- append(vetor2,"auto_sarima")
      }
      
      if (modelo_arma){
        vetor2 <- append(vetor2,"arma")
      }
      
      if (modelo_arima){
        vetor2 <- append(vetor2,"arima")
      }
      
      fit(fit_serie(SERIE,tipo_transformacao = armazem_transformacao(),d = input$serie_ordem,
                    dif = paste0(input$dzin[1],":",input$dzin[2]),  seasonal_dif = paste0(input$dzao[1],":",input$dzao[2]),
                    p = paste0(input$pzin[1],":",input$pzin[2]), P = paste0(input$pzao[1],":",input$pzao[2]),
                    q = paste0(input$qzin[1],":",input$qzin[2]), Q = paste0(input$qzao[1],":",input$qzao[2]),
                    lag_seasonal_dif = armazem_lag2(),modelos = vetor2))
      
      if (modelo_usuario){
        valid <- check_model_2(fit(),"usuario")
        if(valid){
          vetor2 <- setdiff(vetor2,"usuario")
          showNotification("Não foi possível ajustar o modelo usuario",type = "error")
        }
      }
      
      if (modelo_ingenuo){
        valid <- check_model_2(fit(),"modelo_pia")
        if(valid){
          vetor2 <- setdiff(vetor2,"modelo_pia")
          showNotification("Não foi possível ajustar o modelo modelo_pia",type = "error")
        }
      }
      
      if (modelo_sarima){
        valid <- check_model_2(fit(),"auto_sarima")
        if(valid){
          vetor2 <- setdiff(vetor2,"auto_sarima")
          showNotification("Não foi possível ajustar o modelo auto_sarima",type = "error")
        }
      }
      
      if (modelo_arma){
        valid <- check_model_2(fit(),"arma")
        if(valid){
          vetor2 <- setdiff(vetor2,"arma")
          showNotification("Não foi possível ajustar o modelo arma",type = "error")
        }
      }
      
      if (modelo_arima){
        valid <- check_model_2(fit(),"arima")
        if(valid){
          vetor2 <- setdiff(vetor2,"arima")
          showNotification("Não foi possível ajustar o modelo arima",type = "error")
        }
      }
      
      
      updatePickerInput(session,"model",choices = vetor2)
      
      showNotification("Geração de modelos finalizada",type = "message")}
    
  })
  
  
  output$summary2 <- renderPrint({
    if (is.null(fit())){
      "Nenhum modelo gerado!"
    }else{
      fit()
    }
  })
  
  
  output$plot1 <- renderPlot({
    if (is.null(fit()) ){
      h3("Nenhum modelo gerado!")
    }else{
      plot_raiz <- plot_raiz(fit(),input$model)
      
      plot_raiz
    }
  })
  
  output$plot2 <- renderPlot({
    if (is.null(fit())){
      print("Nenhum modelo gerado!")
    }else{
      plot_res <- plot_res(fit(),input$model)
      
      plot_res}
  })
  
  output$plot3 <- renderPlot({
    if (is.null(fit())){
      Graph <- NULL
      Graph
    }else if (!is.null(fit()) & armazem_transformacao() == "inversa" ){
      shinyalert(title="Predição não disponível",text="Gráfico de predição não está disponível para séries 
                 transformadas inversamente ")
      Graph <- NULL
      Graph
    }else{
      serie <- armazem_serie_original()
      plot_pred <- plot_predicao(fit(),input$model,input$lag_pred,serie)
      
      plot_pred}
  })
  
  output$plot4 <- renderPrint({
    if (is.null(fit())){
      "Nenhum modelo gerado!"
    }else{
      plot_report <-  plot_report(fit(),input$model) 
      
      plot_report
    }
  })
  
  pop_est <- function(){
    observeEvent(input$tabs, {
      if (!is.null(input$tabs) && input$tabs == "autoc") {
        serie<-Serie_Atual()
        kk <- teste_kpss(serie,!!sym(colnames(serie)[2]))
        j <- round(kk[2],3)
        if (j < 0.05){
          shinyalert(
            title = "A série possivelmente não é estacionária!",
            text = "Não tire conclusões acerca da ordem de um modelo preditivo.",
            type = "warning",
            confirmButtonCol = "#11104d")
          
          
        } } })
  }
  
  previous_serie <- reactiveVal(NULL)
  observe({
    if (input$tabs == "autoc" && !identical(Serie_Atual(), previous_serie())) {
      serie <- Serie_Atual()
      kk <- teste_kpss(serie, !!sym(colnames(serie)[2]))
      j <- round(kk[2], 3)
      if (j < 0.05) {
        shinyalert(
          title = "A série possivelmente não é estacionária!",
          text = "Não tire conclusões acerca da ordem de um modelo preditivo.",
          type = "warning",
          confirmButtonCol = "#11104d"
        )
      }
      # Atualiza o objeto reativo anterior
      previous_serie(Serie_Atual())
    }
  })
  
  
  observeEvent(input$decomp,{
    
    showNotification("Algumas informações foram conservadas", duration = 3)
    if (armazem_diff1()){
      updateSliderInput(session, "dzin", value = c(armazem_ordem1(),armazem_ordem1()))}
    if (armazem_diff2()){
      updateSliderInput(session, "dzao", value = c(armazem_ordem2(),armazem_ordem2()))}
    estado("decomp")
    shinyalert(title = "Processamento",type = "warning",
               text = "Dependendo dos parâmetros escolhidos a busca do melhor modelo 
                   pode ser custosa computacionalmente resultando em um tempo de execução maior",
               confirmButtonCol = "#11104d")
  })
  
  observeEvent(input$gerar_modelo,{
    shinyjs::enable("decomp")
    showNotification("Algumas informações foram conservadas", duration = 3)
    if (armazem_diff1()){
      updateSliderInput(session, "dzin", value = c(armazem_ordem1(),armazem_ordem1()))}
    if (armazem_diff2()){
      updateSliderInput(session, "dzao", value = c(armazem_ordem2(),armazem_ordem2()))}
    estado("decomp")
    shinyalert(title = "Processamento",type = "warning",
               text = "Dependendo dos parâmetros escolhidos a busca do melhor modelo 
                   pode ser custosa computacionalmente resultando em um tempo de execução maior",
               confirmButtonCol = "#11104d"
    )
  })
  
  observeEvent(input$Modelo_Ingenuo,{
    showNotification("Esse modelo só é recomendado para séries estacionárias",type = "message")
    
  }) 
  
  observeEvent(input$Homicídio,{
    session$sendCustomMessage(type = "scrollCallback", 1)
  })
  
  
  observeEvent(input$Homicidio,{
    shinyjs::enable("botao_clicado")
    
  })
  
  
  
}





shinyApp(ui, server)




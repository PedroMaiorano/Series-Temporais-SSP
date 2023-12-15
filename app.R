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


plan(multisession)

#install.packages("tseries",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("glue",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("trend",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("aTSA",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("forecast",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("funtimes",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")





Side_hom <- 
  fluidRow(
    h3("Filtro dos dados"),
    br(),
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
  fluidRow(
    h3("Filtro dos dados"),
    br(),
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



Side_lat <-  fluidRow( 
  h3("Filtro dos dados"),
  br(),
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

Side_lcs <- fluidRow(
  h3("Filtro dos dados"),
  br(),
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



Side_mdi <- fluidRow(
  h3("Filtro dos dados"),
  br(),
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
  br(),
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

Side_mor <- fluidRow(
  h3("Filtro dos dados"),
  br(),
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

Side_iml <- fluidRow(
  h3("Filtro dos dados"),
  br(),
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2016-04-01", end = "2022-12-31",
                 min = "2016-04-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = ),
  
  actionButton("aplicar","Modificar os dados"),
  actionButton("reset","Não usar Filtro"))  

Side_dad <- fluidRow(
  h3("Filtro dos dados"),
  br(),
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2021-01-05", end = "2022-12-31",
                 min = "2021-01-05", max = "2022-12-31",
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
      "condomínio comercial",
      "condomínio residencial",
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
      "condomínio comercial",
      "condomínio residencial",
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


Side_rouc <-  fluidRow(
  h3("Filtro dos dados"),
  br(),
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2010-01-01", end = "2022-12-31",
                 min = "2010-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1),
  sliderTextInput(
    inputId = "quantidade_celular",
    label = "Quantidade furtada", 
    choices = c(
      "0", "1", "10", "100", "1000", "101", "1011", "102", "103", "104", "1050", "106", "107",
      "1080", "109", "11", "110", "1100", "111", "1111", "1115", "1117", "1119", "112", "113", "114",
      "1145", "115", "116", "1169", "117", "118", "119", "1197", "12", "120", "121", "1210", "1213", "123",
      "124", "125", "126", "127", "128", "1281", "129", "13", "130", "1300", "131", "132", "133", "134",
      "135", "1351", "1353", "1354", "1355", "1358", "137", "138", "14", "140", "1404", "141", "142", "143",
      "144", "146", "1470", "14728864", "148", "149", "15", "150", "1500", "1513", "152", "153", "1530",
      "155", "1550", "1553", "156", "1560", "157", "159", "16", "160", "1612", "1615", "162", "163", "1650",
      "1654", "166", "1665", "167", "1677", "1679", "168", "168455", "17", "170", "1707", "171", "1729",
      "1733", "174", "1741", "175", "176", "1777", "178", "1786", "179", "1798", "18", "180", "1800", "1804",
      "1808", "181", "1819", "182", "183", "184", "185", "1854", "186", "1867", "1871", "1880", "1883", "189",
      "19", "190", "1906", "191", "192", "1920", "193", "195", "1957", "196", "1965", "1966", "1968", "1971",
      "198", "1980", "1981", "1989", "199", "1990", "1996", "1999", "2", "20", "200", "2000", "201", "204",
      "207", "21", "210", "212", "213", "218", "219", "2199", "22", "220", "221", "222", "226", "228",
      "229", "23", "230", "231", "233", "239", "24", "240", "249", "25", "250", "2500", "251", "253",
      "256", "257", "258", "26", "260", "264", "265", "266", "27", "274", "2751", "28", "280", "2800",
      "29", "291", "2944", "297", "298", "3", "30", "300", "3000", "301", "305", "308", "309", "31",
      "3100", "311", "314", "32", "329", "33", "330", "34", "346", "3465", "348", "35", "350", "3551",
      "36", "37", "370", "376", "378", "38", "380", "384", "39", "4", "40", "41", "42", "424", "43",
      "433", "44", "4400", "45", "46", "47", "48", "483", "486", "488", "49", "492", "498", "499", "5",
      "50", "500", "5000", "501", "51", "52", "53", "54", "542", "547", "55", "551", "557", "56", "57",
      "58", "59", "6", "60", "600", "6000", "61", "612", "62", "621", "629", "63", "64", "65", "66",
      "660", "67", "670", "672", "68", "688", "69", "7", "70", "700", "71", "72", "720", "73", "730",
      "74", "75", "750", "751", "76", "768", "77", "779", "78", "785", "79", "799", "8", "80", "800",
      "804", "81", "815", "82", "83", "84", "85", "850", "87", "88", "880", "89", "899", "9", "90",
      "91", "92", "93", "930", "938", "94", "9413", "949", "9579", "96", "9675", "97", "98", "9844",
      "9860", "99", "999", "9999", "não informado"
    ),
    selected = c(
      "0", "1", "10", "100", "1000", "101", "1011", "102", "103", "104", "1050", "106", "107",
      "1080", "109", "11", "110", "1100", "111", "1111", "1115", "1117", "1119", "112", "113", "114",
      "1145", "115", "116", "1169", "117", "118", "119", "1197", "12", "120", "121", "1210", "1213", "123",
      "124", "125", "126", "127", "128", "1281", "129", "13", "130", "1300", "131", "132", "133", "134",
      "135", "1351", "1353", "1354", "1355", "1358", "137", "138", "14", "140", "1404", "141", "142", "143",
      "144", "146", "1470", "14728864", "148", "149", "15", "150", "1500", "1513", "152", "153", "1530",
      "155", "1550", "1553", "156", "1560", "157", "159", "16", "160", "1612", "1615", "162", "163", "1650",
      "1654", "166", "1665", "167", "1677", "1679", "168", "168455", "17", "170", "1707", "171", "1729",
      "1733", "174", "1741", "175", "176", "1777", "178", "1786", "179", "1798", "18", "180", "1800", "1804",
      "1808", "181", "1819", "182", "183", "184", "185", "1854", "186", "1867", "1871", "1880", "1883", "189",
      "19", "190", "1906", "191", "192", "1920", "193", "195", "1957", "196", "1965", "1966", "1968", "1971",
      "198", "1980", "1981", "1989", "199", "1990", "1996", "1999", "2", "20", "200", "2000", "201", "204",
      "207", "21", "210", "212", "213", "218", "219", "2199", "22", "220", "221", "222", "226", "228",
      "229", "23", "230", "231", "233", "239", "24", "240", "249", "25", "250", "2500", "251", "253",
      "256", "257", "258", "26", "260", "264", "265", "266", "27", "274", "2751", "28", "280", "2800",
      "29", "291", "2944", "297", "298", "3", "30", "300", "3000", "301", "305", "308", "309", "31",
      "3100", "311", "314", "32", "329", "33", "330", "34", "346", "3465", "348", "35", "350", "3551",
      "36", "37", "370", "376", "378", "38", "380", "384", "39", "4", "40", "41", "42", "424", "43",
      "433", "44", "4400", "45", "46", "47", "48", "483", "486", "488", "49", "492", "498", "499", "5",
      "50", "500", "5000", "501", "51", "52", "53", "54", "542", "547", "55", "551", "557", "56", "57",
      "58", "59", "6", "60", "600", "6000", "61", "612", "62", "621", "629", "63", "64", "65", "66",
      "660", "67", "670", "672", "68", "688", "69", "7", "70", "700", "71", "72", "720", "73", "730",
      "74", "75", "750", "751", "76", "768", "77", "779", "78", "785", "79", "799", "8", "80", "800",
      "804", "81", "815", "82", "83", "84", "85", "850", "87", "88", "880", "89", "899", "9", "90",
      "91", "92", "93", "930", "938", "94", "9413", "949", "9579", "96", "9675", "97", "98", "9844",
      "9860", "99", "999", "9999", "não informado"
    )[c(1,356)]
  ),
  pickerInput(
    inputId = "descr_local",
    label = "Descrição do local", 
    choices = c(
      "area não ocupada", "centro comerc./empresarial", "comércio e serviços",
      "condomínio comercial", "condomínio residencial", "entidade assistencial",
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
      "area não ocupada", "centro comerc./empresarial", "comércio e serviços",
      "condomínio comercial", "condomínio residencial", "entidade assistencial",
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

Side_furc <- fluidRow(
  h3("Filtro dos dados"),
  br(),
  dateRangeInput('dateRange2',
                 label = "Filtrar por data",
                 start = "2010-01-01", end = "2022-12-31",
                 min = "2010-01-01", max = "2022-12-31",
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'pt-BR', weekstart = 1),
  sliderTextInput(
    inputId = "quantidade_celular",
    label = "Quantidade furtada", 
    choices = c(
      "0", "1", "10", "100", "1000", "1006", "1007", "101", "1011", "102", "103",
      "105", "106", "108", "1080", "109", "11", "110", "1100", "111", "1119", "112",
      "113", "1150", "116", "1161", "117", "1179", "1188", "119", "12", "120", "121",
      "122", "123", "124", "125", "126", "1270", "129", "13", "130", "131", "132",
      "133", "135", "1353", "1353133", "1354", "135401508", "1354410068891", "1355",
      "1357", "1358692", "1359", "136", "1367", "137", "138", "1382", "139", "14",
      "140", "142", "1440", "146", "147", "15", "150", "1500", "152", "154", "156",
      "157", "159", "1597", "16", "160", "162", "165", "166", "169", "1693", "17",
      "170", "171", "173", "176", "17731", "178", "1783", "18", "180", "181", "1815",
      "1818", "182", "1845", "19", "191", "1918", "1941", "195", "1951", "1959",
      "1966", "197", "1970", "1974", "198", "1999", "2", "20", "200", "2000", "203",
      "206", "208", "21", "210", "213", "214", "216", "217", "218", "22", "223",
      "2240", "23", "24", "240", "248", "249", "25", "250", "254", "255", "256",
      "258", "259", "26", "264", "265", "27", "270", "274", "278", "28", "280",
      "282", "286", "288", "29", "293", "2996", "3", "30", "300", "303", "304", "31",
      "310", "316", "319", "32", "320", "33", "34", "340", "35", "3500", "3531",
      "3581", "36", "360", "361", "364", "37", "373", "38", "39", "399", "4", "40",
      "400", "4000", "405", "41", "417", "42", "4204", "424", "426", "43", "433",
      "44", "45", "46", "47", "470", "48", "480", "49", "498", "5", "50", "500",
      "501", "51", "52", "522", "53", "54", "542", "547", "56", "560", "57", "570",
      "58", "59", "6", "60", "600", "6000", "61", "62", "63", "64", "647", "649",
      "65", "651", "66", "67", "68", "69", "697", "699", "7", "70", "700", "71",
      "710", "72", "73", "74", "749", "75", "750", "76", "77", "78", "79", "798",
      "8", "80", "800", "81", "84", "85", "86", "860", "87", "88", "89", "899", "9",
      "90", "900", "91", "92", "93", "95", "96", "961", "9736", "98", "99", "9999",
      "9999999", "99999999", "não informado"
    ),
    selected = c(
      "0", "1", "10", "100", "1000", "1006", "1007", "101", "1011", "102", "103",
      "105", "106", "108", "1080", "109", "11", "110", "1100", "111", "1119", "112",
      "113", "1150", "116", "1161", "117", "1179", "1188", "119", "12", "120", "121",
      "122", "123", "124", "125", "126", "1270", "129", "13", "130", "131", "132",
      "133", "135", "1353", "1353133", "1354", "135401508", "1354410068891", "1355",
      "1357", "1358692", "1359", "136", "1367", "137", "138", "1382", "139", "14",
      "140", "142", "1440", "146", "147", "15", "150", "1500", "152", "154", "156",
      "157", "159", "1597", "16", "160", "162", "165", "166", "169", "1693", "17",
      "170", "171", "173", "176", "17731", "178", "1783", "18", "180", "181", "1815",
      "1818", "182", "1845", "19", "191", "1918", "1941", "195", "1951", "1959",
      "1966", "197", "1970", "1974", "198", "1999", "2", "20", "200", "2000", "203",
      "206", "208", "21", "210", "213", "214", "216", "217", "218", "22", "223",
      "2240", "23", "24", "240", "248", "249", "25", "250", "254", "255", "256",
      "258", "259", "26", "264", "265", "27", "270", "274", "278", "28", "280",
      "282", "286", "288", "29", "293", "2996", "3", "30", "300", "303", "304", "31",
      "310", "316", "319", "32", "320", "33", "34", "340", "35", "3500", "3531",
      "3581", "36", "360", "361", "364", "37", "373", "38", "39", "399", "4", "40",
      "400", "4000", "405", "41", "417", "42", "4204", "424", "426", "43", "433",
      "44", "45", "46", "47", "470", "48", "480", "49", "498", "5", "50", "500",
      "501", "51", "52", "522", "53", "54", "542", "547", "56", "560", "57", "570",
      "58", "59", "6", "60", "600", "6000", "61", "62", "63", "64", "647", "649",
      "65", "651", "66", "67", "68", "69", "697", "699", "7", "70", "700", "71",
      "710", "72", "73", "74", "749", "75", "750", "76", "77", "78", "79", "798",
      "8", "80", "800", "81", "84", "85", "86", "860", "87", "88", "89", "899", "9",
      "90", "900", "91", "92", "93", "95", "96", "961", "9736", "98", "99", "9999",
      "9999999", "99999999", "não informado"
    )[c(1,283)]
  ),
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

Side_rouv <- fluidRow(
  h3("Filtro dos dados"),
  br(),
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
    choices = c(
      "automóvel",
      "bicicleta",
      "bonde",
      "caminhão",
      "caminhão trator",
      "caminhonete",
      "camioneta",
      "carro de mão",
      "carroça",
      "chassi-plataforma",
      "ciclomoto",
      "inexistente",
      "micro-ônibus",
      "motociclo",
      "motoneta",
      "motor casa",
      "não informado",
      "ônibus",
      "quadriciclo",
      "reboque",
      "semi-reboque",
      "side-car",
      "trator esteiras",
      "trator misto",
      "trator rodas",
      "triciclo",
      "utilitário"
    ),
    selected = c(
      "automóvel",
      "bicicleta",
      "bonde",
      "caminhão",
      "caminhão trator",
      "caminhonete",
      "camioneta",
      "carro de mão",
      "carroça",
      "chassi-plataforma",
      "ciclomoto",
      "inexistente",
      "micro-ônibus",
      "motociclo",
      "motoneta",
      "motor casa",
      "não informado",
      "ônibus",
      "quadriciclo",
      "reboque",
      "semi-reboque",
      "side-car",
      "trator esteiras",
      "trator misto",
      "trator rodas",
      "triciclo",
      "utilitário"
    ),
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
Side_furv <- fluidRow(
  h3("Filtro dos dados"),
  br(),
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
    choices = c(
      "automóvel",
      "bicicleta",
      "caminhão",
      "caminhão trator",
      "caminhonete",
      "camioneta",
      "carroça",
      "charrete",
      "chassi-plataforma",
      "ciclomoto",
      "inexistente",
      "micro-ônibus",
      "motociclo",
      "motoneta",
      "motor casa",
      "não informado",
      "ônibus",
      "quadriciclo",
      "reboque",
      "semi-reboque",
      "side-car",
      "trator esteiras",
      "trator misto",
      "trator rodas",
      "triciclo",
      "utilitário"
    ),
    selected = c(
      "automóvel",
      "bicicleta",
      "caminhão",
      "caminhão trator",
      "caminhonete",
      "camioneta",
      "carroça",
      "charrete",
      "chassi-plataforma",
      "ciclomoto",
      "inexistente",
      "micro-ônibus",
      "motociclo",
      "motoneta",
      "motor casa",
      "não informado",
      "ônibus",
      "quadriciclo",
      "reboque",
      "semi-reboque",
      "side-car",
      "trator esteiras",
      "trator misto",
      "trator rodas",
      "triciclo",
      "utilitário"
    ),
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




dados_filtrados_tempo <- function(dados, granularidade = "ano") {
  data <- dados %>% 
    filter(data_bo_reg != -1) %>% 
    mutate(data_bo_reg = ymd(data_bo_reg)) %>%  
    filter(year(data_bo_reg) >= 2003) %>% 
    filter(year(data_bo_reg) <= 2022)
  
  index_col <- switch(granularidade,
                      "ano" = year(data$data_bo_reg),
                      "sem" = yearhalf(data$data_bo_reg),
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



get_pqd <- function(serie_temporal, ci  = 0.95,var){
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
    p = 0
    q = sequencia$lengths[1]
    modelo = glue('pdq(0,0,{q})')
    
    
    
    
  } else if(length(sequencia_partial$lengths) == 2){
    q = 0
    p = sequencia_partial$lengths[1]
    modelo = glue('pdq({p},0,0)')
    
    
  }else if(length(sequencia$lengths) >= 2 | length(sequencia_partial$lengths) >= 2){
    
    q = sequencia$lengths[1]
    p = sequencia_partial$lengths[1]
    modelo = glue('pdq({p},0,{q})')
    
  } else{ modelo = 'Não é possível encontrar um modelo'}
  
  return(modelo)
  
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


fit_serie <- function(serie,tipo_transformacao = 'identidade', dif ='1:2' , seasonal_dif = '0:1' ,p = '0:3',q = '0:3',P = '0:2',Q = '0:2',lag_seasonal_dif = "automatico",modelos){
  serie_modificada = get_transformacoes_modelo(serie,tipo_transformacao)
  
  pqd_modelo_sugerido =  get_pqd(serie_modificada,var = 'transformacao')
  if(pqd_modelo_sugerido == "N?o ? possivel encontrar um modelo"){stop('Verifique se a s?rie ? ru?do branco')}
  
  
  # se for automatico ele utiliza a frequencia da serie
  # o lag da parte sazonal deve ser um inteiro
  # guess_frequency() sempre retorna um inteiro, para os per?odos tratados nessse trabalho.
  # ?nica exce??o tempo = semana , retonra 52.18. Utilizamos floor(52.18) = 52
  
  if(lag_seasonal_dif == "automatico"){lag_seasonal_dif =  serie_modificada %>% pull(granularidade) %>% guess_frequency() %>% floor() }
  
  
  
  # Ajuste dos modelos
  
  string_transformacao = switch(tipo_transformacao,
                                identidade = 'total',
                                box_cox = glue('box_cox(total, {lambda})'),
                                sqrt = 'sqrt(total)',
                                inversa = '1/total',
                                log = 'log(total)')
  
  #usuario
  
  PQD = ifelse(seasonal_dif == 0,'PDQ(0,0,0)', glue('PDQ({P},{seasonal_dif},{Q}, period = {lag_seasonal_dif})'))
  string_modelo_usuario= glue('0 + pdq({p},{dif},{q}) + {PQD}')
  formula_usuario = as.formula(paste(string_transformacao,'~',string_modelo_usuario))
  
  
  # modelo sugerido
  
  string_modelo_sugerido = glue('0 + {pqd_modelo_sugerido} + PDQ(0,0,0)')
  formula_modelo_sugerido = as.formula(paste(string_transformacao,'~',string_modelo_sugerido))
  
  
  # SARIMA
  
  formula_SARIMA = as.formula(glue('{string_transformacao} ~ 0'))
  
  fit_usuario <- if ('usuario' %in% modelos) {
    serie %>%
      model(usuario = ARIMA(formula_usuario, stepwise = FALSE))
  } else {
    NA
  }
  
  fit_ingenuo <- if ('modelo_pia' %in% modelos) {
    serie %>%
      model(modelo_pia = ARIMA(formula_modelo_sugerido, stepwise = FALSE))
  } else {
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
    auto_sarima = fit_AUTO_sarima,
    .name_repair = c('minimal')
  )
  
  # para o relat?rio
  fit <- fit %>% select(!where(is.na))
  
  return(fit)
  
  
  # para o relatorio
  
}







# Ui
ui <- fluidPage(
  useShinyjs(),
  tags$style(
    HTML(
      "
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
  
  uiOutput("conteudo")
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
  Lista_Tempo <- reactiveVal(c("semanal","mensal","trimestral","semestral","anual"))
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
  shinyjs::disable("consulta")
  shinyjs::disable("exp")
  shinyjs::disable("map")
  shinyjs::disable("series")
  shinyjs::disable("diference")
  shinyjs::disable("decomp")
  
  ##### oBServes inciaiis
  
  
  observe({
    dado_filtrado(dado_real())
  })
  
  observeEvent(input$button1,{
    estado("consulta")
    modalContent <- modalDialog(
      title = "Aviso!",
      "Modificações dos dados serão propagados para todas as análises.
                  Para retornar e utilizar os dados originais clique em *Não usar Filtro ",
      footer = tagList(
        actionButton("close_popup", "Fechar")
      )
    )
    
    # Mostra o modal dialog
    showModal(modalContent)
    
    # Adiciona reação ao botão de fechar
    observeEvent(input$close_popup, {
      removeModal()
    }
    )
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
      
      
      ##################### Inicio 
      
      
      
      output$inicial <- renderUI({ 
        mainPanel(
          fluidRow(
            column(12, align = "center", h1(paste0(tema()), style = "text-align: center;"))
          ),
          fluidRow(
            column(3,actionButton("tes", "Escolher outro tema", class = "custom-button") ),
            column(2,offset = 6, actionButton("button1", "Visualizar dados", class = "custom-button"))),
          
          if (tema() == "IML") {
            
            Side1(Side_iml)
            
            if (is.null(dado7())) {
              dado7(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/iml_parcial.csv"))
              dado7(dado7() %>% mutate(data_ocorr = as.Date(datahora_iml_reg)))
              dado7(dado7() %>% mutate(data_bo_reg = as.Date(datahora_iml_reg)))
            }
            dado_real(dado7())
            
            fluidRow(
              HTML("Os dados apresentam informações básicas sobre todas as 
                           entradas de óbitos no IML desde 2013, quando foi implantado no Estado, pela Superintendência da Polícia 
                           Técnico-Científica, o serviço digital Gestão de Laudos (GDL). (Fonte: GDL da SPTC).")
            )
            
          }
          
          
          else if (tema() == "Latrocínio") {
            Side1(Side_lat)
            
            
            if (is.null(dado3())) {
              dado3(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/latrocinio_parcial.csv"))
            }
            
            dado_real(dado3())
            
            fluidRow(
              HTML("
              <p>O latrocínio é um crime hediondo descrito pelo parágrafo 3 do artigo 157 do código penal, é definido como a subtração de coisa móvel alheia mediante grave ameaça ou violência a pessoa, em que da violência resultar lesão corporal grave ou morte, sem agravantes ou alterações de pena descritas.</p>
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de janeiro de 2018 a dezembro de 2022.</p>
            ") )
          }
          
          
          else if (tema() == "LCSM") {
            Side1(Side_lcs)
            
            if (is.null(dado4())) {
              dado4(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/lesaoCSM_parcial.csv"))
            }
            
            dado_real(dado4())
            
            fluidRow(
              HTML("
              <p>A lesão corporal seguida de morte é descrita pelo parágrafo 3 do artigo 129 do código penal, definida pela ofensa à integridade corporal ou saúde de outrem tendo por resultado a morte, com as circunstâncias evidenciando que o agente não quis o resultado, nem assumiu o risco de produzi-lo.</p>
              <p>É considerado agravado quando:</p>
              <ul>
                <li>Se a lesão for praticada contra ascendente, descendente, irmão, cônjuge ou companheiro, ou com quem conviva ou tenha convivido, ou, ainda, prevalecendo-se o agente das relações domésticas, de coabitação ou de hospitalidade</li>
                <li>Se a lesão for praticada contra autoridade ou agente descrito nos arts. 142 e 144 da Constituição Federal, integrantes do sistema prisional e da Força Nacional de Segurança Pública, no exercício da função ou em decorrência dela, ou contra seu cônjuge, companheiro ou parente consanguíneo até terceiro grau, em razão dessa condição, a pena é aumentada de um a dois terços</li>
                <li>Se a lesão for praticada contra a mulher, por razões da condição do sexo feminino</li>
              </ul>
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
            ") )  
          }
          
          
          else if (tema() == "Roubo de Veículo") {
            Side1(Side_rouv)
            
            
            if (is.null(dado11())) {
              dado11(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/rouboVeiculo_parcial.csv"))
              dado11(  dado11() %>%
                         mutate(data_bo_reg = as.Date(datahora_bo_reg),
                                nome_municipio_circ=cidade_ocorr))
              
            }
            
            dado_real(dado11())
            
            fluidRow(
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
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
            ") )
            
            
            
          }
          
          
          else if (tema() == "Furto de Veículo") {
            Side1(Side_furv)
            
            if (is.null(dado12())) {
              dado12(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/furtoVeiculo_parcial.csv"))
              dado12(  dado12() %>%
                         mutate(data_bo_reg = as.Date(datahora_bo_reg),
                                nome_municipio_circ=cidade_ocorr))
              
            }
            
            dado_real(dado12())
            
            fluidRow(
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
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
            ") )
            
            
          }
          
          
          else if (tema() == "MDIP") {
            Side1(Side_mdi)
            
            if (is.null(dado5())) {
              dado5(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/morteDIP_parcial.csv"))
            }
            
            dado_real(dado5())
            
            fluidRow(
              HTML("
              <p>A morte decorrente de intervenção policial é um crime militar impróprio, tipificado no artigo 205 artigo 9º, inciso II, alínea b da Constituição Federal de 1988, caracterizado pelos abusos cometidos pelas forças policiais.</p>
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
            ")  )
          }
          
          
          else if (tema() == "Morte Suspeita") {
            Side1(Side_mor)
            
            if (is.null(dado6())) {
              dado6(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/morteSuspeita_parcial.csv"))
              dado6(dado6() %>% mutate(data_bo_reg = as.Date(datahora_bo_reg)))
            }
            
            dado_real(dado6())
            
            fluidRow(
              HTML("
              <p>A morte suspeita é definida nos incisos I a IV, do Artigo 2º, da Portaria DGP nº 14/2005, descrita como:</p>
              <ol>
                <li>Encontro de cadáver sem lesões aparentes: Encontro de cadáver, ou parte relevante deste, em qualquer estágio de decomposição, no qual existam lesões aparentes ou quaisquer outras circunstâncias que, mesmo indiciariamente, apontem para a produção violenta da morte</li>
                <li>Dúvidas razoáveis quanto a suicídio ou morte provocada: Morte violenta em que subsistam dúvidas razoáveis quanto a tratar-se de suicídio ou morte provocada por outrem</li>
                <li>Morte acidental: Morte não natural onde existam indícios de causação acidental do evento exclusivamente por ato não intencional da própria vítima</li>
                <li>Morte súbita e natural: Morte súbita, sem causa determinante aparente, ocorrida de modo imprevisto, com a vítima fora do respectivo domicílio e sem a assistência de médico, familiar ou responsável</li>
              </ol>
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
            "))
            
          }
          
          
          else if (tema() == "Dados Criminais") {
            Side1(Side_dad)
            
            if (is.null(dado8())) {
              dado8(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/dadosCriminais_parcial.csv", header = TRUE))
            }
            
            dado_real(dado8())
            
            fluidRow(
              HTML("Contém todos os dados de boletins de ocorrência, dos temas presentes, do ano de 2022.")  )
            
            
          }
          
          else if (tema() == "Roubo de Celular") {
            Side1(Side_rouc)
            
            if (is.null(dado9())) {
              dado9(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/rouboCelular_parcial.csv"))
              dado9(  dado9() %>%
                        mutate(data_bo_reg = as.Date(datahora_bo_reg),
                               nome_municipio_circ=cidade_ocorr
                        ))
              
            }
            
            dado_real(dado9())
            
            fluidRow(
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
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
            ")   )
            
          }
          
          
          else if (tema() == "Furto de Celular") {
            Side1(Side_furc)
            column(8,
                   textOutput("furto_texto")
            )
            
            if (is.null(dado10())) {
              
              dado10(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/furtoCelular_parcial.csv"))
              dado10(  dado10() %>%
                         mutate(qtde_celular = ifelse(qtde_celular == "não informado", "-1", qtde_celular),
                                data_bo_reg = as.Date(datahora_bo_reg),
                                nome_municipio_circ=cidade_ocorr
                         ))
              
            }
            
            dado_real(dado10())
            
            output$furto_texto <- renderUI({
              fluidRow(
                
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
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
            ")
                
              )
            })
            
          }
          
          
          else if (tema() == "Homicídio") {
            Side1(Side_hom)
            
            
            if (is.null(dado1())) {
              dado1(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/homicidioDoloso_parcial.csv"))
            }
            
            dado_real(dado1())
            fluidRow(
              
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
            <p>A base traz algumas dessas qualificações incluindo feminicídio, trazendo boletins de janeiro de 2003 a dezembro de 2022.</p>
          ")
              
            )
            
            
            
            
            
            
            
          }
          
          
          
          else if (tema() == "Feminicídio") {
            Side1(Side_fem)
            
            
            if (is.null(dado2())) {
              dado2(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/feminicidio_parcial.csv"))
              dado2(dado2() %>% mutate(data_bo_reg = as.Date(datahora_bo_reg)))
            }
            
            dado_real(dado2())
            
            fluidRow(
              
              HTML("
              <p>O feminicídio, tal qual o homicídio, é descrito pelo artigo 121 do Código Penal, é definido como o assassinato de uma mulher em razão das condições do sexo feminino, de forma que o homicídio torna-se feminicídio quando, em conjunto ao assassinato, há violência doméstica ou familiar e menosprezo ou discriminação à condição da mulher, tendo como agravantes quando o crime é praticado:</p>
              <ul>
                <li>Durante a gestação ou nos 3 meses posteriores ao parto</li>
                <li>Contra pessoa menor de 14 (catorze) anos, maior de 60 (sessenta) anos, com deficiência ou portadora de doenças degenerativas que acarretem condição limitante ou de vulnerabilidade física ou mental</li>
                <li>Na presença física ou virtual de descendente ou de ascendente da vítima</li>
                <li>Em descumprimento das medidas protetivas de urgência previstas nos incisos I, II e III do caput do art. 22 da Lei nº 11.340, de 7 de agosto de 2006</li>
              </ul>
              <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de abril de 2015 a dezembro de 2022.</p>
            ")          )
            
            
            
          }
          
          
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
  criarObservador2("LCSM", tema)
  criarObservador2("MDIP", tema)
  criarObservador2("Morte Suspeita", tema)
  criarObservador2("IML", tema)
  criarObservador2("Dados Criminais", tema)
  criarObservador2("Roubo de Celular", tema)
  criarObservador2("Furto de Celular", tema)
  criarObservador2("Roubo de Veículo", tema)
  criarObservador2("Furto de Veículo", tema)
  
  
  
  render_initial_content <- function(){
    mainPanel(
      titlePanel("Escolha um tema"),
      br(),
      div(
        id = "mostrar_categ",
        
        fluidRow(id = "botoes_categorias_1",
                 column(2,
                        actionButton("Homicídio", "Homicídio", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Feminicídio", "Feminicídio", class = "custom-button",
                                     style = "margin-right: 50px;")
                 ),
                 column(2,
                        actionButton("Latrocínio", "Latrocínio", class = "custom-button")
                 ),
                 column(2,
                        actionButton("LCSM", "LCSM", class = "custom-button")
                 )
        ),
        fluidRow(id = "botoes_categorias_2",
                 column(2,
                        actionButton("MDIP", "MDIP", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Morte Suspeita", "Morte Suspeita", class = "custom-button")
                 ),
                 column(2,
                        actionButton("IML", "IML", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Dados Criminais", "Dados Criminais", class = "custom-button")
                 )
        ),
        fluidRow(id = "botoes_categorias_3",
                 column(2,
                        actionButton("Roubo de Celular", "Roubo de Celular", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Furto de Celular", "Furto de Celular", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Roubo de Veículo", "Roubo de Veículo", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Furto de Veículo", "Furto de Veículo", class = "custom-button")
                 )
        )),
      hr())
  }
  
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
                         
                         sidebarLayout(
                           sidebarPanel(
                             titlePanel("Painel Interativo de análises"),
                             fluidRow(
                               column(12,h3("Sobre")),
                               column(12, p("O PIA foi desenvolvido com base nos dados fornecidos pela Secretaria de Segurança Pública do Estado de São Paulo ao longo dos anos. Este portal oferece uma variedade de ferramentas que permitem aos usuários realizar análises aoexploratórias, criar visualizações gráficas e espaciais ao longo do tempo, tudo isso com a opção de filtrar ou não os dados conforme necessário.",
                               ))),
                             hr(), 
                             titlePanel("Objetivos"),
                             fluidRow(
                               column(12, p("Essa plataforma é o resultado do projeto da disciplina de Séries Temporais onde visa tornar acessível a análise de dados, permitino que pessoas de diferentes campos e níveis explorem insights de maneira intuitiva e personalizada. Como resultado a teoria é transportada para a pática.",
                               )))
                           ),
                           
                           uiOutput("inicial")
                         )
                         
                         
                       },
                       
                       "series" = {
                         fluidPage(
                           h1(tema(), style = "text-align: center;"),
                           hr(),
                           fluidRow(
                             column(2,
                                    verticalLayout(
                                      #h1(tema()),
                                      
                                      h3("Opções"),
                                      
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
                                      ),
                                      hr(),
                                      h3("Diferenciação"),
                                      materialSwitch(
                                        inputId = "Id077",
                                        label = "Diferenciar", 
                                        value = FALSE,
                                        status = "primary"
                                      ),
                                      fluidRow(
                                        column(6,numericInput("serie_ordem", "Ordem", 1, min = 1, max = 5, step = NA))),
                                      hr(),
                                      h3("Diferenciação Sazonal"),
                                      materialSwitch(
                                        inputId = "Id078",
                                        label = "Ativado", 
                                        value = FALSE,
                                        status = "primary"
                                      ),
                                      fluidRow(column(6,numericInput("serie_ordem2", "Ordem", 1, min = 1, max = 5, step = NA)),
                                               column(6,numericInput("serie_lag2", "Lag Sazonal", 1, min = 1, max = 5, step = NA)))
                                      
                                      
                                      
                                    )),
                             column(8,
                                    tabsetPanel( id = "tabs",
                                                 tabPanel("Gráfico no Tempo",
                                                          plotlyOutput("cont"),
                                                 ),
                                                 tabPanel("Gráfico de Sazonalidade",
                                                          plotlyOutput("cont2"),
                                                 ),
                                                 tabPanel("Gráfico de Defasagens",
                                                          plotlyOutput("cont3"),
                                                 ),
                                                 tabPanel("Autocorrelação",
                                                          value = "autoc",
                                                          plotlyOutput("auto1"),
                                                          br(),br(),br(),br(),br(),
                                                          br(),br(),br(),br(),br(),
                                                          br(),br(),br(),br(),br(),
                                                          plotlyOutput("auto2")),
                                                 
                                                 tabPanel("Decomposição",
                                                          plotlyOutput("decomp_serie")))),
                             column(2,
                                    verticalLayout(
                                      fluidRow(column(8,
                                                      h2("Testes")),column(2,br(),actionButton("info_testes",label=NULL,icon=icon("info-sign", lib = "glyphicon")))),
                                      actionButton("teste1","ADF"),
                                      actionButton("teste2","KPSS"),
                                      actionButton("teste3","Cox Stuart"),
                                      actionButton("teste4","Kendall"),
                                      actionButton("teste5","Wavk"),
                                      actionButton("teste6","Sazonal"),
                                      hr(),
                                      h2("Modelo"),
                                      actionButton("gerar_modelo","Criação de Modelos"),
                                      
                                    ))),
                         )
                         
                         
                         
                         
                         
                         
                         
                       }
                       ,
                       
                       "map" = {
                         fluidRow(
                           leafletOutput("MAPA")
                           
                         )
                         
                       },
                       "diference" = {},
                       
                       "decomp" = {
                         fluidPage(
                           
                           fluidRow(
                             column(2,
                                    verticalLayout(
                                      h1(tema()),
                                      
                                      h3("Modelos"),
                                      
                                      materialSwitch(
                                        inputId = "Modelo_Usuario",
                                        label = "Modelo Usuário", 
                                        value = FALSE,
                                        status = "primary"
                                      ),
                                      fluidRow(column(6,numericInput("pzin", "p", 1, min = 1, max = 5, step = NA)),
                                               column(6,numericInput("pzao", "P", 1, min = 1, max = 5, step = NA))),
                                      fluidRow(column(6,numericInput("qzin", "q", 1, min = 1, max = 5, step = NA)),
                                               column(6,numericInput("qzao", "Q", 1, min = 1, max = 5, step = NA))),
                                      fluidRow(column(6,numericInput("dzin", "d", 1, min = 1, max = 5, step = NA)),
                                               column(6,numericInput("dzao", "D", 1, min = 1, max = 5, step = NA))),
                                      
                                      
                                      
                                      
                                      materialSwitch(
                                        inputId = "Modelo_Ingenuo",
                                        label = "Modelo Ingênuo", 
                                        value = FALSE,
                                        status = "primary"
                                      ),
                                      
                                      materialSwitch(
                                        inputId = "Modelo_Sarima",
                                        label = "Modelo Auto SARIMA", 
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
                                    )),
                             column(8,
                                    tabsetPanel(
                                      tabPanel("modelos",
                                               verbatimTextOutput("summary2")),
                                      
                                      tabPanel("Raízes características",
                                               plotOutput("plot1")),
                                      
                                      tabPanel("Resíduos",
                                               plotOutput("plot2")),
                                      
                                      tabPanel("Predição",
                                               plotOutput("plot3")),
                                      
                                      tabPanel("Summary",
                                               verbatimTextOutput("plot4")),
                                      
                                    )),
                             column(2,
                                    verticalLayout(
                                      h2("Testes"),
                                      actionButton("test7","Ljung Box"),
                                      h2("Relatório"),
                                      actionButton("teste1","Gerar Relatório")
                                    ))))
                       },
                       
                       
                       
                       
                       
                       "consulta" = {
                         fluidPage(
                           
                           fluidRow(
                             column(2,
                                    verticalLayout(
                                      h1(tema()),
                                      hr(),
                                      Side1() )),
                             
                             column(8,
                                    tabsetPanel(
                                      
                                      tabPanel("Informações gerais",
                                               fluidRow(
                                                 plotlyOutput("serie_ano"),
                                                 column(6,tableOutput("tabela1")),
                                                 column(6,tableOutput("tabela2")))),
                                      
                                      tabPanel("Gráficos",
                                               plotlyOutput("graficos")),
                                      
                                      tabPanel("Tabela",
                                               DTOutput("tabela_1")))),
                             column(2,
                                    tabsetPanel(
                                      tabPanel(
                                        'Gráficos',
                                        fluidRow(
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
                                               fluidRow(
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
                             )
                           ))
                         
                       },
                       "exp" = {
                         fluidPage(
                           h2("Nada por enquanto")
                         )
                         
                       },
                       fluidRow(
                         column(12, h2("Conteúdo da página Padrão")),
                         column(12, p("Este é o conteúdo da página Padrão."))
                       )
    )
    
    return(conteudo)
  })
  
  observeEvent(input$toggle_sidebar, {
    if (shinyjs::hasClass("sidebarPanelId", "collapsed")) {
      shinyjs::removeClass("sidebarPanelId", "collapsed")
    } else {
      shinyjs::addClass("sidebarPanelId", "collapsed")
    }
  })
  
  
  observeEvent(input$consulta, {
    modalContent <- modalDialog(
      title = "Aviso!",
      "Modificações dos dados serão propagados para todas as análises.
                  Para retornar e utilizar os dados originais clique em *Não usar Filtro ",
      footer = tagList(
        actionButton("close_popup", "Fechar")
      )
    )
    
    # Mostra o modal dialog
    showModal(modalContent)
    
    # Adiciona reação ao botão de fechar
    observeEvent(input$close_popup, {
      removeModal()
    }
    )
    
    
    
  })
  
  
  
  observeEvent(input$aplicar, {
    shinyjs::enable("map")
    shinyjs::enable("series")
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
    data <- input$dateRange2
    cor <- input$cor_pele
    tveiculo <- input$tipo_veiculo
    anov <- input$ano_mod_veiculo
    qtd_cel <- input$quantidade_celular
    descr_local <- input$descr_local
    natureza <- input$natureza  
    sexo <- input$sexo
    corp <- input$corp 
    
    desdobramento <- input$desdobramento
    
    if (!is.null(faixa_etaria) || !is.null(data) || !is.null(cor) || !is.null(tveiculo) || !is.null(anov) || !is.null(qtd_cel) || !is.null(descr_local) || !is.null(natureza) || !is.null(sexo) || !is.null(desdobramento) || !is.null(corp)) {
      
      filtro <- TRUE
      
      if (!is.null(data)) {
        filtro <- filtro & (dado_real()$data_ocorr >= data[1] & dado_real()$data_ocorr <= data[2])
      }
      
      if(tema()== "Homicídio" | tema()=="Feminicídio" | tema()=="Latrocínio" | tema()=="LCSM" | tema()=="MDIP"){
        if (!is.null(cor)) {
          filtro <- filtro & (dado_real()$cor_pele %in% cor)
        }}
      
      if(tema()== "Homicídio" | tema()=="Feminicídio" | tema()=="Latrocínio" | tema()=="LCSM" | tema()=="MDIP" | tema()=="Morte Suspeita"){
        if (!is.null(faixa_etaria)) {
          filtro <- filtro & (dado_real()$idade_pessoa >= faixa_etaria[1] & dado_real()$idade_pessoa <= faixa_etaria[2])
        }}
      
      if (tema()== "Homicídio"| tema()== "Latrocínio" | tema()== "LCSM" | tema()=="MDIP" ) {
        if (!is.null(sexo)) {
          filtro <- filtro & (dado_real()$sexo_pessoa %in% sexo)
        }}
      
      if(tema()== "MDIP"){
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
          filtro <- filtro & (dado_real()$ano_veiculo %in% anov)
        }}
      
      if(tema()=="Roubo de Celular" | tema()=="Furto de Celular"){
        if (!is.null(qtd_cel)) {
          filtro <- filtro & (dado_real()$qtde_celular >= as.integer(qtd_cel[1]) & dado_real()$qtde_celular <= as.integer(qtd_cel[2]))
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
    shinyjs::enable("map")
    shinyjs::enable("series")
  })
  
  output$graficos <- renderPlotly({
    var1 <- input$Var1
    var2 <- input$Var2
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
        
        return(Graph)
        
      } else{
        Graph <- ggplot(dado_filtrado(), aes(x = !!sym(var1))) +
          geom_histogram(fill = "skyblue", color = "black", bins = 10, alpha = 0.7) +
          geom_density(color = "red") +
          labs(title = "Distribuição da Variável Contínua", x = "Valor", y = "Frequência")
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
            labs(title = "Contagem de Combinações de Cor de Pele e Profissão",
                 x = "Cor de Pele",
                 y = "Contagem",
                 fill = "Profissão") +
            theme_minimal()
          
          Graph <- ggplotly(Graph)
          Graph <- Graph %>% layout(width = 800, height = 600)
          
          
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
          Graph <- Graph %>% layout(width = 800, height = 600)
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
          Graph <- Graph %>% layout(width = 800, height = 600)
          return(Graph)
          
        }else{
          Graph <- ggplot(dado_filtado(), aes(x = !!sym(var1), y = !!sym(var2))) +
            geom_point() +
            labs(title = "Scatter Plot entre Duas Variáveis Contínuas",
                 x = "Variável Contínua 1",
                 y = "Variável Contínua 2") +
            theme_minimal()
          Graph <- ggplotly(Graph)
          Graph <- Graph %>% layout(width = 800, height = 600)
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
      df2 <- dado_real()
      df2$longitude_ocorr <- as.numeric(df2$longitude_ocorr)
      df2$latitude_ocorr <- as.numeric(df2$latitude_ocorr)
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
      serie <- dados_filtrados_tempo(dado_filtrado(),Serie_Tempo())
      armazem_serie_original(serie)
      serie <- get_transformacoes(serie,Serie_Transformacao())
      
      
      if(input$Id077 == TRUE){
        lag <- 1
        ordem <- input$serie_ordem
        
        serie <- diff(serie,lag,ordem)
        
        if(input$Id078 == TRUE){
          
          lag2 <- input$serie_lag2
          ordem2 <- input$serie_ordem2
          
          serie <- diff(serie,lag2,ordem2)
        }
      }
      
      
      serie
    }
  })
  
  
  
  output$cont <- renderPlotly({
    Graph <- NULL
    transformacao <- Serie_Transformacao()
    Graph <- autoplot(Serie_Atual(), .vars = !!sym(colnames(Serie_Atual())[2]) ) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste('Número de ocorrências por ', Graph_Tempo())) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly_chart <- ggplotly(Graph)
    
    plotly_chart <- plotly_chart %>% layout(width = 800, height = 600)
    
    plotly_chart
    
  })
  
  
  output$cont2 <- renderPlotly({
    
    Graph <- NULL
    
    Graph <- Serie_Atual() %>% gg_season(y = !!sym(colnames(Serie_Atual())[2]), labels = "none") + xlab(paste(Graph_Tempo())) + theme_pubclean()
    
    plotly_chart <- ggplotly(Graph)
    
    plotly_chart <- plotly_chart %>% layout(width = 800, height = 600)
    
    plotly_chart
  })
  
  output$cont3 <- renderPlotly({
    Graph <- NULL
    
    Graph <- Serie_Atual() %>% gg_lag(y = !!sym(colnames(Serie_Atual())[2]), lags = 1:12) + xlab('lag') + theme_classic()
    
    plotly_chart <- ggplotly(Graph)
    
    plotly_chart <- plotly_chart %>% layout(width = 800, height = 600)
    
    plotly_chart
  })
  
  
  output$auto1 <- renderPlotly({
    Graph <- NULL
    serie <- Serie_Atual()
    
    Graph <- autoplot(serie %>% ACF()) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste('Autocorrelação -- Defasagem:', Graph_Tempo())) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly_chart <- ggplotly(Graph)
    
    plotly_chart <- plotly_chart %>% layout(width = 800, height = 600)
    
    plotly_chart 
    
  })
  
  output$auto2 <- renderPlotly({
    Graph <- NULL
    serie <- Serie_Atual()
    
    Graph <- autoplot(serie %>% PACF()) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste('Autocorrelação Parcial -- Defasagem:', Graph_Tempo())) +
      theme(plot.title = element_text(hjust = 0.5))
    
    plotly_chart <- ggplotly(Graph)
    
    plotly_chart <- plotly_chart %>% layout(width = 800, height = 600)
    
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
    
    plotly_chart <- plotly_chart %>% layout(width = 800, height = 600)
    
    plotly_chart
  })
  
  
  
  
  
  
  
  observeEvent(input$teste1,{
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
    
    observeEvent(input$close_popup, {
      removeModal()
    })
  })
  
  observeEvent(input$teste6,{
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
    
    observeEvent(input$close_popup, {
      removeModal()
    })
  })
  
  observeEvent(input$teste4,{
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
    
    observeEvent(input$close_popup, {
      removeModal()
    })
  })
  
  observeEvent(input$teste3,{
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
    
    observeEvent(input$close_popup, {
      removeModal()
    })
  })
  
  
  observeEvent(input$teste2,{
    serie <- Serie_Atual()
    
    kpss1 <- teste_kpss(serie, !!sym(colnames(serie)[2]))
    kpss2 <- teste_kpss_numero_diff(serie, !!sym(colnames(serie)[2]))
    kpss3 <- teste_kpss_season_diff(serie, !!sym(colnames(serie)[2]))
    
    texto_summary  <- capture.output(print(kpss1))
    texto_summary2 <- capture.output(print(kpss2))
    texto_summary3 <- capture.output(print(kpss3))
    
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
      cat(texto_summary2, sep = "\n")
      cat(texto_summary3, sep = "\n")
    })
    
    showModal(modalContent)
    
    observeEvent(input$close_popup, {
      removeModal()
    })
  })
  
  
  observeEvent(input$teste7,{
    teste7 <- teste_ljung_box(fit(),input$model)
    
    
    
    texto_summary <- capture.output(print(teste7))
    
    modalContent <- modalDialog(
      title = "Teste Ljung Box",
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
    
    observeEvent(input$close_popup, {
      removeModal()
    })
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
    
    plotly_chart <- plotly_chart %>% layout(width = 800, height = 600)
    
    plotly_chart
  })
  
  
  
  
  
  observeEvent(input$sugerido,{ 
    SERIE <- armazem_serie_original()
    colnames(SERIE)[2] <- "total"
    vetor2 <- c()
    
    modelo_usuario <- input$Modelo_Usuario
    
    modelo_ingenuo <- input$Modelo_Ingenuo
    
    modelo_sarima <- input$Modelo_Sarima
    
    if (modelo_usuario){
      vetor2 <- append(vetor2,"usuario")
    }
    
    if (modelo_ingenuo){
      vetor2 <- append(vetor2,"modelo_pia")
    }
    
    if (modelo_sarima){
      vetor2 <- append(vetor2,"auto_sarima")
    }
    
    fit(fit_serie(SERIE,modelos = vetor2))
    
    updatePickerInput(session,"model",choices = vetor2)
  })
  
  
  output$summary2 <- renderPrint({
    if (is.null(fit())){
      "Nenhum modelo gerado!"
    }else{
      fit()
    }
  })
  
  
  output$plot1 <- renderPlot({
    if (is.null(fit())){
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
      "Nenhum modelo gerado!"
    }else{
      serie <- armazem_serie_original()
      plot_pred <- plot_predicao(fit(),input$model,12,serie)
      
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
          modalContent <- modalDialog(
            title = "A série possivelmente não é estacionária!",
            "Não tire conclusões acerca da ordem de um modelo preditivo.",
            footer = tagList(
              actionButton("close_popup", "Fechar")
            )
          )
          
          # Mostra o modal dialog
          showModal(modalContent)
          
          # Adiciona reação ao botão de fechar
          observeEvent(input$close_popup, {
            removeModal()
          })
          
        } } })
  }
  
  
  
  observeEvent(input$tabs, {
    if (!is.null(input$tabs) && input$tabs == "autoc") {
      serie<-Serie_Atual()
      kk <- teste_kpss(serie,!!sym(colnames(serie)[2]))
      j <- round(kk[2],3)
      if (j < 0.05){
        modalContent <- modalDialog(
          title = "A série possivelmente não é estacionária!",
          "Não tire conclusões acerca da ordem de um modelo preditivo.",
          footer = tagList(
            actionButton("close_popup", "Fechar")
          )
        )
        
        # Mostra o modal dialog
        showModal(modalContent)
        
        # Adiciona reação ao botão de fechar
        observeEvent(input$close_popup, {
          removeModal()
        })
        
      } } })
  
  
  
  
  observeEvent(input$gerar_modelo,{
    shinyjs::enable("decomp")
    
    estado("decomp")
    
  })
  
  
  
}





shinyApp(ui, server)




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
        fit(fit_serie(SERIE,tipo_transformacao = armazem_transformacao(),
                      modelo_usuario = input$Modelo_Usuario ,
                      modelo_ingenuo = input$Modelo_Ingenuo,
                      modelo_auto = input$Modelo_Sarima))
        
        updatePickerInput(session,"model",choices=list("usuario","ingenuo","AUTO_sarima"))
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
      
      
      
      addTooltip(session=session,id="ADF",title="Hello! This is a hover pop-up. You'll have to click to see the next one.")
      
      
      
      
      
      
      
    }
    
    
    
    
    
    shinyApp(ui, server)

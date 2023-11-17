library(rstatix)
library(plotly)
library(ggplot2)
library(dplyr)
library(magrittr)
library(geobr)
library(pdftables)
library(readr)
library(stringr)
library(abjutils)
library(ggspatial)
library(tibble)
library(knitr)
library(sf)	 	
url <-
  "https://cetesb.sp.gov.br/aguas-interiores/wp-content/uploads/sites/12/2019/10/Ap%C3%AAndice-C_Dados-de-Saneamento-por-Munic%C3%ADpio.pdf" 


#Fazer o download do arquivo PDF

download.file(url, 
              destfile = "c:/trabalho_r/dados/apendice_c_2018.pdf", 
              method = "curl" 
)			  

# Converter o arquivo PDF em CSV.

pdftables::convert_pdf("c:/trabalho_r/dados/apendice_c_2018.pdf",
                       output_file = "c:/trabalho_r/dados/apendice_c_2018.csv",
                       api_key = "a1pnyn6zkqbb")

DbApendice_c_2018 <-
  readr::read_csv(
    "c:/trabalho_r/dados/apendice_c_2018.csv", 
    col_names = c(
      # define o nome das colunas
      "ugrhi",
      "municipio",
      "consessao",
      "pop_urbana",
      "atendimento_coleta_porc",
      "atendimento_tratamento_porc",
      "eficiencia",
      "carga_poluidora_potencial",
      "carga_poluidora_remanecente",
      "ictem",
      "corpo_receptor"
    ),
    locale = readr::locale(encoding = "UTF-8"),
    skip = 5 
  )				   

#Vamos fazer a analise de dados

#Vai mostrar os valor valor faltantes
sapply(DbApendice_c_2018,function(x) sum(is.na(x)))
#Vai mostrar os valor valores indefinidos
sapply(DbApendice_c_2018,function(x) sum(is.nan(x)))


# Filtrar linhas que não contém nome de municípios, esta parte faz parte dos tratamentos 
# dos dados.
# Faço o filtro e coloco o conteudo na mesma estrutura de memoria 
DbApendice_c_2018 <- DbApendice_c_2018 %>%
  dplyr::filter(!municipio %in% c("Estado de São Paulo", "Município", NA, "MUNICÍPIO")) 

# Esta parte vai sair no console a estrutura da do dados DbApendice_c_2018, nele é possivel
# verificar que todos os campos são do tipo chr, que para o nosso trabalho não pode continuar
tibble::glimpse(DbApendice_c_2018)

# Os campos da estrutura veio todos caracter e temos que converter para os tipos corretos
# Os campos atendimento_coleta_porc e atendimento_tratamento_porc , eficiencia , carga_poluidora_potencial 
# carga_poluidora_remanecente e ictem foram convertidos para double
DbApendice_c_2018 <- DbApendice_c_2018 %>%
  dplyr::mutate(
    pop_urbana = as.double(pop_urbana) , 
    atendimento_coleta_porc = as.double(atendimento_coleta_porc),
    atendimento_tratamento_porc = as.double(atendimento_tratamento_porc),
    eficiencia = as.double(eficiencia),
    
    #Tratamento para retirar o virgual e transformar em ponto, esta parte faz parte dos trabamento de dados
    carga_poluidora_potencial =  stringr::str_replace_all(carga_poluidora_potencial, "\\.", "") %>%
      as.double(),
    
    carga_poluidora_remanecente =  stringr::str_replace_all(carga_poluidora_remanecente, "\\.", "") %>%
      as.double(),
    ictem =  stringr::str_replace_all(ictem, ",", "\\.") %>% as.double()
    
  )

# Esta parte vai sair no console a estrutura da do dados DbApendice_c_2018, depois das transformações
# nele é possivel ver que os campos convertidos.
tibble::glimpse(DbApendice_c_2018)


# Muito importante é conhecer os seus dados, abaixo vai dois exemplo de funções
#retorna a media do coluna atendimento_coleta_porc
mean(DbApendice_c_2018$atendimento_coleta_porc)

#podemos ver toda as colunas numericas para fazer uma analise inicial dos dados
summary(DbApendice_c_2018)

#retorna a distribuicao dos valores da coluna  atendimento_coleta_porc
hist(DbApendice_c_2018$atendimento_coleta_porc, col = "red")
hist(DbApendice_c_2018$ictem, col = "blue")

#vamos analisar os valores minimo e maximos da coluna atendimento_coleta_porc 
min(DbApendice_c_2018$atendimento_coleta_porc)
max(DbApendice_c_2018$atendimento_coleta_porc)

#Podemos analisar os valores minimos de duas colunas.
summarize_at(DbApendice_c_2018,vars(atendimento_coleta_porc,atendimento_tratamento_porc ),min )

#podemos analisar a amplitude de uma determinada coluna
# no valor abaixo vemos que temos uma diferença muito grande entre os valores desta coluna, isto pode 
# indicar um outlayer para esta coluna, retorna os valores minimos e maximos
range(DbApendice_c_2018$carga_poluidora_remanecente ) 

#Vamos analisar o Quartis da coluna atendimento_coleta_porc
#função quantile em R é usada para calcular os quantis de uma distribuição de dados. 
#Ela é útil para analisar a dispersão e a distribuição dos dados, ajudando a compreender a sua estrutura. 
#Por meio da determinação de quantis, é possível entender a localização e a dispersão dos valores em um conjunto 
#de dados, o que é crucial para identificar padrões, outliers e tendências.
#A análise dos quantis ajuda na compreensão da assimetria e da dispersão dos dados, permitindo uma compreensão mais profunda 
#da distribuição dos dados. Isso é essencial para a realização de análises estatísticas e para a tomada de decisões
quantile(DbApendice_c_2018$atendimento_coleta_porc)

#O IQR (Interquartile Range) em R é uma medida estatística que indica a dispersão dos dados em torno da mediana. 
#Ele é calculado subtraindo o primeiro quartil (Q1) do terceiro quartil (Q3) de um conjunto de dados. 
#O IQR é útil para identificar a variação dos dados, ajudando a compreender a amplitude em que a maioria dos dados está concentrada. 
#Sua análise é crucial para detectar a presença de outliers e entender a distribuição dos dados. 
#Ao comparar o IQR entre diferentes conjuntos de dados, é possível ter insights sobre a variabilidade relativa entre eles, 
#o que auxilia na identificação de padrões e tendências em análises estatísticas e de dados.
IQR(DbApendice_c_2018$atendimento_coleta_porc)

#A função summary em R é utilizada para obter uma visão geral dos dados, gerando estatísticas descritivas 
#como média, mediana, mínimo, máximo e quartis para um conjunto de valores. Ela é útil para uma rápida compreensão 
#da distribuição dos dados, destacando informações-chave sobre o conjunto de valores, o que facilita a identificação
#de possíveis outliers, assimetrias e tendências.
summary(DbApendice_c_2018$atendimento_coleta_porc)

#A função boxplot em R é usada para visualizar a distribuição de um conjunto de dados, destacando medidas estatísticas 
#importantes, como quartis, mediana e possíveis outliers.
#Nesta analise da coluna carga_poluidora é possivel ver que temos 3 valores muito maior que todos os outros valores, 
##  isto que chamamos de outliers
boxplot(DbApendice_c_2018$carga_poluidora_remanecente)

#Vamos agora analisar os outliers da carga_poluidora_remanecente e como podemos tratar estes valores.
DbApendice_c_2018 %>% identify_outliers(carga_poluidora_remanecente)
# vamos gravar em uma estrutura os valores outliers
outlier <- c(boxplot.stats(DbApendice_c_2018$carga_poluidora_remanecente)$out)
print(outlier)
#vamos gravar em uma outra estrutra todos os valores da estrutra DbApendice_c_2018
# menos os valores identificados como outliers
DbApendice_c_2018_s_outlier_total <- DbApendice_c_2018[-c(which(DbApendice_c_2018$carga_poluidora_remanecente %in% outlier)),]

# Podemos agora comparar as duas a coluna carga_poluidora_remanecente das duas estruturas
summary(DbApendice_c_2018_s_outlier_total$carga_poluidora_remanecente)
summary(DbApendice_c_2018$carga_poluidora_remanecente)

#Vamos identificar o maior outlier
DbApendice_c_2018_maior_outlier<-DbApendice_c_2018 %>% filter( carga_poluidora_remanecente == max(DbApendice_c_2018$carga_poluidora_remanecente))
#Vamos criar uma nova tabela sem este valor.
DbApendice_c_2018_s_outlier_parcial<-DbApendice_c_2018 %>% filter( carga_poluidora_remanecente == max(DbApendice_c_2018$carga_poluidora_remanecente))

# vamos calcular algumas medidas de dispersão variente e desvio padrao
#A análise da variância e do desvio padrão é importante na Estatística para compreender a dispersão 
#dos dados em torno da média. A variância mede a extensão da dispersão dos valores em relação à média, 
#enquanto o desvio padrão é a medida da dispersão dos valores em torno da média. 
#A análise dessas medidas permite avaliar o grau de variabilidade dos dados e identificar o quão distantes 
#os valores estão da média, o que é crucial para entender a consistência e a confiabilidade dos dados em uma distribuição
# Podemos calcular a variente dos valores
var(DbApendice_c_2018$ictem)
#Podemos calcular desvio padrao
sd(DbApendice_c_2018$ictem)

table(DbApendice_c_2018$consessao)

# Lê a base de dados espaciais dos municípios do Estado de São Paulo,
# no ano de 2018, segundo o IBGE, isto sera necessario pois base da CETESB não possui
# o codigo do IBGE e vamos precisar dele para ter as coordenadas geograficas
municipios_sp_ibge <- geobr::read_municipality("SP", 2022)

ugrhis <- tibble::tibble(
  ugrhi = c("2",
            "3",
            "5",
            "6"
            ),
  nome_ugrhi = c(
    " Paraíba do Sul",
    " Litoral Norte",
    " Piracicaba/Capivari/Jundiaí",
    " Alto Tietê"
    )
)

# vamos normalizar o nome do municpio passando para minusculo, retirando o - e trocando  por vazio
# e retirando os acentos., vamos criar uma coluna nova chamada nome_muni
municipios_sp_ibge <-
  municipios_sp_ibge %>% dplyr::mutate(
    nome_muni = stringr::str_to_lower(name_muni),
    nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
    nome_muni = abjutils::rm_accent(nome_muni)
  )

DbApendice_c_2018 <- DbApendice_c_2018 %>% dplyr::mutate(
  nome_muni =  dplyr::case_when(
    municipio == "São Luís do Paraitinga" ~
      "São Luiz do Paraitinga",
    TRUE ~ municipio
  ),
  nome_muni = stringr::str_to_lower(nome_muni),
  nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
  nome_muni = abjutils::rm_accent(nome_muni))

DbApendice_c_2018_geo <-
  dplyr::full_join(municipios_sp_ibge, DbApendice_c_2018) %>%
  dplyr::left_join(ugrhis)

DbSenamento <- DbApendice_c_2018_geo %>%
  dplyr::select(-nome_muni,-municipio,-code_state) %>% # Remove colunas duplicadas
  dplyr::filter(ugrhi %in% ugrhis$ugrhi) %>% # Filtra a coluna UGRHI. Apenas as UGRHIS que estão na tibble
  # criada, permanecerão.
  dplyr::mutate(porc_real_trat = atendimento_tratamento_porc * atendimento_coleta_porc / 100) # Cria uma nova
# coluna, com o cálculo do número real de porcentagem de tratamento de esgoto.


# Escolha do tema do mapa
tema_mapa <-
  theme_bw() + 
  
  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.6,
      size = 9
    ),
    axis.text.x = element_text(size = 9),
    axis.title.y = element_text(size = rel(0.9)),
    axis.title.x = element_text(size = rel(0.9)),
    panel.grid.major = element_line(
      color = gray(0.8),
      linetype = "dashed",
      size = 0.1
    ),
    panel.background = element_rect(fill = "white") +
      annotation_scale(location = "br", width_hint = 0.30)
  )

## mapa da região dos ugrhi  2,3,5,6
DbSenamento %>% # Base de dados usada
  ggplot() + # Inicia o gráfico ggplot
  geom_sf(data = DbApendice_c_2018_geo,
          # Camada do mapa da base completa (Estado SP)
          alpha = .9,
          color = NA) +
  geom_sf(aes(fill = nome_ugrhi)) + # Camada do mapa da base saneamento
  # Adiciona Título e Legendas
  labs(fill = "UGRHI",
       title = "Municípios analisados, segundo a UGRHI") +
  # Adiciona o Norte Geográfico
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  # Adiciona o tema criado anteriormente
  tema_mapa 

## mapa da região por coleta de esgoto
DbSenamento %>%
  ggplot() +
  geom_sf(aes(fill = atendimento_coleta_porc)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + # Escala de cores
  labs(fill = "Porcentagem de \natendimento de \ncoleta de esgoto",
       title = "Porcentagem de atendimento de coleta de esgoto, por município",
       subtitle = "Dados da CETESB, para o ano de 2018.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

## mapa da região por porc_real_trat
DbSenamento %>%
  ggplot() +
  geom_sf(aes(fill = porc_real_trat)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100)) +
  labs(fill = "% de \natendimento de \ntratamento de esgoto ",
       title = "Porcentagem de atendimento de tratamento de esgoto, por município",
       subtitle = "Calculado a partir de dados da CETESB, para o ano de 2018.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 


## mapa das areas de concessão
DbSenamento %>%
  ggplot() +
  geom_sf(aes(fill = consessao)) +
  labs(fill = "Empresas \nConsessionaria",
       title = "Empresas concessionarias de serviços",
       subtitle = "Dados da CETESB, para o ano de 2018.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

## mapa das areas mostrando a carga poluidora potencial
DbSenamento %>%
  ggplot() +
  geom_sf(aes(fill = carga_poluidora_potencial)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, max(DbSenamento$carga_poluidora_potencial))) + # Escala de cores
  labs(fill = "Valor de \ncarga poluidora\npotencial",
       title = "Valor carga poluidora potencial por município",
       subtitle = "Dados da CETESB, para o ano de 2018.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

## mapa das areas com Indicador de Coleta e Tratabilidade de Esgoto da População Urbana de Município
DbSenamento %>%
  ggplot() +
  geom_sf(aes(fill = ictem)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, max(DbSenamento$ictem))) +
  labs(fill = "Indcador de \na 0 a 10",
       title = "Indicador de Coleta e Tratabilidade de Esgoto da População Urbana de Município",
       subtitle = "Calculado a partir de dados da CETESB, para o ano de 2018.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

## mapa das areas mostrando a carga poluidora remancente
DbSenamento %>%
  ggplot() +
  geom_sf(aes(fill = carga_poluidora_remanecente)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, max(DbSenamento$carga_poluidora_remanecente))) + # Escala de cores
  labs(fill = "Valor de \ncarga poluidora\npotencial",
       title = "Valor carga poluidora remanecente por município",
       subtitle = "Dados da CETESB, para o ano de 2018.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 


## vamos usar a função read_csv2 pois o delimitador é o ; e não a , como no arquivo anterior
dbMunicipio_Saneamento <-
  readr::read_csv2(
    "c:/trabalho_r/dados/municipios_saneamento.csv", # Qual arquivo CSV quero carregar
    col_names = c(
      # define o nome das colunas
      "Cidade",
      "UF",
      "Populacao",
      "Possui_Plano_Municipal",
      "Perc_Sem_agua",
      "Perc_sem_Esgoto",
      "Perc_sem_coleta_lixo",
      "Perc_dom_suj_inundacao"
    ),
    locale = readr::locale(encoding = "UTF-8"),
    skip = 1 
  )				   

dbMunicipio_Saneamento <- dbMunicipio_Saneamento %>% dplyr::mutate(
  Perc_Sem_agua =  dplyr::case_when(
    Perc_Sem_agua == "Atendimento Pleno" ~
      "0",
    TRUE ~ Perc_Sem_agua
  ),
  Perc_Sem_agua = stringr::str_to_lower(Perc_Sem_agua),
  Perc_Sem_agua = stringr::str_replace_all(Perc_Sem_agua, "%", ""),
#
Perc_sem_Esgoto =  dplyr::case_when(
  Perc_sem_Esgoto == "Atendimento Pleno" ~
    "0",
  TRUE ~ Perc_sem_Esgoto
),
Perc_sem_Esgoto = stringr::str_replace_all(Perc_sem_Esgoto, "%", ""),
#
Perc_sem_coleta_lixo =  dplyr::case_when(
  Perc_sem_coleta_lixo == "Atendimento Pleno" ~
    "0",
  TRUE ~ Perc_sem_Esgoto
),
Perc_sem_coleta_lixo = stringr::str_replace_all(Perc_sem_coleta_lixo, "%", ""),
#
Perc_dom_suj_inundacao =  dplyr::case_when(
  Perc_dom_suj_inundacao == "Não há" ~
    "0",
  TRUE ~ Perc_dom_suj_inundacao
),
Perc_dom_suj_inundacao = stringr::str_replace_all(Perc_dom_suj_inundacao, "%", ""),
)

dbMunicipio_Saneamento <- dbMunicipio_Saneamento %>% dplyr::mutate(
  nome_muni =  dplyr::case_when(
    Cidade == "São Luís do Paraitinga" ~
      "São Luiz do Paraitinga",
    TRUE ~ Cidade
  ),
  nome_muni = stringr::str_to_lower(nome_muni),
  nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
  nome_muni = abjutils::rm_accent(nome_muni))

dbMunicipio_Saneamento_geo <-
  dplyr::full_join(DbApendice_c_2018_geo, dbMunicipio_Saneamento) %>%
  dplyr::left_join(ugrhis)

#
dbMunicipio_Saneamento_geo <- dbMunicipio_Saneamento_geo %>%
  dplyr::select(-name_muni,-abbrev_state,-municipio,-code_state,-code_region,-name_region,-UF, -name_state)

dbMunicipio_Saneamento_geo <- dbMunicipio_Saneamento_geo %>%
  dplyr::filter(ugrhi %in% ugrhis$ugrhi)  # Filtra a coluna UGRHI. Apenas as UGRHIS que estão na tibble
  

#
dbMunicipio_Saneamento_geo <- dbMunicipio_Saneamento_geo %>%
  dplyr::mutate(
    #Tratamento para retirar o virgual e transformar em ponto, esta parte faz parte dos trabamento de dados
    Perc_Sem_agua =  stringr::str_replace_all(Perc_Sem_agua, ",", ".")%>% as.double(),
    Perc_sem_coleta_lixo =  stringr::str_replace_all(Perc_sem_coleta_lixo, ",", ".")%>% as.double(),
    Perc_sem_Esgoto =  stringr::str_replace_all(Perc_sem_Esgoto, ",", ".")%>% as.double(),
    Perc_dom_suj_inundacao =  stringr::str_replace_all(Perc_dom_suj_inundacao, ",", ".")%>% as.double()
  )


## mapa das regiões sujeitas a inundações.
dbMunicipio_Saneamento_geo %>%
  ggplot() +
  geom_sf(aes(fill = Perc_dom_suj_inundacao)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + # Escala de cores
  labs(fill = "Porcentagem de \nDomicilios de \nSujeitos a inundação",
       title = "Porcentagem de domicilios sujeitos a inundação",
       subtitle = "Dados do Instituto agua e Saneamento 2023.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

## mapa das regiões sem coleta de lixo.
dbMunicipio_Saneamento_geo %>%
  ggplot() +
  geom_sf(aes(fill = Perc_sem_coleta_lixo)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + # Escala de cores
  labs(fill = "Porcentagem de \nDomicilios de \nSem coleta de lixo",
       title = "Porcentagem de domicilios sem coleta de lixo",
       subtitle = "Dados do Instituto agua e Saneamento 2023.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

## mapa das regiões sem Agua.
dbMunicipio_Saneamento_geo %>%
  ggplot() +
  geom_sf(aes(fill = Perc_Sem_agua)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + # Escala de cores
  labs(fill = "Porcentagem de \nDomicilios de \nSem Agua",
       title = "Porcentagem de domicilios sem agua",
       subtitle = "Dados do Instituto agua e Saneamento 2023.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

## mapa das regiões sem Agua.
dbMunicipio_Saneamento_geo %>%
  ggplot() +
  geom_sf(aes(fill = Perc_sem_Esgoto)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100)) + # Escala de cores
  labs(fill = "Porcentagem de \nDomicilios de \nSem Esgoto",
       title = "Porcentagem de domicilios sem esgoto",
       subtitle = "Dados do Instituto agua e Saneamento 2023.") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

#Vamos analisar um pouco das opções de graficos
#Queremos ver as Quantidade de municpios, administrador por uma concessariaria de energia na ugrhi =5
#Piracicaba/Capivari/Jundiaí, no qual esta contido a cidade de Bragança Paulista
# No Codigo abaixo vamos primeiro fazer um filtro na coluna ugrhi =5
DbApendice_c_2018_ugrhi_5 <- DbApendice_c_2018 %>% filter( ugrhi == 5)

# Apos isto totalizar a quantidade de Consessao, e vamos armazenar o valor no campo Qunatidade
tabela_contagem <- DbApendice_c_2018_ugrhi_5 %>%
  group_by(consessao) %>%
  summarise(Quantidade = n())

# No Exemplo abaixo, vamos ver um grafico de barra.
ggplot(tabela_contagem, aes(x = consessao, y = Quantidade)) +
  geom_bar(stat = "identity", fill = "steelblue") +labs( title = "Quantidade de Empresas Concessionarias",
                                                         subtitle = "UGRHI (Piracicaba/Capivari/Jundiaí)",
                                                         x = "Consessao", y = "Quantidade")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) 

# No Exemplo abaixo, vamos ver um grafico de Pizza
plot_ly(tabela_contagem, labels = ~consessao, values = ~Quantidade, type = 'pie')

#Claramente no grafico de Pizza como tenho 20 elementos não ficou legal, vamos fazer um novo recorte queremos 
#mostrar somente os 5 primeiras empresas com as maiores quantidades de consessão de municipio.
#para isto temos que ordenar primeiro a nossa tabela.
tbtot_concessao_ordenada <- tabela_contagem[order(tabela_contagem$Quantidade, decreasing = TRUE), ]

#Apos isto vamos criar uma outra tabela com os 5 primeiros registros da tabela tbtot_concessao_ordenada
tbtot_concessao_top5 <-  tbtot_concessao_ordenada[1:5, ]

#Voltamos a ver o grafico de pizza com menos elementos
plot_ly(tbtot_concessao_top5, labels = ~consessao, values = ~Quantidade, type = 'pie',
        pull = c(0.2, 0, 0, 0,0),
        textposition = "inside",
        textinfo = "label+percent",
        insidetextorientation="tangential"
        )|>
  layout(title = "Graficos das 5 maiores consessionarias",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  

#Vamos agora fazer um grafico no formato de Dunuts :-)
plot_ly(tbtot_concessao_top5, labels = ~consessao, values = ~Quantidade,
        textposition = "inside",
        textinfo = "label+percent",
        insidetextorientation="tangential"
)|>
  add_pie(hole = 0.6) |>   
  layout(title = "Graficos das 5 maiores consessionarias",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
###########################
#Vamos trazer os dados de Piuai , para mostrar o indicadore de percentual sem coleta de lixo
# pega o arquivo minunicipios_saneamento_piaui.csv e armazena na estrutura dbMunicipio_Saneamento_piaui
dbMunicipio_Saneamento_piaui<-
  readr::read_csv2(
    "c:/trabalho_r/dados/municipios_saneamento_piaui.csv", # Qual arquivo CSV quero carregar
    col_names = c(
      # define o nome das colunas
      "Cidade",
      "UF",
      "Populacao",
      "Possui_Plano_Municipal",
      "Perc_Sem_agua",
      "Perc_sem_Esgoto",
      "Perc_sem_coleta_lixo",
      "Perc_dom_suj_inundacao"
    ),
    locale = readr::locale(encoding = "UTF-8"),
    skip = 1 
  )				   

#faz o tratamento nas colunas para poder trabalhar com estes dados.
dbMunicipio_Saneamento_piaui <- dbMunicipio_Saneamento_piaui %>% dplyr::mutate(
  # troca o valor Atendimento Pleno por 0
  Perc_Sem_agua =  dplyr::case_when(
    Perc_Sem_agua == "Atendimento Pleno" ~
      "0",
    TRUE ~ Perc_Sem_agua
  ),
  # troca o valor Atendimento Pleno por % por nada
  Perc_Sem_agua = stringr::str_replace_all(Perc_Sem_agua, "%", ""),
  # troca o valor Atendimento Pleno por 0
  Perc_sem_Esgoto =  dplyr::case_when(
    Perc_sem_Esgoto == "Atendimento Pleno" ~
      "0",
    TRUE ~ Perc_sem_Esgoto
  ),
  Perc_sem_Esgoto = stringr::str_replace_all(Perc_sem_Esgoto, "%", ""),
  # troca o valor Atendimento Pleno por 0
  Perc_sem_coleta_lixo =  dplyr::case_when(
    Perc_sem_coleta_lixo == "Atendimento Pleno" ~
      "0",
    TRUE ~ Perc_sem_coleta_lixo
  ),
  # troca o valor Atendimento Pleno por % por nada
  Perc_sem_coleta_lixo = stringr::str_replace_all(Perc_sem_coleta_lixo, "%", ""),
  Perc_sem_coleta_lixo = stringr::str_replace_all(Perc_sem_coleta_lixo, "--", ""),
  Perc_sem_coleta_lixo = stringr::str_replace_all(Perc_sem_coleta_lixo, ",", "."),
  Perc_sem_coleta_lixo = as.double(Perc_sem_coleta_lixo),
  Perc_dom_suj_inundacao =  dplyr::case_when(
    Perc_dom_suj_inundacao == "Não há" ~
      "0",
    TRUE ~ Perc_dom_suj_inundacao
  ),
  # troca o valor Atendimento Pleno por % por nada
  Perc_dom_suj_inundacao = stringr::str_replace_all(Perc_dom_suj_inundacao, "%", ""),
)
view(dbMunicipio_Saneamento_piaui)

dbMunicipio_Saneamento_piaui <-
  dbMunicipio_Saneamento_piaui %>% dplyr::mutate(
    nome_muni = stringr::str_to_lower(Cidade),
    nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
    nome_muni = abjutils::rm_accent(nome_muni)
  )

# Recupera  os dados de Geolocalizacao do Estado de Piaui 
municipios_piau_ibge <- geobr::read_municipality("PI", 2022)

municipios_piau_ibge <-
  municipios_piau_ibge %>% dplyr::mutate(
    nome_muni = stringr::str_to_lower(name_muni),
    nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
    nome_muni = abjutils::rm_accent(nome_muni)
  )

#faz o join unindo as duas estruturas, o join e feito pela coluna nome_muni que tem nas duas tabelas
DbApendice_c_2018_piau_geo_total <-
  dplyr::full_join(municipios_piau_ibge, dbMunicipio_Saneamento_piaui)

#copia os as linhas que a coluna Perc_sem_coleta_lixo é diferente de NA
DbApendice_c_2018_piau_geo_parcial <- DbApendice_c_2018_piau_geo_total %>%
  dplyr::filter(!Perc_sem_coleta_lixo %in% c(NA)) 

#copia os as linhas que a coluna Perc_sem_coleta_lixo são maior que zero, pois havia 1 caso negativo 
DbApendice_c_2018_piau_geo_parcial <- DbApendice_c_2018_piau_geo_parcial %>%
  dplyr::filter(Perc_sem_coleta_lixo >0 ) 
view(DbApendice_c_2018_piau_geo_parcial)

#Mostra dos dados o grafico do estado de PIAU
DbApendice_c_2018_piau_geo_parcial %>% # Base de dados usada
  ggplot() + # Inicia o gráfico ggplot
  geom_sf(data = DbApendice_c_2018_piau_geo_total,
          # Camada do mapa da base completa (Estado do PIAUI)
          alpha = .9,
          color = NA) +
  #  geom_sf(aes(fill = DbApendice_c_2018_piau_geo_parcial$name_muni))   
  geom_sf(aes(fill = Perc_sem_coleta_lixo)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100)) +  
  # Adiciona Título e Legendas
  labs(fill = "Percentual\nSem Coleta de Lixo",
       title = "Municípios do Estado de Piauí",
       subtitle = "Percentual Sem Coleta de Lixo"
  ) +
  # Adiciona o Norte Geográfico
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  # Adiciona o tema criado anteriormente
  tema_mapa 

dbMunicipio_Saneamento_geo_sp <-
  dplyr::full_join(DbApendice_c_2018_geo, dbMunicipio_Saneamento)


dbMunicipio_Saneamento_geo_sp_parcial<-dbMunicipio_Saneamento_geo_sp %>% filter( Perc_sem_coleta_lixo != '--')

dbMunicipio_Saneamento_geo_sp_parcial <- dbMunicipio_Saneamento_geo_sp_parcial %>%
  dplyr::filter(Perc_sem_coleta_lixo >0 ) 

dbMunicipio_Saneamento_geo_sp_parcial <- dbMunicipio_Saneamento_geo_sp_parcial %>%
  dplyr::mutate(
    #Tratamento para retirar o virgual e transformar em ponto, esta parte faz parte dos trabamento de dados
    Perc_sem_coleta_lixo =  stringr::str_replace_all(Perc_sem_coleta_lixo, ",", ".")%>% as.double(),
  )


#Mostra dos dados o grafico do estado de São Paulo
dbMunicipio_Saneamento_geo_sp_parcial %>% 
  ggplot() + # Inicia o gráfico ggplot
  geom_sf(data = DbApendice_c_2018_geo,
          # Camada do mapa da base completa (Estado SP)
          alpha = .9,
          color = NA) +
  #  geom_sf(aes(fill = DbApendice_c_2018_piau_geo_parcial$name_muni))   
  geom_sf(aes(fill = Perc_sem_coleta_lixo)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100)) +  
  # Adiciona Título e Legendas
  labs(fill = "Percentual\nSem Coleta de Lixo",
       title = "Municípios do Estado de São Paulo",
       subtitle = "Percentual Sem Coleta de Lixo") +
  # Adiciona o Norte Geográfico
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  # Adiciona o tema criado anteriormente
  tema_mapa 
############################
#vamos agora trazer uma base que tem alguns indices, e o que vamos usar é o IDHM da escucação
dbMunicipio_idhm_piaui<-
  readr::read_csv2(
    "c:/trabalho_r/dados/dados_idhm_piaui.csv", # Qual arquivo CSV quero carregar
    col_names = c(
      # define o nome das colunas
      "Municipios",
      "IDHM_Educacao_2010",
      "Sub_freq_IDHM_Educ_2010",
      "Sub_esc_IDHM_Educ"    ),
    locale = readr::locale(encoding = "UTF-8"),
    skip = 1 
  )				   

#vamos filtrar os dados que vazios
dbMunicipio_idhm_piaui <- dbMunicipio_idhm_piaui %>% dplyr::filter(!IDHM_Educacao_2010 %in% c( NA )) 


#vamos criar uma coluna nome_muni para poder fazer o join com a tabela georeferenciada
#nesta tabela vamos criar o IDHM_EDUCACAO_2010
dbMunicipio_idhm_piaui <- dbMunicipio_idhm_piaui %>% dplyr::mutate(
  nome_muni = substr(Municipios, 1, nchar(Municipios) -5),
  nome_muni = stringr::str_to_lower(nome_muni),
  nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
  nome_muni = abjutils::rm_accent(nome_muni),
  resultado_idhm_educacao = case_when(
    IDHM_Educacao_2010 >= 0 & IDHM_Educacao_2010 <= 0.499 ~ "Muito baixo",
    IDHM_Educacao_2010 > 0.499 & IDHM_Educacao_2010 <= 0.599 ~ "Baixo",
    IDHM_Educacao_2010 > 0.599 & IDHM_Educacao_2010 <= 0.699 ~ "Médio",
    IDHM_Educacao_2010 > 0.699 & IDHM_Educacao_2010 <= 0.799 ~ "Alto",
    IDHM_Educacao_2010 > 0.799 ~  "Muito Alto")
  
)

#Sera realizado o JOIN com a tabela GeoReferenciada com a tabela dbMunicipio_idhm_piaui
DbApendice_c_2018_piau_geo_idhm <-
  dplyr::full_join(DbApendice_c_2018_piau_geo_total,dbMunicipio_idhm_piaui )


DbApendice_c_2018_piau_geo_idhm_parcial <- DbApendice_c_2018_piau_geo_idhm %>%
  dplyr::filter(nome_muni %in% DbApendice_c_2018_piau_geo_parcial$nome_muni) 


#Mostra dos dados o grafico do estado de PIAU
DbApendice_c_2018_piau_geo_idhm_parcial %>% # Base de dados usada
  ggplot() + # Inicia o gráfico ggplot
  geom_sf(data = DbApendice_c_2018_piau_geo_total,
          # Camada do mapa da base completa (Estado do PIAUI)
          alpha = .9,
          color = NA) +
  #  geom_sf(aes(fill = DbApendice_c_2018_piau_geo_parcial$name_muni))   
  geom_sf(aes(fill = resultado_idhm_educacao)) +
  # Adiciona Título e Legendas
  labs(fill = "Avaliação\nDHM Educação",
       title = "Municípios do Estado de Piauí - Avaliação do IDHM Educação") +
  # Adiciona o Norte Geográfico
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

##
#vamos agora trazer uma base que tem alguns indices, e o que vamos usar é o IDHM da escucação
dbMunicipio_idhm_sao_paulo<-
  readr::read_csv2(
    "c:/trabalho_r/dados/dados_idhm_sao_paulo.csv", # Qual arquivo CSV quero carregar
    col_names = c(
      # define o nome das colunas
      "Municipios",
      "IDHM_2010",
      "IDHM_Educacao_2010"  ),
    locale = readr::locale(encoding = "UTF-8"),
    skip = 1 
  )			

#vamos filtrar os dados que vazios
dbMunicipio_idhm_sao_paulo <- dbMunicipio_idhm_sao_paulo %>% dplyr::filter(!IDHM_Educacao_2010 %in% c( NA )) 

dbMunicipio_idhm_sao_paulo <- dbMunicipio_idhm_sao_paulo %>% dplyr::mutate(
  nome_muni = substr(Municipios, 1, nchar(Municipios) -5),
  nome_muni = stringr::str_to_lower(nome_muni),
  nome_muni = stringr::str_replace_all(nome_muni, "-", " "),
  nome_muni = abjutils::rm_accent(nome_muni),
  resultado_idhm_educacao = case_when(
    IDHM_Educacao_2010 >= 0 & IDHM_Educacao_2010 <= 0.499 ~ "Muito baixo",
    IDHM_Educacao_2010 > 0.499 & IDHM_Educacao_2010 <= 0.599 ~ "Baixo",
    IDHM_Educacao_2010 > 0.599 & IDHM_Educacao_2010 <= 0.699 ~ "Médio",
    IDHM_Educacao_2010 > 0.699 & IDHM_Educacao_2010 <= 0.799 ~ "Alto",
    IDHM_Educacao_2010 > 0.799 ~  "Muito Alto")
  
)

#Sera realizado o JOIN com a tabela GeoReferenciada com a tabela dbMunicipio_idhm_piaui
DbApendice_c_2018_sao_paulo_geo_idhm <-
  dplyr::full_join(municipios_sp_ibge,dbMunicipio_idhm_sao_paulo )

DbApendice_c_2018_sao_paulo_geo_idhm <- DbApendice_c_2018_sao_paulo_geo_idhm %>% dplyr::filter(!IDHM_Educacao_2010 %in% c( NA )) 

#Vamos mostrar o mapa dos municipios de São Paulo, com base do campo resultado_idhm_educacao 
DbApendice_c_2018_sao_paulo_geo_idhm %>% # Base de dados usada
  ggplot() + # Inicia o gráfico ggplot
  geom_sf(data = municipios_sp_ibge,
          # Camada do mapa da base completa (Estado do PIAUI)
          alpha = .9,
          color = NA) +
  geom_sf(aes(fill = resultado_idhm_educacao)) +
  # Adiciona Título e Legendas
  labs(fill = "Avaliação\nDHM Educação",
       title = "Municípios do Estado de São Paulo",
       subtitle = "Avaliação do IDHM Educação") +
  # Adiciona o Norte Geográfico
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 
############################################################################################################
#                      MANIPULAÇÃO DOS DADOS ELEITORAIS
############################################################################################################

rm(list = ls())

# Pacotes:
library(dplyr)
library(tidyr)
library(stringr)
library(electionsBR)
library(gdata)
library(stringi)
library(eeptools)
library(lubridate)
library(gmodels)
library(summarytools)

# Carregando as bases de dados eleitorais previamente extraídas:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
load("resultados_electionsBR")


# ----------------------------------------------------------------------------------------------------------


# Função que realiza o procedimento de manipulação em todas as bases de dados:

funcao_tse_resultados <- function(dados_tse_resultados) {
  
  # Mantendo apenas candidatos à prefeito e eleições de primeiro turno:
  dados_eleicao <- dados_tse_resultados %>%
    select(ANO_ELEICAO, SIGLA_UF, NOME_MUNICIPIO, CODIGO_MUNICIPIO, NUMERO_ZONA, SQ_CANDIDATO,
           NOME_CANDIDATO, NOME_URNA_CANDIDATO, TOTAL_VOTOS, DESC_SIT_CAND_TOT,
           CODIGO_SIT_CAND_TOT, DESCRICAO_CARGO, SIGLA_PARTIDO, NUMERO_PARTIDO,  everything()) %>%
    filter(DESCRICAO_CARGO == "PREFEITO", NUM_TURNO == 1) %>%
    mutate(NUMERO_ZONA = paste("zona", as.character(NUMERO_ZONA), sep = "_")) %>%
    mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(NOME_MUNICIPIO, "Latin-ASCII")), "-", " "),
           MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
           MUNICIPIO = str_squish(MUNICIPIO),
           MUNICIPIO = str_replace_all(MUNICIPIO, "'", ""),
           MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
           MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
           MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
           MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
           MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
           MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
           MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
           MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
           MUNICIPIO = str_replace_all(MUNICIPIO, "´", '')) %>%
    mutate(cod_uf = as.character(case_when(SIGLA_UF == "RO" ~ 11,
                              SIGLA_UF == "AC" ~ 12,
                              SIGLA_UF == "AM" ~ 13,
                              SIGLA_UF == "RR" ~ 14,
                              SIGLA_UF == "PA" ~ 15,
                              SIGLA_UF == "AP" ~ 16,
                              SIGLA_UF == "TO" ~ 17,
                              SIGLA_UF == "MA" ~ 21,
                              SIGLA_UF == "PI" ~ 22,
                              SIGLA_UF == "CE" ~ 23,
                              SIGLA_UF == "RN" ~ 24,
                              SIGLA_UF == "PB" ~ 25,
                              SIGLA_UF == "PE" ~ 26,
                              SIGLA_UF == "AL" ~ 27,
                              SIGLA_UF == "SE" ~ 28,
                              SIGLA_UF == "BA" ~ 29,
                              SIGLA_UF == "MG" ~ 31,
                              SIGLA_UF == "ES" ~ 32,
                              SIGLA_UF == "RJ" ~ 33,
                              SIGLA_UF == "SP" ~ 35,
                              SIGLA_UF == "PR" ~ 41,
                              SIGLA_UF == "SC" ~ 42,
                              SIGLA_UF == "RS" ~ 43,
                              SIGLA_UF == "MS" ~ 50,
                              SIGLA_UF == "MT" ~ 51,
                              SIGLA_UF == "GO" ~ 52,
                              SIGLA_UF == "DF" ~ 53))) %>% 
    select(-NOME_MUNICIPIO, everything()) %>% 
    select(ANO_ELEICAO, cod_uf, SIGLA_UF, MUNICIPIO, CODIGO_MUNICIPIO, NUMERO_ZONA, SQ_CANDIDATO,
           NOME_CANDIDATO, NOME_URNA_CANDIDATO, TOTAL_VOTOS, DESC_SIT_CAND_TOT,
           CODIGO_SIT_CAND_TOT, DESCRICAO_CARGO, SIGLA_PARTIDO, NUMERO_PARTIDO,  everything()) %>% 
    rename(NUMERO_CANDIDATO = NUMERO_CAND)
  
  # Arrumando a base de dados, de modo a termos cada candidato representado em uma linha,
  # e o número de votos total é dado pela soma dos votos em cada zona eleitoral.
  
  # Fazendo isso em vários comandos:
  dados_eleicao <- dados_eleicao %>%
    spread(key = NUMERO_ZONA, value = TOTAL_VOTOS) 
  
  # Criando a variável "votos totais", que soma os votos em cada zona eleitoral:
  dados_eleicao$votos_candidato = rowSums(dados_eleicao %>% select(starts_with("zona_")), na.rm = TRUE)
  
  # Criando as variáveis de total de votos válidos e vote share:
  dados_eleicao <- dados_eleicao %>%
    select(ANO_ELEICAO, cod_uf, SIGLA_UF, MUNICIPIO, CODIGO_MUNICIPIO, SQ_CANDIDATO,
           NOME_CANDIDATO, NOME_URNA_CANDIDATO, votos_candidato, DESC_SIT_CAND_TOT,
           CODIGO_SIT_CAND_TOT, DESCRICAO_CARGO, SIGLA_PARTIDO, NUMERO_PARTIDO,
           everything(), -starts_with("zona")) %>%
    group_by(CODIGO_MUNICIPIO, MUNICIPIO) %>%
    mutate(votos_eleicao = sum(votos_candidato), vote_share = votos_candidato/votos_eleicao) %>%
    ungroup() %>%
    mutate(regiao = case_when(SIGLA_UF %in% c('AL', 'BA', 'CE', 'MA', 'PI', 'PE', 'PB', 'RN', 'SE') ~ 'nordeste',
                                     SIGLA_UF %in% c('GO', 'MT', 'MS', 'DF') ~ 'centro-oeste',
                                     SIGLA_UF %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 'norte',
                                     SIGLA_UF %in% c('PR', 'RS', 'SC') ~ 'sul',
                                     SIGLA_UF %in% c('ES', 'MG', 'RJ', 'SP') ~ 'sudeste')) %>%
    select(ANO_ELEICAO, cod_uf, SIGLA_UF, MUNICIPIO, NOME_CANDIDATO, NOME_URNA_CANDIDATO,
           votos_candidato, vote_share, votos_eleicao, DESC_SIT_CAND_TOT, SIGLA_PARTIDO,
           NUMERO_PARTIDO, DESCRICAO_CARGO, SIGLA_PARTIDO, NUMERO_PARTIDO, everything())
  
  
  # Criando uma variável que indica o número de candidatos em cada eleição:
  dados_eleicao <- dados_eleicao %>%
    group_by(CODIGO_MUNICIPIO, MUNICIPIO) %>%
    summarise(numero_candidatos = n()) %>%
    inner_join(dados_eleicao, by = c("MUNICIPIO", "CODIGO_MUNICIPIO")) %>% 
    ungroup() %>% 
  filter(!(CODIGO_MUNICIPIO %in% c(19909, 19305) & ANO_ELEICAO == 2000))
    
}


# Utilizando a função para arrumar a base de dados de cada ano:
resultados_2000 <- funcao_tse_resultados(resultados_2000_electionsbr)
resultados_2004 <- funcao_tse_resultados(resultados_2004_electionsbr)
resultados_2008 <- funcao_tse_resultados(resultados_2008_electionsbr)
resultados_2012 <- funcao_tse_resultados(resultados_2012_electionsbr)


resultados <- bind_rows(resultados_2000, resultados_2004, resultados_2008, resultados_2012) %>% arrange(SIGLA_UF, MUNICIPIO)

# ----------------------------------------------------------------------------------------------------


# Corrigindo o nome de alguns municípios:
resultados$MUNICIPIO <- gsub(' +', ' ', resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "NOVO HORIZONTE DOESTE"] <- "NOVO HORIZONTE DO OESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ELDORADO DOS CARAJAS"] <- "ELDORADO DO CARAJAS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SALINOPLIS"] <- "SALINOPOLIS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA ISABEL DO PARA"] <- "SANTA IZABEL DO PARA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "COUTO DE MAGALHAES"] <- "COUTO MAGALHAES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO VALERIO DA NATIVIDADE"] <- "SAO VALERIO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "GOVERNADOR LUIS ROCHA"] <- "GOVERNADOR LUIZ ROCHA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SENADOR LA ROQUE"] <- "SENADOR LA ROCQUE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "COCAL DO ALVES"] <- "COCAL DOS ALVES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "LAGOA DO SAO FRANCISCO"] <- "LAGOA DE SAO FRANCISCO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "PALMERAIS"] <- "PALMEIRAIS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO FRANCISCO DE ASSIS DO PIA"] <- "SAO FRANCISCO DE ASSIS DO PIAUI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "DEPUTADO IRAPUAN RIBEIRO"] <- "DEPUTADO IRAPUAN PINHEIRO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "DEPUTADO IRAPUAN RIBEIRO"] <- "DEPUTADO IRAPUAN PINHEIRO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ITAPAGE"] <- "ITAPAJE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "PENA FORTE"] <- "PENAFORTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO LUIZ DO CURU"] <- "SAO LUIS DO CURU"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ALTO DOS RODRIGUES"] <- "ALTO DO RODRIGUES"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 24 & resultados$MUNICIPIO == "PRESIDENTE JUSCELINO", "SERRA CAIADA", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "BELEM DO BREJO DA CRUZ"] <- "BELEM DO BREJO DO CRUZ"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 25 & resultados$MUNICIPIO == "SANTAREM", "JOCA CLAUDINO", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO DOMINGOS DE POMBAL"] <- "SAO DOMINGOS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO JOSE DO BREJO DA CRUZ"] <- "SAO JOSE DO BREJO DO CRUZ"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO SEBASTIAO DE LAGOA DE ROC"] <- "SAO SEBASTIAO DE LAGOA DE ROCA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SERIDO"] <- "SAO VICENTE DO SERIDO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CAMPO DE SANTANA"] <- "TACIMA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "VIEROPOLIS"] <- "VIEIROPOLIS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BELEM DE SAO FRANCISCO"] <- "BELEM DO SAO FRANCISCO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CARNAUBEIRAS DA PENHA"] <- "CARNAUBEIRA DA PENHA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "IGUARACI"] <- "IGUARACY"
resultados$MUNICIPIO[resultados$MUNICIPIO == "LAGOA DO ITAENGA"] <- "LAGOA DE ITAENGA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTANA DE IPANEMA"] <- "SANTANA DO IPANEMA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTANA DE SAO FRANCISCO"] <- "SANTANA DO SAO FRANCISCO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "GOVERNADOR LOMANTO JUNIOR"] <- "BARRO PRETO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "MUQUEM DE SAO FRANCISCO"] <- "MUQUEM DO SAO FRANCISCO"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 29 & resultados$MUNICIPIO == "SANTA TERESINHA", "SANTA TEREZINHA", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "AMPARO DA SERRA"] <- "AMPARO DO SERRA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BRASOPOLIS"] <- "BRAZOPOLIS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CAPURITA"] <- "CAPUTIRA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "DONA EUZEBIA"] <- "DONA EUSEBIA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ITABIRINHA DE MANTENA"] <- "ITABIRINHA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "LUIZBURGO"] <- "LUISBURGO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "MORRO DO GARCA"] <- "MORRO DA GARCA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA RITA DO JACUTINGA"] <- "SANTA RITA DE JACUTINGA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA RITA DO IBITIPOCA"] <- "SANTA RITA DE IBITIPOCA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO SEBASTIAO DA VARGEM ALEGR"] <- "SAO SEBASTIAO DA VARGEM ALEGRE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO TOME DAS LETRAS"] <- "SAO THOME DAS LETRAS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CACHOEIRO DO ITAPEMIRIM"] <- "CACHOEIRO DE ITAPEMIRIM"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ARMACAO DE BUZIOS"] <- "ARMACAO DOS BUZIOS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "PARATI"] <- "PARATY"
resultados$MUNICIPIO[resultados$MUNICIPIO == "TRAJANO DE MORAIS"] <- "TRAJANO DE MORAES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "VARRE E SAI"] <- "VARRE SAI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BRODOSQUI"] <- "BRODOWSKI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "EMBU"] <- "EMBU DAS ARTES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO MIGUEL DE TOUROS"] <- "SAO MIGUEL DO GOSTOSO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "FLORINIA"] <- "FLORINEA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "GUARAPARES"] <- "GUARARAPES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "MOJI DAS CRUZES"] <- "MOGI DAS CRUZES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "MOJI GUACU"] <- "MOGI GUACU"
resultados$MUNICIPIO[resultados$MUNICIPIO == "MOJI MIRIM"] <- "MOGI MIRIM"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO LUIS DO PARAITINGA"] <- "SAO LUIZ DO PARAITINGA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SERRO AZUL"] <- "CERRO AZUL"
resultados$MUNICIPIO[resultados$MUNICIPIO == "VILA ALTA"] <- "ALTO PARAISO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "PICARRAS"] <- "BALNEARIO PICARRAS"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 42 & resultados$MUNICIPIO == "PRESIDENTE CASTELO BRANCO", "PRESIDENTE CASTELLO BRANCO", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO MIGUEL DOESTE"] <- "SAO MIGUEL DO OESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "WITTMARSUM"] <- "WITMARSUM"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANT ANA DO LIVRAMENTO"] <- "SANTANA DO LIVRAMENTO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "TAQUARACU DO SUL"] <- "TAQUARUCU DO SUL"
resultados$MUNICIPIO[resultados$MUNICIPIO == "DEADAPOLIS"] <- "DEODAPOLIS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "VILA BELA DA SANTISSIMA TRIND"] <- "VILA BELA DA SANTISSIMA TRINDADE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "POXOREO"] <- "POXOREU"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ROSARIO DO OESTE"] <- "ROSARIO OESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "AGUA LINDAS DE GOIAS"] <- "AGUAS LINDAS DE GOIAS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "PALMEIRA DE GOIAS"] <- "PALMEIRAS DE GOIAS"

resultados$MUNICIPIO[resultados$MUNICIPIO == "MANUEL URBANO"] <- "MANOEL URBANO"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 12 & resultados$MUNICIPIO == "SANTA ROSA", "SANTA ROSA DO PURUS", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "SENADOR TEOTONIO VILELA"] <- "TEOTONIO VILELA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BOA VISTA DE RAMOS"] <- "BOA VISTA DO RAMOS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "NOVO AYRAO"] <- "NOVO AIRAO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "AGUA BRANCA DO AMAPARI"] <- "PEDRA BRANCA DO AMAPARI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "AMAPARI"] <- "PEDRA BRANCA DO AMAPARI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "AGUA QUENTE"] <- "ERICO CARDOSO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "GOVERNADOR LOMANTO JUNIOR"] <- "BARRO PRETO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "QUINJINGUE"] <- "QUIJINGUE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "DEP IRAPUAN PINHEIRO"] <- "DEPUTADO IRAPUAN PINHEIRO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "EUZEBIO"] <- "EUSEBIO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "GRANGEIRO"] <- "GRANJEIRO"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 29 & resultados$MUNICIPIO == "OURO BRANCO", "PINDAI", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "ANSELMO DA FONSECA"] <- "CAEM"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 21 & resultados$MUNICIPIO == "AGUA DOCE", "AGUA DOCE DO MARANHAO", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "AGUAS LINDAS"] <- "AGUAS LINDAS DE GOIAS"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 22 & resultados$MUNICIPIO == "ALAGOINHA", "ALAGOINHA DO PIAUI", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "ALMERIM"] <- "ALMEIRIM"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ALTA FLORESTA DO OESTE"] <- "ALTA FLORESTA DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ALTO DA BOA VISTA"] <- "ALTO BOA VISTA"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 52 & resultados$MUNICIPIO == "ALTO PARAISO", "ALTO PARAISO DE GOIAS", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "ALVORADA DO OESTE"] <- "ALVORADA DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "AMPARO DO SAO FRANCISCO"] <- "AMPARO DE SAO FRANCISCO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "APARECIDA DO TABUADO"] <- "APARECIDA DO TABOADO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SENADOR CATUNDA"] <- "CATUNDA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "TEJUSSUOCA"] <- "TEJUCUOCA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA MARIA DO JETIBA"] <- "SANTA MARIA DE JETIBA"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 32 & resultados$MUNICIPIO == "SAO DOMINGOS", "SAO DOMINGOS DO NORTE", resultados$MUNICIPIO)
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 52 & resultados$MUNICIPIO == "BOM JESUS", "BOM JESUS DE GOIAS", resultados$MUNICIPIO)
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 52 & resultados$MUNICIPIO == "MUNDO NOVO DE GOIAS", "MUNDO NOVO", resultados$MUNICIPIO)
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 52 & resultados$MUNICIPIO == "VALPARAISO", "VALPARAISO DE GOIAS", resultados$MUNICIPIO)
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 21 & resultados$MUNICIPIO == "BELA VISTA", "BELA VISTA DO MARANHAO", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "GOVERNADOR EDSON LOBAO"] <- "GOVERNADOR EDISON LOBAO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "LUIS DOMINGUES DO MARANHAO"] <- "LUIS DOMINGUES"	
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO RAIMUNDO DA DOCA BEZERRA"] <- "SAO RAIMUNDO DO DOCA BEZERRA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CONCEICAO DA PEDRA"] <- "CONCEICAO DAS PEDRAS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "GOUVEA"] <- "GOUVEIA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ITAMOJI"] <- "ITAMOGI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BATAGUACU"] <- "BATAGUASSU"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BATAIPORA"] <- "BATAYPORA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "JUTY"] <- "JUTI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "FIGUEIROPOLES DOESTE"] <- "FIGUEIROPOLIS DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "NOVA BANDEIRANTE"] <- "NOVA BANDEIRANTES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "PORTO ESPEREDIAO"] <- "PORTO ESPERIDIAO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "QUATRO MARCOS"] <- "SAO JOSE DOS QUATRO MARCOS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "TAPURA"] <- "TAPURAH"
resultados$MUNICIPIO[resultados$MUNICIPIO == "VILA BELA STSSMA TRINDADE"] <- "VILA BELA DA SANTISSIMA TRINDADE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "VIZEU"] <- "VISEU"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BARAUNAS"] <- "AREIA DE BARAUNAS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA CECILIA DE UMBUZEIRO"] <- "SANTA CECILIA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO JOSE DO BREJO CRUZ"] <- "SAO JOSE DO BREJO DO CRUZ"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO SEB. DE LAGOA DE ROCA"] <- "SAO SEBASTIAO DE LAGOA DE ROCA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO SEBASTIAO DE LAGOA DE ROCA"] <- "SAO SEBASTIAO DE LAGOA DE ROCA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CABO"] <- "CABO DE SANTO AGOSTINHO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ITAMARACA"] <- "ILHA DE ITAMARACA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "JABOATAO"] <- "JABOATAO DOS GUARARAPES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO CAETANO"] <- "SAO CAITANO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CONSELHEIRO MAYRINCK"] <- "CONSELHEIRO MAIRINCK"
resultados$MUNICIPIO[resultados$MUNICIPIO == "DIAMANTE DO OESTE"] <- "DIAMANTE DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ITAGUAGE"] <- "ITAGUAJE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ITAPEJARA DO OESTE"] <- "ITAPEJARA DOESTE"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 41 & resultados$MUNICIPIO == "LUISIANIA", "LUIZIANA", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "MOREIRA SALLES"] <- "MOREIRA SALES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "MUNHOZ DE MELLO"] <- "MUNHOZ DE MELO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA ANA DO ITARARE"] <- "SANTANA DO ITARARE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA CRUZ DO MONTE CASTELO"] <- "SANTA CRUZ DE MONTE CASTELO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA IZABEL DO IVAI"] <- "SANTA ISABEL DO IVAI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CAMPOS"] <- "CAMPOS DOS GOYTACAZES"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 33 & resultados$MUNICIPIO == "CAMPO GRANDE", "PATY DO ALFERES", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "PATI DO ALFERES"] <- "PATY DO ALFERES"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 24 & resultados$MUNICIPIO == "CAMPO GRANDE", "AUGUSTO SEVERO", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "ESPIRITO SANTO DO OESTE"] <- "PARAU"
resultados$MUNICIPIO[resultados$MUNICIPIO == "FERNANDO PEDROSA"] <- "FERNANDO PEDROZA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "JANUARIO CICCO"] <- "BOA SAUDE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO JOSE DE CAMPESTRE"] <- "SAO JOSE DO CAMPESTRE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "ESPIGAO DO OESTE"] <- "ESPIGAO DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "MACHADINHO DO OESTE"] <- "MACHADINHO DOESTE"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 11 & resultados$MUNICIPIO == "NOVA BRASILANDIA", "NOVA BRASILANDIA DOESTE", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "NOVA BRASILANDIA DO OESTE"] <- "NOVA BRASILANDIA DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTA LUZIA DO OESTE"] <- "SANTA LUZIA DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO FELIPE DO OESTE"] <- "SAO FELIPE DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO LUIZ DO ANAUA"] <- "SAO LUIZ"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CHIAPETA"] <- "CHIAPETTA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO LUIS GONZAGA"] <- "SAO LUIZ GONZAGA"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 43 & resultados$MUNICIPIO == "TRINDADE", "TRINDADE DO SUL", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "BALNEARIO DE BARRA DO SUL"] <- "BALNEARIO BARRA DO SUL"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BALNEARIO DE CAMBORIU"] <- "BALNEARIO CAMBORIU"
resultados$MUNICIPIO[resultados$MUNICIPIO == "HERVAL DO OESTE"] <- "HERVAL DOESTE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "LAGEADO GRANDE"] <- "LAJEADO GRANDE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "LUIS ALVES"] <- "LUIZ ALVES"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CANINDE DO SAO FRANCISCO"] <- "CANINDE DE SAO FRANCISCO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "GRACCHO CARDOSO"] <- "GRACHO CARDOSO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BADY BASSIT"] <- "BADY BASSITT"
resultados$MUNICIPIO[resultados$MUNICIPIO == "BERNADINO DE CAMPOS"] <- "BERNARDINO DE CAMPOS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "IPAUCU"] <- "IPAUSSU"
resultados$MUNICIPIO[resultados$MUNICIPIO == "LUISIANIA"] <- "LUIZIANIA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "PIRACUNUNGA"] <- "PIRASSUNUNGA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SALMORAO"] <- "SALMOURAO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SANTO ANTONIO DA POSSE"] <- "SANTO ANTONIO DE POSSE"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SUDMENUCCI"] <- "SUD MENNUCCI"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SUZANOPOLIS"] <- "SUZANAPOLIS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "VALPARAIZO"] <- "VALPARAISO"
resultados$MUNICIPIO[resultados$MUNICIPIO == "CHAPADA DA AREIA"] <- "CHAPADA DE AREIA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "DARCYNOPOLIS"] <- "DARCINOPOLIS"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 17 & resultados$MUNICIPIO == "DIVINOPOLIS", "DIVINOPOLIS DO TOCANTINS", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "LAGEADO"] <- "LAJEADO"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 17 & resultados$MUNICIPIO == "MONTE SANTO", "MONTE SANTO DO TOCANTINS", resultados$MUNICIPIO)
resultados$MUNICIPIO[resultados$MUNICIPIO == "OLIVEIRA DO TOCANTINS"] <- "OLIVEIRA DE FATIMA"
resultados$MUNICIPIO[resultados$MUNICIPIO == "PINDORAMA DE GOIAS"] <- "PINDORAMA DO TOCANTINS"
resultados$MUNICIPIO[resultados$MUNICIPIO == "SAO VALERIO DO TOCANTINS"] <- "SAO VALERIO"
resultados$MUNICIPIO <- ifelse(resultados$cod_uf == 17 & resultados$MUNICIPIO == "BANDEIRANTE", "BANDEIRANTES DO TOCANTINS", resultados$MUNICIPIO)



# ----------------------------------------------------------------------------------------------------------------
# Verificando se existe alguma inconsistência entre os nomes dos municípios na base MUNIC e os nomes oficiais do IBGE:
load("municipios_ibge")
resultados_anti <- resultados %>%
  anti_join(municipios_ibge %>% select(cod_uf, MUNICIPIO, cod_municipio_ibge, cod_mun),
            by = c("cod_uf", "MUNICIPIO")) %>% 
  arrange(SIGLA_UF, MUNICIPIO) %>% 
  select(SIGLA_UF, MUNICIPIO, everything())
keep(resultados, resultados_anti, municipios_ibge, all = TRUE, sure = TRUE)
rm(resultados_anti)
# ----------------------------------------------------------------------------------------------------------------


resultados_2000 <- resultados %>% filter(ANO_ELEICAO == 2000)
resultados_2004 <- resultados %>% filter(ANO_ELEICAO == 2004)
resultados_2008 <- resultados %>% filter(ANO_ELEICAO == 2008)
resultados_2012 <- resultados %>% filter(ANO_ELEICAO == 2012)


# ------------------------------------------------------------------------------------------------------------------

# Criando dataframe que contém o número de eleitores aptos por município:

load("detalhes_electionsBR")

funcao_tse_eleitores <- function(dados_tse_detalhes) {

eleitores_por_municipio <- dados_tse_detalhes %>%
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(NOME_MUNICIPIO, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ""),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", '')) %>%
  filter(DESCRICAO_CARGO == "PREFEITO") %>%
  group_by(MUNICIPIO, CODIGO_MUNICIPIO) %>%
  summarise(numero_eleitores = sum(QTD_APTOS), eleitores_presentes = sum(QTD_COMPARECIMENTO),
            abstencoes = sum(QTD_ABSTENCOES), votos_nominais_totais_eleicao = sum(QTD_VOTOS_NOMINAIS)) %>% 
  ungroup()
}


eleitores_municipio_2000 <- funcao_tse_eleitores(detalhes_2000_electionsbr)
eleitores_municipio_2004 <- funcao_tse_eleitores(detalhes_2004_electionsbr)
eleitores_municipio_2008 <- funcao_tse_eleitores(detalhes_2008_electionsbr)
eleitores_municipio_2012 <- funcao_tse_eleitores(detalhes_2012_electionsbr)




# ---------------------------------------------------------------------------------------------------------------

# Criando dataframe que contém informações sobre os candidatos:

load("candidatos_electionsBR")

funcao_tse_candidatos <- function(dados_tse_candidatos) {

base <- dados_tse_candidatos %>%
  filter(NUM_TURNO == 1 & DESCRICAO_CARGO == "PREFEITO") %>%
  select(ANO_ELEICAO, SIGLA_UF, SIGLA_UE, NOME_CANDIDATO, NOME_URNA_CANDIDATO, NUMERO_CANDIDATO,
         SIGLA_PARTIDO, DATA_NASCIMENTO, CODIGO_SEXO, DESCRICAO_SEXO, COD_GRAU_INSTRUCAO,
         DESCRICAO_GRAU_INSTRUCAO, CODIGO_ESTADO_CIVIL, DESCRICAO_ESTADO_CIVIL)

}

candidatos_2000 <- unique(funcao_tse_candidatos(candidatos_2000_electionsbr))
candidatos_2004 <- unique(funcao_tse_candidatos(candidatos_2004_electionsbr))
candidatos_2008 <- unique(funcao_tse_candidatos(candidatos_2008_electionsbr))
candidatos_2012 <- unique(funcao_tse_candidatos(candidatos_2012_electionsbr))


rm(list = ls(pattern = 'electionsbr'))
# ---------------------------------------------------------------------------------------------------------------

# Unindo as bases de dados de resultados das eleições, detalhes de eleitores, candidatos e codigos de municipios do IBGE:
eleicao_2000 <- resultados_2000 %>% 
  left_join(eleitores_municipio_2000 %>% select(-MUNICIPIO), by = "CODIGO_MUNICIPIO") %>%
  left_join(municipios_ibge %>% select(MUNICIPIO, cod_municipio_ibge, uf, cod_uf), by = c("cod_uf", "MUNICIPIO")) %>% 
  left_join(candidatos_2000) %>% 
  select(ANO_ELEICAO, cod_municipio_ibge, CODIGO_MUNICIPIO_TSE = CODIGO_MUNICIPIO, cod_uf, SIGLA_UF, uf, MUNICIPIO,
         numero_eleitores, eleitores_presentes, abstencoes, votos_nominais_totais_eleicao, NOME_CANDIDATO, 
         votos_candidato, vote_share, votos_eleicao, DESC_SIT_CAND_TOT, SIGLA_PARTIDO, NUMERO_PARTIDO,
         DESCRICAO_CARGO, SIGLA_PARTIDO, NUMERO_PARTIDO, everything()) %>% 
  mutate_at(vars(CODIGO_SEXO, COD_GRAU_INSTRUCAO, CODIGO_ESTADO_CIVIL) , as.character)

eleicao_2004 <- resultados_2004 %>% 
  left_join(eleitores_municipio_2004 %>% select(-MUNICIPIO), by = "CODIGO_MUNICIPIO") %>%
  left_join(municipios_ibge %>% select(MUNICIPIO, cod_municipio_ibge, uf, cod_uf), by = c("cod_uf", "MUNICIPIO")) %>% 
  left_join(candidatos_2004) %>% 
  select(ANO_ELEICAO, cod_municipio_ibge, CODIGO_MUNICIPIO_TSE = CODIGO_MUNICIPIO, cod_uf, SIGLA_UF, uf, MUNICIPIO,
         numero_eleitores, eleitores_presentes, abstencoes, votos_nominais_totais_eleicao, NOME_CANDIDATO, 
         votos_candidato, vote_share, votos_eleicao, DESC_SIT_CAND_TOT, SIGLA_PARTIDO, NUMERO_PARTIDO,
         DESCRICAO_CARGO, SIGLA_PARTIDO, NUMERO_PARTIDO, everything()) %>% 
  mutate_at(vars(CODIGO_SEXO, COD_GRAU_INSTRUCAO, CODIGO_ESTADO_CIVIL) , as.character)

eleicao_2008 <- resultados_2008 %>% 
  left_join(eleitores_municipio_2008 %>% select(-MUNICIPIO), by = "CODIGO_MUNICIPIO") %>%
  left_join(municipios_ibge %>% select(MUNICIPIO, cod_municipio_ibge, uf, cod_uf), by = c("cod_uf", "MUNICIPIO")) %>% 
  left_join(candidatos_2008) %>% 
  select(ANO_ELEICAO, cod_municipio_ibge, CODIGO_MUNICIPIO_TSE = CODIGO_MUNICIPIO, cod_uf, SIGLA_UF, uf, MUNICIPIO,
         numero_eleitores, eleitores_presentes, abstencoes, votos_nominais_totais_eleicao, NOME_CANDIDATO, 
         votos_candidato, vote_share, votos_eleicao, DESC_SIT_CAND_TOT, SIGLA_PARTIDO, NUMERO_PARTIDO,
         DESCRICAO_CARGO, SIGLA_PARTIDO, NUMERO_PARTIDO, everything()) %>% 
  mutate_at(vars(CODIGO_SEXO, COD_GRAU_INSTRUCAO, CODIGO_ESTADO_CIVIL) , as.character)

eleicao_2012 <- resultados_2012 %>% 
  left_join(eleitores_municipio_2012 %>% select(-MUNICIPIO), by = "CODIGO_MUNICIPIO") %>%
  left_join(municipios_ibge %>% select(MUNICIPIO, cod_municipio_ibge, uf, cod_uf), by = c("cod_uf", "MUNICIPIO")) %>% 
  left_join(candidatos_2012) %>% 
  select(ANO_ELEICAO, cod_municipio_ibge, CODIGO_MUNICIPIO_TSE = CODIGO_MUNICIPIO, cod_uf, SIGLA_UF, uf, MUNICIPIO,
         numero_eleitores, eleitores_presentes, abstencoes, votos_nominais_totais_eleicao, NOME_CANDIDATO, 
         votos_candidato, vote_share, votos_eleicao, DESC_SIT_CAND_TOT, SIGLA_PARTIDO, NUMERO_PARTIDO,
         DESCRICAO_CARGO, SIGLA_PARTIDO, NUMERO_PARTIDO, everything()) %>% 
  mutate_at(vars(CODIGO_SEXO, COD_GRAU_INSTRUCAO, CODIGO_ESTADO_CIVIL) , as.character)



eleicao <- bind_rows(eleicao_2000, eleicao_2004, eleicao_2008, eleicao_2012) %>% arrange(SIGLA_UF, MUNICIPIO) %>%
  mutate(mandato = case_when(ANO_ELEICAO == 2000 ~ "2001 a 2004",
                             ANO_ELEICAO == 2004 ~ "2005 a 2008",
                             ANO_ELEICAO == 2008 ~ "2009 a 2012",
                             ANO_ELEICAO == 2012 ~ "2013 a 2016")) %>% 
  mutate(DATA_NASCIMENTO = str_remove_all(DATA_NASCIMENTO, pattern = "/"))


# Criando a variável Idade do candidato:
eleicao$DATA_NASCIMENTO[eleicao$DATA_NASCIMENTO == "1061970"] <- "10061970"
eleicao$DATA_NASCIMENTO[eleicao$DATA_NASCIMENTO == "2051929"] <- "20051929"
eleicao$DATA_NASCIMENTO[eleicao$DATA_NASCIMENTO == "2011949"] <- "20011949"
eleicao$DATA_NASCIMENTO[eleicao$DATA_NASCIMENTO == "01010952"] <- "01011952"

eleicao <- eleicao %>%
  mutate_at('DATA_NASCIMENTO', as.Date, format = "%d%m%Y") 

eleicao$DATA_NASCIMENTO[eleicao$DATA_NASCIMENTO < "1900-01-01"] <- NA
eleicao$DATA_NASCIMENTO[eleicao$DATA_NASCIMENTO > "1999-01-01"] <- NA


eleicao <- eleicao %>% 
  mutate(idade_candidato = case_when(ANO_ELEICAO == 2000 ~ floor((as.Date('2000-10-01') - DATA_NASCIMENTO)/365),
                                     ANO_ELEICAO == 2004 ~ floor((as.Date('2004-10-03') - DATA_NASCIMENTO)/365),
                                     ANO_ELEICAO == 2008 ~ floor((as.Date('2008-10-05') - DATA_NASCIMENTO)/365),
                                     ANO_ELEICAO == 2012 ~ floor((as.Date('2012-10-07') - DATA_NASCIMENTO)/365)))


# Removendo candidatos duplicados:
eleicao <- eleicao %>%
  filter(!(CODIGO_MUNICIPIO_TSE == 90530 & ANO_ELEICAO == 2012 & NOME_CANDIDATO == "BENTO AFONSO OLIVEIRA DE SOUZA" & DESCRICAO_ESTADO_CIVIL == "CASADO(A)")) %>% 
  filter(!(CODIGO_MUNICIPIO_TSE == 94331 & ANO_ELEICAO == 2012 & as.character(DATA_NASCIMENTO) == "1954-06-07"))

keep(eleicao, all = TRUE, sure = TRUE)


# --------------------------------------------------------------------------------------------------------
# Frequência de cada partido na base de dados:
freq(eleicao$SIGLA_PARTIDO)
frequencia_partidos <- eleicao %>% filter(numero_candidatos == 2) %>% group_by(SIGLA_PARTIDO) %>% summarise(q = n()) %>% arrange(desc(q))

# Classificação ideológica dos partidos brasileiros de acordo com artigos de ciência política:
# Partidos são classificados como de esquerda ou direita apenas quando a classificação é UNÂNIME ENTRE OS DIVERSOS ESTUDOS: 
partidos_eleicao <- unique(eleicao$SIGLA_PARTIDO)
partidos_esquerda <- c("PCB", "PPS", "PC do B", "PDT", "PSB", "PT", "PV", "PSOL", "PPL")
partidos_centro <- c("PMDB", "PSDB", "PSL", "PST", "PTB", "PTR", "PR", "PDC", "PSDC", "PST", "PSC", "PTR", "PHS", "PTN")
partidos_direita <- c("PDS", "PPB", "PP", "PFL", "DEM", "PJ", "PRN", "PTC", "PL", "PRONA", "PRP", "PSD")
partidos_indefinidos <- c("PT do B", "PMN", "PRB", "PRTB")


# partidos_esquerda <- c("PCB", "PPS", "PC do B", "PDT", "PMN", "PPS", "PSB", "PSOL", "PT", "PV", "PSTU", "PPL", "REDE")
# partidos_centro <- c("PSL", "PMDB", "PSDB", "PHS", "PODEMOS", "PTN")
# partidos_direita <- c("PDC", "PSDC", "PDS", "PPB", "PP", "PFL", "DEM", "PJ", "PRN", "PTC", "PL", "PR", "PRONA", "PRP", "PSC", "PSD", "PST", "PTB", "PTR", "PROS", "NOVO")
# partidos_indefinidos <- c("PRTB", "PRB", "PEN", "SD", "AVANTE", "PT do B")


# Adicionando a variável indicadora de partidos de esquerda:
eleicao <- eleicao %>%
  mutate(ideologia_partido = case_when(SIGLA_PARTIDO %in% partidos_esquerda ~ "esquerda",
                                       SIGLA_PARTIDO %in% partidos_centro ~ "centro",
                                       SIGLA_PARTIDO %in% partidos_direita ~ "direita",
                                       !(SIGLA_PARTIDO %in% c(partidos_esquerda, partidos_centro, partidos_direita)) ~ "indefinida")) %>% 
  mutate(partido_esquerda = as.integer(ideologia_partido == "esquerda"),
         partido_centro = as.integer(ideologia_partido == "centro"),
         partido_direita = as.integer(ideologia_partido == "direita"),
         partido_indefinido = as.integer(ideologia_partido == "indefinida"))
  
# Analisando quais partidos estão sendo considerados:
table(eleicao$ideologia_partido)
summarytools::freq(eleicao$ideologia_partido)


# Arrumando e reordenando variáveis:
eleicao <- eleicao %>% 
  select(-c(SQ_CANDIDATO, CODIGO_SIT_CAND_TOT, DATA_GERACAO, HORA_GERACAO, DESCRICAO_ELEICAO, CODIGO_CARGO,
            COD_SIT_CAND_SUPERIOR, COD_SIT_CAND_SUPERIOR, CODIGO_SIT_CANDIDATO, SEQUENCIAL_LEGENDA,
            DESC_SIT_CAND_SUPERIOR)) %>%
  mutate(taxa_abstencao = abstencoes/numero_eleitores) %>% 
  select(ANO_ELEICAO, mandato, cod_municipio_ibge, CODIGO_MUNICIPIO_TSE, regiao,
         cod_uf:SIGLA_PARTIDO, ideologia_partido, NUMERO_PARTIDO:DATA_NASCIMENTO, idade_candidato, everything()) %>% 
  select(ANO_ELEICAO:abstencoes, taxa_abstencao, everything())

rm(list = ls(pattern = "partidos_"))




# Visualizando os partidos e suas ideologias:
partidos_ideologias <- eleicao %>% group_by(SIGLA_PARTIDO, ideologia_partido) %>% summarise() %>% arrange(ideologia_partido, SIGLA_PARTIDO)
partidos_ideologias


# --------------------------------------------------------------------------------------------------------------------

# Mantendo somente eleições com dois candidatos e em municípios com menos de 200 mil habitantes, e que portanto não
# existe a possibilidade de Segundo turno:
eleicao_restrita <- eleicao %>%
  filter(numero_candidatos == 2, numero_eleitores < 200000) %>%
  arrange(ANO_ELEICAO, SIGLA_UF, MUNICIPIO, DESC_SIT_CAND_TOT) %>%
  mutate(margem_vitoria = case_when(DESC_SIT_CAND_TOT == "ELEITO" ~ vote_share - lead(vote_share),
                                    DESC_SIT_CAND_TOT == "NÃO ELEITO" ~ vote_share - lag(vote_share))) %>%
  select(ANO_ELEICAO:vote_share, margem_vitoria, everything())



# Mantendo somente eleições nas quais ambos os candidatos possuem candidaturas com status DEFERIDO:
eleicao_restrita <- eleicao_restrita %>% 
  mutate(candidatura_deferida = (DESC_SIT_CANDIDATO == "DEFERIDO")) %>% 
  group_by(cod_municipio_ibge, mandato) %>% 
  mutate(numero_candidatos_deferidos = sum(candidatura_deferida)) %>% 
  filter(numero_candidatos_deferidos == 2) %>% 
  ungroup()


# Removendo eleições em que um partido indefinido está em disputa:

# eleicoes_com_partidos_indefinidos <- eleicao_restrita %>% 
#   group_by(cod_municipio_ibge, ANO_ELEICAO) %>% 
#   summarise(numero_partidos_indefinidos = sum(partido_indefinido)) %>%
#   ungroup() %>% 
#   filter(numero_partidos_indefinidos > 0)
# 
# eleicao_restrita <- anti_join(eleicao_restrita, eleicoes_com_partidos_indefinidos)
# rm(eleicoes_com_partidos_indefinidos)


eleicao_restrita <- eleicao_restrita %>% 
  group_by(cod_municipio_ibge, ANO_ELEICAO) %>% 
  mutate(numero_partidos_indefinidos = sum(partido_indefinido)) %>%
  filter(numero_partidos_indefinidos == 0) %>% 
  ungroup()


# Adicionando a variável que indica quantos partidos de cada ideologia disputam a eleicao:
eleicao_restrita <- eleicao_restrita %>% 
  group_by(cod_municipio_ibge, ANO_ELEICAO) %>% 
  mutate(numero_partidos_direita = sum(partido_direita),
            numero_partidos_esquerda = sum(partido_esquerda),
            numero_partidos_centro = sum(partido_centro),) %>% 
  ungroup()


# Removendo observações de município com problemas nos dados (soma das margem de vtória e derrota não é igual a 1),
# e eleicoes empatadas em número de votos (margem de vitoria igual a zero):
eleicao_restrita <- eleicao_restrita %>% 
  filter(!(cod_municipio_ibge == 2802601 & mandato == "2005 a 2008")) %>% 
  filter(margem_vitoria != 0)




# Adicionando variável que indica o left_vote_share:
eleicao_restrita <- eleicao_restrita %>%
  mutate(margem_vitoria_esquerda = case_when(numero_partidos_esquerda == 0 | numero_partidos_esquerda == 2 ~ 999,
                                             numero_partidos_esquerda != 0 & ideologia_partido == "esquerda" ~ margem_vitoria)) %>% 
  select(ANO_ELEICAO:margem_vitoria, margem_vitoria_esquerda, everything()) %>% 
  ungroup() %>% 
  arrange(cod_municipio_ibge, ANO_ELEICAO, ideologia_partido) %>% 
  fill(margem_vitoria_esquerda, .direction = "up") %>% 
  mutate(margem_vitoria_esquerda = replace(margem_vitoria_esquerda, margem_vitoria_esquerda == 999, NA))
  

freq(eleicao_restrita$SIGLA_PARTIDO)

# Visualizando os partidos e suas ideologias:
partidos_ideologias_restrito <- eleicao_restrita %>% group_by(SIGLA_PARTIDO, ideologia_partido) %>% summarise() %>% arrange(ideologia_partido, SIGLA_PARTIDO)
partidos_ideologias_restrito


# -------------------------------------------------------------------------------------------------------------


# Mantendo somente eleições disputadas entre um candidato de esquerda contra outro de direita ou centro:
eleicao_esquerda <- eleicao_restrita %>% filter(numero_partidos_esquerda == 1)
  
# Mantendo somente eleições disputadas entre um candidato de esquerda contra outro de direita:
eleicao_esquerda_direita <- eleicao_restrita %>% filter(numero_partidos_esquerda == 1, numero_partidos_direita == 1)

# Por enquanto vou manter a análise com eleições entre partidos de esquerda contra os demais (centro e direita):


# Realizando últimas manutenções na base para poder unir com a base de dados de outcomes:
# eleicao_pronta <- eleicao_esquerda %>%
#   filter(DESC_SIT_CAND_TOT == "ELEITO") %>%
#   select(cod_municipio_ibge, mandato, sigla_uf = SIGLA_UF, MUNICIPIO, taxa_abstencao, margem_vitoria_esquerda,
#          ideologia_partido_eleito = ideologia_partido, partido_eleito = SIGLA_PARTIDO,
#          idade_prefeito_eleito = idade_candidato, sexo_prefeito = DESCRICAO_SEXO,
#          grau_instrucao_prefeito = DESCRICAO_GRAU_INSTRUCAO, estado_civil_prefeito = DESCRICAO_ESTADO_CIVIL) %>%
#   arrange(cod_municipio_ibge, mandato)
  

eleicao_pronta <- eleicao_esquerda_direita %>%
  filter(DESC_SIT_CAND_TOT == "ELEITO") %>%
  select(cod_municipio_ibge, mandato, sigla_uf = SIGLA_UF, MUNICIPIO, taxa_abstencao, margem_vitoria_esquerda,
         ideologia_partido_eleito = ideologia_partido, partido_eleito = SIGLA_PARTIDO,
         idade_prefeito_eleito = idade_candidato, sexo_prefeito = DESCRICAO_SEXO,
         grau_instrucao_prefeito = DESCRICAO_GRAU_INSTRUCAO, estado_civil_prefeito = DESCRICAO_ESTADO_CIVIL) %>%
  arrange(cod_municipio_ibge, mandato)



getwd()
save(eleicao_pronta, file = "eleicoes")


# Analisando quais partidos estão sendo considerados:
table(eleicao_pronta$ideologia_partido_eleito)
summarytools::freq(eleicao_pronta$ideologia_partido_eleito)
summarytools::descr(eleicao_pronta$margem_vitoria_esquerda, transpose = T, stats = "common", round.digits = 3)


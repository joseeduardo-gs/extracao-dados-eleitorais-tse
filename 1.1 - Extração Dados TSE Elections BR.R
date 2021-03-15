############################################################################################################
#                      EXTRAÇÃO DADOS ELEITORAIS TSE USANDO PACOTE electionsBR
############################################################################################################

rm(list = ls())

# Pacotes
library(electionsBR)

# O pacote electionsBR possui funções próprias que extraem diversos dados eleitorais do site do TSE, e realizam
# alguns procedimentos de limpeza preliminares.

# RESULTADOS DAS ELEIÇÕES MUNICIPAIS:

# Aqui, extraímos os dados das eleições municipais de 2004, 2008 e 2012.
resultados_2000_electionsbr <- vote_mun_zone_local(2000)
resultados_2004_electionsbr <- vote_mun_zone_local(2004)
resultados_2008_electionsbr <- vote_mun_zone_local(2008)
resultados_2012_electionsbr <- vote_mun_zone_local(2012)

# -----------------------------------------------------------------------------------------------------------

# INFORMAÇÕES SOBRE OS CANDIDATOS:

# Aqui, extraímos os dados das eleições municipais de 2004, 2008 e 2012. 
candidatos_2000_electionsbr <- candidate_local(2000)
candidatos_2004_electionsbr <- candidate_local(2004)
candidatos_2008_electionsbr <- candidate_local(2008)
candidatos_2012_electionsbr <- candidate_local(2012)


# -------------------------------------------------------------------------------------------------------------

# DADOS SOBRE PARTIDOS E COLIGAÇÕES

partidos_2000_electionsbr <- party_mun_zone_local(2000)
partidos_2004_electionsbr <- party_mun_zone_local(2004)
partidos_2008_electionsbr <- party_mun_zone_local(2008)
partidos_2012_electionsbr <- party_mun_zone_local(2012)


# ------------------------------------------------------------------------------------------------------------

# DETALHES SOBRE AS ELEIÇÕES (CONTÉM INFORMAÇÕES SOBRE O NÚMERO DE ELEITORES POR MUNICÍPIO)

detalhes_2000_electionsbr <- details_mun_zone_local(2000)
detalhes_2004_electionsbr <- details_mun_zone_local(2004)
detalhes_2008_electionsbr <- details_mun_zone_local(2008)
detalhes_2012_electionsbr <- details_mun_zone_local(2012)



# ----------------------------------------------------------------------------------------------------------

# SALVANDO AS BASES EXTRAÍDAS:

setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(list = c('resultados_2000_electionsbr', 'resultados_2004_electionsbr', 'resultados_2008_electionsbr', 'resultados_2012_electionsbr'), file = 'resultados_electionsBR')
save(list = c('candidatos_2000_electionsbr', 'candidatos_2004_electionsbr', 'candidatos_2008_electionsbr', 'candidatos_2012_electionsbr'), file = 'candidatos_electionsBR')
save(list = c('partidos_2000_electionsbr', 'partidos_2004_electionsbr', 'partidos_2008_electionsbr', 'partidos_2012_electionsbr'), file = 'partidos_electionsBR')
save(list = c('detalhes_2000_electionsbr', 'detalhes_2004_electionsbr', 'detalhes_2008_electionsbr', 'detalhes_2012_electionsbr'), file = 'detalhes_electionsBR')


load("resultados_electionsBR")
load("candidatos_electionsBR")
load("partidos_electionsBR")
load("detalhes_electionsBR")

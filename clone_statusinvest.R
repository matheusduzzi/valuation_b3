# Carregando dados de bibliotecas
library(rvest)
library(tidyr)
library(dplyr)

# Coletando todas as empresas da B3 
dados_ivalor = data.frame()
for (i in 1:28) {
  link_base = paste0("https://www.ivalor.com.br/empresas/listagem?p=",i)
  doc1 <- read_html(link_base)
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="tab_empresas"]') %>% html_table() -> table_names)
  table_names = as.data.frame(table_names)
  dados_ivalor = rbind(dados_ivalor,table_names)
}
dados_ivalor = dados_ivalor %>% mutate(`Ações` = strsplit(as.character(`Ações`), " ")) %>% unnest(`Ações`)
dados_ivalor$Ações = ifelse(dados_ivalor$Ações == dados_ivalor$Ações[397],0,dados_ivalor$Ações)
dados_ivalor = dados_ivalor %>% filter(`Ações` != 0) %>% select(-N.,-Logo)

lista_acoes = tolower(dados_ivalor$Ações)

######### PREPARANDO DATA FRAMES #################

## valuation

valuation_geral = data.frame()
valuation = c("aux","aux","aux","aux","aux","aux","aux","aux","aux","aux","aux","aux","aux","aux","aux")
valuation_geral = rbind(valuation_geral,valuation)

##### endividamento

endividamento_geral = data.frame()
endividamento = c("aux","aux","aux","aux","aux","aux","aux")
endividamento_geral = rbind(endividamento_geral,endividamento)

##### eficiencia

eficiencia_geral = data.frame()
eficiencia = c("aux","aux","aux","aux","aux")
eficiencia_geral = rbind(eficiencia_geral,eficiencia)

##### rentabilidade

rentabilidade_geral = data.frame()
rentabilidade = c("aux","aux","aux","aux","aux")
rentabilidade_geral = rbind(rentabilidade_geral,rentabilidade)

##### crescimento

crescimento_geral = data.frame()
crescimento = c("aux","aux","aux")
crescimento_geral = rbind(crescimento_geral,crescimento)

################ ITERANDO NA PÁGINA DOS TICKERS #######################

for (i in 1:length(lista_acoes)) {
  
  Sys.sleep(sample(25:45, 1))
  
  link_base = paste0("https://statusinvest.com.br/acoes/",lista_acoes[i])
  doc1 <- read_html("https://google.com")
  try(doc1 <- read_html(link_base))
  
  ###### valuation
  
  # DY
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[1]/div/div/strong') %>% html_text() -> DY)
  DY = as.numeric(gsub("%","",gsub(",",".",DY)))/100
  
  #P/L
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[2]/div/div/strong') %>% html_text() -> PL)
  PL = as.numeric(gsub(",",".",PL))
  
  #PEG RATIO
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[3]/div/div/strong') %>% html_text() -> PEGRATIO)
  PEGRATIO = as.numeric(gsub(",",".",PEGRATIO))
  
  #P/VP
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[4]/div/div/strong') %>% html_text() -> PVP)
  PVP = as.numeric(gsub(",",".",PVP))
  
  #EV/EBITDA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[5]/div/div/strong') %>% html_text() -> EVEBITDA)
  EVEBITDA = as.numeric(gsub(",",".",EVEBITDA))
  
  #EV/EBIT
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[6]/div/div/strong') %>% html_text() -> EVEBIT)
  EVEBIT = as.numeric(gsub(",",".",EVEBIT))
  
  #P/EBITDA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[7]/div/div/strong') %>% html_text() -> PEBITDA)
  PEBITDA = as.numeric(gsub(",",".",PEBITDA))
  
  #P/EBIT
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[8]/div/div/strong') %>% html_text() -> PEBIT)
  PEBIT = as.numeric(gsub(",",".",PEBIT))
  
  #VPA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[9]/div/div/strong') %>% html_text() -> VPA)
  VPA = as.numeric(gsub(",",".",VPA))
  
  #P/ATIVO
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[10]/div/div/strong') %>% html_text() -> PATIVO)
  PATIVO = as.numeric(gsub(",",".",PATIVO))
  
  #LPA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[11]/div/div/strong') %>% html_text() -> LPA)
  LPA = as.numeric(gsub(",",".",LPA))
  
  #P/SR
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[12]/div/div/strong') %>% html_text() -> PSR)
  PSR = as.numeric(gsub(",",".",PSR))
  
  #P/CAP. GIRO
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[13]/div/div/strong') %>% html_text() -> PCAPGIRO)
  PCAPGIRO = as.numeric(gsub(",",".",PCAPGIRO))
  
  #P/ATIVO CIRC. LIQ.
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[1]/div/div[14]/div/div/strong') %>% html_text() -> PATIVOCIRCLIQ)
  PATIVOCIRCLIQ = as.numeric(gsub(",",".",PATIVOCIRCLIQ))
  
  valuation = c(
    DY,PL,PEGRATIO,PVP,
    EVEBITDA,
    EVEBIT,
    PEBITDA,
    PEBIT,
    VPA,
    PATIVO,
    LPA,
    PSR,
    PCAPGIRO,
    PATIVOCIRCLIQ,lista_acoes[i])
  valuation_geral = rbind(valuation_geral,valuation)
  
  ##### endividamento
  
  #DIVLIQUIDA/PL
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[2]/div/div[1]/div/div/strong') %>% html_text() -> DIVLIQUIDAPL)
  DIVLIQUIDAPL = as.numeric(gsub(",",".",DIVLIQUIDAPL))
  
  #DIVLIQUIDA/EBITDA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[2]/div/div[2]/div/div/strong') %>% html_text() -> DIVLIQUIDAEBITDA)
  DIVLIQUIDAEBITDA = as.numeric(gsub(",",".",DIVLIQUIDAEBITDA))
  
  #DIVLIQUID/EBIT
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[2]/div/div[3]/div/div/strong') %>% html_text() -> DIVLIQUIDAEBIT)
  DIVLIQUIDAEBIT = as.numeric(gsub(",",".",DIVLIQUIDAEBIT))
  
  #PL/ATIVOS
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[2]/div/div[4]/div/div/strong') %>% html_text() -> PLATIVOS)
  PLATIVOS = as.numeric(gsub(",",".",PLATIVOS))
  
  #PASSIVOS/ATIVOS
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[2]/div/div[5]/div/div/strong') %>% html_text() -> PASSIVOSATIVOS)
  PASSIVOSATIVOS = as.numeric(gsub(",",".",PASSIVOSATIVOS))
  
  #LIQ.CORRENTE
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[2]/div/div[6]/div/div/strong') %>% html_text() -> LIQCORRENTE)
  LIQCORRENTE = as.numeric(gsub(",",".",LIQCORRENTE))
  
  endividamento = c(DIVLIQUIDAPL,DIVLIQUIDAEBITDA,DIVLIQUIDAEBIT,PLATIVOS,PASSIVOSATIVOS,LIQCORRENTE,lista_acoes[i])
  endividamento_geral = rbind(endividamento_geral,endividamento)
  
  ##### rentabilidade
  
  # M.BRUTA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[3]/div/div[1]/div/div/strong') %>% html_text() -> M_BRUTA)
  M_BRUTA = as.numeric(gsub("%","",gsub(",",".",M_BRUTA)))/100
  
  # M.EBITDA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[3]/div/div[2]/div/div/strong') %>% html_text() -> M_EBITDA)
  M_EBITDA = as.numeric(gsub("%","",gsub(",",".",M_EBITDA)))/100
  
  # M.EBIT
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[3]/div/div[3]/div/div/strong') %>% html_text() -> M_EBIT)
  M_EBIT = as.numeric(gsub("%","",gsub(",",".",M_EBIT)))/100
  
  # M.LÍQUIDA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[3]/div/div[4]/div/div/strong') %>% html_text() -> M_LIQUIDA)
  M_LIQUIDA = as.numeric(gsub("%","",gsub(",",".",M_LIQUIDA)))/100
  
  eficiencia = c(
    M_BRUTA,M_EBITDA,M_EBIT,M_LIQUIDA,lista_acoes[i])
  eficiencia_geral = rbind(eficiencia_geral,eficiencia)
  
  #ROE
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[4]/div/div[1]/div/div/strong') %>% html_text() -> ROE)
  ROE = as.numeric(gsub("%","",gsub(",",".",ROE)))/100
  
  #ROA
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[4]/div/div[2]/div/div/strong') %>% html_text() -> ROA)
  ROA = as.numeric(gsub("%","",gsub(",",".",DY)))/100
  
  #ROIC
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[4]/div/div[3]/div/div/strong') %>% html_text() -> ROIC)
  ROIC = as.numeric(gsub("%","",gsub(",",".",ROIC)))/100
  
  #GIRO_ATIVOS
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[4]/div/div[4]/div/div/strong') %>% html_text() -> GIRO_ATIVOS)
  GIRO_ATIVOS = as.numeric(gsub(",",".",GIRO_ATIVOS))
  
  rentabilidade = c(ROE,ROA,ROIC,GIRO_ATIVOS,lista_acoes[i])
  rentabilidade_geral = rbind(rentabilidade_geral,rentabilidade)
  
  ##### crescimento
  
  # CAGR RECEITAS 5 ANOS
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[5]/div/div[1]/div/div/strong') %>% html_text() -> CAGR_REC)
  CAGR_REC = as.numeric(gsub("%","",gsub(",",".",CAGR_REC)))/100
  
  # CAGR LUCROS 5 ANOS
  try(doc1 %>% 
        html_nodes(xpath = '//*[@id="indicators-section"]/div[2]/div/div[5]/div/div[2]/div/div/strong') %>% html_text() -> CAGR_LUC)
  CAGR_LUC = as.numeric(gsub("%","",gsub(",",".",CAGR_LUC)))/100
  
  crescimento = c(CAGR_REC,CAGR_LUC,lista_acoes[i])
  crescimento_geral = rbind(crescimento_geral,crescimento)
  
}

######### ARRUMANDO NOMES DE COLUNAS #####################

colnames(valuation_geral) = c(
  "DY","PL","PEG_RATIO","P_VP",
  "EV_EBITDA","EV_EBIT","P_EBITDA",
  'P_EBIT',"VPA","P_ATIVO",
  "LPA","PSR","P_CAPGIRO",
  "P_ATIVOCIRCLIQ","TICKER")
colnames(endividamento_geral) = c("DIVLIQUIDA_PL","DIVLIQUIDA_EBITDA","DIVLIQUIDA_EBIT","PL_ATIVOS","PASSIVOS_ATIVOS","LIQ_CORRENTE","TICKER")
colnames(eficiencia_geral) = c("M_BRUTA","M_EBITDA","M_EBIT","M_LIQUIDA","TICKER")
colnames(rentabilidade_geral) = c("ROE","ROA","ROIC","GIRO_ATIVOS","TICKER")
colnames(crescimento_geral) = c("CAGR_REC","CAGR_LUC","TICKER")

valuation_geral = valuation_geral[-1,]
endividamento_geral = endividamento_geral[-1,]
eficiencia_geral = eficiencia_geral[-1,]
rentabilidade_geral = rentabilidade_geral[-1,]
crescimento_geral = crescimento_geral[-1,]

valuation_geral$TICKER = toupper(valuation_geral$TICKER)
endividamento_geral$TICKER = toupper(endividamento_geral$TICKER)
eficiencia_geral$TICKER = toupper(eficiencia_geral$TICKER)
rentabilidade_geral$TICKER = toupper(rentabilidade_geral$TICKER)
crescimento_geral$TICKER = toupper(crescimento_geral$TICKER)

dados_unificados = cbind(cbind(cbind(cbind(cbind(dados_ivalor,valuation_geral),endividamento_geral),eficiencia_geral),rentabilidade_geral),crescimento_geral)
dados_unificados = dados_unificados %>% select(-TICKER)
dados_unificados[,9:38] = sapply(dados_unificados[,9:38],as.numeric)
write.csv(dados_unificados,"dados_statusinvest.csv")

# Base preço dos alimentos Ceasa
Dados_Ceasa_Preco <- readxl::read_excel("E:/edime/Thalis/MEU/Ceasa/shiny_ceasa/R/utils/Dados.xlsx", 
                                col_types = c("text", "text", "text", "numeric", "date"))


data <- Dados_Ceasa_Preco  %>% dplyr::select(id,Produto,date,value) %>% drop_na()
#data <- Dados_Ceasa_Preco  %>% dplyr::select(id,date,value) %>% drop_na()
remove(Dados_Ceasa_Preco)








                                  
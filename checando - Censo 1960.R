rm(list=ls())
library(tidyverse)
library(readxl)
library(descr)
library(data.table)
options(scipen = 999)
#===================================================================================================================================
#### DEFININDO OPCOES GLOBAIS
#######################################

# Aponta para o diretorio de trabalho, que contem os arquivos de dados e input
#setwd("E:/Dropbox-Ro/Dropbox/Rogerio/Bancos_Dados/Censos/Censo 1960/Checagens e Consistencias")
setwd("C:/Dropbox/Rogerio/Bancos_Dados/Censos/Censo 1960/Checagens e Consistencias")

output_dir <- "C:/Dropbox/Rogerio/Bancos_Dados/Censos/Censo 1960/"

input_file = "Census1960_input_Sample_1.27.xlsx"

#===================================================================================================================================
#### DEFININDO FUNCOES QUE SERAO USADAS POSTERIORMENTE
#######################################

# Funcao para remover um caractere de espaco branco localizado entre dois valores numericos numa string
# Algumas linhas aparentemente corrompidas no arquivo de dados parecem ser consertadas depois da remocao
# desses espacos

remove_interSpace <- function(x, min_times, max_times){
        
        expression <- paste0("[[:digit:]][[:blank:]]{",min_times,",",max_times,"}[[:digit:]]")
        
        positions <- gregexpr(expression, text = x) %>% 
                unlist() + 1
        
        i = positions[1]
        for(i in positions){
                
                test <- substr(x, i, i)
        
                j = i
                while((j <= (i + max_times - 1)) & test == " "){
                        substr(x, j, j) <- "."
                        j    <- j + 1
                        test <- substr(x, j, j)
                }
        }

        x <- strsplit(x, split = ".", fixed = T) %>% 
                unlist() %>% 
                paste0(collapse = "")
        x
}


# Funcao para inserir um conjunto de caracteres numa string a partir de uma posicao (numero do caracter)
# Algumas linhas aparentemente corrompidas no arquivo de dados parecem ser consertadas depois da insercao
# de alguns caracteres (espacos em branco e/ou barras invertidas \)

insert_interCharacter <- function(x, pos, character){
        
        num_char <- nchar(x)
        
        x <- paste0(substr(x, start = 1, stop = pos), 
                    character,
                    substr(x, start = pos + 1, stop = num_char))
        
        x
}


#dados_string = c60_string_pess
#input = input_person

#dados_string = c60_string_hh
#input = input_hh

# Funcao para aplicar o layout de daddos e testar se as observacoes de cada variavel contem valores listados
# no dicionario de codigos. Cria uma coluna adicional para cada variavel, que consiste num teste logico:
# ** TRUE  = o registro contem valor valido para a variavel 
# ** FALSE = o valor observado é invalido

aplicaTesta_layout <- function(dados_string,
                               input){
        
        censo1960_data = list()
        
        for(i in 1:nrow(input)){
                
                print(input$Var[i])
                
                # Separando colunas segundo o arquivo de input
                censo1960_data[[input$Var[i]]] <- substr(dados_string, 
                                                         start = input$Start[i], 
                                                         stop  = input$End[i]) 
                
                # Nao testa os place holders
                if(length(grep("place", input$Var[i])) >= 1){
                        next
                        
                }else{
                        
                        # Carregando a lista de valores validos para cada variavel (Segundo o dicionario de codigos)
                        Valid_values = input$Valid_values[i]
                        
                        if(is.na(Valid_values)){
                                # Se nao houver valores pre-determinados (como no caso de variaveis continuas), procura ao menos caracteres estranhos
                                linhas <- 1:length(censo1960_data[[input$Var[i]]])
                                valores_estranhos <- grep(x = censo1960_data[[input$Var[i]]], pattern = "[^[:digit:]]")
                                censo1960_data[[paste0("test_",input$Var[i])]] <- !(linhas %in% valores_estranhos)
                                
                                next      
                        }else{
                                # Separando a lista de valores
                                if(Valid_values == "sheet"){
                                        Valid_values   <- read_xlsx(input_file, sheet = input$Var[i])
                                        Valid_values   <- Valid_values$value
                                }else{
                                        Valid_values = strsplit(Valid_values, ";") %>% unlist()        
                                }
                                
                                # As variaveis ainda estao em formato character. Espacos (em branco) indicam que para aquele registro a informacao nao
                                # se aplica. No entanto espacos sao lidos como characters de tamanho zero ("" ao inves de " ") na lista de valores validos.  
                                # Para as checagens temos substituir valores vazios por espacos
                                Valid_values = as.character(Valid_values)
                                
                                # para variaveis com mais de um digito, o espaco tambem tem que ter mais de um digito
                                max_nchar = max(nchar(Valid_values), na.rm = T)
                                espaco    = paste0(rep(" ", max_nchar), collapse = "")
                                
                                Valid_values[nchar(Valid_values) == 0] <- espaco
                                Valid_values[is.na(Valid_values)]      <- espaco
                                
                                # Conferindo se todos os valores observados encontram-se listados na lista de codigos
                                censo1960_data[[paste0("test_",input$Var[i])]] <- censo1960_data[[input$Var[i]]] %in% as.character(Valid_values)
                        }        
                }
        }
        as_tibble(censo1960_data)
}


#===================================================================================================================================
#### ANALISES GERAIS PRELIMINARES
#######################################

c60_string <- readLines("HHOLDA.txt")
n_linhas   <- length(c60_string)

# Todos os registros apresentam o mesmo tamanho
caracteres = nchar(c60_string)
table(caracteres)

# Testa se a variavel que identifica domicilios e pessoas contem apenas valores validos (a principio: 1, 2 ou 3)
teste_char17_dom_pess = substr(c60_string, start = 17, stop = 17)
table(teste_char17_dom_pess)

# Substituindo o hifen, que parece indicar o fim de certos  registros de pessoas, por espaco
c60_string <- gsub(x = c60_string, pattern = "-", " ")



#===================================================================================================================================
#### ANALISE DOS REGISTROS DE DOMICILIO
#######################################

# OBSERVACOES PRELIMINARES:
# Caracteres de 7 a 16 (10 caracteres) não são lidos no arquivo de domicílio. 
# Caracteres de 35 a 54 (20 caracteres) não são lidos no arquivo de domicílio. Mas isso esta OK. So ha valores nos registros de individuos
# caractere 32 nao esta sendo lido nos domicilios (1257 espaços vazios, 173209 valores zero, 1 valor 2)

# Os arquivos de input para domicilios e pessoas foram construidos a partir dos enderecos de caracteres utilizados 
# na sintaxe em formato SPSS que acompanhava o arquivo de dados.


# As seguintes variaveis estao citadas no dicionario de codigos (feito para a amostra de 25%), mas nao estao sendo abertos
# no arquivo de dados dessa amostra de 1,27% (e nao parece ser o caso de estarem contidas nos caracteres de 7 a 16)
# V119 - TOTAL FAMÍLIAS
# V120 - TOTAL MORADORES
# V121 - PESO DOMICÍLIO
# V122 - FILLER
# -- A principio, é possivel calcular V119 e V120 a posteriori.
# -- Se os dados forem mesmo fruto de uma amostra aleatoria simples e autoponderada, a V121 pode ser calculada como 1/0,0127 para todos os registros
# -- O conteudo da v122 é desconhecido (mas provavelmente nao ser trata de nada substantivo)

# As variaveis de identificacao listadas no inicio do dicionario também nao estao sendo abertas na sintaxe:
# em formato SPSS
# 1 - PASTA 
# 2 - BOLETIM
# 3 - IDENTIFICAÇÃO
# 4 - DIGITO VERIFICADOR
# -- Essas informacoes provavelmente estao contidas no arquivo de dados da amostra de 1,27%, em algum lugar nos caracteres 1 a 16

# A variavel "V100 - TOTAL DE PESSOAS", listada no dicionario, nao parece estar contida entre os primeiros caracteres do arquivo de dados

# As variáveis UF (unidade da federacao) e RECD (tipo de registro) nao estao listadas no dicionario, mas são referenciadas na sintaxe
# Segundo o dicionario para a amostra de 25%, se a variavel "3 - IDENTIFICAÇÃO" contiver apenas zeros, entao o registro é de domicilios.
# Essa não é a mesma lógica aplicada na sintaxe (onde domicílios são RECD == 1)

# No entanto, cruzando a variável RECD com a variável sobre RELAÇÃO COM O CHEFE, é possível inferir o significado das alternativas 
# 1 - Domicilio (ou familia?)
# 2 - Chefe do domicilio (ou da familia?)
# 3 - Demais moradores

# Carregando o arquivo de input que define as posicoes das variaveis
input_hh   <- read_xlsx(input_file, sheet = "HouseHold_open")

# Tomando apenas os dados de domicilio
c60_string_hh <- c60_string[which(teste_char17_dom_pess == 1)]
num_linha_dom <- (1:n_linhas)[which(teste_char17_dom_pess == 1)]


# Aplica o layout e testa se as observacoes em cada variavel estao listadas no dicionario
censo1960_hh <- aplicaTesta_layout(dados_string = c60_string_hh,
                                   input        = input_hh) %>%
        mutate(num_linha_dom = num_linha_dom) %>% # adicionamos uma coluna que indica a linha onde o registro se encontra no arquivo de dados
        select(num_linha_dom, everything())

# Identifica linhas que contem ao menos um erro
censo1960_hh$contains_invalid <- censo1960_hh %>% 
        select(starts_with("test_")) %>%
        mutate_all(function(x) as.numeric(!(x))) %>%
        replace(is.na(.), 0) %>%
        rowSums(.)


#########################
# Teste de consistencia da variavel de ID do domicilio/familia
# Como essa informacao parece ter sido adicionada no banco posteriormente, nao parece haver problemas com ela
# O importante é checar os seguintes aspectos:
# -- Ela de poder ser transformada em numerica sem gerar nenhum missing
# -- Não Pode haver valores repetidos (afinal se trata de um ID)

id_numeric <- as.numeric(censo1960_hh$ID)
sum(is.na(id_numeric)) # Nenhum valor missing
freq_id = table(id_numeric)
freq_id[freq_id==1]
freq_id[freq_id > 1]   # Nenhum valor repetido

# Tudo ok!

#########################

# Exibe os resultados 
#censo1960_hh %>% 
#        filter(contains_invalid > 0) %>%
#        View()


# Grava num objeto o identificador dos registros problematicos

# Problema 1: presenca de caracteres nao listados como validos pelo dicionario de codigo
hh_linhas_problemas_1 <- censo1960_hh %>% 
        filter(contains_invalid > 0) %>%
        .$num_linha_dom

# Problema 2: presenca de outros tipos de caracteres invalidos
hh_linhas_problemas_2 = grep(pattern = "[[:alpha:]]|[[:cntrl:]]|[[:punct:]]" , 
                               x       = gsub(pattern     = "[\\]", 
                                              replacement = "", 
                                              x           = c60_string_hh))
hh_linhas_problemas_2 = censo1960_hh[hh_linhas_problemas_2,"num_linha_dom"] %>% unlist()


# Problema 3: presenca de espacos (até 3) entre caracteres
hh_linhas_problemas_3 = grep(pattern = "[[:digit:]][[:blank:]]{1,3}[[:digit:]]",
                             x       = c60_string_hh)
hh_linhas_problemas_3 = censo1960_hh[hh_linhas_problemas_3,"num_linha_dom"] %>% unlist()


# Compilando num objeto todas as linhas detectadas como problemáticas
hh_linhas_problemas = c(hh_linhas_problemas_1, hh_linhas_problemas_2, hh_linhas_problemas_3) %>%
        unique() %>%
        sort()


write.csv2(x = censo1960_hh %>% 
                   mutate(string = c60_string_hh) %>%
                   filter(num_linha_dom %in% hh_linhas_problemas) %>%
                   select(num_linha_dom, string, starts_with("test_"), contains_invalid),
           file = "linhas_problematicas_domicilios.csv",
           row.names = F)

##############################
## OUTROS TESTES DE CONSISTENCIA

# Nos domicilios coletivos (V101 == 3) as variaveis V102 a V113 devem estar em branco
censo1960_hh %>%
        filter(!(num_linha_dom %in% hh_linhas_problemas)) %>% # neste passo nao analisaremos os registros problematicos
        filter(V101 == 3) %>%
        select(starts_with("V")) %>%
        select(V102:V113) %>%
        map(., function(x) freq(x, plot=F)) # as frequencias estao todas OK

# O cruzamento entre a especie de domicilio da v101 (particular unico, particular com + familias, coletivo) e o 
# tipo de domicilio da v102 (duravel, rustico, improvisado etc) nao deve apresentar valores nos cruzamentos:
# -- SE v101 = particular unico ou 1a familia, v102 deve ser preenchida
# -- SE v101 = coletivo, v102 é vazia
# -- SE v101 = 2a/3a familia ou boletim individual, v102 é vazia
with(censo1960_hh %>% filter(!(num_linha_dom %in% hh_linhas_problemas)), {
        crosstab(V101, V102)   # tudo ok
})


# Nos domicilios particulares improvisados (V102 == 6) as variaveis V103 a V113 devem estar em branco
censo1960_hh %>%
        filter(!(num_linha_dom %in% hh_linhas_problemas)) %>% # neste passo nao analisaremos os registros problematicos
        filter(V102 == 6) %>%
        select(starts_with("V")) %>%
        select(V103:V113) %>%
        map(., function(x) freq(x, plot=F)) # tudo ok

##############################
## CONCLUSOES E OBSERVACOES FINAIS

# -- Com excessao dos 9 registros problematicos identificados, o arquivo de dados de domicilios parece consistente

# -- IMPORTANTE:
# O arquivo de dados de domicilios parece dizer respeito, na verdade, a dados de FAMILIAS. Os indicios sao os seguinte:
#       1 - A variavel V101 identifica familias dentro do domicilio particular. Se v101 = 4, trata-se da segunda familia dentro
#           de um domicilio particular. 
#       2 - Registros identificados como 2a ou 3a familia (v101 == 4 ou v101 == 5) nao contem informacoes para as variaveis v102 a 113.
#           Ver abaixo:

censo1960_hh %>%
        filter(!(num_linha_dom %in% hh_linhas_problemas)) %>% # neste passo nao analisaremos os registros problematicos
        filter(V101 %in% c(4,5)) %>%
        select(starts_with("V")) %>%
        select(V102:V113) %>%
        map(., function(x) freq(x, plot=F)) # Os registros identificados com v101 == 4 ou v101 == 5 nao contem valores validos


# Se essa suspeita estiver correta, entao apenas os registros identificados como v101 == 1, 2 ou 3 sao de fato domicilios. 
# A funcao "build_id_1960" aplica entao um algoritmo que cria IDs de domicilios a partir dessa suposicao.


#===================================================================================================================================
# ANALISE DOS REGISTROS DE PESSOA

# Carregando o arquivo de input que define as posicoes das variaveis
input_person  <- read_xlsx(input_file, sheet = "Person_open")

# Tomando apenas os dados de pessoas (Assumindo que qualquer valor diferente de 1 na coluna 17 indica pessoas)
# Essa suposicao é baseada na sintaxe (SPSS) que acompanhava o arquivo de dados que recebi. A principio, teria
# Sido escrita por Adalberto Cardoso e Marcelo Paixao
c60_string_pess <- c60_string[which(teste_char17_dom_pess != 1)]
num_linha_pess <- (1:n_linhas)[which(teste_char17_dom_pess != 1)]


censo1960_pess <- aplicaTesta_layout(dados_string = c60_string_pess,
                                   input        = input_person) %>%
        mutate(num_linha_pess = num_linha_pess) %>% # adicionamos uma coluna que indica a linha onde o registro se encontra no arquivo de dados
        select(num_linha_pess, everything())


# Identifica linhas que contem ao menos um erro
censo1960_pess$contains_invalid <- censo1960_pess %>% 
        select(starts_with("test_")) %>%                          # A princípio, estava excluindo propositalmente a V217 e V218 (total de filhos tidos e vivos). 
                                                                  # O dicionario dessas variaveis parece conter erros. O valor 2 nao esta listado. 
                                                                  # E há pessoas com mais de 30 filhos.
        mutate_all(function(x) as.numeric(!(x))) %>%
        mutate_all(function(x) ifelse(is.na(x), 0, x)) %>% 
        rowSums(.) 


# Grava num objeto o identificador dos registros problematicos

# Problema 1: presenca de caracteres nao listados como validos pelo dicionario de codigo
pess_linhas_problemas_1 <- censo1960_pess %>% 
        filter(contains_invalid > 0) %>%
        .$num_linha_pess


# Problema 2: presenca de outros tipos de caracteres invalidos
pess_linhas_problemas_2 = grep(pattern = "[[:alpha:]]|[[:cntrl:]]|[[:punct:]]" , 
                               x       = gsub(pattern     = "[\\]", 
                                              replacement = "", 
                                              x           = c60_string_pess))
pess_linhas_problemas_2 = censo1960_pess[pess_linhas_problemas_2,"num_linha_pess"] %>% unlist()


# Problema 3: presenca de espacos (até 3) entre caracteres
pess_linhas_problemas_3 = grep(pattern = "[[:digit:]][[:blank:]]{1,3}[[:digit:]]",
                             x       = c60_string_pess)
pess_linhas_problemas_3 = censo1960_pess[pess_linhas_problemas_3,"num_linha_pess"] %>% unlist()


# Compilando num objeto todas as linhas detectadas como problemáticas
pess_linhas_problemas = c(pess_linhas_problemas_1, pess_linhas_problemas_2, pess_linhas_problemas_3) %>%
        unique() %>%
        sort()

write.csv2(x = censo1960_pess %>% 
                   mutate(string = c60_string_pess) %>%
                   filter(num_linha_pess %in% pess_linhas_problemas) %>%
                   select(num_linha_pess, string, starts_with("test_"), contains_invalid),
           file = "linhas_problematicas_pessoas.csv",
           row.names = F)
           

##############################################################################################################
##############################################################################################################
##############################################################################################################
# Corrigindo os registros de domicilios

#c60_string_hh_copy  <- c60_string_hh
#c60_string_hh <- c60_string_hh_copy 

checks_hh   <- read_xlsx("check_line_by_line.xlsx", sheet = "households")


diagnosis <- rep(3, nrow(censo1960_hh)) #rep("OK", nrow(censo1960_hh))
for(i in 1:nrow(checks_hh)){
        print(i)
        
        diag = checks_hh$diagnostico[i]
        line = checks_hh$n_line[i]       # identificador da linha no arquivo original (que inclui domicilios e pessoas)
        j = which(num_linha_dom == line) # numero da linha no arquivo apenas de domicilios
        
        if(diag == "ok"){
                diagnosis[j] = 2 #"IGNORABLE PROBLEM - one (or few) corrupted variables"
        }else if(diag == "corrupted_registry"){
                id <- substr(c60_string_hh[j], 56, 62) 
                c60_string_hh[j] <- paste0(c(rep(" ", 54),"\\",id), collapse = "")
                diagnosis[j] = 4 #"UNIGNORABLE PROBLEM - The whole registry is corrupted and was disregarded"
        }else{
                diag = strsplit(diag, ";") %>% 
                        unlist() %>%
                        strsplit("=")
                
                # Deletando espacos excessivos
                if(as.numeric(diag[[1]][2]) > 0){
                        c60_string_hh[j] <- remove_interSpace(c60_string_hh[j], min_times = 1, max_times = 3)
                        
                        # Executando pela segunda vez propositalmente
                        c60_string_hh[j] <- remove_interSpace(c60_string_hh[j], min_times = 1, max_times = 3)
                }
                
                # Identificando se ha espaços para inserir
                n_char_to_insert = 62 - nchar(c60_string_hh[j])
                
                if(n_char_to_insert > 0){
                        if(as.numeric(diag[[3]][2]) > 0){
                                char_to_insert = paste0(c(rep(" ", n_char_to_insert - 1), "\\"), collapse = "")
                        }else{
                                
                                char_to_insert = paste0(rep(" ", n_char_to_insert), collapse = "")
                        }
                        
                        if(as.numeric(diag[[3]][2]) > 0){
                                c60_string_hh[j] <- insert_interCharacter(c60_string_hh[j],
                                                                            pos = nchar(c60_string_hh[j])-7, 
                                                                            character = char_to_insert)        
                        }else{
                                c60_string_hh[j] <- insert_interCharacter(c60_string_hh[j],
                                                                            pos = nchar(c60_string_hh[j])-8, 
                                                                            character = char_to_insert)
                        }
                }
                diagnosis[j] = 1 #"CORRECTED PROBLEM - Displaced characters in the registry were repositioned"
        }
}

censo1960_hh <- aplicaTesta_layout(dados_string = c60_string_hh,
                                   input        = input_hh) %>%
        mutate(num_linha_dom = num_linha_dom,    # adicionamos uma coluna que indica a linha onde o registro se encontra no arquivo de dados
               cem_diagnosis_dom = diagnosis) %>% 
        select(num_linha_dom, everything())



# Identifica linhas que contem ao menos um erro
censo1960_hh$cem_vars_still_problematic_dom <- censo1960_hh %>% 
        select(starts_with("test_")) %>%  
        mutate_all(function(x) as.numeric(!(x))) %>%
        mutate_all(function(x) ifelse(is.na(x), 0, x)) %>% 
        rowSums(.)

#censo1960_hh %>% 
#        filter(cem_diagnosis_dom == "CORRECTED PROBLEM - Displaced characters in the registry were repositioned") %>%
#        View() # tudo OK!

#freq(censo1960_hh$cem_diagnosis_dom)

# Identificando as variaveis problematicas nos casos problematicos

casos_problematicos <- censo1960_hh %>%
        filter(cem_vars_still_problematic_dom > 0) 

teste_problematicos <- casos_problematicos %>%
        select(starts_with("test_"), -test_ID, -test_barra)

casos_problematicos$cem_problematic_vars_list_dom = ""
vars <- names(teste_problematicos)
for(var in vars){
        print(var)
        problematic_cases <- which(!teste_problematicos[[var]])
        
        var_name = gsub(pattern = "test_", replacement = "" , x = var)
        
        casos_problematicos[problematic_cases, "cem_problematic_vars_list_dom"] = 
                paste(casos_problematicos[problematic_cases, ]$cem_problematic_vars_list_dom, var_name)
}

casos_problematicos$cem_problematic_vars_list_dom <- str_trim(casos_problematicos$cem_problematic_vars_list_dom)

censo1960_hh <- left_join(x = censo1960_hh,
                          y = casos_problematicos %>%
                                  select(num_linha_dom, cem_problematic_vars_list_dom),
                          by = "num_linha_dom")
                                  

# Substituindo valores invalidos por missing
num_linha_dom_prob <- censo1960_hh %>%
        filter(cem_vars_still_problematic_dom >= 1) %>%
        .$num_linha_dom

#i = num_linha_dom_prob[7]
for(i in num_linha_dom_prob){
        print(i)
        vars_prob <- censo1960_hh[censo1960_hh$num_linha_dom == i, "cem_problematic_vars_list_dom"] %>% 
                unlist() %>%
                strsplit(split = " ") %>%
                unlist() %>%
                str_trim()
        var = vars_prob[2]
        for(var in vars_prob){
                print(paste("---", var))
                censo1960_hh[censo1960_hh$num_linha_dom == i,][[var]] <- NA        
        }
}


# Compilando o banco de domicilios (quase) final 
censo1960_hh_numeric <- censo1960_hh%>%
        select(-starts_with("test_"), - barra) %>%
        select(-cem_diagnosis_dom, -cem_problematic_vars_list_dom,
               -starts_with("place_")) %>%
        mutate_all(as.numeric)

censo1960_hh_character <- censo1960_hh %>%
        select(cem_diagnosis_dom, cem_problematic_vars_list_dom,
               starts_with("place_"))
        
censo1960_hh_preliminar <- cbind(censo1960_hh_numeric, censo1960_hh_character) %>% as_tibble()




##############################################################################################################
##############################################################################################################
##############################################################################################################
# Corrigindo os registros de pessoas

#c60_string_pess_copy  <- c60_string_pess
#c60_string_pess <- c60_string_pess_copy
checks_pess <- read_xlsx("check_line_by_line.xlsx", sheet = "persons")

diagnosis <- rep(3, nrow(censo1960_pess)) #rep("OK", nrow(censo1960_pess))
#i = 65
for(i in 1:nrow(checks_pess)){
        print(i)
        
        diag = checks_pess$diagnostico[i]
        line = checks_pess$n_line[i]
        j = which(num_linha_pess == line)
        
        if(diag == "ok"){
                diagnosis[j] = 2 #"IGNORABLE PROBLEM - one (or few) corrupted variables"
        }else if(diag == "corrupted_registry"){
                id <- substr(c60_string_pess[j], 56, 62) 
                c60_string_pess[j] <- paste0(c(rep(" ", 54),"\\",id), collapse = "")
                diagnosis[j] = 4 #"UNIGNORABLE PROBLEM - The whole registry is corrupted and was disregarded"
        }else{
                diag = strsplit(diag, ";") %>% 
                        unlist() %>%
                        strsplit("=")
                
                # Deletando espacos excessivos
                if(as.numeric(diag[[1]][2]) > 0){
                        
                        c60_string_pess[j] <- remove_interSpace(c60_string_pess[j], min_times = 1, max_times = 3)
                        
                        # Executando pela segunda vez propositalmente
                        c60_string_pess[j] <- remove_interSpace(c60_string_pess[j], min_times = 1, max_times = 3)
                        
                }
                
                n_char_to_insert = 62 - nchar(c60_string_pess[j])
                
                if(n_char_to_insert > 0){
                        if(as.numeric(diag[[3]][2]) > 0){
                                char_to_insert = paste0(c(rep(" ", n_char_to_insert - 1), "\\"), collapse = "")
                        }else{
                                char_to_insert = paste0(rep(" ", n_char_to_insert), collapse = "")
                        }
                        
                        if(as.numeric(diag[[3]][2]) > 0){
                                c60_string_pess[j] <- insert_interCharacter(c60_string_pess[j],
                                                                            pos = nchar(c60_string_pess[j])-7, 
                                                                            character = char_to_insert)        
                        }else{
                                c60_string_pess[j] <- insert_interCharacter(c60_string_pess[j],
                                                                            pos = nchar(c60_string_pess[j])-8, 
                                                                            character = char_to_insert)
                        }
                }
                diagnosis[j] = 1 #"CORRECTED PROBLEM - Displaced characters in the registry were repositioned"
        }
}

censo1960_pess <- aplicaTesta_layout(dados_string = c60_string_pess,
                                     input        = input_person) %>%
        mutate(num_linha_pess = num_linha_pess,    # adicionamos uma coluna que indica a linha onde o registro se encontra no arquivo de dados
               cem_diagnosis_pess = diagnosis) %>% 
        select(num_linha_pess, everything())

#freq(censo1960_pess$cem_diagnosis_pess)

# Identifica linhas que contem ao menos um erro
censo1960_pess$cem_vars_still_problematic_pess <- censo1960_pess %>% 
        select(starts_with("test_")) %>%  
        mutate_all(function(x) as.numeric(!(x))) %>%
        mutate_all(function(x) ifelse(is.na(x), 0, x)) %>% 
        rowSums(.) 

#censo1960_pess %>%
#        filter(cem_diagnosis_pess == "CORRECTED PROBLEM - Displaced characters in the registry were repositioned") %>%
#        View()


# Identificando as variaveis problematicas nos casos problematicos
casos_problematicos <- censo1960_pess %>%
        filter(cem_vars_still_problematic_pess > 0) 

teste_problematicos <- casos_problematicos %>%
        select(starts_with("test_"), -test_ID, -test_barra)

casos_problematicos$cem_problematic_vars_list_pess = ""
vars <- names(teste_problematicos)
for(var in vars){
        print(var)
        problematic_cases <- which(!teste_problematicos[[var]])
        
        var_name = gsub(pattern = "test_", replacement = "" , x = var)
        
        casos_problematicos[problematic_cases, "cem_problematic_vars_list_pess"] = 
                paste(casos_problematicos[problematic_cases, ]$cem_problematic_vars_list_pess, var_name)
}

casos_problematicos$cem_problematic_vars_list_pess <- str_trim(casos_problematicos$cem_problematic_vars_list_pess)

censo1960_pess <- left_join(x = censo1960_pess,
                          y = casos_problematicos %>%
                                  select(num_linha_pess, cem_problematic_vars_list_pess),
                          by = "num_linha_pess")


# Substituindo valores invalidos por missing
num_linha_pess_prob <- censo1960_pess %>%
        filter(cem_vars_still_problematic_pess >= 1) %>%
        .$num_linha_pess

for(i in num_linha_pess_prob){
        print(i)
        vars_prob <- censo1960_pess[censo1960_pess$num_linha_pess == i, "cem_problematic_vars_list_pess"] %>% 
                unlist() %>%
                strsplit(split = " ") %>%
                unlist()
        
        for(var in vars_prob){
                print(paste("---", var))
                censo1960_pess[censo1960_pess$num_linha_pess == i,][[var]] <- NA        
        }        
}


# Compilando o banco de pessoas (quase) final 
censo1960_pess_numeric <- censo1960_pess%>%
        select(-starts_with("test_"), - barra) %>% 
        select(-cem_diagnosis_pess, -cem_problematic_vars_list_pess,
               -starts_with("place_")) %>%
        mutate_all(as.numeric)


censo1960_pess_character <- censo1960_pess %>%
        select(cem_diagnosis_pess, cem_problematic_vars_list_pess,
               starts_with("place_"))

censo1960_pess_semifinal <- cbind(censo1960_pess_numeric, censo1960_pess_character) %>% as_tibble()


##############################################################################################################
# Identificando domicílios

# A familia 165232 tinha informacao invalida na V101 (valor 0). 
# Alteramos isso para 1 ("Domic. Part Unico") e atribuimos um ID de domicilio
censo1960_hh_preliminar <- censo1960_hh_preliminar %>% 
        mutate(V101 = ifelse(ID == 165232, 1, V101))

# Ha um caso de missing para essa variavel. Mas, por definicao, se esta no banco de domicilios/familias,
# entao seu valor deve ser igual a 1
censo1960_hh_preliminar$REC_TYPE = 1

# Criando variaveis de ID de domicilio e de familia - separadas
censo1960_hh_preliminar <- censo1960_hh_preliminar %>%
        arrange(ID, V101) %>%
        mutate(cem_IDdomicilio   = ifelse(V101 %in% c(1,2,3), ID, NA),
               cem_tipofamilia   = ifelse(V101 %in% c(1,2), 1, ifelse(V101 %in% c(4,5), 2, NA)),
               n_linha = 1:n())
# CODIFICAÇÃO - cem_tipofamilia
# 1  - Familia principal ou unica
# 2  - Outras familias
# NA - Nao se aplica (domicilios coletivos)


linhas_familias <- censo1960_hh_preliminar %>%
        filter(is.na(cem_IDdomicilio)) %>% 
        .$n_linha

# Para familias secundarias
for(linha_familia in linhas_familias){
        
        print(linha_familia)
        
        caso_anterior <- censo1960_hh_preliminar[(linha_familia - 1) , "cem_tipofamilia"] == 1 
        dim(caso_anterior) <- NULL
        
        if( caso_anterior & !is.na(caso_anterior)){
                censo1960_hh_preliminar[linha_familia , "cem_IDdomicilio"] <- censo1960_hh_preliminar[(linha_familia-1) , "cem_IDdomicilio"]
        }
}


# Para familias terciarias
linhas_familias = which(is.na(censo1960_hh_preliminar$cem_IDdomicilio))
for(linha_familia in linhas_familias){
        
        print(linha_familia)
        
        caso_anterior <- censo1960_hh_preliminar[(linha_familia - 1) , "V101"] == 4
        proprio_caso  <- censo1960_hh_preliminar[(linha_familia ) , "V101"] == 5
                
        dim(caso_anterior) <- NULL
        dim(proprio_caso) <- NULL
        
        if( caso_anterior & proprio_caso & !is.na(caso_anterior) & !is.na(proprio_caso)){
                censo1960_hh_preliminar[linha_familia , "cem_IDdomicilio"] <- censo1960_hh_preliminar[(linha_familia-1) , "cem_IDdomicilio"]
        }
}



censo1960_hh_preliminar <- censo1960_hh_preliminar %>%
        rename(cem_IDfamilia = ID)
censo1960_hh_preliminar$n_linha = NULL

#############
# Ajustes

# Aparentemente, essa familia 155539 constitui um domicilio autonomo. Nao faz parte do domicilio 155537
# Trata-se de um registro de domicilio corrompido. Que, quando, mais adiante, tem suas informacoes levadas para os 
# registros de pessoas, apresenta discrepancia entre os dados de pessoas e domicilios
censo1960_hh_preliminar <- censo1960_hh_preliminar %>%
        mutate(cem_IDdomicilio = ifelse(cem_IDfamilia == 155539, 155539, cem_IDdomicilio),
               V101            = ifelse(cem_IDfamilia == 155539, 1, V101))



#############

rm(var)
test = censo1960_hh_preliminar %>%
        select(-num_linha_dom, -V101, -cem_IDfamilia, -cem_vars_still_problematic_dom,
               -cem_diagnosis_dom, -cem_problematic_vars_list_dom, -cem_tipofamilia) %>%
        select_if(function(x) !is.character(x)) %>%
        group_by(cem_IDdomicilio) %>%
        summarise_all(var, na.rm= T)
        

domicilios_problema = NULL
for(i in 2:ncol(test)){
        domicilios_problema <- c(domicilios_problema, test[which(test[,i] > 0 & !is.na(test[,2])),"cem_IDdomicilio"] %>% unlist())
}
domicilios_problema <- domicilios_problema %>% unique() ##### NENHUM PROBLEMA

censo1960_hh_preliminar %>%
        filter(cem_IDdomicilio %in% domicilios_problema) # nao ha domicilios que apresentam variacao de caracteristicas entre familias


#Assumo que a familia 121705 pertence ao domicilio da familia 121704
#censo1960_hh_preliminar %>%
#        filter(cem_IDfamilia %in% c(121704, 121705)) %>% View()
               


##############################################################################################################
# Testes para conectar o banco de pessoas ao banco de domicilios/familias

# 1 - Checando se o numero de linhas no banco de domicilios/familias é identico ao número de IDs
length(censo1960_hh_preliminar$cem_IDfamilia)
length(censo1960_hh_preliminar$cem_IDfamilia %>% unique()) #ok

length(censo1960_hh_preliminar$place_holder_09_16 %>% unique()) #ok -- indicando novamente que essa string pode indicar ID de domicílios/famílias

# 2 - Checando se o numero de IDs de domicilio/familia no banco de pessoas é  identico ao número de IDs no banco de domicilios
censo1960_pess_semifinal <- censo1960_pess_semifinal %>%
        rename(cem_IDfamilia = ID)
length(censo1960_pess_semifinal$cem_IDfamilia %>% unique()) # ok

length(censo1960_pess_semifinal$place_holder_09_16 %>% unique()) - 174467 # DISCREPÂNCIA... QUE PODE INDICAR QUE ESSA STRING OU NÃO É O ID DE FAMÍLIA OU ESTÁ CORROMPIDA

# Todos os IDs de familia listados no arquivo de pessoas estao contemplados no arquivo de domicilios:
sum( !(censo1960_pess_semifinal$cem_IDfamilia %in% censo1960_hh_preliminar$cem_IDfamilia) ) # ok!


####################### MERGE DE DOMICILIOS E PESSOAS #######################

#censo1960_pess_semifinal <- censo1960_pess_semifinal %>% 
#        select(UF_pess, V116_pess, V118_pess, 
#               starts_with("V2"), 
#               cem_IDfamilia, cem_vars_still_problematic_pess, cem_diagnosis_pess, cem_problematic_vars_list_pess) %>%
#        rename(UF   = UF_pess,
#               V116 = V116_pess,
#               V118 = V118_pess)


# TRAZENDO OS IDs de DOMICILIO PARA OS INDIVIDUOS -- O que possibilitará o merge com o banco 
# APENAS de domicilios (e nao de familias) depois
censo1960_pess_semifinal <- left_join(x = censo1960_pess_semifinal,
                                  y = censo1960_hh_preliminar %>%
                                          select(cem_IDdomicilio,cem_IDfamilia, V101),
                                  by = "cem_IDfamilia")


# BANCO DE DOMICILIOS - SEMIFINAL - ELIMINANDO FAMILIAS
censo1960_hh_semifinal = censo1960_hh_preliminar %>%
        filter(!(V101 %in% c(4,5))) %>%  # remove familias secundarias e terciarias -- mantendo apenas familias unicas, principais e domicilios coletivos
        select(-cem_tipofamilia, -cem_IDfamilia, -REC_TYPE) # remove variaveis desnecessarias ou apenas de familia



##################################################
# Problemas nos registros das linhas 293555 e 293556

# Como se vê, estão, ambos, com problemas de gravação. O 293555, inclusive, é um dos piores
writeLines(c60_string[293555:293556])
writeLines(c60_string[293550:293579]) # dentro de um contexto maior

# Estão justamente na fronteira entre os casos de Sergipe (UF=30) e Bahia (UF=31). 
# Estão sendo atribuidos ao domicilio de Sergipe, de ID=0046690. Porém, iniciam a string com o valor 31 -- que indica Bahia.
# Além disso, o registro 293555 contém a palavra BAHIA -- evidência mais forte ainda de que se trataria de um caso daquela UF
#
# Temos entao que decidir o que fazer com esses dois registros.

# OBSERVAÇÃO 1 - SOBRE COMO OS IDs DE DOMICILIOS (CARACTERES 56-62) PARECEM TER SIDO CRIADOS
# A numeracao de ids, que parece ter sido adicionada ao arquivo de dados posteriormente, seguindo a seguinte logica:
# --- Sempre que encontra o valor 1 no caractere 17, inicia um novo numero de domicilio/familia
# Como, nesse caso, o registro da linha 293555 esta corrompido, nao houve nova contagem do id.
# provavelmente, por esta razao o numero de domicilio continuou o mesmo há presente nos registros anteriores.

# OBSERVAÇÃO 2 - PARA INICIAR UM NOVO ID, TERÍAMOS DUAS SAIDAS
# 1 - ASSUMIR QUE O REGISTRO DA LINHA 293555 É UM DOMICILIO (o que poderia ser feito com substr(c60_string[293555], 17, 17) <- "1") 
#     Mas há dois problemas ao proceder assim:
#     a) modificação dos dados originais
#     b) o indivíduo do registro 293556 vai morar sozinho. E, aparentemente, ele está na posição de filho (v203=9, caractere 20),
#        e tem apenas 15 anos (caracteres 22 e 23)
#
# 2 - CRIAR UMA NOVA LINHA NO BANCO DE DADOS, QUE REPRESENTE O DOMICILIO/A FAMILIA FALTANTE
#
# ESSAS DUAS SOLUÇÕES PARECEM SER MUITO RADICAIS.


#################################################################
### POR ENQUANTO, A MELHOR OPÇÃO PARECE SER NÃO MEXER EM NADA ###
#################################################################



####################################################################################################


# BANCO SEMIFINAL DE PESSOAS - COM VARIAVEIS DE DOMICILIOS
censo1960_pess_semifinal <- left_join(x = censo1960_pess_semifinal, 
                                  y = censo1960_hh_semifinal %>%
                                          select(-V101),  # nao seleciona a variavel V101 pois ela ja foi importada antes
                                  by = "cem_IDdomicilio") %>%
        rename(UF_pess   = UF.x,
               UF_dom    = UF.y,
               V116_pess = V116.x,
               V116_dom  = V116.y,
               V118_pess = V118.x,
               V118_dom  = V118.y)


####################### IMPUTACAO DE VALORES FALTANTES #######################



# Identificando casos em que ha missing apenas numa das fontes de informacao (ou pessoas ou domicilios)
censo1960_pess_semifinal <- censo1960_pess_semifinal %>%
        mutate(cem_UF_missingDom_notmissingPerson   = as.numeric(is.na(UF_dom)    & !is.na(UF_pess)  ),
               cem_UF_missingPerson_notmissingDom   = as.numeric(is.na(UF_pess)   & !is.na(UF_dom)   ),
               cem_V116_missingDom_notmissingPerson = as.numeric(is.na(V116_dom)  & !is.na(V116_pess)),
               cem_V116_missingPerson_notmissingDom = as.numeric(is.na(V116_pess) & !is.na(V116_dom) ),
               cem_V118_missingDom_notmissingPerson = as.numeric(is.na(V118_dom)  & !is.na(V118_pess)),
               cem_V118_missingPerson_notmissingDom = as.numeric(is.na(V118_pess) & !is.na(V118_dom))
               )


# Complementando informacoes: quando ha missing na variavel de domicilio, atribui o valor observado no registro de pessoa e vice-versa 
censo1960_pess_semifinal <- censo1960_pess_semifinal %>% 
        mutate(UF_dom  = ifelse(cem_UF_missingDom_notmissingPerson == 1, UF_pess, UF_dom),
               UF_pess = ifelse(cem_UF_missingPerson_notmissingDom == 1, UF_dom,  UF_pess),
               
               V116_dom  = ifelse(cem_V116_missingDom_notmissingPerson == 1, V116_pess, V116_dom),
               V116_pess = ifelse(cem_V116_missingPerson_notmissingDom == 1, V116_dom,  V116_pess),
               
               V118_dom  = ifelse(cem_V118_missingDom_notmissingPerson == 1, V118_pess, V118_dom),
               V118_pess = ifelse(cem_V118_missingPerson_notmissingDom == 1, V118_dom,  V118_pess)
               )

# Deletando variaveis temporarias
censo1960_pess_semifinal$cem_UF_missingDom_notmissingPerson   = NULL
censo1960_pess_semifinal$cem_UF_missingPerson_notmissingDom   = NULL
censo1960_pess_semifinal$cem_V116_missingDom_notmissingPerson = NULL
censo1960_pess_semifinal$cem_V116_missingPerson_notmissingDom = NULL
censo1960_pess_semifinal$cem_V118_missingDom_notmissingPerson = NULL
censo1960_pess_semifinal$cem_V118_missingPerson_notmissingDom = NULL
        
###################### AJUSTES PONTUAIS: ATRIBUINDO VALORES VALIDOS PARA PESSOAS COM MISSING DENTRO DE DOMICILIOS EM QUE 
###################### HA INDIVIDUOS COM VALORES VALIDOS PARA SERVIREM DE "DOADORES"


censo1960_pess_semifinal = data.table(censo1960_pess_semifinal)


# Identificando casos de missing na variavel de UF
censo1960_pess_semifinal[ , num_NA_UF_dom  := sum(is.na(UF_dom)),  by = cem_IDfamilia]
censo1960_pess_semifinal[ , num_NA_UF_pess := sum(is.na(UF_pess)), by = cem_IDfamilia]
censo1960_pess_semifinal[ num_NA_UF_dom > 0  | num_NA_UF_pess > 0 , ] %>% View()

        # DOMICILIO/FAMILIA 155539 - Era um caso problematico na variavel UF. Mas era possivel
        # resolver esse problema com valores doadores de outros casos
        censo1960_pess_semifinal[cem_IDfamilia == 155539, UF_pess := 81] 
        censo1960_pess_semifinal[cem_IDfamilia == 155539, UF_dom  := 81] 
        censo1960_pess_semifinal[cem_IDfamilia == 155539, V116_pess := 8006] 
        censo1960_pess_semifinal[cem_IDfamilia == 155539, V116_dom  := 8006] 
        censo1960_pess_semifinal[cem_IDfamilia == 155539, V118_pess := 5] 
        censo1960_pess_semifinal[cem_IDfamilia == 155539, V118_dom  := 5] 
        censo1960_pess_semifinal[cem_IDfamilia == 155539, V101 := 1] 


# Identificando casos de missing na variavel v116. Depois de resolver o caso da familia 155539, apenas sobra um registro
censo1960_pess_semifinal[ , num_NA_V116_dom  := sum(is.na(V116_dom)),  by = cem_IDfamilia]
censo1960_pess_semifinal[ , num_NA_V116_pess := sum(is.na(V116_pess)), by = cem_IDfamilia]
censo1960_pess_semifinal[ num_NA_V116_dom > 0  | num_NA_V116_pess > 0 , ] %>% View()


# DOMICILIO/FAMILIA 145606
                
        # O Problema é que só há uma pessoa nesse domicilio. Não é possível tomar o valor de doadores dentro do domicilio
        censo1960_pess_semifinal[ cem_IDdomicilio == 145606] %>% View()
        
        # NADA A FAZER - HÁ MISSING NA VARIAVEL V116 TANTO PARA PESSOAS COMO PARA DOMICILIOS E SÓ UMA PESSOA HABITA O DOMICILIO
        censo1960_pess_semifinal[cem_IDfamilia %in% 145604:145608] %>% View() 


        
# Identificando casos de missing na variavel v118. Depois de resolver o caso da familia 155539, nao sobre nenhum registro
censo1960_pess_semifinal[ , num_NA_V118_dom  := sum(is.na(V118_dom)),  by = cem_IDfamilia]
censo1960_pess_semifinal[ , num_NA_V118_pess := sum(is.na(V118_pess)), by = cem_IDfamilia]
censo1960_pess_semifinal[ num_NA_V118_dom > 0  | num_NA_V118_pess > 0 , ] %>% nrow() # nenhum problema


# Transformado o objeto de volta em tibble
censo1960_pess_semifinal = as_tibble(censo1960_pess_semifinal) %>%
        select(-starts_with("num_NA_"))


####################### CONSISTENCIAS ENTRE VARIAVEIS DE DOMICILIO E PESSOAS #######################

censo1960_pess_semifinal$cem_dissonant_UF   = as.numeric(!(censo1960_pess_semifinal$UF_pess   == censo1960_pess_semifinal$UF_dom))
censo1960_pess_semifinal$cem_dissonant_V116 = as.numeric(!(censo1960_pess_semifinal$V116_pess == censo1960_pess_semifinal$V116_dom))
censo1960_pess_semifinal$cem_dissonant_V118 = as.numeric(!(censo1960_pess_semifinal$V118_pess == censo1960_pess_semifinal$V118_dom))


# AINDA FALTA TERMINAR ISSO AQUI....

# Problemas que permanecem na variável UF
censo1960_pess_semifinal %>%
        filter((cem_dissonant_UF %in% 1   | is.na(cem_dissonant_UF)  )) %>%
        select(UF_dom, UF_pess, cem_diagnosis_dom, cem_diagnosis_pess, cem_IDdomicilio, cem_IDfamilia, everything()) %>%
        View() # 3 dissonancias

        # DOMICILIO/FAMILIA 2192
        censo1960_pess_semifinal %>%
                filter(cem_IDdomicilio %in% 2190:2194) %>% View()

# Problemas que permanecem na variável V116
censo1960_pess_semifinal %>%
        filter( (cem_dissonant_V116 == 1 | is.na(cem_dissonant_V116)) ) %>% 
        nrow() # 44 dissonancias

# Problemas que permanecem na variável V118
censo1960_pess_semifinal %>%
        filter((cem_dissonant_V118 == 1 | is.na(cem_dissonant_V118))) %>%
        nrow() # 280 dissonancias

# Total de problemas que permanecem no banco
censo1960_pess_semifinal %>%
        filter((cem_dissonant_UF == 1   | is.na(cem_dissonant_UF) ) |
               (cem_dissonant_V116 == 1 | is.na(cem_dissonant_V116))|
               (cem_dissonant_V118 == 1 | is.na(cem_dissonant_V118))) %>%
        select(UF_pess, UF_dom, 
               V116_pess, V116_dom, 
               V118_pess, V118_dom, 
               cem_diagnosis_pess, cem_diagnosis_dom,
               everything()) %>% nrow #%>% View() # 299 dissonancias ao total


####################### RE-CRIACAO DO BANCO DE DOMICILIOS A PARTIR DO DE PESSOAS (NECESSARIO APOS AS IMPUTACOES) #######################


# Fazendo ultimas transformacoes necessarias
censo1960_pess_semifinal <- censo1960_pess_semifinal %>%
        mutate(ind_morador = as.numeric(V203 %in% c(0:3, 7:9)),
               ind_morador = ifelse(is.na(ind_morador), 0, ind_morador),
               cem_wgt = 1/0.0127) %>%   ### CRIA A VARIAVEL PESO
        group_by(cem_IDdomicilio) %>%
        mutate(cem_num_moradores = n(),
               cem_num_familias  = length(unique(cem_IDfamilia)), # CRIA VARIAVEL QUE ESTAVA LISTADA NO DICIONARIO
               cem_num_familias  = ifelse(V101 == 3, NA, cem_num_familias), 
               cem_num_pessoas   = sum(ind_morador),              # CRIA VARIAVEL QUE ESTAVA LISTADA NO DICIONARIO
               ind_morador = NULL) %>%
        ungroup()
        

max_sem_na = function(x) {
        max(x, na.rm = T)
        }
replace_infinite = function(x) {
        ifelse(!is.finite(x), NA, x)
}

censo1960_hh_final <- censo1960_pess_semifinal %>%
        select(UF_dom,  starts_with("V1"),
               ends_with("_dom"), -ends_with("pess"), 
               cem_IDdomicilio,
               cem_num_pessoas, cem_num_moradores, cem_num_familias, cem_wgt,
               cem_dissonant_UF,cem_dissonant_V116, cem_dissonant_V118, 
               -cem_vars_still_problematic_dom, -num_linha_dom, -cem_problematic_vars_list_dom) %>%
        group_by(cem_IDdomicilio) %>%
        summarise_all(max_sem_na) %>%
        ungroup() %>%
        mutate_all(replace_infinite) %>%
        rename(UF   = UF_dom,
               V116 = V116_dom, 
               V118 = V118_dom)

censo1960_hh_tmp <- censo1960_pess_semifinal %>% 
        select(cem_IDdomicilio, cem_problematic_vars_list_dom) %>%
        group_by(cem_IDdomicilio) %>%
        summarise(cem_problematic_vars_list_dom = first(cem_problematic_vars_list_dom))

censo1960_hh_final <- censo1960_hh_final %>%
        left_join(x  = .,
                  y  = censo1960_hh_tmp,
                  by = "cem_IDdomicilio") 
        
names(censo1960_hh_final) = tolower(names(censo1960_hh_final))

censo1960_hh_final <- censo1960_hh_final %>%
        select(uf, v116, v118, cem_iddomicilio, 
               v101, v102, cem_wgt,
               v103, v104, v105, v106, v107, v108,
               v109, v110, v111, v112, v113, 
               cem_num_pessoas, cem_num_moradores,
               cem_num_familias, cem_diagnosis_dom,
               cem_problematic_vars_list_dom,
               cem_dissonant_uf, cem_dissonant_v116,
               cem_dissonant_v118)

censo1960_hh_final <- censo1960_hh_final %>%
        mutate(cem_problematic_vars_list_dom = str_replace_all(string = cem_problematic_vars_list_dom,
                                                               pattern = "REC_TYPE",
                                                               replacement = "") %>%
                       str_replace_all(pattern = "  ",
                                       replacement = " ") %>% 
                       tolower()
        )


############################

names(censo1960_pess_semifinal) = tolower(names(censo1960_pess_semifinal))


censo1960_pess_final <- censo1960_pess_semifinal %>%
        rename(cem_idindividuo = num_linha_pess,
               v204b = age) %>% 
        select(uf_pess, uf_dom, 
               v116_pess, v116_dom, 
               v118_pess, v118_dom,
               starts_with("v"),
               starts_with("cem_id"),
               starts_with("c"),
               everything(),
               -rec_type, 
               -starts_with("place"),
               -num_linha_dom,
               -cem_vars_still_problematic_dom, -cem_vars_still_problematic_pess) %>% 
        select(starts_with("u"), starts_with("v"), everything()) 


censo1960_pess_final <- censo1960_pess_final %>%
        select(uf_pess,uf_dom,v116_pess,v116_dom,v118_pess,v118_dom,
               cem_idindividuo,cem_idfamilia,cem_iddomicilio,cem_wgt,
               v202,v203,v204,v204b,v205,v206,v207,v208,v209,v299,
               v210,v211,v212,v213,v214,v215,v216,v217,v218,v219,
               v220,v221,v223,v223b,v224,v101,v102,v103,v104,v105,
               v106,v107,v108,v109,v110,v111,v112,v113,
               cem_num_pessoas,cem_num_moradores,cem_num_familias,
               cem_diagnosis_pess,cem_problematic_vars_list_pess,cem_diagnosis_dom,
               cem_problematic_vars_list_dom,cem_dissonant_uf,cem_dissonant_v116,
               cem_dissonant_v118)


censo1960_pess_final <- censo1960_pess_final %>% 
        mutate(cem_problematic_vars_list_pess = str_replace_all(string = cem_problematic_vars_list_pess,
                                                                pattern = "AGE",
                                                                replacement = "v204b") %>%
                       str_replace_all(pattern = "REC_TYPE",
                                       replacement = "") %>%
                       str_replace_all(pattern = "  ",
                                       replacement = " ") %>% 
                       tolower(),
               cem_problematic_vars_list_dom = str_replace_all(string = cem_problematic_vars_list_dom,
                                                               pattern = "REC_TYPE",
                                                               replacement = "") %>%
                       str_replace_all(pattern = "  ",
                                       replacement = " ") %>% 
                       tolower()
        )

####################### ULTIMOS TESTES DE CONSISTENCIA

# Domicilios

# Nenhum domicilio tem mais dormitórios do que comodos
sum(censo1960_hh_final$v112 < censo1960_hh_final$v113, na.rm=T) #ok

table(censo1960_hh_final$v101, censo1960_hh_final$v102) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v103) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v104) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v105) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v106) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v107) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v108) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v109) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v110) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v111) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v112) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v113) #ok
table(censo1960_hh_final$v101, censo1960_hh_final$v118) #ok


table(censo1960_hh_final$v102, censo1960_hh_final$v103) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v104) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v105) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v106) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v107) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v108) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v109) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v110) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v111) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v112) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v113) #ok
table(censo1960_hh_final$v102, censo1960_hh_final$v118) #ok


# Pessoas
table(censo1960_pess_final$v204b, censo1960_pess_final$v204) #ok

table(censo1960_pess_final$v207, censo1960_pess_final$v208) #problema em dois casos

censo1960_pess_semifinal %>%
        filter(v207 == 76 & v208 == 9) %>%
        View()

table(censo1960_pess_final$v209, censo1960_pess_final$v299) #ajustes necessários

censo1960_pess_final <- censo1960_pess_final %>%
        mutate(v299 = ifelse(v209 == 2, NA, v299)) # O valor zero na v299 significa, na realidade, um não se aplica 

table(censo1960_pess_final$v210, censo1960_pess_final$v209) #ok


with(censo1960_pess_final %>% filter(v204 == 0), {
        print(table(v204b, v211)) #ok        
        print(table(v204b, v212)) #ok        
        print(table(v204b, v213)) #ok    
        print(table(v204b, v214)) #ok    
        print(table(v204b, v215)) #ok    
        print(table(v204b, v216)) #ok    
        print(table(v204b, v217)) #ok
        print(table(v204b, v218)) #ok    
        print(table(v204b, v219)) #ok
        print(table(v204b, v220)) #ok
        print(table(v204b, v221)) #ok
        print(table(v204b, v223)) #ok   
        print(table(v204b, v223b)) #ok    
})


with(censo1960_pess_final %>% filter(v204 == 1), {
        print("v211")
        print(table(v204b, v211)) #ok        
        print("------------------------------------------------")
        
        print("v212")
        print(table(v204b, v212)) #ok        
        print("------------------------------------------------")
        
        print("v213")
        print(table(v204b, v213)) #ok    
        print("------------------------------------------------")
        
        print("v214")
        print(table(v204b, v214)) #ok    
        print("------------------------------------------------")
        
})

tabela_idade_casamento <- with(censo1960_pess_final %>% filter(v204 > 0),{
        table(v204b, v216)})
#write.csv2(tabela_idade_casamento, "c:/users/rogerio/desktop/tabela_idade_casamento.csv")

with(censo1960_pess_final %>% filter(v204 == 1), {
        print("v215")
        print(table(v204b, v215)) #ok    
        print("------------------------------------------------")
        
        print("v216")
        print(table(v204b, v216)) #ok    
        print("------------------------------------------------")
        
        print("v217")
        print(table(v204b, v217)) #ok
        print("------------------------------------------------")
        
        print("v219")
        print(table(v204b, v219)) #ok
        print("------------------------------------------------")
        
        print("v220")
        print(table(v204b, v220)) #ok
        print("------------------------------------------------")
})

with(censo1960_pess_final %>% filter(v204 == 1), {
        print("v221")
        print(table(v204b, v221)) #ok
        print("------------------------------------------------")
        
        print("v223")
        print(table(v204b, v223)) #ok   
        print("------------------------------------------------")
        
        print("v223b")
        print(table(v204b, v223b)) #ok    
})



####################### SALvANDO ARQUIvOS FINAIS

write.csv(x = censo1960_hh_final,   
          file = paste0(output_dir, "/Censo.1960.brasil.domicilios.amostra.1.27porcento.csv"),
          na="",
          row.names = F, 
          fileEncoding = "utf-8",
          quote = TRUE)


write.csv(x = censo1960_pess_final,   
          file = paste0(output_dir, "/Censo.1960.brasil.pessoas.amostra.1.27porcento.csv"),
          na="",
          row.names = F, 
          fileEncoding = "utf-8",
          quote = TRUE)


names(censo1960_hh_final)
names(censo1960_pess_final)

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


# Funcao para aplicar o layout de daddos e testar se as observacoes de cada variavel contem valores listados
# no dicionario de codigos. Cria uma coluna adicional para cada variavel, que consiste num teste logico:
# ** TRUE  = o registro contem valor valido para a variavel 
# ** FALSE = o valor observado ? invalido
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
diagnosis <- rep(3, nrow(censo1960_hh)) #rep("OK", nrow(censo1960_hh))
for(i in 1:nrow(checks_hh)){
        #print(i)
        
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
                
                # Identificando se ha espa?os para inserir
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

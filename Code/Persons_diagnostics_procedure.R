diagnosis <- rep(3, nrow(censo1960_pess))
for(i in 1:nrow(checks_pess)){
        #print(i)
        
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
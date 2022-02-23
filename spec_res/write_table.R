write_longtable = function(data, grp){
         nn = unique(data$nn)   
         str_fin = character(length(nn))
         for(k in 1:length(nn)){
            
            
            n = unique(data$n_i)
            m = unique(data$m_ij)
            str_ij = character(length(n))
            for(j in 1:3){
               
               ab = which(data$nn == nn[k] & data$n_i == n[j] & data$grp == grp)
               str = character(4) 
               for(i in 1:length(ab)){
                  
                  ttt = data[ab[i], ]
                  str[i] = paste(round(c(ttt$m_ij, ttt$wald, ttt$anv, ttt$maxt), 4), collapse = " & ")
                  if(i > 1) str[i] = paste("& &", str[i])
               }
               
               str_ij[j] = paste(str, collapse = "\\\\")
               
            }
            
            str_fin[k] = paste0("&\\multirow{", length(m), "}{*}{", n,"}", "&", str_ij, collapse = "\\\\")
            #if(k > 1) str_fin[k] = paste0("&", str_fin[k])
            
         }
         start_com = "\\begin{longtable}{c | c | c | c | c | c}"
         head_mat = "d & $n_i$ & $m_{ij}$ & Wald-Type & ANOVA-Type & MCTP \\\\\\hline"
         tab = paste0("\\multirow{", length(n), "}{*}{", nn, "}", str_fin, collapse = "\\\\")
         
         end_com = "\\end{longtable}"
         
         tab_fin = paste0(start_com, head_mat, tab, end_com)
         return(cat(tab_fin))
}

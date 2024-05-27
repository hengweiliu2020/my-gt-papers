# create a function cat_stat

cat_stat <- function(df, my_var, my_label,  category, catlabel) {
  
  count0 <- 
    df %>%                   
    group_by(TRT01A, {{my_var}}) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  count3 <- 
    df %>%                   
    group_by(TRT01A) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  
  count3$n <- count3$unique_subj
  count3 <- count3[c("TRT01A","n")]
  
  m_count0 <- merge(count0, count3, by=c("TRT01A"), all=TRUE)
  
  m_count0 <- m_count0 %>%
    mutate(category={{my_var}})
  
  m_count0 <- m_count0[c("category","TRT01A","n","unique_subj")]
  
  m_count0$value <- ifelse(is.na(m_count0$unique_subj),"0", paste(m_count0$unique_subj, "(", format(round(100*m_count0$unique_subj/m_count0$n, 1), nsmall = 1), ")"))
  
  a1 <- m_count0 %>%
    pivot_wider(id_cols=c("category"), values_from=value, names_from=TRT01A)
  
  
  # set up a frame b3, it will be merged with a1
 
 catn <- matrix(1:(dim(category)[1]))
 mat1 <- cbind(catn, unlist(category), unlist(catlabel))
 b1 <- data.frame(mat1)
 
 mat2 <- matrix(NA, nrow = 1, ncol = 3)
 b2 <- data.frame(mat2)
 
 b2$X1 <- dim(category)[1]+1
 b2$X2 <- "MISSING"
 b2$X3 <- "Missing"
 
 b3 <- rbind(b1,b2)
   
  b3$catn <- b3$X1
  b3$category <-  b3$X2
  b3$catlabel <-  b3$X3
  
  final <- merge(a1, b3, by=c("category"), all=TRUE)

  final[is.na(final)] <- '0'
 
  final$tag <- my_label
  final2 <- final %>%
    arrange(catn) %>%
    select (-starts_with('X'), -catn, -category)
  

  return(final2)
  
}

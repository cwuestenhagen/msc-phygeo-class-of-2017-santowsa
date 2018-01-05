setwd("D:/mpg/skript/da/da-ws-07-1")
source("ffs_funktion.R")

tab <- readRDS("D:/mpg/daten/tab/feldfruechte_clean.RDS")

tab_numeric <- lapply(tab[,-c(1:5)],
                      function(fn){
                        fn<- sub(",",".",fn,fixed = T)
                        as.numeric(fn)
                      })

data <- data.frame(tab_numeric)
dep <- names(data[1])
vars <- names(data[2:10])
selected_vars <- NULL

while((ffs_funktion(data = data, dep = dep, vars = vars,
                    selected_vars = selected_vars)[[2]]) 
      >= max(ffs_funktion(data = data, dep = dep, vars = vars,
                          selected_vars = selected_vars)[[3]]$Adj_R_sqrd)){
  
  result <- ffs_funktion(data = data, dep = dep, vars = vars,
                         selected_vars = selected_vars)
  
  vars <- vars[-which(vars == result[[1]])]
}

print(result)



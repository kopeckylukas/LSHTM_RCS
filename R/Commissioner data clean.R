library("tidyverse")
library("readxl")

{
  path <- "data/com_data/19-20/DECEMBER-2019-CANCER-WAITING-TIMES-COMMISSIONER-WORKBOOK-PROVISIONAL.xlsx"
  period <- as.Date('2019-12-01') #yyy-mm-dd
  newName <- "data/com_done/december-2019.csv"
  
  
  sheet_no <- length(excel_sheets(path))
  
  sheet2 <-read_excel(path, sheet=2,col_names = FALSE)
  cg2 <- which(sheet2[,2] == "CCG CODE")
  sheet2$standard <- unlist(sheet2[1,1])
  sheet2$period <- period
  colnames(sheet2) <- c("nah","ccg_code","ccg_name","cancer_type","total_treated","within_standard","braches","performance","standard","period")
  sheet2 <- sheet2[(cg2+1):nrow(sheet2),2:ncol(sheet2)]
    
  newSheet <- sheet2
  
  
  for(i in 3:sheet_no){
    
    
    
    sheet3 <-read_excel(path, sheet=i,col_names = FALSE)
    cg <- which(sheet3[,2] == "CCG CODE")
    sheet3$standard <- unlist(sheet3[1,1])
    sheet3$period <- period
    if(ncol(sheet3) == 11){
      colnames(sheet3) <- c("nah","ccg_code","ccg_name","settings","cancer_type","total_treated","within_standard","braches","performance","standard","period")
      sheet3 <- sheet3[(cg+1):nrow(sheet3),c(2:3,5:ncol(sheet3))]
    }else{
      colnames(sheet3) <- c("nah","ccg_code","ccg_name","cancer_type","total_treated","within_standard","braches","performance","standard","period")
      sheet3 <- sheet3[(cg+1):nrow(sheet3),2:ncol(sheet3)]
    }
    
    newSheet <- rbind(newSheet, sheet3)
  
  }
  
    write_csv(newSheet,newName,na="")

}


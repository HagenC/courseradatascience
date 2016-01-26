
#Rscript for year frequency oveer time. 
setwd("C:/Users/UNC/Desktop/tmpiPSYCH")

#import Haplogr_Gender_PKU_Control and YOB_PUK_ID


#############################################################################################################################
#5 first 1981-1986 and 2000-2005

Dist_year_haplo <- merge(Haplogr_Gender_PKU_Con, YOB_PUK_ID, by = "PKU_sample_ID")


DB_81_86 <- subset(Dist_year_haplo, year_birth < 1987, select = c("PKU_sample_ID", "Haplogr", "year_birth", "Gender"))
DB_00_05 <- subset(Dist_year_haplo, year_birth > 1999, select = c("PKU_sample_ID", "Haplogr", "year_birth", "Gender"))



library(plyr)
library(dplyr)

HaploStat_f <- function(df) {
  
  tmp1 <- df %>% count(Haplogr)
  Total <- sum(tmp1$n)
  tmp3 <- cbind(tmp1, Total)
  tmp3$freq <- ((tmp3$n/tmp3$Total)*100)
  return(tmp3)
  
  
}


DB_5_last <- HaploStat_f(DB_00_05)
DB_5_last["Periode"] <- "2000-2005"

DB_5_first <- HaploStat_f(DB_81_86)
DB_5_first["Periode"] <- "1981-1986"

DB_5 <- rbind(DB_5_first, DB_5_last)

library(xlsx)
write.xlsx(DB_5, "Z:/UNC_June2015/GWAS mtDNA/Fødested/5_firstANDlastyears_Haplo_CON.xlsx")
##################################################################################################################
#year interval

Dist_year_haplo <- merge(Haplogr_Gender_PKU_Con, YOB_PUK_ID, by = "PKU_sample_ID")


year_bas <- function(df, yr){
    tmp1 <- subset(df, year_birth == yr, select = c("PKU_sample_ID", "Haplogr", "year_birth", "Gender"))
  return(tmp1)
}


D81 <- year_bas(Dist_year_haplo, 1981)
D82 <- year_bas(Dist_year_haplo, 1982)
D83 <- year_bas(Dist_year_haplo, 1983)
D84 <- year_bas(Dist_year_haplo, 1984)
D85 <- year_bas(Dist_year_haplo, 1985)
D86 <- year_bas(Dist_year_haplo, 1986)
D87 <- year_bas(Dist_year_haplo, 1987)
D88 <- year_bas(Dist_year_haplo, 1988)
D89 <- year_bas(Dist_year_haplo, 1989)
D90 <- year_bas(Dist_year_haplo, 1990)
D91 <- year_bas(Dist_year_haplo, 1991)
D92 <- year_bas(Dist_year_haplo, 1992)
D93 <- year_bas(Dist_year_haplo, 1993)
D94 <- year_bas(Dist_year_haplo, 1994)
D95 <- year_bas(Dist_year_haplo, 1995)
D96 <- year_bas(Dist_year_haplo, 1996)
D97 <- year_bas(Dist_year_haplo, 1997)
D98 <- year_bas(Dist_year_haplo, 1998)
D99 <- year_bas(Dist_year_haplo, 1999)
D00 <- year_bas(Dist_year_haplo, 2000)
D01 <- year_bas(Dist_year_haplo, 2001)
D02 <- year_bas(Dist_year_haplo, 2002)
D03 <- year_bas(Dist_year_haplo, 2003)
D04 <- year_bas(Dist_year_haplo, 2004)
D05 <- year_bas(Dist_year_haplo, 2005)



library(plyr)
library(dplyr)

HaploStat_f <- function(df) {
  
 
  tmp1 <- df %>% count(Haplogr)
  Total <- sum(tmp1$n)
  tmp3 <- cbind(tmp1, Total)
  tmp3$freq <- ((tmp3$n/tmp3$Total)*100)
  return(tmp3)
  
  
}

HaploStat_EU <- function(df) {
  
  tmp1 <- df[grep("H", df$Haplogr), ]
  tmp2 <- df[grep("V", df$Haplogr), ]
  tmp3 <- df[grep("U", df$Haplogr), ]
  tmp4 <- df[grep("K", df$Haplogr), ]
  tmp5 <- df[grep("J", df$Haplogr), ]
  tmp6 <- df[grep("T", df$Haplogr), ]
  tmp7 <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
  tmp8 <- tmp7 %>% count(Haplogr)
  Total <- sum(tmp8$n)
  tmp9 <- cbind(tmp8, Total)
  tmp9$freq <- ((tmp9$n/tmp9$Total)*100)
  return(tmp9)
  
  
}




DB81 <- HaploStat_EU(D81)
DB81["Year"] <- "1981"
DB82 <- HaploStat_EU(D82)
DB82["Year"] <- "1982"
DB83 <- HaploStat_EU(D83)
DB83["Year"] <- "1983"
DB84 <- HaploStat_EU(D84)
DB84["Year"] <- "1984"
DB85 <- HaploStat_EU(D85)
DB85["Year"] <- "1985"
DB86 <- HaploStat_EU(D86)
DB86["Year"] <- "1986"
DB87 <- HaploStat_EU(D87)
DB87["Year"] <- "1987"
DB88 <- HaploStat_EU(D88)
DB88["Year"] <- "1988"
DB89 <- HaploStat_EU(D89)
DB89["Year"] <- "1989"
DB90 <- HaploStat_EU(D90)
DB90["Year"] <- "1990"
DB91 <- HaploStat_EU(D91)
DB91["Year"] <- "1991"
DB92 <- HaploStat_EU(D92)
DB92["Year"] <- "1992"
DB93 <- HaploStat_EU(D93)
DB93["Year"] <- "1993"
DB94 <- HaploStat_EU(D94)
DB94["Year"] <- "1994"
DB95 <- HaploStat_EU(D95)
DB95["Year"] <- "1995"
DB96 <- HaploStat_EU(D96)
DB96["Year"] <- "1996"
DB97 <- HaploStat_EU(D97)
DB97["Year"] <- "1997"
DB98 <- HaploStat_EU(D98)
DB98["Year"] <- "1998"
DB99 <- HaploStat_EU(D99)
DB99["Year"] <- "1999"
DB00 <- HaploStat_EU(D00)
DB00["Year"] <- "2000"
DB01 <- HaploStat_EU(D01)
DB01["Year"] <- "2001"
DB02 <- HaploStat_EU(D02)
DB02["Year"] <- "2002"
DB03 <- HaploStat_EU(D03)
DB03["Year"] <- "2003"
DB04 <- HaploStat_EU(D04)
DB04["Year"] <- "2004"
DB05 <- HaploStat_EU(D05)
DB05["Year"] <- "2005"


BB <- bind_cols(DB81, DB82, DB83, DB84, DB85, DB86, DB87, DB88, DB89, DB90, DB91, DB92, DB93, DB94, DB95, DB96, DB97, DB98, DB99, DB00, DB01, DB02, DB03, DB04, DB05)  

library(xlsx)
write.xlsx(BB, "Z:/UNC_June2015/GWAS mtDNA/Fødested/yearbasis_Haplo_CON.xlsx")


########################################################
#speaman corr

BD <- rbind(DB81, DB82, DB83, DB84, DB85, DB86, DB87, DB88, DB89, DB90, DB91, DB92, DB93, DB94, DB95, DB96, DB97, DB98, DB99, DB00, DB01, DB02, DB03, DB04, DB05)  


Grep_haplo <- function(df, ha_G){
  
  tmp1 <- df[grep(ha_G, df$Haplogr), ]
  tmp2 <- transform(tmp1, Year = as.numeric(Year))
  tmp3 <- subset(tmp2, select = c(Year, freq))
  colnames(tmp3) <- c("Year", ha_G) 
  
  return(tmp3)
  
}



H_year <-  Grep_haplo(BD, "H")
V_year <-  Grep_haplo(BD, "V")
U_year <-  Grep_haplo(BD, "U")
K_year <-  Grep_haplo(BD, "K")
J_year <-  Grep_haplo(BD, "J")
T_year <-  Grep_haplo(BD, "T")

DB <- cbind(H_year, V_year$V, U_year$U, K_year$K, J_year$J, T_year$T)


panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}



pairs(DB, lower.panel = panel.smooth, upper.panel = panel.cor)
cor(DB, method = "spearman")

library(stats)
library(Rarity)
plot <- cor(J_corr$Year, J_corr$freq, method = "spearman")
corPlot(DB, method = "spearman")



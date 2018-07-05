#############################################################################
# Program to prepare Short-term indicators. 
# Short term indicators
# Auteur: David Bescond ILO / Department of statistics
# Date:  Feb 2009. last update May 2014
#############################################################################
library(RODBC,quietly=TRUE)
library(openxlsx,quietly=TRUE)



key_OLD 	<- c("CODE_COLLECTION","CODE_COUNTRY", "CODE_SOURCE", "CODE_INDICATOR","CODE_SEX","CODE_CL1","CODE_CL2","CODE_CL3","CODE_CL4", "CODE_CL5")
note 		<- c("NOTES_FREQUENCY_CODE","NOTES_CLASSIF_CODE","NOTES_INDICATOR_CODE","NOTES_SOURCE_CODE","CURRENCY_CODE","NOTES_FREE_TEXT")
note_OLD 	<- c("NOTE_FREQUENCY","NOTE_CL1","NOTE_INDICATOR","NOTE_SOURCE","NOTE_CURRENCY","NOTE_FREE_TEXT")
add 		<- c("ADD_REPOSITORY","ADD_STATUS") 

key_QTA     <- c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Time")
key_ALL     <- c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Time","Sex_Item_Code","Classif1_Item_Code","Classif2_Item_Code","Classif3_Item_Code","Classif4_Item_Code","Classif5_Item_Code")
ALL_COL     <- c("Country_Code","Collection_Code","Indicator_Code","Survey_Code","Sex_Version_Code","Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Time","Sex_Item_Code","Classif1_Item_Code","Classif2_Item_Code","Classif3_Item_Code","Classif4_Item_Code","Classif5_Item_Code","Obs_Value","Flag","Currency","Value_Notes_String","Qtable_Notes_String","Free_Text_Notes")
KEY_ORACLE 	<- c("Lang", "Country_Code", "Country_Label", "Collection_Code", "Collection_Label", "Indicator_Code", "Indicator_Label", "Survey_Code", "Survey_Label", "Sex_Version_Code", "Classif1_Version_Code","Classif2_Version_Code","Classif3_Version_Code","Classif4_Version_Code","Classif5_Version_Code","Sex_Item_Code", "Sex_Item_Label", "Classif1_Item_Code", "Classif1_Item_Label", "Classif2_Item_Code", "Classif2_Item_Label", "Classif3_Item_Code", "Classif3_Item_Label", "Classif4_Item_Code", "Classif4_Item_Label", "Classif5_Item_Code", "Classif5_Item_Label", "Freq", "Time", "Obs_Value", "Flag", "Currency", "Value_Notes_String", "Qtable_Notes_String", "Free_Text_Notes")







# key_Name 	<- paste(key, 	collapse="/")
note_Name 	<- paste(note, 	collapse="/")
add_Name 	<- paste(add, 	collapse="/") 


My_CleanNA_NEW <- function(X, ref= NA){
X <- as.tbl(X)
if(!empty(X)){
for (i in 1:ncol(X)){
		X <- eval(parse(text= paste0("  X %>% mutate(",paste0(colnames(X[,i]))," = ifelse(",paste0(colnames(X[,i])),"%in%c('NaN',NA,'','NA'),",ref,",",paste0(colnames(X[,i])),") 	)"))) 
		X <- eval(parse(text= paste0("  X %>% mutate(",paste0(colnames(X[,i]))," = as.character(",paste0(colnames(X[,i])),") 	)"))) 
	}
}
return(X)
}

My_Round_Matrice <- function(X,d){											

	for (i in 1:ncol(X)){
		X[,i]<- round(as.numeric(X[,i]),d)
		}

return(X)
} 

My_Vlookup <- function(X,X_code,X_text,Y,Y_code,Y_text,Delete_X_code=FALSE){					

X <- as.data.frame(X)
# permet de creer la colonne X_text si elle n'existe pas
if(!X_text%in%colnames(X)){X <- as.data.frame(cbind(rep(NA,nrow(X)),X, stringsAsFactors=FALSE));colnames(X)[1] <- X_text}

# creer un colonnes supplementaire si le mapping doit se faire avec une combinaison de colonnes existantes
i <- 1
X <- as.data.frame(cbind(PASS_ID = X[,X_code[i]],X, stringsAsFactors=FALSE))
if(length(X_code)>1){
while(!length(X_code)%in%i) {i <- i+1;X$PASS_ID <-paste(X$PASS_ID,X[,X_code[i]],sep="_")}}

if(Delete_X_code){X <- X[,!colnames(X)%in%X_code]}

# idem avec Y : creer un colonnes supplementaire si le mapping doit se faire avec une combinaison de colonnes existantes
i <- 1
Y <- as.data.frame(cbind(PASS_ID = Y[,Y_code[i]],Y, stringsAsFactors=FALSE))
if(length(Y_code)>1){
while(!length(Y_code)%in%i) {i <- i+1;Y$PASS_ID <-paste(Y$PASS_ID,Y[,Y_code[i]],sep="_")}}


# mapping
X[,X_text] <- Y[match(as.character(X$PASS_ID), as.character(Y$PASS_ID)),Y_text]
if(is.factor(X[,X_text])){
X[,X_text] <- as.character(X[,X_text])
}


return(X[,!colnames(X)%in%"PASS_ID"])
}

My_Duplicate_Quarters <- function(X,note){         			

# X <- Base

X <- My_unsplit_KEY(X,"NOTE",note)


# Duplicate quarterly data second month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("S","T","P") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("S","T","P") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M03"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("S","T","P") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M04" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("S","T","P") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M06"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("S","T","P") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M07" 		
REF_6 <- X[X$NOTE_FREQUENCY%in%c("S","T","P") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),] ; 	substr(REF_6$TIME_PERIOD,7,9) <- "M09"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("S","T","P") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),] ;	substr(REF_7$TIME_PERIOD,7,9) <- "M10" 		
REF_8 <- X[X$NOTE_FREQUENCY%in%c("S","T","P") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),] ; 	substr(REF_8$TIME_PERIOD,7,9) <- "M12"
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8)

# Duplicate quarterly data first month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("R") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M02"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("R") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M03"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("R") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M05" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("R") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M06"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("R") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M08" 		
REF_6 <- X[X$NOTE_FREQUENCY%in%c("R") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),] ; 	substr(REF_6$TIME_PERIOD,7,9) <- "M09"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("R") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),] ;	substr(REF_7$TIME_PERIOD,7,9) <- "M11" 		
REF_8 <- X[X$NOTE_FREQUENCY%in%c("R") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),] ; 	substr(REF_8$TIME_PERIOD,7,9) <- "M12"
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8)

# Duplicate quarterly data third month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("Q") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("Q") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M02"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("Q") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M04" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("Q") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M05"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("Q") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M07" 		
REF_6 <- X[X$NOTE_FREQUENCY%in%c("Q") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),] ; 	substr(REF_6$TIME_PERIOD,7,9) <- "M08"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("Q") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),] ;	substr(REF_7$TIME_PERIOD,7,9) <- "M10" 		
REF_8 <- X[X$NOTE_FREQUENCY%in%c("Q") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),] ; 	substr(REF_8$TIME_PERIOD,7,9) <- "M11"
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8)

# Duplicate semi annual data first month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M02"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M03"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M04" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M05"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M06" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M08"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M09"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M10" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M11"
REF_10<- X[X$NOTE_FREQUENCY%in%c("A") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),]	;	substr(REF_10$TIME_PERIOD,7,9)<- "M12" 
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10)

# Duplicate semi annual data second month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("B","V") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("B","V") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M03"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("B","V") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M04" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("B","V") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M05"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("B","V") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M06" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("B") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M07"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("B") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M09"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("B") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M10" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("B") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M11"
REF_10<- X[X$NOTE_FREQUENCY%in%c("B") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),]	;	substr(REF_10$TIME_PERIOD,7,9)<- "M12" 
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10)

# Duplicate semi annual data third month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("C","G","W") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("C","G","W") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M02"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("C","G","W") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M04" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("C","G","W") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M05"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("C","G","W") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M06" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("C","G","W","V","n") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M07"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("C","G","W","V","n") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M08"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("C","G","W","V","n") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M10" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("C","G","W","V","n") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M11"
REF_10<- X[X$NOTE_FREQUENCY%in%c("C","G","W","V","n") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),]	;	substr(REF_10$TIME_PERIOD,7,9)<- "M12" 
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10)

# Duplicate semi annual data fourth month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("D","N") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("D","N") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M02"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("D","N") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M03" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("D","N") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M05"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("D","N") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M06" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("D") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M07"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("D") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M08"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("D") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M09" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("D") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M11"
REF_10<- X[X$NOTE_FREQUENCY%in%c("D") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),]	;	substr(REF_10$TIME_PERIOD,7,9)<- "M12" 
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10)

# Duplicate semi annual data 5th month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("E","n") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("E","n") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M02"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("E","n") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M03" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("E","n") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M04"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("E","n") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M06" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("E","N") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M07"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("E","N") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M08"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("E","N") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M09" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("E","N") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M10"
REF_10<- X[X$NOTE_FREQUENCY%in%c("E","N") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),]	;	substr(REF_10$TIME_PERIOD,7,9)<- "M12" 
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10)

# Duplicate semi annual data 6th month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M02"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M03" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M04"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M05" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M07"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M08"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M09" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M10"
REF_10<- X[X$NOTE_FREQUENCY%in%c("F") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),]	;	substr(REF_10$TIME_PERIOD,7,9)<- "M11" 
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,REF_10)

# Duplicate tri annual data first month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M02"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M03"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M01"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M04" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M06"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M07" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M05"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M08"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M10"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M11" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("H") & substr(X$TIME_PERIOD,7,9)%in%c("M09"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M12"
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9)

# Duplicate tri annual data second month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M03"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M02"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M04" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M05"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M07" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M06"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M08"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M09"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M11" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("I") & substr(X$TIME_PERIOD,7,9)%in%c("M10"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M12"
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9)

# Duplicate tri annual data third month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M02"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M03"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M04" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M05"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M06" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M07"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M08"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M09"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M10" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("J","L") & substr(X$TIME_PERIOD,7,9)%in%c("M11"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M12"
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9)

# Duplicate tri annual data third month
REF_1 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),]	;	substr(REF_1$TIME_PERIOD,7,9) <- "M01"
REF_2 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),]	;	substr(REF_2$TIME_PERIOD,7,9) <- "M02"
REF_3 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M04"),]	;	substr(REF_3$TIME_PERIOD,7,9) <- "M03" 		
REF_4 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),] ; 	substr(REF_4$TIME_PERIOD,7,9) <- "M05"
REF_5 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),]	;	substr(REF_5$TIME_PERIOD,7,9) <- "M06" 
REF_6 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M08"),]	;	substr(REF_6$TIME_PERIOD,7,9) <- "M07"
REF_7 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),]	;	substr(REF_7$TIME_PERIOD,7,9) <- "M09"
REF_8 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),]	;	substr(REF_8$TIME_PERIOD,7,9) <- "M10" 		
REF_9 <- X[X$NOTE_FREQUENCY%in%c("K") & substr(X$TIME_PERIOD,7,9)%in%c("M12"),] ; 	substr(REF_9$TIME_PERIOD,7,9) <- "M11"
X <- as.data.frame(rbind(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9,X))
rm(REF_1,REF_2,REF_3,REF_4,REF_5,REF_6,REF_7,REF_8,REF_9)


X <- X[,!colnames(X)%in%note]

return(X)
}

My_prepare_TEXT <- function(X, Lang){ #jan 2014

#     X <- Y

# Vlookup part

#   X <- Base 

test1 <- My_unsplit_KEY(X[,c("KEY","CODE_CL1")],"CODE_CL1",c("CODE_1", "CODE_2", "CODE_3"),"_")
test2 <- My_unsplit_KEY(X[,c("KEY","CODE_CL2")],"CODE_CL2",c("CODE_1", "CODE_2", "CODE_3"),"_")
				   
							   
X <- cbind(	ID_SOURCE 	= substr(X$CODE_SOURCE,1,2),
			CODE_SURVEY = substr(X$CODE_SOURCE,4,7),
			CODE_RANK_IND	= 1:nrow(X),
			CODE_SEX_VS = substr(X$CODE_SEX,1,3),
			CODE_CL1_VS = paste(test1$CODE_1,test1$CODE_2, sep="_"),
			CODE_CL2_VS = paste(test2$CODE_1,test2$CODE_2, sep="_"),
			CODE_RANK_SEXVS	= 1:nrow(X),
			CODE_RANK_CLVS	= 1:nrow(X),
			CODE_RANK_CL2VS	= 1:nrow(X),
			CODE_RANK_SEX = NA,
			CODE_RANK_CL	= 1:nrow(X),
			CODE_RANK_CL2	= 1:nrow(X),
			X,stringsAsFactors=FALSE)
			
			
#X$CODE_CL1_VS <- gsub("NA_NA","NA",X$CODE_CL1_VS)				  
#X$CODE_CL2_VS <- gsub("NA_NA","NA",X$CODE_CL2_VS)				  

rm(test1,test2)



REF_SOURCE <- CODE$SOURCE
REF_SOURCE$CODE_SOURCE <- substr(REF_SOURCE$SRC_CODE,1,2)



X <- My_Vlookup(X,"ID_SOURCE","LABEL_SOURCE",REF_SOURCE,"CODE_SOURCE",paste("SRC_TEXT_SHORT_",Lang, sep=""))
X <- My_Vlookup(X,"ID_SOURCE","ID_SOURCE",REF_SOURCE,"CODE_SOURCE","CODE_RANK")

rm(REF_SOURCE)
X <- My_Vlookup(X,"CODE_INDICATOR","CODE_RANK_IND",CODE$SUBTOPIC,"SBT_CODE","CODE_RANK")
X <- My_Vlookup(X,"CODE_SEX_VS","CODE_RANK_SEXVS",CODE$CLASS_VS,"CLV_CODE","CODE_RANK")
X <- My_Vlookup(X,"CODE_CL1_VS","CODE_RANK_CLVS",CODE$CLASS_VS,"CLV_CODE","CODE_RANK")
X <- My_Vlookup(X,"CODE_CL2_VS","CODE_RANK_CL2VS",CODE$CLASS_VS,"CLV_CODE","CODE_RANK")
X <- My_Vlookup(X,"CODE_SEX","CODE_RANK_SEX",CODE$CLASS_VS_DETAIL,"CLA_CODE","CODE_RANK")
X <- My_Vlookup(X,"CODE_CL1","CODE_RANK_CL",CODE$CLASS_VS_DETAIL,"CLA_CODE","CODE_RANK")
X <- My_Vlookup(X,"CODE_CL2","CODE_RANK_CL2",CODE$CLASS_VS_DETAIL,"CLA_CODE","CODE_RANK")

X <- My_Vlookup(X,"CODE_COUNTRY","ADD_REGION_GEO",CODE$COUNTRY,"COU_CODE_ISO3",paste("REGION_GEO_",Lang, sep=""))



X <- My_Vlookup(X,"CODE_COUNTRY","LABEL_COUNTRY",CODE$COUNTRY,"COU_CODE_ISO3",paste("COU_TEXT_",Lang, sep=""))
X <- My_Vlookup(X,"CODE_INDICATOR","LABEL_INDICATOR",CODE$SUBTOPIC,"SBT_CODE",paste("SBT_TEXT_",Lang, sep=""))
X <- My_Vlookup(X,"CODE_SEX_VS","LABEL_SEXVS",CODE$CLASS_VS,"CLV_CODE",paste("CLV_TEXT_",Lang, sep=""))
X <- My_Vlookup(X,"CODE_CL1_VS","LABEL_CLASS1",CODE$CLASS_VS,"CLV_CODE",paste("CLV_TEXT_",Lang, sep=""))
X <- My_Vlookup(X,"CODE_CL2_VS","LABEL_CLASS2",CODE$CLASS_VS,"CLV_CODE",paste("CLV_TEXT_",Lang, sep=""))
X <- My_Vlookup(X,"CODE_SEX","LABEL_SEX",CODE$CLASS_VS_DETAIL,"CLA_CODE",paste("CLA_TEXT_",Lang, sep=""))
X <- My_Vlookup(X,"CODE_CL1","LABEL_CLASS_DETAIL",CODE$CLASS_VS_DETAIL,"CLA_CODE",paste("CLA_TEXT_",Lang, sep=""))
X <- My_Vlookup(X,"CODE_CL2","LABEL_CLASS_DETAIL2",CODE$CLASS_VS_DETAIL,"CLA_CODE",paste("CLA_TEXT_",Lang, sep=""))

AND <- ADD_AND[ADD_AND$CODE%in%"AND",paste("TITLE","_",Lang,sep="")]
X[!X$LABEL_SEXVS%in%NA,"LABEL_CLASS"] <- X[!X$LABEL_SEXVS%in%NA,"LABEL_SEXVS"]
X[!X$LABEL_CLASS1%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS"] <- paste(X[!X$LABEL_CLASS1%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS"]," REPLACE ",MY_TOLOWER_TEXT(X[!X$LABEL_CLASS1%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS1"]) ,sep="")
X[X$LABEL_SEXVS%in%NA,"LABEL_CLASS"] <- X[X$LABEL_SEXVS%in%NA,"LABEL_CLASS1"]
X[!X$LABEL_CLASS2%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS"] <- paste(X[!X$LABEL_CLASS2%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS"]," ",AND," ",MY_TOLOWER_TEXT(X[!X$LABEL_CLASS2%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS2"]) ,sep="")
X[!X$LABEL_CLASS2%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS"] <- gsub(" REPLACE ",", ",X[!X$LABEL_CLASS2%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS"])
X[X$LABEL_CLASS2%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS"] <- gsub("REPLACE",AND,X[X$LABEL_CLASS2%in%NA & !X$LABEL_CLASS%in%NA,"LABEL_CLASS"])
rm(AND)



X <- X[order(X$LABEL_COUNTRY,X$ID_SOURCE,X$CODE_SURVEY,X$CODE_RANK_IND,X$CODE_RANK_SEXVS,X$CODE_RANK_CLVS,X$CODE_RANK_CL2VS,X$CODE_RANK_SEX,X$CODE_RANK_CL,X$CODE_RANK_CL2),]
X 	<- X[,!colnames(X)%in%c("CODE_SURVEY","CODE_RANK_IND","LABEL_SEXVS","LABEL_CLASS1","LABEL_CLASS2","CODE_RANK_SEX","CODE_RANK_CL","CODE_RANK_CL2")]




X[!X$OBS_STATUS%in%c("",NA),"OBS_STATUS"] <- substr(X[!X$OBS_STATUS%in%c("",NA),"OBS_STATUS"],1,1)
X <- My_Vlookup(X,"OBS_STATUS","OBS_STATUS",CODE$NOTES,"CODNOTE",paste("TEXT_",Lang, sep=""))
X[!X$TEST_N_BRK_P%in%c("",NA),"TEST_NOTE"] <- substr(X[!X$TEST_N_BRK_P%in%c("",NA),"TEST_N_BRK_P"],7,9)

test <-  substr(X$TEST_NOTE,1,1)%in%"Q" & X$NOTE_FREQUENCY%in%c("P","Q","R","S","T")
X[test,"TEST_NOTE"] <- paste("D",substr(X[test,"TEST_NOTE"],2,3),sep="")
test <-  substr(X$TEST_NOTE,1,1)%in%"Q" & X$NOTE_FREQUENCY%in%c("A","B","C","G","V","W","D","E","F","N","n")
X[test,"TEST_NOTE"] <- paste("E",substr(X[test,"TEST_NOTE"],2,3),sep="")

test <-  substr(X$TEST_NOTE,1,1)%in%"M" & X$NOTE_FREQUENCY%in%c("P","Q","R","S","T")
X[test,"TEST_NOTE"] <- paste("Q",substr(X[test,"TEST_NOTE"],2,3),sep="")
test <-  substr(X$TEST_NOTE,1,1)%in%"M" & X$NOTE_FREQUENCY%in%c("A","B","C","G","V","W","D","E","F","N","n")
X[test,"TEST_NOTE"] <- paste("B",substr(X[test,"TEST_NOTE"],2,3),sep="")
test <-  substr(X$TEST_NOTE,1,1)%in%"M" & X$NOTE_FREQUENCY%in%c("a","b","c","d","e","f","g","h","i","U","j","k")
X[test,"TEST_NOTE"] <- paste("A",substr(X[test,"TEST_NOTE"],2,3),sep="")




test <- X$TEST_NOTE%in%c(NA,"") & !X$TEST_N_BRK_P%in%c("",NA) & X$NOTE_FREQUENCY%in%c("m")
X[test,"TEST_NOTE"] <- "YYYYY"





X <- My_Vlookup(X,"TEST_NOTE","TEST_NOTE",CODE$HELP,"CODNOTE",paste("TEXT_",Lang, sep=""))


X[!X$TEST_NOTE%in%c("",NA),"OBS_STATUS"] <- paste(X[!X$TEST_NOTE%in%c("",NA),"TEST_NOTE"]," ",substr(X[!X$TEST_NOTE%in%c("",NA),"TEST_N_BRK_P"],2,5),", ",X[!X$TEST_NOTE%in%c("",NA),"OBS_STATUS"],sep="")
X$TEST_NOTE <- rep(NA,nrow(X))



X[substr(X$ADD_LChgeDate,1,1)%in%"Y","TEST_NOTE"] <- substr(X[substr(X$ADD_LChgeDate,1,1)%in%"Y","ADD_LChgeDate"],7,9)
X <- My_Vlookup(X,"TEST_NOTE","TEST_NOTE",CODE$HELP,"CODNOTE",paste("TEXT_",Lang, sep=""))
X[!X$TEST_NOTE%in%c("",NA),"ADD_LChgeDate"] <- paste("(",X[!X$TEST_NOTE%in%c("",NA),"TEST_NOTE"]," ",substr(X[!X$TEST_NOTE%in%c("",NA),"ADD_LChgeDate"],4,5),")",sep="")
X$TEST_NOTE <- rep(NA,nrow(X))
X[substr(X$ADD_LChgeDate,1,1)%in%"Y","ADD_LChgeDate"] <- paste("'(",substr(X[substr(X$ADD_LChgeDate,1,1)%in%"Y","ADD_LChgeDate"],2,5),")",sep="")


X[substr(X$ADD_LDataDate,1,1)%in%"Y","TEST_NOTE"] <- substr(X[substr(X$ADD_LDataDate,1,1)%in%"Y","ADD_LDataDate"],7,9)
X <- My_Vlookup(X,"TEST_NOTE","TEST_NOTE",CODE$HELP,"CODNOTE",paste("TEXT_",Lang, sep=""))
X[!X$TEST_NOTE%in%NA,"ADD_LDataDate"] <- paste("(",X[!X$TEST_NOTE%in%NA,"TEST_NOTE"]," ",substr(X[!X$TEST_NOTE%in%NA,"ADD_LDataDate"],4,5),")",sep="")
X$TEST_NOTE <- rep(NA,nrow(X))
X[substr(X$ADD_LDataDate,1,1)%in%"Y","ADD_LDataDate"] <- paste("'(",substr(X[substr(X$ADD_LDataDate,1,1)%in%"Y","ADD_LDataDate"],2,5) ,")",sep="")







##### My_vlookup matricielle pour remplacer les codes "NOTS" par le texte correspondant dans la CODE_Table CODE$NOTES

X <- My_unsplit_NOTE(X,"NOTE_FREQUENCY",CODE$FREQUENCY,Lang)
X <- My_unsplit_NOTE(X,"NOTE_CL1",CODE$NOTE,Lang)
X <- My_unsplit_NOTE(X,"NOTE_INDICATOR",CODE$NOTE,Lang)
X <- My_unsplit_NOTE(X,"NOTE_SOURCE",CODE$NOTE,Lang)







a <- as.data.frame(cbind(TEXT_SBT = levels(as.factor(X$LABEL_INDICATOR)),CODE_RANK = 1:nlevels(as.factor(X$LABEL_INDICATOR)),stringsAsFactors=FALSE))
a <- My_Vlookup(a,"TEXT_SBT","CODE_RANK",CODE$SUBTOPIC,paste("SBT_TEXT_",Lang, sep=""),"CODE_RANK")
a <- a[order(a$CODE_RANK),]
a[a$TEXT_SBT%in%"","CODE_RANK"] <- 999
a[!a$TEXT_SBT%in%"","CODE_RANK"] <- 1:nrow(a[!a$TEXT_SBT%in%"",])

X <- My_Vlookup(X,"LABEL_INDICATOR","CODE_RANK",a,"TEXT_SBT","CODE_RANK")
X[X$CODE_RANK%in%999,"CODE_RANK"] <- ""
X[X$CODE_RANK%in%c(1,2,3,4,5,6,7,8,9),"CODE_RANK"] <- paste("0" , X[X$CODE_RANK%in%c(1,2,3,4,5,6,7,8,9),"CODE_RANK"],sep="")  
X[!X$LABEL_INDICATOR%in%"","LABEL_INDICATOR"] <- paste(X[!X$LABEL_INDICATOR%in%"","CODE_RANK"],". ",X[!X$LABEL_INDICATOR%in%"","LABEL_INDICATOR"],sep="")
rm(a)




a <- as.data.frame(cbind(	TEXT_CLV = levels(as.factor(X$LABEL_CLASS)),
							CODE_RANK = 1:nlevels(as.factor(X$LABEL_CLASS)),
							CODE_RANK_SEXVS = 1:nlevels(as.factor(X$LABEL_CLASS)),
							CODE_RANK_CLVS = 1:nlevels(as.factor(X$LABEL_CLASS)),
							CODE_RANK_CL2VS = 1:nlevels(as.factor(X$LABEL_CLASS))
							),
	
							stringsAsFactors=FALSE)
	

			
a <- My_Vlookup(a,"TEXT_CLV","CODE_RANK_SEXVS",X,"LABEL_CLASS","CODE_RANK_SEXVS")
a <- My_Vlookup(a,"TEXT_CLV","CODE_RANK_CLVS",X,"LABEL_CLASS","CODE_RANK_CLVS")
a <- My_Vlookup(a,"TEXT_CLV","CODE_RANK_CL2VS",X,"LABEL_CLASS","CODE_RANK_CL2VS")
a$CODE_RANK <- paste0(a$CODE_RANK_SEXVS,a$CODE_RANK_CLVS,a$CODE_RANK_CL2VS)
							


a <- a[order(a$CODE_RANK),]
a[a$TEXT_CLV%in%"","CODE_RANK"] <- 999
a[!a$TEXT_CLV%in%"","CODE_RANK"] <- 1:nrow(a[!a$TEXT_CLV%in%"",])

X <- My_Vlookup(X,"LABEL_CLASS","CODE_RANK",a,"TEXT_CLV","CODE_RANK")
X[X$CODE_RANK%in%999,"CODE_RANK"] <- ""
X[X$CODE_RANK%in%c(1,2,3,4,5,6,7,8,9),"CODE_RANK"] <- paste("0" , X[X$CODE_RANK%in%c(1,2,3,4,5,6,7,8,9),"CODE_RANK"],sep="")  
X[!X$LABEL_CLASS%in%"","LABEL_CLASS"] <- paste(X[!X$LABEL_CLASS%in%"","CODE_RANK"],". ",X[!X$LABEL_CLASS%in%"","LABEL_CLASS"],sep="")



#X[substr(X$CODE_SEX,1,3)%in%"SEX","LABEL_CLASS"] <- paste(X[substr(X$CODE_SEX,1,3)%in%"SEX" ,"LABEL_CLASS"],"&",rep(CODE$CLASS[match("SEX", CODE$CLASS[,"CLY_CODE"]),paste("CLY_TEXT_",Lang, sep="")],length(X[substr(X$CODE_SEX,1,3)%in%"SEX","LABEL_CLASS"])),sep=" ")
#X$LABEL_CLASS <- gsub("NO & ","",X$LABEL_CLASS)

# X[substr(X$CODE_SEX,1,3)%in%"SEX","LABEL_CLASS"] <- 	paste(
														# substr(X[substr(X$CODE_SEX,1,3)%in%"SEX","LABEL_CLASS"],1,3),
														# rep(CODE$CLASS[match("SEX", CODE$CLASS[,"CLY_CODE"]),paste("CLY_TEXT_",Lang, sep="")],length(X[substr(X$CODE_SEX,1,3)%in%"SEX","LABEL_CLASS"])),
														# "&",	
														# substr(X[substr(X$CODE_SEX,1,3)%in%"SEX" ,"LABEL_CLASS"],5,length(X[substr(X$CODE_SEX,1,3)%in%"SEX","LABEL_CLASS"])),
														# sep=" ")
# X$LABEL_CLASS <- gsub("& NO","",X$LABEL_CLASS)


N_Year <- colnames(X)[substr(colnames(X),6,6)%in%"" & substr(colnames(X),1,1)%in%"Y"]
N_Quarter <- colnames(X)[substr(colnames(X),6,7)%in%"_Q"]
N_Month <- colnames(X)[substr(colnames(X),6,7)%in%"_M"]






ref <- "blabla"
for (i in 1:length(N_Year)){
if(nlevels(as.factor(X[,N_Year[i]]))%in%0){
ref <- c(ref,N_Year[i])
}
}
for (i in 1:length(N_Quarter)){
if(nlevels(as.factor(X[,N_Quarter[i]]))%in%0){
ref <- c(ref,N_Quarter[i])
}
}
for (i in 1:length(N_Month)){
if(nlevels(as.factor(X[,N_Month[i]]))%in%0){
ref <- c(ref,N_Month[i])
}
}








############### check and complete ADD in
X <- My_Vlookup(X,"CODE_COUNTRY","NOTE_CURRENCY",CODE$COUNTRY,"COU_CODE_ISO3","CODE_CURRENCY")
X <- My_Vlookup(X,"CODE_CL1","ADD_ISIC3",CODE$CLASS_VS_DETAIL,"CLA_CODE","ISIC3")
X[!substr(X$CODE_INDICATOR,1,3)%in%"EAR","NOTE_CURRENCY"] <- NA

X[substr(X$CODE_INDICATOR,1,3)%in%"CPI","NOTE_INDICATOR"] <- X[substr(X$CODE_INDICATOR,1,3)%in%"CPI","NOTE_FREE_TEXT"] 
X[substr(X$CODE_INDICATOR,1,3)%in%"CPI","NOTE_FREE_TEXT"] <- NA
ref <- c(EN = "Currency unit: ",FR = "Unité monétaire: ", SP="Unidad monetaria: " )

if(length(X[substr(X$CODE_INDICATOR,1,3)%in%"EAR" & X$NOTE_INDICATOR%in%NA,"NOTE_INDICATOR"])>0){
X[substr(X$CODE_INDICATOR,1,3)%in%"EAR" & X$NOTE_INDICATOR%in%NA,"NOTE_INDICATOR"] <- paste(ref[Lang],X[substr(X$CODE_INDICATOR,1,3)%in%"EAR" & X$NOTE_INDICATOR%in%NA,"NOTE_CURRENCY"],sep="")}

if(length(X[substr(X$CODE_INDICATOR,1,3)%in%"EAR" & !X$NOTE_INDICATOR%in%NA,"NOTE_INDICATOR"])>0){
X[substr(X$CODE_INDICATOR,1,3)%in%"EAR" & !X$NOTE_INDICATOR%in%NA,"NOTE_INDICATOR"] <- paste(X[substr(X$CODE_INDICATOR,1,3)%in%"EAR" & !X$NOTE_INDICATOR%in%NA,"NOTE_INDICATOR"]," / ",ref[Lang],X[substr(X$CODE_INDICATOR,1,3)%in%"EAR" & !X$NOTE_INDICATOR%in%NA,"NOTE_CURRENCY"],sep="")}



X[substr(X$CODE_INDICATOR,10,11)%in%"RT" & !X$ADD_LData%in%c("",NA),"ADD_LDataUnit"] <- "%" 
X[substr(X$CODE_INDICATOR,10,11)%in%"NB" & !X$ADD_LData%in%c("",NA),"ADD_LDataUnit"] <- "'000" 
X[substr(X$CODE_INDICATOR,10,11)%in%"RT" & !X$ADD_LChge%in%c("",NA),"ADD_LChgeUnit"] <- "pp" 
X[!substr(X$CODE_INDICATOR,10,11)%in%"RT" & !X$ADD_LChge%in%c("",NA),"ADD_LChgeUnit"] <- "%" 

X 	<- X[,!colnames(X)%in%c("CODE_RANK","CODE_RANK_SEXVS","CODE_RANK_CLVS","CODE_RANK_CL2VS","CODE_SEX_VS","CODE_CL1_VS","CODE_CL2_VS","TEST_N_BRK_P","NOTE_CURRENCY","TEST_NOTE","TEST_OTH1","TEST_OTH2","TEST_AGE","TEST_CPI_BASE",ref,"ID_SOURCE")]









return(X)

}
 
MY_TOLOWER_TEXT <- function(X){
X <- paste(tolower(substr(X,1,1)),substr(X,2,nchar(X)),sep="")
return(X)
}

My_Header <- function(DB,Lang){

#DB <- X

N_Year <- colnames(DB)[substr(colnames(DB),6,6)%in%"" & substr(colnames(DB),1,1)%in%"Y"]
N_Quarter <- colnames(DB)[substr(colnames(DB),6,7)%in%"_Q"]
N_Month <- colnames(DB)[substr(colnames(DB),6,7)%in%"_M"]



DEF <- as.data.frame(cbind(	CODE = colnames(DB), 
							STATUS = colnames(DB), 
							SIZE = colnames(DB), 
							HEADER1 = rep("",ncol(DB)),
							HEADER2 = colnames(DB))						
							,stringsAsFactors=FALSE)
							

DEF[DEF$CODE%in%N_Quarter,"HEADER2"] <- substr(N_Quarter,7,9)
DEF[DEF$CODE%in%N_Month,"HEADER2"] <- substr(N_Month,7,9)
DEF[,"STATUS"] 	<- CODE$HEADER[match(DEF[,"HEADER2"], CODE$HEADER[,"CODNOTE"]),"STATUS"]
DEF[,"SIZE"] 	<- CODE$HEADER[match(DEF[,"HEADER2"], CODE$HEADER[,"CODNOTE"]),"SIZE"]





ref_year <- length(N_Year)								# Min 3 years


if(length(N_Year)>2){
DEF[DEF$CODE%in%N_Year[length(N_Year)-2],"HEADER1"] <- "YEARLY_AVERAGE"
DEF[DEF$CODE%in%N_Year[1:(length(N_Year)-3)],"STATUS"] <- "HIDE"
} else{
DEF[DEF$CODE%in%N_Year[1],"HEADER1"] <- "YEARLY_AVERAGE"
}
DEF[DEF$CODE%in%N_Year,"SIZE"] <- 5.14



ref_quarter <- length(N_Quarter)						# Min 5 quarters


if(length(N_Quarter)>4){
DEF[DEF$CODE%in%N_Quarter[length(N_Quarter)-4],"HEADER1"] <- "QUARTERLY_AVERAGE"
DEF[DEF$CODE%in%N_Quarter[1:(length(N_Quarter)-5)],"STATUS"] <- "HIDE"
} else{
DEF[DEF$CODE%in%N_Quarter[1],"HEADER1"] <- "QUARTERLY_AVERAGE"
}


if(length(N_Month)>9){
DEF[DEF$CODE%in%N_Month[1:(length(N_Month)-10)],"STATUS"] <- "HIDE"	# Min 10 months
DEF[,"HEADER1"] <- CODE$HEADER[match(DEF[,"HEADER1"], CODE$HEADER[,"CODNOTE"]),paste("TEXT_",Lang, sep="")]
DEF[DEF$CODE%in%N_Month & DEF$HEADER2%in%"M01","HEADER1"] <- substr(DEF[DEF$CODE%in%N_Month & DEF$HEADER2%in%"M01","CODE"],2,5)
DEF[DEF$CODE%in%N_Month[length(N_Month)-9],"HEADER1"] <- substr(DEF[DEF$CODE%in%N_Month[length(N_Month)-9],"CODE"],2,5)
} else{
DEF[,"HEADER1"] <- CODE$HEADER[match(DEF[,"HEADER1"], CODE$HEADER[,"CODNOTE"]),paste("TEXT_",Lang, sep="")]
DEF[DEF$CODE%in%N_Month & DEF$HEADER2%in%"M01","HEADER1"] <- substr(DEF[DEF$CODE%in%N_Month & DEF$HEADER2%in%"M01","CODE"],2,5)
DEF[DEF$CODE%in%N_Month[1],"HEADER1"] <- substr(DEF[DEF$CODE%in%N_Month[1],"CODE"],2,5)
}



DEF[,"HEADER2"] <- CODE$HEADER[match(DEF[,"HEADER2"], CODE$HEADER[,"CODNOTE"]),paste("TEXT_",Lang, sep="")]

DEF[DEF$CODE%in%N_Quarter,"HEADER2"] <- paste(DEF[DEF$CODE%in%N_Quarter,"HEADER2"],substr(N_Quarter,4,5),sep=" ")

DEF[DEF$CODE%in%N_Year,"HEADER2"] <- substr(N_Year,2,5)




DEF <- as.data.frame(t(cbind(	TITLE="",
							DEF,							
							T2 = "",
							B0 = "",
							B1 = "",
							B2 = "",
							B3 = "",
							B4 = "",
							B5 = "",stringsAsFactors=FALSE)),stringsAsFactors=FALSE)

colnames(DEF) <- DEF["CODE",] 


							
DEF["T2","LABEL_COUNTRY"] <- CODE$HEADER[CODE$HEADER[,"CODNOTE"]%in%"T2",paste("TEXT_",Lang, sep="")]
DEF["B1","LABEL_COUNTRY"] <- CODE$HEADER[CODE$HEADER[,"CODNOTE"]%in%"B1",paste("TEXT_",Lang, sep="")]
DEF["B2","LABEL_COUNTRY"] <- CODE$HEADER[CODE$HEADER[,"CODNOTE"]%in%"B2",paste("TEXT_",Lang, sep="")]
DEF["B3","LABEL_COUNTRY"] <- CODE$HEADER[CODE$HEADER[,"CODNOTE"]%in%"B3",paste("TEXT_",Lang, sep="")]
DEF["B4","LABEL_COUNTRY"] <- CODE$HEADER[CODE$HEADER[,"CODNOTE"]%in%"B4",paste("TEXT_",Lang, sep="")]
DEF["B5","LABEL_COUNTRY"] <- CODE$HEADER[CODE$HEADER[,"CODNOTE"]%in%"B5",paste("TEXT_",Lang, sep="")]
								

							
								
								
								
								
								
								
								
								
return(DEF)
}

My_delete_empty_columns <- function(DB){

# DB <- X









N_Year <- colnames(DB)[!substr(colnames(DB),7,7)%in%c("Q","M") & substr(colnames(DB),1,1)%in%"Y"]
N_Quarter <- colnames(DB)[substr(colnames(DB),7,7)%in%"Q" & substr(colnames(DB),1,1)%in%"Y"]
N_Month <- colnames(DB)[substr(colnames(DB),7,7)%in%"M" & substr(colnames(DB),1,1)%in%"Y"]



i <- length(N_Month)+1
while (i!=1){
i <- i -1
test <- levels(as.factor(DB[,N_Month[i]]))
test <- test[!test%in%c(NA,"","NA")]
ifelse(!length(test) %in% 0, i <-1,DB <- DB[,!colnames(DB)%in%N_Month[i]])
}


i <- length(N_Quarter)+1
while (i!=1){
i <- i -1
test <- levels(as.factor(DB[,N_Quarter[i]]))
test <- test[!test%in%c(NA,"","NA")]
ifelse(!length(test) %in% 0, i <-1,DB <- DB[,!colnames(DB)%in%N_Quarter[i]])
}



i <- length(N_Year)+1
while (i!=1){
i <- i -1
test <- levels(as.factor(DB[,N_Year[i]]))
test <- test[!test%in%c(NA,"","NA")]
ifelse(!length(test) %in% 0, i <-1,DB <- DB[,!colnames(DB)%in%N_Year[i]])
}



N_Year <- colnames(DB)[!substr(colnames(DB),7,7)%in%c("Q","M") & substr(colnames(DB),1,1)%in%"Y"]
N_Quarter <- colnames(DB)[substr(colnames(DB),7,7)%in%"Q" & substr(colnames(DB),1,1)%in%"Y"]
N_Month <- colnames(DB)[substr(colnames(DB),7,7)%in%"M" & substr(colnames(DB),1,1)%in%"Y"]




i <- 0
while (i!=length(N_Month)){
i <- i + 1
if(length(N_Month)%in%0){
i <-length(N_Month)
} else{
test <- levels(as.factor(DB[,N_Month[i]]))
test <- test[!test%in%c(NA,"","NA")]
ifelse(!length(test) %in% 0, i <-length(N_Month),DB <- DB[,!colnames(DB)%in%N_Month[i]])
}
}


i <- 0
while (i!=length(N_Quarter)){
i <- i + 1
if(length(N_Quarter)%in%0){
i <-length(N_Quarter)
} else{
test <- levels(as.factor(DB[,N_Quarter[i]]))
test <- test[!test%in%c(NA,"","NA")]
ifelse(!length(test) %in% 0, i <-length(N_Quarter),DB <- DB[,!colnames(DB)%in%N_Quarter[i]])
}
}

i <- 0
while (i!=length(N_Year)){
i <- i + 1
if(length(N_Year)%in%0){
i <-length(N_Year)
} else{
test <- levels(as.factor(DB[,N_Year[i]]))
test <- test[!test%in%c(NA,"","NA")]
ifelse(!length(test) %in% 0, i <-length(N_Year),DB <- DB[,!colnames(DB)%in%N_Year[i]])
}
}








return(DB)

}



My_Estimation_Weighted_Median <- function(X,INFO,N_Weigth,N_CAT,Weight_CAT){                        
GLOBAL <- 1:(ncol(X[[1]]) - INFO)   

for (i in 1:length(X)){
Y <- X[[i]]

CAT <- levels(as.factor(Y[,N_CAT]))
res    <- 1:(ncol(Y) - INFO)               # Init
    for (j in 1:2){                         # Boucle sur les Type
        Wmed     <- 1
        Wcount      <- 1
        for (k in 1:(ncol(Y) - INFO)){  	# Boucle sur les mois
            mois <- INFO + k
            Z <- Y[Y[,N_CAT] %in% CAT[j],]    # Selection du Type
            Z <- Z[order(Z[,mois]),]       	# Tri par le mois de reference
            Z <- Z[is.finite(Z[,mois]),]   	# SupPsion des valeurs manquantes
            ref <- sum(Z[,N_Weigth])/2      # Mediane des poids
            cum <- cumsum(Z[,N_Weigth])     # Poids cumulés
            b1 <-length(cum[cum<ref])       # Borne inférieur de la médiane
            ifelse(b1 %in% 0,med <- Z[1,mois],med <- Z[b1+1,mois])                 # Attention borne inf peut etre 0
            Wmed <- c(Wmed,med)
            Wcount <- c(Wcount, nrow(Z))    # Compteur
        }
        res <- cbind(res, Wcount[-1],Wmed[-1])
    }
    resultat <- cbind(res[,3],res[,5],(res[,3]*Weight_CAT[1]/100 + res[,5]*Weight_CAT[2]/100),res[,2],res[,4],(res[,2]+res[,4]))

GLOBAL <- rbind(GLOBAL,t(resultat))
}
GLOBAL[1,] <- colnames(X[[1]][(INFO + 1):ncol(X[[1]])])
rownames(GLOBAL)<-c("Months", rep(c("Developed countries","Developing countries", "World", "Developed countries Count","Developing countries Count", "World Count"),length(X)))

return(GLOBAL) 
}

My_Global_Change_Create <- function(X){							 

# X <- Base_DELTA
X <- X[X$CODE_CL2%in%"XXX_XXX_XXX",]

X <- X[!substr(X$CODE_INDICATOR,8,8)%in%"1",] # selected NSA
X <- X[X$ADD_SELECTION%in%"Y",]





X <- as.data.frame(cbind(			CODE_COUNTRY 	= 	X$CODE_COUNTRY,
					TOPIC 		= 	paste(X$CODE_INDICATOR,X$CODE_CL1,sep="/"),
					REGION_DEV	= 	NA,
					COUNTRY		= 	NA,
					CODE_SEX	= 	X$CODE_SEX,
					EAP			=	NA,
					X[,substr(colnames(X),7,7)%in%"Q"]),
stringsAsFactors=FALSE)

X <- X[,!substr(colnames(X),2,5)%in%c("2000")]	 



ref <- colnames(X[,substr(colnames(X),7,7)%in%"Q"])




for (i in 1:length(ref)){
X[,ref[i]] <- gsub("b"," ",X[,ref[i]]) 
X[,ref[i]] <- gsub("e"," ",X[,ref[i]]) 
X[,ref[i]] <- gsub(" ","",X[,ref[i]]) 
X[,ref[i]] <- as.numeric(X[,ref[i]]) 
}





X <- X[!X$CODE_COUNTRY%in%c("CHN","IND"),] # delete China India

META <- as.data.frame(read.csv(paste(MY_Chem,"Processing/ALL_META/STI_CODE_EAP_WEIGHT.csv",sep=""),na.strings = "",stringsAsFactors =FALSE))

X <- My_Vlookup(X,"CODE_COUNTRY","REGION_DEV",META,"COU_CODE_ISO3","REGION_DEV_EN")
X <- My_Vlookup(X,"CODE_COUNTRY","COUNTRY",CODE$COUNTRY,"COU_CODE_ISO3","COU_TEXT_EN")
X <- My_Vlookup(X,"CODE_COUNTRY","EAP",META,"COU_CODE_ISO3","WEIGHTS")



X <- X[,!colnames(X)%in%"CODE_COUNTRY"]


                              
UNER_TOT_T		<- X[X$TOPIC%in%"UNE_DEAP_RT/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_T",]			# Unemployment rate
UNER_TOT_M  	<- X[X$TOPIC%in%"UNE_DEAP_RT/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_M",]				# Male
UNER_TOT_F  	<- X[X$TOPIC%in%"UNE_DEAP_RT/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_F",]				# Female
UNE_TOT_T		<- X[X$TOPIC%in%"UNE_TUNE_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_T",]			# Unemployment
UNE_TOT_M		<- X[X$TOPIC%in%"UNE_TUNE_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_M",]				# Male
UNE_TOT_F		<- X[X$TOPIC%in%"UNE_TUNE_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_F",]				# Female
EMP_TOT_T		<- X[X$TOPIC%in%"EMP_TEMP_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_T",]			# Employment
EMP_TOT_M		<- X[X$TOPIC%in%"EMP_TEMP_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_M",]				# Male
EMP_TOT_F		<- X[X$TOPIC%in%"EMP_TEMP_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_F",]				# Female
EMP_TOT_NAG_T	<- X[X$TOPIC%in%"EMP_TEMP_NB/ECO_SECTOR_NAG" & X$CODE_SEX%in%"SEX_T",]			#  - in non agricultural activities	
EMP_TOT_NAG_M	<- X[X$TOPIC%in%"EMP_TEMP_NB/ECO_SECTOR_NAG" & X$CODE_SEX%in%"SEX_M",]				# Male
EMP_TOT_NAG_F	<- X[X$TOPIC%in%"EMP_TEMP_NB/ECO_SECTOR_NAG" & X$CODE_SEX%in%"SEX_F",]				# Female
EMP_TOT_MAN_T	<- X[X$TOPIC%in%"EMP_TEMP_NB/ECO_AGGREGATE_MAN" & X$CODE_SEX%in%"SEX_T",]			#  - in manufacturing
EMP_TOT_MAN_M	<- X[X$TOPIC%in%"EMP_TEMP_NB/ECO_AGGREGATE_MAN" & X$CODE_SEX%in%"SEX_M",]				# Male
EMP_TOT_MAN_F	<- X[X$TOPIC%in%"EMP_TEMP_NB/ECO_AGGREGATE_MAN" & X$CODE_SEX%in%"SEX_F",]				# Female
EMP_PAID_T		<- X[X$TOPIC%in%"EES_TEES_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_T",]			# Employees
EMP_PAID_M		<- X[X$TOPIC%in%"EES_TEES_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_M",]				# Male
EMP_PAID_F		<- X[X$TOPIC%in%"EES_TEES_NB/XXX_XXX_XXX" & X$CODE_SEX%in%"SEX_F",]				# Female
EMP_PAID_NAG_T	<- X[X$TOPIC%in%"EES_TEES_NB/ECO_SECTOR_NAG" & X$CODE_SEX%in%"SEX_T",]			#  - in non agricultural activities	
EMP_PAID_NAG_M	<- X[X$TOPIC%in%"EES_TEES_NB/ECO_SECTOR_NAG" & X$CODE_SEX%in%"SEX_M",]				# Male
EMP_PAID_NAG_F	<- X[X$TOPIC%in%"EES_TEES_NB/ECO_SECTOR_NAG" & X$CODE_SEX%in%"SEX_F",]				# Female
EMP_PAID_MAN_T	<- X[X$TOPIC%in%"EES_TEES_NB/ECO_AGGREGATE_MAN" & X$CODE_SEX%in%"SEX_T",]			#  - in manufacturing
EMP_PAID_MAN_M	<- X[X$TOPIC%in%"EES_TEES_NB/ECO_AGGREGATE_MAN" & X$CODE_SEX%in%"SEX_M",]				# Male
EMP_PAID_MAN_F	<- X[X$TOPIC%in%"EES_TEES_NB/ECO_AGGREGATE_MAN" & X$CODE_SEX%in%"SEX_F",]				# Female
# creation du liste de multiple data frame
DB <- list(UNER_TOT_T,UNER_TOT_M,UNER_TOT_F, UNE_TOT_T,UNE_TOT_M,UNE_TOT_F, EMP_TOT_T,EMP_TOT_M,EMP_TOT_F, EMP_TOT_NAG_T,EMP_TOT_NAG_M,EMP_TOT_NAG_F, EMP_TOT_MAN_T,EMP_TOT_MAN_M,EMP_TOT_MAN_F, EMP_PAID_T,EMP_PAID_M,EMP_PAID_F, EMP_PAID_NAG_T,EMP_PAID_NAG_M,EMP_PAID_NAG_F, EMP_PAID_MAN_T,EMP_PAID_MAN_M,EMP_PAID_MAN_F) #   CHOMAGEMale, CHOMAGEFemale

INFO <- 5												# nombre de metadata
W_DEV  <- c(16.1793385808659,83.8206614191341)			# poids de chaque region 2010
GLOBAL <- My_Estimation_Weighted_Median(DB,INFO,"EAP","REGION_DEV",W_DEV)

#GLOBAL <- GLOBAL[,37:ncol(GLOBAL)]





write.csv(GLOBAL, paste(MY_Chem,"Processing/_MIG_STI/ON_STI_TS_Rdata/Global_Change_Data.csv",sep=""),quote = TRUE,fileEncoding  = "UTF-8",row.names = FALSE,na="")

}

My_Map_Data_Prepare <- function(DATA,DELTA){		

# 
# DATA <- Base ; DELTA <- Base_DELTA

#library(dataframes2xls) 
library(WriteXLS)

DATA <- DATA[DATA$CODE_CL2%in%"XXX_XXX_XXX",]
DELTA <- DELTA[DELTA$CODE_CL2%in%"XXX_XXX_XXX",]

DATA <- DATA[!substr(DATA$CODE_INDICATOR,8,8)%in%"1",] # selected NSA
DELTA <- DELTA[!substr(DATA$CODE_INDICATOR,8,8)%in%"1",] # selected NSA


						# 
X <- as.data.frame(rbind(		cbind(
					TOPIC 		= 	paste(DATA$CODE_INDICATOR,DATA$CODE_CL1,sep="/"),
					country 	= 	DATA$CODE_COUNTRY,
					TOGGLE1		= 	substr(DATA$CODE_SEX,5,5),
					TOGGLE2		= 	rep("L",nrow(DATA)),
									DATA[,substr(colnames(DATA),7,7)%in%"M"],
					N_SLT		=	DATA$ADD_SELECTION,stringsAsFactors=FALSE),
					
					cbind(
					TOPIC 		= 	paste(DELTA$CODE_INDICATOR,DELTA$CODE_CL1,sep="/"),
					country 	= 	DELTA$CODE_COUNTRY,
					TOGGLE1		= 	substr(DELTA$CODE_SEX,5,5),
					TOGGLE2		= 	rep("O",nrow(DELTA)),
									DELTA[,substr(colnames(DELTA),7,7)%in%"M"],
					N_SLT		=	DELTA$ADD_SELECTION,stringsAsFactors=FALSE)
					))

X <- My_delete_empty_columns(X)
Y <- X[X$country%in%"USA",]			
Y <- My_delete_empty_columns(Y)
test <- colnames(Y)
X <- X[,test]
		
			
X <- X[!X$country%in%"EUM",] # delete Eursotat

X <- X[X$N_SLT%in%"Y",]
X <- X[,!colnames(X)%in%"N_SLT"]

X[X$country%in%"PSE","country"] <- "WBG"  # ok
X[X$country%in%"KOS","country"] <- "UVK"  # ok
X[X$country%in%"AND","country"] <- "ADO"  # ok

X[X$country%in%"IMN","country"] <- "IMY"



X <- X[,!substr(colnames(X),2,5)%in%c("2000","2001","2002","2003","2004","2005","2006")]


N_Month <- colnames(X)[substr(colnames(X),6,7)%in%"_M"]
	
for (i in 1:length(N_Month)){
X[,N_Month[i]] <- gsub(" b","",X[,N_Month[i]]) 
X[,N_Month[i]] <- gsub(" d","",X[,N_Month[i]])
X[,N_Month[i]] <- gsub(" e","",X[,N_Month[i]]) 
X[,N_Month[i]] <- gsub(" i","",X[,N_Month[i]]) 
#X[,N_Month[i]] <- gsub(" ","",X[,N_Month[i]]) 
X[,N_Month[i]] <- as.numeric(X[,N_Month[i]]) 
}


X[,N_Month] <- My_Round_Matrice(X[,N_Month],1)



X <- X[!X$country%in%c("CHN","IND"),] # delete China India






# INDICATORS
LFP_TOT_RT <- X[X$TOPIC%in%"EAP_DWAP_RT/XXX_XXX_XXX",-1]		# Activity rate
LFP_YTH_RT <- X[X$TOPIC%in%"EAP_DWAP_RT/AGE_AGGREGATE_Y15-24",-1]		#  - youth 


ETP_TOT_RT <- X[X$TOPIC%in%"EMP_DWAP_RT/XXX_XXX_XXX",-1]		# Employment rate
ETP_YTH_RT <- X[X$TOPIC%in%"EMP_DWAP_RT/AGE_AGGREGATE_Y15-24",-1]		#  - youth 



ETE_TOT_RT <- X[X$TOPIC%in%"EES_DEMP_RT/XXX_XXX_XXX",-1]		# Share of employees rate

UNE_TOT_RT <- X[X$TOPIC%in%"UNE_DEAP_RT/XXX_XXX_XXX",-1]		# Unemployment rate
UNE_YTH_RT <- X[X$TOPIC%in%"UNE_DEAP_RT/AGE_AGGREGATE_Y15-24",-1] 	#  - youth 



INA_TOT_RT <- X[X$TOPIC%in%"EIP_DWAP_RT/XXX_XXX_XXX",-1]		# Inactivity rate
INA_YTH_RT <- X[X$TOPIC%in%"EIP_DWAP_RT/AGE_AGGREGATE_Y15-24",-1]		#  - youth 



# STATITICS
WAP_TOT_NB <- X[X$TOPIC%in%"POP_XWAP_NB/XXX_XXX_XXX",-1] 		# Working age population
WAP_YTH_NB <- X[X$TOPIC%in%"POP_XWAP_NB/AGE_AGGREGATE_Y15-24",-1]		#  - youth 


EAP_TOT_NB <- X[X$TOPIC%in%"EAP_TEAP_NB/XXX_XXX_XXX",-1] 		# Economically active population
EAP_YTH_NB <- X[X$TOPIC%in%"EAP_TEAP_NB/AGE_AGGREGATE_Y15-24",-1]		#  - youth 



EMP_TOT_NB <- X[X$TOPIC%in%"EMP_TEMP_NB/XXX_XXX_XXX",-1] 	# Employment
EMP_YTH_NB <- X[X$TOPIC%in%"EMP_TEMP_NB/AGE_AGGREGATE_Y15-24",-1]		#  - youth 



EMP_NAG_NB <- X[X$TOPIC%in%"EMP_TEMP_NB/ECO_SECTOR_NAG",-1] 	#  - in non agricultural activities	
EMP_MAN_NB <- X[X$TOPIC%in%"EMP_TEMP_NB/ECO_AGGREGATE_MAN",-1] 	#  - in manufacturing

EES_TOT_NB <- X[X$TOPIC%in%"EES_TEES_NB/XXX_XXX_XXX",-1] 	# Employees	
EES_NAG_NB <- X[X$TOPIC%in%"EES_TEES_NB/ECO_SECTOR_NAG",-1] 	#  - in non agricultural activities	
EES_MAN_NB <- X[X$TOPIC%in%"EES_TEES_NB/ECO_AGGREGATE_MAN",-1] 	#  - in manufacturing

UNE_TOT_NB <- X[X$TOPIC%in%"UNE_TUNE_NB/XXX_XXX_XXX",-1]		# Unemployment
UNE_YTH_NB <- X[X$TOPIC%in%"UNE_TUNE_NB/AGE_AGGREGATE_Y15-24",-1] 	#  - youth 


EIP_TOT_NB <- X[X$TOPIC%in%"EIP_TEIP_NB/XXX_XXX_XXX",-1] 		# Economically inactive population
EIP_YTH_NB <- X[X$TOPIC%in%"EIP_TEIP_NB/AGE_AGGREGATE_Y15-24",-1]		#  - youth 




# impression dans un fichier Excel
NameFile <- paste(MY_Chem,"Dissemination/MAP/ILO_data_INDICATOR1.xls",sep="")
ref <- c("LFP_TOT_RT","LFP_YTH_RT","ETP_TOT_RT","ETP_YTH_RT","ETE_TOT_RT")
WriteXLS(ref,NameFile,ref,perl = perlpath)


NameFile <- paste(MY_Chem,"Dissemination/MAP/ILO_data_INDICATOR2.xls",sep="")
ref <- c("UNE_TOT_RT","UNE_YTH_RT","INA_TOT_RT","INA_YTH_RT")
WriteXLS(ref,NameFile,ref,perl = perlpath)

NameFile <- paste(MY_Chem,"Dissemination/MAP/ILO_data_STATISTIC1.xls",sep="")
ref <- c("WAP_TOT_NB","WAP_YTH_NB","EAP_TOT_NB","EAP_YTH_NB","EMP_TOT_NB","EMP_YTH_NB","EMP_NAG_NB")
WriteXLS(ref,NameFile,ref,perl = perlpath)


NameFile <- paste(MY_Chem,"Dissemination/MAP/ILO_data_STATISTIC2.xls",sep="")
ref <- c("EMP_MAN_NB","EES_TOT_NB","EES_NAG_NB","EES_MAN_NB","UNE_TOT_NB","UNE_YTH_NB","EIP_TOT_NB","EIP_YTH_NB")
WriteXLS(ref,NameFile,ref,perl = perlpath)

}

My_prepare_ILO_SA <- function(X){   	# OK Jan 2014 only CL2 = XXX

# X <- Base


Lang <- "EN"
# Vlookup part
X <- X[X$CODE_INDICATOR%in%c("POP_XWAP_NB","EAP_TEAP_NB","EMP_TEMP_NB","EES_TEES_NB","UNE_TUNE_NB","UNE_DEAP_RT") & X$CODE_CL1%in%c("XXX_XXX_XXX","AGE_AGGREGATE_Y15-24","AGE_AGGREGATE_Y25-54","AGE_AGGREGATE_Y55-64","AGE_YTHADULT_Y15-64","ECO_SECTOR_AGR","ECO_SECTOR_IND","ECO_AGGREGATE_MAN","ECO_AGGREGATE_CON","ECO_SECTOR_SER")  & X$CODE_CL2%in%c("XXX_XXX_XXX") & !substr(X$CODE_INDICATOR,8,8)%in%"1",]


X <- My_Vlookup(X,"CODE_COUNTRY","REF_OECD",CODE$COUNTRY,"COU_CODE_ISO3","SA")

X <- X[!X$REF_OECD%in%1,] # delete OECD as yet SA data
X <- X[,colnames(X)[!colnames(X)%in%"REF_OECD"]] # delete test oecd columns
X <- X[substr(X$CODE_SOURCE,1,2)%in%"BA",]
X <- X[substr(X$ADD_LDataDate,6,6)%in%"_",] # delete yearly data
tt <- c("E","F","a","b","c","d","e","f","g","h","i","U","j","k","m")

X <- X[!X$NOTE_FREQUENCY%in%tt,]



MY_META <- as.data.frame(cbind(ID = X$KEY,
									OBS_STATUS = X$OBS_STATUS,
									NOTES_FREQUENCY_CODE = X$NOTE_FREQUENCY,
									NOTES_CLASSIF_CODE = X$NOTE_CL1,
									NOTES_INDICATOR_CODE = X$NOTE_INDICATOR,
									NOTES_SOURCE_CODE = X$NOTE_SOURCE),stringsAsFactors=FALSE)

MY_META$ID <- gsub("/XXX_XXX_XXX","",MY_META$ID) 

write.csv(MY_META,paste(MY_Chem,"Collection/REP_ILO/Input_SA/MY_META.csv",sep=""),row.names = FALSE,na = "")
rm(MY_META)

#X[X$CODE_CL1%in%"XXX_XXX_XXX","CODE_CL1"] <- ""

Y <- My_prepare_TEXT(X,"EN")
Quarterly_INFO <- as.data.frame(cbind(ID = Y$KEY,
										Y[,c("OBS_STATUS","NOTE_FREQUENCY","NOTE_CL1","NOTE_INDICATOR","NOTE_SOURCE")],stringsAsFactors=FALSE))

Quarterly_INFO$ID <- gsub("/XXX_XXX_XXX","",Quarterly_INFO$ID) 		
colnames(Quarterly_INFO)[colnames(Quarterly_INFO)%in%"NOTE_FREQUENCY"] <- "NOTES_FREQUENCY_CODE"
colnames(Quarterly_INFO)[colnames(Quarterly_INFO)%in%"NOTE_CL1"] <- "NOTES_CLASSIF_CODE"
colnames(Quarterly_INFO)[colnames(Quarterly_INFO)%in%"NOTE_INDICATOR"] <- "NOTES_INDICATOR_CODE"
colnames(Quarterly_INFO)[colnames(Quarterly_INFO)%in%"NOTE_SOURCE"] <- "NOTES_SOURCE_CODE"

colnames(Quarterly_INFO)[1] <- "COLLECTION_CODE/COUNTRY_CODE/SOURCE_CODE/INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE"		

write.csv(Quarterly_INFO,paste(MY_Chem,"Collection/REP_ILO/Input_SA/Quarterly_INFO.csv",sep=""),row.names = FALSE,na = "")
rm(Y)


X <- My_delete_empty_columns(X)

N_Quarter <- colnames(X)[substr(colnames(X),6,7)%in%"_Q"]
N_Year <- colnames(X)[substr(colnames(X),6,6)%in%"" & substr(colnames(X),1,1)%in%"Y"]
N_Month <- colnames(X)[substr(colnames(X),6,7)%in%"_M"]
X <- X[,!colnames(X)%in%c(N_Year,N_Month),]


Quarterly_Data <- as.data.frame(cbind(ID = X$KEY,
									X[,c("TEST_N_BRK_P",N_Quarter)],stringsAsFactors=FALSE))

				
				
ref <- N_Quarter
							
for (i in 1:length(ref)){
Quarterly_Data[,ref[i]] <- gsub("b","",Quarterly_Data[,ref[i]]) 
Quarterly_Data[,ref[i]] <- gsub(" ","",Quarterly_Data[,ref[i]]) 
Quarterly_Data[,ref[i]] <- as.numeric(Quarterly_Data[,ref[i]]) 
}
print(" ERROR OK, don't worry... DB")
Quarterly_Data$ID <- gsub("/XXX_XXX_XXX","",Quarterly_Data$ID) 	
colnames(Quarterly_Data)[1] <- "COLLECTION_CODE/COUNTRY_CODE/SOURCE_CODE/INDICATOR_CODE/SEX_CODE/CLASSIF1_CODE"		

write.csv(Quarterly_Data,paste(MY_Chem,"Collection/REP_ILO/Input_SA/Quarterly_Data.csv",sep=""),row.names = FALSE,na = "")
rm(X)


}

My_unsplit_KEY <- function(X,ID=1,KEY=1,ref="/"){

#  My_unsplit_KEY(TEST,REF[i],c("T1","T2"),ref=":")


# X <- TEST; ID <- REF[i] ; KEY <- c("T1","T2") ; ref <- ":"

X <- as.data.frame(X, stringsAsFactors=FALSE)
sbt <- strsplit(as.character(X[,ID]),ref)
ifelse(KEY%in%1, n <- max(sapply(sbt, length)), n <- length(KEY))

l <- lapply(sbt, function(X) c(X, rep(NA, n - length(X))))

NEW <- as.data.frame(t(do.call(cbind, l)),stringsAsFactors =FALSE)
for (i in 1:ncol(NEW)){
NEW[,i] <- as.character(NEW[,i])
}


rm(sbt,l)
ifelse(KEY%in%1 ,colnames(NEW) <- paste(rep("PASS",n),1:n,sep=""),colnames(NEW) <- KEY)
X <- as.data.frame(cbind(IDNEW = as.character(X[,ID]),NEW,X),stringsAsFactors =FALSE)
X <- X[,!colnames(X)%in%ID]
X$IDNEW <- as.character(X[,"IDNEW"])
colnames(X)[colnames(X)%in%"IDNEW"] <- ID

return(X)
}

My_unsplit_KEY_NAME <- function(X,ID,KEY=1,ref="/"){


# X <- PASS; ID <- "KEY" ; KEY <- 1 ; ref <- "/"     PASS,"KEY",KEY=1,ref="/"

X$CODE_SEX <- gsub("XXX_X",NA,X$CODE_SEX)
X$CODE_CL1 <- gsub("XXX_XXX_XXX",NA,X$CODE_CL1)
X$CODE_CL2 <- gsub("XXX_XXX_XXX",NA,X$CODE_CL2)
X$KEY <- gsub("/XXX_XXX_XXX","",X$KEY)
X$KEY <- gsub("/XXX_X/","//",X$KEY)

test <- levels(as.factor(X[8:(nrow(X)-7),"LABEL_SEX"]))
test <- test[!test%in%c(NA,"","NA")]
if(length(test) %in% 0) {X <- X[,!colnames(X)%in%"LABEL_SEX"]}

test <- levels(as.factor(X[8:(nrow(X)-7),"LABEL_CLASS_DETAIL"]))
test <- test[!test%in%c(NA,"","NA")]
if(length(test) %in% 0){X <- X[,!colnames(X)%in%"LABEL_CLASS_DETAIL"]}

test <- levels(as.factor(X[8:(nrow(X)-7),"LABEL_CLASS_DETAIL2"]))
test <- test[!test%in%c(NA,"","NA")]
if(length(test) %in% 0){X <- X[,!colnames(X)%in%"LABEL_CLASS_DETAIL2"]}





sbt <- strsplit(X[7:(nrow(X)-7),ID],ref)
n <- max(sapply(sbt, length))

sbt <- as.vector(strsplit(key_Name,ref)[[1]])

new <- sbt[1]
for (i in 2:n){
new <- paste(new,sbt[i],sep="/")
}


X[7,1] <- new


return(X)
}

My_unsplit_NOTE <-  function(X,ID,REFERENCE,Lang = "EN", mapping = "No"){

# X <- PASS ; ID <- "NOTE_FREQUENCY" ; REFERENCE <- CODE$FREQUENCY ; Lang <- "EN" ; mapping = "No"
# "NOTE_FREQUENCY",CODE$FREQUENCY,Lang
NEW <- My_unsplit_KEY(X[,c("KEY",colnames(X)[colnames(X)%in%ID])],ID,1,"_")
colnames(NEW)[substr(colnames(NEW),1,4)%in%"PASS"] <- paste(rep("CAT",length(colnames(NEW)[substr(colnames(NEW),1,4)%in%"PASS"])),1:length(colnames(NEW)[substr(colnames(NEW),1,4)%in%"PASS"]),sep="")
NEW <- My_unsplit_KEY(NEW,ID,1,"_")
colnames(NEW)[substr(colnames(NEW),1,4)%in%"PASS"] <- paste(rep("LABEL",length(colnames(NEW)[substr(colnames(NEW),1,4)%in%"PASS"])),1:length(colnames(NEW)[substr(colnames(NEW),1,4)%in%"PASS"]),sep="")

ref_CAT <- colnames(NEW)[substr(colnames(NEW),1,3)%in%"CAT"]

if(mapping%in%"No"){

for (i in 1:length(ref_CAT)){
NEW <- My_Vlookup(NEW,ref_CAT[i],ref_CAT[i],REFERENCE,colnames(REFERENCE)[1],"LIST")
NEW <- My_Vlookup(NEW,ref_CAT[i],ref_CAT[i],CODE$HELP,colnames(CODE$HELP)[1],paste("TEXT_",Lang, sep=""))
}
ref_LABEL <- colnames(NEW)[substr(colnames(NEW),1,3)%in%"LAB"]

for (i in 1:length(ref_LABEL)){
NEW <- My_Vlookup(NEW,ref_LABEL[i],ref_LABEL[i],REFERENCE,colnames(REFERENCE)[1],paste("TEXT_",Lang, sep=""))
NEW[,ref_LABEL[i]] <- paste(NEW[,ref_CAT[i]],": ",NEW[,ref_LABEL[i]],sep="")
NEW[,ref_LABEL[i]] <- gsub("NA: NA",NA,NEW[,ref_LABEL[i]])
}

if(length(ref_LABEL)>1){
for (i in 2:length(ref_LABEL)){

NEW[,ref_LABEL[1]] <- paste(NEW[,ref_LABEL[1]],NEW[,ref_LABEL[i]],sep=" / ")
NEW[,ref_LABEL[1]] <- gsub(" / NA","",NEW[,ref_LABEL[1]])
NEW[,ref_LABEL[1]] <- gsub("NA",NA,NEW[,ref_LABEL[1]])
}
}



X[,ID] <- NEW[,ref_LABEL[1]]

X[X[,ID]%in%"",ID]<- NA
}

if(mapping%in%"Yes"){

for (i in 1:length(ref_CAT)){
NEW <- My_Vlookup(NEW,ref_CAT[i],ref_CAT[i],REFERENCE,"CODNOTE","MAPPING")
}

if(length(ref_CAT)>1){
for (i in length(ref_CAT):2){
NEW[!NEW[,ref_CAT[i]]%in%c(NA,"","NA"),ref_CAT[i-1]] <- paste(NEW[!NEW[,ref_CAT[i]]%in%c(NA,"","NA"),ref_CAT[i-1]],NEW[!NEW[,ref_CAT[i]]%in%c(NA,"","NA"),ref_CAT[i]],sep="_")
}
}

X[,ID] <- NEW[,ref_CAT[1]]
X[X[,ID]%in%"",ID]<- NA

}



return(X)


}

My_Create_Reshape_CL <- function(Y){

###### Cells levels for the manual data entry

key_reshape <- c("COLLECTION_CODE", "COUNTRY_CODE", "SOURCE_CODE", "INDICATOR_CODE", "SEX_CODE", "CLASSIF1_CODE", "CLASSIF2_CODE", "YEAR", "OBS_STATUS", "NOTES_FREQUENCY_CODE", "NOTES_CLASSIF_CODE", "NOTES_INDICATOR_CODE", "NOTES_SOURCE_CODE", "CURRENCY_CODE", "ADD_REPOSITORY", "ADD_STATUS")

X <- eval(parse(text= paste0("  X %>% mutate(ID = 	paste(",paste0(c(key_reshape), collapse=","),",sep='/')) "))) 
Y <- eval(parse(text= paste0("  X %>% select(-",paste0(c(key_reshape), collapse=',-'),")"))) 
Y <- as.data.frame(Y) %>% gather(TIME_PERIOD, OBS_VALUE, -ID)
X <- X %>% select( ID ,COLLECTION_CODE, COUNTRY_CODE, SOURCE_CODE, INDICATOR_CODE, SEX_CODE, CLASSIF1_CODE, CLASSIF2_CODE,  YEAR, OBS_STATUS, NOTES_FREQUENCY_CODE, NOTES_CLASSIF_CODE, NOTES_INDICATOR_CODE, NOTES_SOURCE_CODE, CURRENCY_CODE, ADD_REPOSITORY, ADD_STATUS) 
Y <- as.tbl(Y) %>%	left_join(X,by ="ID") %>% 	
					mutate(	ID = as.character(NA),
							TIME_PERIOD = as.character(TIME_PERIOD)   , 
							OBS_VALUE = as.numeric(OBS_VALUE),
							 TIME_PERIOD = ifelse(!TIME_PERIOD%in%"Y",paste0("Y",YEAR,"_",TIME_PERIOD),paste0("Y",YEAR))) %>%
					select(  -YEAR)
					
Y[substr(Y$OBS_STATUS,2,3)%in%"01" & !substr(Y$TIME_PERIOD,7,9) %in%c("M01","Q01",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"02" & !substr(Y$TIME_PERIOD,7,9) %in%c("M02","Q01",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"03" & !substr(Y$TIME_PERIOD,7,9) %in%c("M03","Q01",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"04" & !substr(Y$TIME_PERIOD,7,9) %in%c("M04","Q02",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"05" & !substr(Y$TIME_PERIOD,7,9) %in%c("M05","Q02",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"06" & !substr(Y$TIME_PERIOD,7,9) %in%c("M06","Q02",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"07" & !substr(Y$TIME_PERIOD,7,9) %in%c("M07","Q03",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"08" & !substr(Y$TIME_PERIOD,7,9) %in%c("M08","Q03",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"09" & !substr(Y$TIME_PERIOD,7,9) %in%c("M09","Q03",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"10" & !substr(Y$TIME_PERIOD,7,9) %in%c("M10","Q04",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"11" & !substr(Y$TIME_PERIOD,7,9) %in%c("M11","Q04",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"12" & !substr(Y$TIME_PERIOD,7,9) %in%c("M12","Q04",""),"OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"13" & !substr(Y$TIME_PERIOD,7,9) %in%"Q01","OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"14" & !substr(Y$TIME_PERIOD,7,9) %in%"Q02","OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"15" & !substr(Y$TIME_PERIOD,7,9) %in%"Q03","OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"16" & !substr(Y$TIME_PERIOD,7,9) %in%"Q04","OBS_STATUS"] <- NA
Y[substr(Y$OBS_STATUS,2,3)%in%"17" & !substr(Y$TIME_PERIOD,7,9) %in%"","OBS_STATUS"] <- NA
Y[!Y$OBS_STATUS%in%NA,"OBS_STATUS"] <- substr(Y[!Y$OBS_STATUS%in%NA,"OBS_STATUS"],1,1)
Y <- Y[!Y$OBS_VALUE%in%c(NA,"","NA") | !Y$OBS_STATUS%in%c("",NA,"NA"), ]

					
					
					

return(Y)
} 

My_Resort_Notes_NEW <- function(col_note,SEP = " | "){					# sort concatenate note by type_sort, note_sort and note_id 

#  col_note <- X$Notes_Classif_Code ; SEP = "_"
# #  X <- X
if(length(unique(col_note)[!unique(col_note)%in%NA])>0){
attach(CODE_ORA,warn.conflicts = FALSE)
REF_NOTE_TYPE_SORT <- T_NTY_NOTE_TYPE[,c("NTY_CODE","NTY_SORT")]
REF_NOTE_SORT <- T_NTE_NOTE
detach(CODE_ORA)

REF_NOTE_SORT <- REF_NOTE_SORT[,c("NTE_ID","NTE_SORT","NTE_TYPE_CODE")]
REF_NOTE_SORT[REF_NOTE_SORT$NTE_SORT%in%".","NTE_SORT"] <- 9999 


if(SEP %in% " | "){col_note <- gsub(" | ","_",col_note, fixed = TRUE) ; SEP <- "_"}

aaa <- t(My_unsplit_KEY(cbind(new = unique(col_note), code = unique(col_note)),"code",1,ref=SEP))

# i <- 1
for (i in 1:ncol(aaa)){
my.note.string <- aaa[rownames(aaa)[substr(rownames(aaa),1,4)%in%"PASS"],i]
my_decompose <- My_unsplit_KEY(cbind(new = my.note.string, code = my.note.string) ,"code",1,ref=":")
my_decompose <- as.data.frame(my_decompose[!my_decompose$code%in%NA,])

if(!empty(my_decompose)){
my_decompose <- My_Vlookup(my_decompose,"PASS2","PASS1",REF_NOTE_SORT,"NTE_ID","NTE_TYPE_CODE") 
my_decompose <- My_Vlookup(my_decompose,"PASS1","ID1",REF_NOTE_TYPE_SORT,"NTY_CODE","NTY_SORT") 
my_decompose <- My_Vlookup(my_decompose,"PASS2","ID2",REF_NOTE_SORT,"NTE_ID","NTE_SORT") 
my_decompose <- my_decompose[order(as.numeric(my_decompose$ID1),as.numeric(my_decompose$ID2),as.numeric(my_decompose$PASS2)),]
aaa["new",i] <- paste(my_decompose$new, collapse="_")
}

}

aaa <- as.data.frame(t(aaa))[,c("code","new")]
aaa <- aaa %>% filter(!aaa$code%in%NA)
REF <- 	as.tbl(as.data.frame(cbind(code = col_note, fack = col_note))) %>% 
		left_join(aaa, by = "code")

return(as.character(REF$new))
}
else{
return(as.character(NA))
}


}

My_Transform_notes_NEW <- function(col_note,SEP = " | "){					# from C1:1524 | C2:1523 to #1524$NA#1523$NA

####### col_note <- X$Value_Notes_String
if(length(unique(col_note)[!unique(col_note)%in%NA])>0){
if(SEP %in% " | "){col_note <- gsub(" | ","_",col_note,fixed = TRUE) ; SEP <- "_"}

TEST <- My_unsplit_KEY(cbind(new = unique(col_note), code = unique(col_note)),"code",1,ref=SEP)
TEST <- TEST %>% select(-new) %>% mutate(code = as.character(code))
REF <- colnames(TEST)[substr(colnames(TEST),1,4)%in%"PASS"]
for (i in 1:length(REF)){

TEST <- My_unsplit_KEY(TEST,REF[i],c("T1","T2"),ref=":")

TEST[,REF[i]] <- paste0("#",TEST[,"T2"],"$NA")
TEST <- select(TEST,-T1,-T2)
}

#TEST <- unite_(TEST,"new", REF,remove = FALSE)
TEST <- eval(parse(text= paste0("  TEST %>% mutate(new = 	paste(",paste0(c(REF), collapse=","),",sep='_'))"))) 
	
TEST <- TEST %>% select(code,new) %>%
		mutate(	new = gsub("_#NA$NA","",new, fixed=TRUE),
				new = gsub("#NA$NA",NA,new, fixed=TRUE),
				new = gsub("_","",new, fixed=TRUE)
		)

REF <- 	as.tbl(as.data.frame(cbind(code = col_note, fack = col_note), stringsAsFactors=FALSE)) 
REF <- REF %>% 
		left_join(as.tbl(TEST), by = "code")
return(as.character(REF$new))
}
else{
return(as.character(NA))
}
}

My_Vlookup_VERSION_NEW <- function(col_item){   					# deprecate

#  col_item <- Base$SEX_CODE

if(length(unique(col_item)[!unique(col_item)%in%NA])>0){
TEST <- My_unsplit_KEY(cbind(new = unique(col_item), code = unique(col_item)),"code",1,ref="_")


if(ncol(TEST)==5){
# TEST <- unite_(TEST,"new", c("PASS1","PASS2"),remove = TRUE)
TEST <- eval(parse(text= paste0("  TEST %>% mutate(new = 	paste(",paste0(c("PASS1","PASS2"), collapse=","),",sep='_'))"))) 
	
}
if(ncol(TEST)==4){
TEST <- TEST %>% mutate(new = PASS1) %>% select(code,new)
}

TEST$new <- gsub("NA_NA",NA,TEST$new)
REF <- 	as.tbl(as.data.frame(cbind(code = col_item, fack = col_item), stringsAsFactors=FALSE)) 
REF <- REF %>% 
		left_join(as.tbl(TEST), by = "code")
return(as.character(REF$new))
}
else{
return(as.character(NA))
}

}

My_possibility <- function(COL_REF){


# COL_REF <- "Country_Code"
dns_ID <- "ILOSTAT64"
DB_DATA	<- "ilostat.v_yi_data_internal_dissem"	 ; DB_NOTE	<- "ilostat.v_yi_note_internal_dissem"    
pwd_ID <- "E8hBzras"
uid <- "guest_read_only"

query <- paste0("select distinct data.",COL_REF," from ",DB_DATA," data ")

ch <- odbcConnect(dns_ID,pwd = pwd_ID,believeNRows = FALSE)

X <- sqlQuery(ch,query, stringsAsFactors=FALSE)
close(ch)
return(X)
}

My_Detach_package <- function(pkg, character.only = FALSE){
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

Clean_col_format <- function(X,mode="upload"){

if(mode%in%"upload"){
col_names 	<- c("Country_Code", "Collection_Code", "Indicator_Code", "Source_Code", "Survey_Id", "Sex_Version_Code", "Classif1_Version_Code", "Classif2_Version_Code", "Classif3_Version_Code", "Classif4_Version_Code", "Classif5_Version_Code", "Sex_Code", "Classif1_Code", "Classif2_Code", "Classif3_Code", "Classif4_Code", "Classif5_Code", "Freq_Code", "Time", "Value", "Value_Status_Code", "Currency_Code", "Notes_Source_Code", "Notes_Topic_Code", "Notes_Indicator_Code", "Notes_Classif_Code")

X <- X[,colnames(X)[colnames(X)%in%col_names]]

for (i in 1:length(col_names)){
if(!col_names[i]%in%colnames(X)){

X <- eval(parse(text= paste0("  X %>% mutate(",col_names[i]," = 	as.character(NA))"))) 
	

}


}
X <- X[,col_names]
}
return(X)

}

empty <- function (df) {
    (is.null(df) || nrow(df) == 0 || ncol(df) == 0)
}



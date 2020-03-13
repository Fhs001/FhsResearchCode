# Join results of UV and MV results by site (don't use this for peritoneum!) -----

RR_Liver <- left_join(Regression_Results_Liver3, Liver_MV_Regression_Results3, by="Variables", all=T)
colnames(RR_Liver) <- c("Variables", "UV_OR", "UV_CI_Lower", "UV_CI_Upper", "UV_p_value", "UV_q_value", "MV_OR", "MV_CI_Lower", "MV_CI_Upper", "MV_p_value", "MV_q_value")
RR_Liver <- RR_Liver[11:45,]

RR_Lung <- left_join(Regression_Results_Lung3, Lung_MV_Regression_Results3, by="Variables", all=T)
colnames(RR_Lung) <- c("Variables", "UV_OR", "UV_CI_Lower", "UV_CI_Upper", "UV_p_value", "UV_q_value", "MV_OR", "MV_CI_Lower", "MV_CI_Upper", "MV_p_value", "MV_q_value")
RR_Lung <- RR_Lung[11:45,]


RR_Node <- left_join(Regression_Results_Node3, Node_MV_Regression_Results3, by="Variables", all=T)
colnames(RR_Node) <- c("Variables", "UV_OR", "UV_CI_Lower", "UV_CI_Upper", "UV_p_value", "UV_q_value", "MV_OR", "MV_CI_Lower", "MV_CI_Upper", "MV_p_value", "MV_q_value")
RR_Node<- RR_Node[11:45,]


RR_Peritoneum <- left_join(Regression_Results_Peritoneum3, Peritoneum_MV_Regression_Results3, by="Variables", all=T)
colnames(RR_Peritoneum) <- c("Variables", "UV_OR", "UV_CI_Lower", "UV_CI_Upper", "UV_p_value", "UV_q_value", "MV_OR", "MV_CI_Lower", "MV_CI_Upper", "MV_p_value", "MV_q_value")


# Combine OR and CI columns 

library(dplyr)

# Liver


RR_Liver[,2:4] <- as.numeric(unlist(RR_Liver[,2:4]))
RR_Liver[,2:4] <- round((RR_Liver[,2:4]), digits = 2)


RR_Liver[,7:9] <- as.numeric(as.character(unlist(RR_Liver[,7:9])))
RR_Liver[,7:9] <- round((RR_Liver[,7:9]), digits = 2)

RR_Liver$Brac <- c(")")

RR_Liver$UVORCI <- paste(RR_Liver$UV_OR, RR_Liver$UV_CI_Lower, sep=" (")
RR_Liver$UVORCI <- paste(RR_Liver$UVORCI, RR_Liver$UV_CI_Upper, sep="-")

RR_Liver$UVORCI <- paste(RR_Liver$UVORCI, RR_Liver$Brac, sep="")

RR_Liver$MVORCI <- paste(RR_Liver$MV_OR, RR_Liver$MV_CI_Lower, sep=" (")
RR_Liver$MVORCI <- paste(RR_Liver$MVORCI, RR_Liver$MV_CI_Upper, sep="-")
RR_Liver$MVORCI <- paste(RR_Liver$MVORCI, RR_Liver$Brac, sep="")

RR_Liver$UV_OR <- NULL
RR_Liver$UV_CI_Lower <- NULL
RR_Liver$UV_CI_Upper <- NULL
RR_Liver$MV_OR <- NULL
RR_Liver$MV_CI_Lower <- NULL
RR_Liver$MV_CI_Upper <- NULL
RR_Liver$Brac <- NULL

RR_Liver <- RR_Liver[, c(1, 6, 2, 3, 7, 4, 5)]

RR_Liver <- RR_Liver[c(2,28,3,1,8,
                       7,22,11,16,4,
                       10,26,5,35,17,
                       13,21,6,15,18,
                       14,20,9,12,31,
                       29,32,33,30,27,
                       25,23,24,19,34),] # row order

# Lung


RR_Lung[,2:4] <- as.numeric(unlist(RR_Lung[,2:4]))
RR_Lung[,2:4] <- round((RR_Lung[,2:4]), digits = 2)


RR_Lung[,7:9] <- as.numeric(as.character(unlist(RR_Lung[,7:9])))
RR_Lung[,7:9] <- round((RR_Lung[,7:9]), digits = 2)

RR_Lung$Brac <- c(")")

RR_Lung$UVORCI <- paste(RR_Lung$UV_OR, RR_Lung$UV_CI_Lower, sep=" (")
RR_Lung$UVORCI <- paste(RR_Lung$UVORCI, RR_Lung$UV_CI_Upper, sep="-")

RR_Lung$UVORCI <- paste(RR_Lung$UVORCI, RR_Lung$Brac, sep="")

RR_Lung$MVORCI <- paste(RR_Lung$MV_OR, RR_Lung$MV_CI_Lower, sep=" (")
RR_Lung$MVORCI <- paste(RR_Lung$MVORCI, RR_Lung$MV_CI_Upper, sep="-")
RR_Lung$MVORCI <- paste(RR_Lung$MVORCI, RR_Lung$Brac, sep="")

RR_Lung$UV_OR <- NULL
RR_Lung$UV_CI_Lower <- NULL
RR_Lung$UV_CI_Upper <- NULL
RR_Lung$MV_OR <- NULL
RR_Lung$MV_CI_Lower <- NULL
RR_Lung$MV_CI_Upper <- NULL
RR_Lung$Brac <- NULL

RR_Lung <- RR_Lung[, c(1, 6, 2, 3, 7, 4, 5)]

RR_Lung <- RR_Lung[c(2,28,3,1,8,
                     7,22,11,16,4,
                     10,26,5,35,17,
                     13,21,6,15,18,
                     14,20,9,12,31,
                     29,32,33,30,27,
                     25,23,24,19,34),] # row order

#Node

RR_Node[,2:4] <- as.numeric(unlist(RR_Node[,2:4]))
RR_Node[,2:4] <- round((RR_Node[,2:4]), digits = 2)


RR_Node[,7:9] <- as.numeric(as.character(unlist(RR_Node[,7:9])))
RR_Node[,7:9] <- round((RR_Node[,7:9]), digits = 2)

RR_Node$Brac <- c(")")

RR_Node$UVORCI <- paste(RR_Node$UV_OR, RR_Node$UV_CI_Lower, sep=" (")
RR_Node$UVORCI <- paste(RR_Node$UVORCI, RR_Node$UV_CI_Upper, sep="-")

RR_Node$UVORCI <- paste(RR_Node$UVORCI, RR_Node$Brac, sep="")

RR_Node$MVORCI <- paste(RR_Node$MV_OR, RR_Node$MV_CI_Lower, sep=" (")
RR_Node$MVORCI <- paste(RR_Node$MVORCI, RR_Node$MV_CI_Upper, sep="-")
RR_Node$MVORCI <- paste(RR_Node$MVORCI, RR_Node$Brac, sep="")

RR_Node$UV_OR <- NULL
RR_Node$UV_CI_Lower <- NULL
RR_Node$UV_CI_Upper <- NULL
RR_Node$MV_OR <- NULL
RR_Node$MV_CI_Lower <- NULL
RR_Node$MV_CI_Upper <- NULL
RR_Node$Brac <- NULL

RR_Node <- RR_Node[, c(1, 6, 2, 3, 7, 4, 5)]

RR_Node <- RR_Node[c(2,28,3,1,8,
                     7,22,11,16,4,
                     10,26,5,35,17,
                     13,21,6,15,18,
                     14,20,9,12,31,
                     29,32,33,30,27,
                     25,23,24,19,34),] # row order


#Peritoneum

RR_Peritoneum[,2:4] <- as.numeric(unlist(RR_Peritoneum[,2:4]))
RR_Peritoneum[,2:4] <- round((RR_Peritoneum[,2:4]), digits = 2)


RR_Peritoneum[,7:9] <- as.numeric(as.character(unlist(RR_Peritoneum[,7:9])))
RR_Peritoneum[,7:9] <- round((RR_Peritoneum[,7:9]), digits = 2)

RR_Peritoneum$Brac <- c(")")

RR_Peritoneum$UVORCI <- paste(RR_Peritoneum$UV_OR, RR_Peritoneum$UV_CI_Lower, sep=" (")
RR_Peritoneum$UVORCI <- paste(RR_Peritoneum$UVORCI, RR_Peritoneum$UV_CI_Upper, sep="-")

RR_Peritoneum$UVORCI <- paste(RR_Peritoneum$UVORCI, RR_Peritoneum$Brac, sep="")

RR_Peritoneum$MVORCI <- paste(RR_Peritoneum$MV_OR, RR_Peritoneum$MV_CI_Lower, sep=" (")
RR_Peritoneum$MVORCI <- paste(RR_Peritoneum$MVORCI, RR_Peritoneum$MV_CI_Upper, sep="-")
RR_Peritoneum$MVORCI <- paste(RR_Peritoneum$MVORCI, RR_Peritoneum$Brac, sep="")

RR_Peritoneum$UV_OR <- NULL
RR_Peritoneum$UV_CI_Lower <- NULL
RR_Peritoneum$UV_CI_Upper <- NULL
RR_Peritoneum$MV_OR <- NULL
RR_Peritoneum$MV_CI_Lower <- NULL
RR_Peritoneum$MV_CI_Upper <- NULL
RR_Peritoneum$Brac <- NULL

RR_Peritoneum <- RR_Peritoneum[, c(1, 6, 2, 3, 7, 4, 5)] # column order
rownames(RR_Peritoneum)
list(RR_Peritoneum$Variables)

RR_Peritoneum <- RR_Peritoneum[c(2,28,3,1,8,
                                 7,22,11,16,4,
                                 10,26,5,35,17,
                                 13,21,6,15,18,
                                 14,20,9,12,31,
                                 29,32,33,30,27,
                                 25,23,24,19,34),] # row order

# Write objects into csv files 

write.csv(RR_Liver, "/Users/laurahudson/Documents/UVMV_Liver02.csv")
write.csv(RR_Lung, "/Users/laurahudson/Documents/UVMV_Lung02.csv")
write.csv(RR_Node, "/Users/laurahudson/Documents/UVMV_Node02.csv")
write.csv(RR_Peritoneum, "/Users/laurahudson/Documents/UVMV_Peritoneum02.csv")




# FINAL MULTIVARIATE ANALYSES (Stepwise Backwards Regression) ----
# Liver ----

print(Liver_MV_Regression_Results3$Variables[Liver_MV_Regression_Results3$Adjusted_PVal <= 0.1],)

Model_Liver_Mv <- glm(Liver_Met ~ T_Stage_Binary + N_Stage_Binary + M_Stage_Binary 
                      + Age + Sidedness_Binary +Gender
                      + Cohort
                      #2 + BRAF_V600E
                      + CMS1
                      + CRIS_B
                      + CRIS_D
                      #1 + MSI_Binary
                      #3 + CRIS_A
                      #4 + APC
                      , data = Final_Merge_Analysis, family = binomial)

CheckSamplesLiverFmv <- length(resid(Model_Liver_Mv))
CheckSamplesLiverFmv

table(Final_Merge_Analysis$Cohort)

Summary_Model_Liver_Mv <- summary(Model_Liver_Mv)

Coef_Liver <- Summary_Model_Liver_Mv$coefficients
#Coef_Liver[]

#Coef_Liver[[4]] <- (Coef_Liver[[4]] * -1)  
#Coef_Liver[[4]]

n <- nrow(Coef_Liver)

#### Get p-values from model summary 

P_values <- unname(Coef_Liver[,'Pr(>|z|)'])[2:n]
P_values

#### Get names of coefficients

names(Coef_Liver[,'Pr(>|z|)'])[2:n]

#### Create dataframe with OR, 95%CI and p-value for each dependent variable

Liver_Mv_Regression_Results <- as.data.frame(cbind(names(Coef_Liver[,'Pr(>|z|)'])[2:n],
                                                   exp(Coef_Liver[2:n,1]),
                                                   exp(Coef_Liver[2:n,1] + (qnorm(0.025) * Coef_Liver[2:n,2])),
                                                   exp(Coef_Liver[2:n,1] + (qnorm(0.975) * Coef_Liver[2:n,2])),
                                                   Coef_Liver[2:n,4]))

exp(Coef_Liver[-1,1])


colnames(Liver_Mv_Regression_Results) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
Liver_Mv_Regression_Results[,-1] <- lapply(Liver_Mv_Regression_Results[,-1], function(x) as.numeric(as.character(x)))

Liver_Mv_Regression_Results

## Format results

library(formattable)

as.datatable(
  formattable(Liver_Mv_Regression_Results, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(P_value <= 0.05, "green", "red")))))
) 




# Lung ----

print(Lung_MV_Regression_Results3$Variables[Lung_MV_Regression_Results3$Adjusted_PVal <= 0.1],)


Model_Lung_Mv <- glm(Lung_Met ~ T_Stage_Binary + N_Stage_Binary + M_Stage_Binary 
                     + Age + Sidedness_Binary + Gender
                     + Cohort
                     #3 + MSI_Binary
                     + KRAS
                     + Hypoxia_Buffa
                     
                     #5 + TGFb_Fibroblast
                     #1 + MCP_Cytotoxic_lymphocytes
                     #2 + CMS2
                     #4 + MCP_CD8_T_cells
                     + CN_TP53_isLoss
                     , data = Final_Merge_Analysis, family = binomial)



CheckSamplesLungFmv <- length(resid(Model_Lung_Mv))
CheckSamplesLungFmv

table(Final_Merge_Analysis$Cohort)


Summary_Model_Lung_Mv <- summary(Model_Lung_Mv)

Coef_Lung <- Summary_Model_Lung_Mv$coefficients
#Coef_Lung[]

#Coef_Lung[[4]] <- (Coef_Lung[[4]] * -1)  
#Coef_Lung[[4]]

n <- nrow(Coef_Lung)

#### Get p-values from model summary 

P_values <- unname(Coef_Lung[,'Pr(>|z|)'])[2:n]
P_values

#### Get names of coefficients

names(Coef_Lung[,'Pr(>|z|)'])[2:n]

#### Create dataframe with OR, 95%CI and p-value for each dependent variable

Lung_Mv_Regression_Results <- as.data.frame(cbind(names(Coef_Lung[,'Pr(>|z|)'])[2:n],
                                                  exp(Coef_Lung[2:n,1]),
                                                  exp(Coef_Lung[2:n,1] + (qnorm(0.025) * Coef_Lung[2:n,2])),
                                                  exp(Coef_Lung[2:n,1] + (qnorm(0.975) * Coef_Lung[2:n,2])),
                                                  Coef_Lung[2:n,4]))

exp(Coef_Lung[-1,1])


colnames(Lung_Mv_Regression_Results) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
Lung_Mv_Regression_Results[,-1] <- lapply(Lung_Mv_Regression_Results[,-1], function(x) as.numeric(as.character(x)))

Lung_Mv_Regression_Results

## Format results

library(formattable)

as.datatable(
  formattable(Lung_Mv_Regression_Results, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(P_value <= 0.05, "green", "red")))))
) 





# Node ----

# Select model 

print(Nodal_MV_Regression_Results3$Variables[Nodal_MV_Regression_Results3$Adjusted_PVal <= 0.1],)


Model_Nodal_Mv <- glm(Node_Met ~ T_Stage_Binary + N_Stage_Binary + M_Stage_Binary 
                      + Age + Sidedness_Binary + Gender
                      + Cohort
                      + MCP_Cytotoxic_lymphocytes
                      
                      # + APC
                      , data = Final_Merge_Analysis, family = binomial)

CheckSamplesNodalFmv <- length(resid(Model_Nodal_Mv))
CheckSamplesNodalFmv


Summary_Model_Nodal_Mv <- summary(Model_Nodal_Mv)

Summary_Model_Nodal_Mv$coefficients

n <- nrow(Summary_Model_Nodal_Mv$coefficients)

#### Get p-values from model summary 

P_values <- unname(coef(summary(Model_Nodal_Mv))[,'Pr(>|z|)'])[2:n]
P_values

#### Get names of coefficients

names(coef(summary(Model_Nodal_Mv))[,'Pr(>|z|)'])[2:n]

#### Create dataframe with OR, 95%CI and p-value for each dependent variable

Nodal_Mv_Regression_Results <- as.data.frame(cbind(names(coef(summary(Model_Nodal_Mv))[,'Pr(>|z|)'])[2:n], exp(summary(Model_Nodal_Mv)$coefficients[2:n,1]), exp(summary(Model_Nodal_Mv)$coefficients[2:n,1] + (qnorm(0.025) * summary(Model_Nodal_Mv)$coefficients[2:n,2])), exp(summary(Model_Nodal_Mv)$coefficients[2:n,1] + (qnorm(0.975) * summary(Model_Nodal_Mv)$coefficients[2:n,2])), summary(Model_Nodal_Mv)$coefficients[2:n,4]))

exp(summary(Model_Nodal_Mv)$coefficients[-1,1])


colnames(Nodal_Mv_Regression_Results) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
Nodal_Mv_Regression_Results[,-1] <- lapply(Nodal_Mv_Regression_Results[,-1], function(x) as.numeric(as.character(x)))

Nodal_Mv_Regression_Results

## Format results

library(formattable)

as.datatable(
  formattable(Nodal_Mv_Regression_Results, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(P_value <= 0.05, "green", "red")))))
) 


# Peritoneum ----

print(Peritoneal_MV_Regression_Results3$Variables[Peritoneal_MV_Regression_Results3$Adjusted_PVal <= 0.05],)


Model_Peritoneal_Mv <- glm(Peritoneum_Met ~ T_Stage_Binary + N_Stage_Binary + M_Stage_Binary 
                           + Age + Sidedness_Binary + Gender
                           + Cohort
                           + SMAD4
                           + CMS2
                           , data = Final_Merge_Analysis, family = binomial)

Model_Peritoneal_Mv

CheckSamplesPeritonealFmv <- length(resid(Model_Peritoneal_Mv))
CheckSamplesPeritonealFmv

table(Final_Merge_Analysis$Cohort)

Summary_Model_Peritoneal_Mv <- summary(Model_Peritoneal_Mv)

Summary_Model_Peritoneal_Mv$coefficients

n <- nrow(Summary_Model_Peritoneal_Mv$coefficients)

#### Get p-values from model summary 

P_values <- unname(coef(summary(Model_Peritoneal_Mv))[,'Pr(>|z|)'])[2:n]
P_values

#### Get names of coefficients

names(coef(summary(Model_Peritoneal_Mv))[,'Pr(>|z|)'])[2:n]

#### Create dataframe with OR, 95%CI and p-value for each dependent variable

Peritoneal_Mv_Regression_Results <- as.data.frame(cbind(names(coef(summary(Model_Peritoneal_Mv))[,'Pr(>|z|)'])[2:n], exp(summary(Model_Peritoneal_Mv)$coefficients[2:n,1]), exp(summary(Model_Peritoneal_Mv)$coefficients[2:n,1] + (qnorm(0.025) * summary(Model_Peritoneal_Mv)$coefficients[2:n,2])), exp(summary(Model_Peritoneal_Mv)$coefficients[2:n,1] + (qnorm(0.975) * summary(Model_Peritoneal_Mv)$coefficients[2:n,2])), summary(Model_Peritoneal_Mv)$coefficients[2:n,4]))

exp(summary(Model_Peritoneal_Mv)$coefficients[-1,1])


colnames(Peritoneal_Mv_Regression_Results) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
Peritoneal_Mv_Regression_Results[,-1] <- lapply(Peritoneal_Mv_Regression_Results[,-1], function(x) as.numeric(as.character(x)))

Peritoneal_Mv_Regression_Results

## Format results

library(formattable)

as.datatable(
  formattable(Peritoneal_Mv_Regression_Results, list(
    Variables = formatter("span",
                          style = ~ style(color = ifelse(P_value <= 0.05, "green", "red")))))
) 


# Write objects into csv files ----

#write.csv(Liver_Mv_Regression_Results, "/Users/laurahudson/Documents/MV_Liver_Final_03.csv")
#write.csv(Lung_Mv_Regression_Results, "/Users/laurahudson/Documents/MV_Lung_Final_03.csv")
#write.csv(Nodal_Mv_Regression_Results, "/Users/laurahudson/Documents/MV_Node_Final_03.csv")
# write.csv(Peritoneal_Mv_Regression_Results, "/Users/laurahudson/Documents/MV_Peritoneum_Final_03.csv")


# Format tables -----

# Liver

Liver_Mv_Regression_Results
Format_Liver_MRR <- Liver_Mv_Regression_Results

Format_Liver_MRR1 <- Format_Liver_MRR[(1:6),]
Format_Liver_MRR2 <- Format_Liver_MRR[(10:12),]
Format_Liver_MRR <- rbind(Format_Liver_MRR1,Format_Liver_MRR2)


Format_Liver_MRR[,2:4] <- as.numeric(unlist(Format_Liver_MRR[,2:4]))
Format_Liver_MRR[,2:4] <- round((Format_Liver_MRR[,2:4]), digits = 3)

Format_Liver_MRR$Brac <- c(")")

colnames(Format_Liver_MRR) <- c("Variables", "OR", "CI_Low", "CI_High", "P_value", "Brac") 

Format_Liver_MRR$ORCI <- paste(Format_Liver_MRR$OR, Format_Liver_MRR$CI_Low, sep=" (")
Format_Liver_MRR$ORCI <- paste(Format_Liver_MRR$ORCI, Format_Liver_MRR$CI_High, sep="-")
Format_Liver_MRR$ORCI <- paste(Format_Liver_MRR$ORCI, Format_Liver_MRR$Brac, sep="")

Format_Liver_MRR$OR <- NULL
Format_Liver_MRR$CI_Low <- NULL
Format_Liver_MRR$CI_High <- NULL
Format_Liver_MRR$Brac <- NULL

Format_Liver_MRR <- Format_Liver_MRR[, c(1, 3, 2)]

# Lung

Format_Lung_MRR <- Lung_Mv_Regression_Results

Format_Lung_MRR1 <- Format_Lung_MRR[(1:6),]
Format_Lung_MRR2 <- Format_Lung_MRR[(10:12),]
Format_Lung_MRR <- rbind(Format_Lung_MRR1,Format_Lung_MRR2)


Format_Lung_MRR[,2:4] <- as.numeric(unlist(Format_Lung_MRR[,2:4]))
Format_Lung_MRR[,2:4] <- round((Format_Lung_MRR[,2:4]), digits = 3)

Format_Lung_MRR$Brac <- c(")")

colnames(Format_Lung_MRR) <- c("Variables", "OR", "CI_Low", "CI_High", "P_value", "Brac") 

Format_Lung_MRR$ORCI <- paste(Format_Lung_MRR$OR, Format_Lung_MRR$CI_Low, sep=" (")
Format_Lung_MRR$ORCI <- paste(Format_Lung_MRR$ORCI, Format_Lung_MRR$CI_High, sep="-")
Format_Lung_MRR$ORCI <- paste(Format_Lung_MRR$ORCI, Format_Lung_MRR$Brac, sep="")

Format_Lung_MRR$OR <- NULL
Format_Lung_MRR$CI_Low <- NULL
Format_Lung_MRR$CI_High <- NULL
Format_Lung_MRR$Brac <- NULL

Format_Lung_MRR <- Format_Lung_MRR[, c(1, 3, 2)]

# Node

Format_Nodal_MRR <- Nodal_Mv_Regression_Results

Format_Nodal_MRR1 <- Format_Nodal_MRR[(1:6),]
Format_Nodal_MRR2 <- Format_Nodal_MRR[(10),]
Format_Nodal_MRR <- rbind(Format_Nodal_MRR1,Format_Nodal_MRR2)


Format_Nodal_MRR[,2:4] <- as.numeric(unlist(Format_Nodal_MRR[,2:4]))
Format_Nodal_MRR[,2:4] <- round((Format_Nodal_MRR[,2:4]), digits = 3)

Format_Nodal_MRR$Brac <- c(")")

colnames(Format_Nodal_MRR) <- c("Variables", "OR", "CI_Low", "CI_High", "P_value", "Brac") 

Format_Nodal_MRR$ORCI <- paste(Format_Nodal_MRR$OR, Format_Nodal_MRR$CI_Low, sep=" (")
Format_Nodal_MRR$ORCI <- paste(Format_Nodal_MRR$ORCI, Format_Nodal_MRR$CI_High, sep="-")
Format_Nodal_MRR$ORCI <- paste(Format_Nodal_MRR$ORCI, Format_Nodal_MRR$Brac, sep="")

Format_Nodal_MRR$OR <- NULL
Format_Nodal_MRR$CI_Low <- NULL
Format_Nodal_MRR$CI_High <- NULL
Format_Nodal_MRR$Brac <- NULL

Format_Nodal_MRR <- Format_Nodal_MRR[, c(1, 3, 2)]

# Peritoneum

Format_Peritoneal_MRR <- Peritoneal_Mv_Regression_Results

Format_Peritoneal_MRR1 <- Format_Peritoneal_MRR[(1:6),]
Format_Peritoneal_MRR2 <- Format_Peritoneal_MRR[(9:10),]
Format_Peritoneal_MRR <- rbind(Format_Peritoneal_MRR1,Format_Peritoneal_MRR2)


Format_Peritoneal_MRR[,2:4] <- as.numeric(unlist(Format_Peritoneal_MRR[,2:4]))
Format_Peritoneal_MRR[,2:4] <- round((Format_Peritoneal_MRR[,2:4]), digits = 3)

Format_Peritoneal_MRR$Brac <- c(")")

colnames(Format_Peritoneal_MRR) <- c("Variables", "OR", "CI_Low", "CI_High", "P_value", "Brac") 

Format_Peritoneal_MRR$ORCI <- paste(Format_Peritoneal_MRR$OR, Format_Peritoneal_MRR$CI_Low, sep=" (")
Format_Peritoneal_MRR$ORCI <- paste(Format_Peritoneal_MRR$ORCI, Format_Peritoneal_MRR$CI_High, sep="-")
Format_Peritoneal_MRR$ORCI <- paste(Format_Peritoneal_MRR$ORCI, Format_Peritoneal_MRR$Brac, sep="")

Format_Peritoneal_MRR$OR <- NULL
Format_Peritoneal_MRR$CI_Low <- NULL
Format_Peritoneal_MRR$CI_High <- NULL
Format_Peritoneal_MRR$Brac <- NULL

Format_Peritoneal_MRR <- Format_Peritoneal_MRR[, c(1, 3, 2)]


# Save formatted tables ----

write.csv(Format_Liver_MRR, "/Users/laurahudson/Documents/MV_Liver_Final_05.csv")
write.csv(Format_Lung_MRR, "/Users/laurahudson/Documents/MV_Lung_Final_05.csv")
write.csv(Format_Nodal_MRR, "/Users/laurahudson/Documents/MV_Node_Final_05.csv")
write.csv(Format_Peritoneal_MRR, "/Users/laurahudson/Documents/MV_Peritoneum_Final_05.csv")


# Test Format Tables with cohorts inc -----

# Liver

Liver_Mv_Regression_Results
Format_Liver_MRR <- Liver_Mv_Regression_Results

Format_Liver_MRR[,2:4] <- as.numeric(unlist(Format_Liver_MRR[,2:4]))
Format_Liver_MRR[,2:4] <- round((Format_Liver_MRR[,2:4]), digits = 3)

Format_Liver_MRR$Brac <- c(")")

colnames(Format_Liver_MRR) <- c("Variables", "OR", "CI_Low", "CI_High", "P_value", "Brac") 

Format_Liver_MRR$ORCI <- paste(Format_Liver_MRR$OR, Format_Liver_MRR$CI_Low, sep=" (")
Format_Liver_MRR$ORCI <- paste(Format_Liver_MRR$ORCI, Format_Liver_MRR$CI_High, sep="-")
Format_Liver_MRR$ORCI <- paste(Format_Liver_MRR$ORCI, Format_Liver_MRR$Brac, sep="")

Format_Liver_MRR$OR <- NULL
Format_Liver_MRR$CI_Low <- NULL
Format_Liver_MRR$CI_High <- NULL
Format_Liver_MRR$Brac <- NULL

Format_Liver_MRR <- Format_Liver_MRR[, c(1, 3, 2)]



# Lung

Format_Lung_MRR <- Lung_Mv_Regression_Results


Format_Lung_MRR[,2:4] <- as.numeric(unlist(Format_Lung_MRR[,2:4]))
Format_Lung_MRR[,2:4] <- round((Format_Lung_MRR[,2:4]), digits = 3)

Format_Lung_MRR$Brac <- c(")")

colnames(Format_Lung_MRR) <- c("Variables", "OR", "CI_Low", "CI_High", "P_value", "Brac") 

Format_Lung_MRR$ORCI <- paste(Format_Lung_MRR$OR, Format_Lung_MRR$CI_Low, sep=" (")
Format_Lung_MRR$ORCI <- paste(Format_Lung_MRR$ORCI, Format_Lung_MRR$CI_High, sep="-")
Format_Lung_MRR$ORCI <- paste(Format_Lung_MRR$ORCI, Format_Lung_MRR$Brac, sep="")

Format_Lung_MRR$OR <- NULL
Format_Lung_MRR$CI_Low <- NULL
Format_Lung_MRR$CI_High <- NULL
Format_Lung_MRR$Brac <- NULL

Format_Lung_MRR <- Format_Lung_MRR[, c(1, 3, 2)]

# Node

Format_Nodal_MRR <- Nodal_Mv_Regression_Results


Format_Nodal_MRR[,2:4] <- as.numeric(unlist(Format_Nodal_MRR[,2:4]))
Format_Nodal_MRR[,2:4] <- round((Format_Nodal_MRR[,2:4]), digits = 3)

Format_Nodal_MRR$Brac <- c(")")

colnames(Format_Nodal_MRR) <- c("Variables", "OR", "CI_Low", "CI_High", "P_value", "Brac") 

Format_Nodal_MRR$ORCI <- paste(Format_Nodal_MRR$OR, Format_Nodal_MRR$CI_Low, sep=" (")
Format_Nodal_MRR$ORCI <- paste(Format_Nodal_MRR$ORCI, Format_Nodal_MRR$CI_High, sep="-")
Format_Nodal_MRR$ORCI <- paste(Format_Nodal_MRR$ORCI, Format_Nodal_MRR$Brac, sep="")

Format_Nodal_MRR$OR <- NULL
Format_Nodal_MRR$CI_Low <- NULL
Format_Nodal_MRR$CI_High <- NULL
Format_Nodal_MRR$Brac <- NULL

Format_Nodal_MRR <- Format_Nodal_MRR[, c(1, 3, 2)]

# Peritoneum

Format_Peritoneal_MRR <- Peritoneal_Mv_Regression_Results



Format_Peritoneal_MRR[,2:4] <- as.numeric(unlist(Format_Peritoneal_MRR[,2:4]))
Format_Peritoneal_MRR[,2:4] <- round((Format_Peritoneal_MRR[,2:4]), digits = 3)

Format_Peritoneal_MRR$Brac <- c(")")

colnames(Format_Peritoneal_MRR) <- c("Variables", "OR", "CI_Low", "CI_High", "P_value", "Brac") 

Format_Peritoneal_MRR$ORCI <- paste(Format_Peritoneal_MRR$OR, Format_Peritoneal_MRR$CI_Low, sep=" (")
Format_Peritoneal_MRR$ORCI <- paste(Format_Peritoneal_MRR$ORCI, Format_Peritoneal_MRR$CI_High, sep="-")
Format_Peritoneal_MRR$ORCI <- paste(Format_Peritoneal_MRR$ORCI, Format_Peritoneal_MRR$Brac, sep="")

Format_Peritoneal_MRR$OR <- NULL
Format_Peritoneal_MRR$CI_Low <- NULL
Format_Peritoneal_MRR$CI_High <- NULL
Format_Peritoneal_MRR$Brac <- NULL

Format_Peritoneal_MRR <- Format_Peritoneal_MRR[, c(1, 3, 2)]


# Save formatted tables ----

write.csv(Format_Liver_MRR, "/Users/laurahudson/Documents/MV_Liver_Final_withCohorts.csv")
write.csv(Format_Lung_MRR, "/Users/laurahudson/Documents/MV_Lung_Final_withCohorts.csv")
write.csv(Format_Nodal_MRR, "/Users/laurahudson/Documents/MV_Node_Final_withCohorts.csv")
write.csv(Format_Peritoneal_MRR, "/Users/laurahudson/Documents/MV_Peritoneum_Final_withCohorts.csv")


# Select used samples from Final MV -----

#Liver
Final_Merge_Analysis$Used_Liver <- TRUE
Final_Merge_Analysis$Used_Liver[na.action(Model_Liver_Mv)] <- FALSE
table(Final_Merge_Analysis$Used_Liver)

#Lung
Final_Merge_Analysis$Used_Lung <- TRUE
Final_Merge_Analysis$Used_Lung[na.action(Model_Lung_Mv)] <- FALSE
table(Final_Merge_Analysis$Used_Lung)

#Node
Final_Merge_Analysis$Used_Node <- TRUE
Final_Merge_Analysis$Used_Node[na.action(Model_Nodal_Mv)] <- FALSE
table(Final_Merge_Analysis$Used_Node)

#Peritoneum
Final_Merge_Analysis$Used_Peritoneum <- TRUE
Final_Merge_Analysis$Used_Peritoneum[na.action(Model_Peritoneal_Mv)] <- FALSE
table(Final_Merge_Analysis$Used_Peritoneum)

# Save Files

FMV_Liver <- Final_Merge_Analysis[Final_Merge_Analysis$Used_Liver == T,]
FMV_Lung <- Final_Merge_Analysis[Final_Merge_Analysis$Used_Lung == T,]
FMV_Node <- Final_Merge_Analysis[Final_Merge_Analysis$Used_Node == T,]
FMV_Peritoneum <- Final_Merge_Analysis[Final_Merge_Analysis$Used_Peritoneum == T,]

write.csv(FMV_Liver, "/Users/laurahudson/Documents/FMV_Liver.csv")
write.csv(FMV_Lung, "/Users/laurahudson/Documents/FMV_Lung.csv")
write.csv(FMV_Node, "/Users/laurahudson/Documents/FMV_Node.csv")
write.csv(FMV_Peritoneum, "/Users/laurahudson/Documents/FMV_Peritoneum.csv")





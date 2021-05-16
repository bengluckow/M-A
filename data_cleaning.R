#Data cleaning for the M&A Datasets
library(dplyr)

#Downloading and combining files
m_a_2020_2021 <- read_excel("Desktop/Junior Year/ANN/Project/data/quantclub_2020_final.xlsx",skip = 1)
m_a_2015_2020 <- read_excel("Desktop/Junior Year/ANN/Project/data/quantclub_2015_2020_final.xlsx",skip = 1)
m_a_2005_2015 <- read_excel("Desktop/Junior Year/ANN/Project/data/qcnd_2005_2015_final.xlsx",skip = 1)
m_a_2000_2005 <- read_excel("Desktop/Junior Year/ANN/Project/data/qcnd_2000_2005.xlsx",skip = 1)
m_a_1997_2000 <- read_excel("Desktop/Junior Year/ANN/Project/data/qcnd_1997_2000.xlsx",skip = 1)
m_a_1993_1997 <- read_excel("Desktop/Junior Year/ANN/Project/data/qcnd_1993_1997.xlsx",skip = 1)
m_a_1990_1993 <- read_excel("Desktop/Junior Year/ANN/Project/data/qcnd_1990_1993.xlsx",skip = 1)
vol <- read.csv("Desktop/Junior Year/ANN/Project/data/volatility.csv")


m_a_final <- rbind(m_a_1990_1993,m_a_1993_1997,m_a_1997_2000)
m_a_final <- rbind(m_a_final,m_a_2000_2005)
colnames(m_a_2005_2015) <- colnames(m_a_final)
colnames(m_a_2015_2020) <- colnames(m_a_final)
colnames(m_a_2020_2021) <- colnames(m_a_final)
m_a_final <- rbind(m_a_final,m_a_2005_2015,m_a_2015_2020,m_a_2020_2021)

#write.csv(m_a_final,"Desktop/Junior Year/ANN/Project/data/m_a_final.csv")

#Renaming and Dropping Variables

m_a_NN <- subset(m_a_final,select = -c(2,3,4,6,7,9,18,19,21,26,30,31,32,37,38,39,42,43,49,50,53,55,56,57,58,66,67,69,72,73,77,78,79,80,81,82,83,85))
m_a_NN <- m_a_NN[m_a_NN$Status == 'Withdrawn' | m_a_NN$Status == 'Completed',]

colnames(m_a_NN) <- c("announced",'t_industry','a_industry','status','pct_acq','pct_owned_after','pct_sought','value_mil','rvndt',
                      'enterprise_value','equity_value','t_net_sales','ebit_12_mo','pre-tax_inc_12mo','net_inc_12mo','t_net_assets',
                      't_total_assets','t_ebitda_ltm','offer_eps_ratio','one_d_prem','one_w_prem','four_w_prem','spinoff','white_knight',
                      'lbo_firm','t_bankrupt','hf_involvement','challenged_deal','con_sought','con_offered','divestiture','defensive_tactics',
                      'syn_after_tax','a_financial','going_priv','liquidation','litigation','lbo','m_a_type','num_bidders','pct_cash','pct_other','pct_stock', 'privitization','pto','recap','a_pub_status'
                      )

#Steps Left to Complete for Project
  #1. Change Column Types (All Numeric) -- Done!

m_a_NN$same_industry <- ifelse(m_a_NN$t_industry == m_a_NN$a_industry,1,0) #flag for same industry.
m_a_NN <- subset(m_a_NN, select=-c(2,3))

m_a_NN$outcome <- ifelse(m_a_NN$status == 'Completed',1,0) #target variable. 1 if deal is successful, 0 otherwise
m_a_NN <- subset(m_a_NN, select=-c(2))

m_a_NN$pct_acq <- ifelse(is.na(m_a_NN$pct_acq),0,m_a_NN$pct_acq)
m_a_NN$pct_owned_after <- ifelse(is.na(m_a_NN$pct_owned_after),0,m_a_NN$pct_owned_after)
m_a_NN$pct_sought <- ifelse(is.na(m_a_NN$pct_sought),0,m_a_NN$pct_sought)

m_a_NN <- m_a_NN[!is.na(m_a_NN$value_mil),]
m_a_NN <- subset(m_a_NN, select=-c(6,7,8,9,10,11,12,13))

m_a_NN <- m_a_NN[!is.na(m_a_NN$t_total_assets),]
m_a_NN <- m_a_NN[!is.na(m_a_NN$one_d_prem),]

m_a_NN <- subset(m_a_NN, select=-c(7,8))

m_a_NN <- m_a_NN[!is.na(m_a_NN$one_w_prem),]
m_a_NN <- m_a_NN[!is.na(m_a_NN$four_w_prem),]

m_a_NN$spinoff <- ifelse(m_a_NN$spinoff=='Yes',1,0)
m_a_NN$white_knight <- ifelse(m_a_NN$white_knight=='Yes',1,0)
m_a_NN$lbo_firm <- ifelse(m_a_NN$lbo_firm=='Yes',1,0)
m_a_NN$hf_involvement <- ifelse(m_a_NN$hf_involvement=='Yes',1,0)
m_a_NN$challenged_deal <- ifelse(m_a_NN$challenged_deal=='Yes',1,0)
m_a_NN$t_bankrupt <- ifelse(m_a_NN$t_bankrupt=='Yes',1,0)
m_a_NN$divestiture <- ifelse(m_a_NN$divestiture=='Yes',1,0)
m_a_NN$defensive_tactics <- ifelse(m_a_NN$defensive_tactics!='Not Applicable',1,0)
m_a_NN$syn_after_tax <- ifelse(is.na(m_a_NN$syn_after_tax),0,m_a_NN$syn_after_tax)
m_a_NN$a_financial <- ifelse(m_a_NN$a_financial == "Yes",1,0)
m_a_NN$going_priv <- ifelse(m_a_NN$going_priv == "Yes",1,0)
m_a_NN$liquidation <- ifelse(m_a_NN$liquidation == "Yes",1,0)
m_a_NN$litigation <- ifelse(m_a_NN$litigation == "Yes",1,0)
m_a_NN$lbo <- ifelse(m_a_NN$lbo == "Yes",1,0)

m_a_NN <- subset(m_a_NN, select=-c(16,17,26))

m_a_NN$pct_cash <- ifelse(is.na(m_a_NN$pct_cash),0,m_a_NN$pct_cash)
m_a_NN$pct_other <- ifelse(is.na(m_a_NN$pct_other),0,m_a_NN$pct_other)
m_a_NN$pct_stock <- ifelse(is.na(m_a_NN$pct_stock),0,m_a_NN$pct_stock)

m_a_NN$privitization <- ifelse(m_a_NN$privitization == "Yes",1,0)
m_a_NN$pto <- ifelse(m_a_NN$pto == "Yes",1,0)
m_a_NN$recap <- ifelse(m_a_NN$recap == "Yes",1,0)
m_a_NN$a_pub_status <- ifelse(m_a_NN$a_pub_status == "Public",1,0)
  
  #2. Merge Volatility Data

m_a_NN$announced <- as.Date(m_a_NN$announced)
colnames(vol)[1] <- 'announced'
vol$announced <- as.Date(vol$announced)
final_data <- merge(m_a_NN, vol, by='announced')
final_data <- final_data[!(final_data$VIXCLS == '.'),]
final_data$VIXCLS <- as.numeric(final_data$VIXCLS)

#Need to fix data leakages: get rid of pct_acq, pct_owned_after
final_data <- subset(final_data,select=-c(2,3))
final_data <- final_data[final_data$pct_sought>0,]
final_data <- final_data[final_data$pct_cash + final_data$pct_other + final_data$pct_stock > 0,]


write.csv(final_data,"Desktop/Junior Year/ANN/Project/data/clean_m&a.csv")


model <-  lm(outcome ~ ., data = final_data[1:10000,])
summary(model)

predictions <- predict(model,final_data[10001:14997,])

predictions[predictions > .5] <-  1
predictions[predictions < .5] <-  0
sum(abs(predictions - final_data$outcome[10001:14997]))


Ytest <- final_data$outcome[10001:14997]
withdrawn <-Ytest[Ytest == 0]
predicted_withdrawn <- predictions[Ytest == 0]

sum(abs(withdrawn-predicted_withdrawn))


install.packages('corrplot')
install.packages('caret')


library(corrplot)
library(MASS)
library(tidyverse)
library(caret)
#1.load data
data = read.csv('human-freedom-index-data.csv')
data[is.na(data)] = 0  # NA = 0

#2.data analysis parts- correlation of independent variables. In this step, consider collinearity and minimize data
pf_rol = data[, c('pf_rol_procedural', 'pf_rol_civil', 'pf_rol_criminal', 'pf_rol')]
cor(pf_rol)
corrplot(cor(pf_rol))

pf_ss = data[, c('pf_ss_homicide', 'pf_ss_disappearances_disap','pf_ss_disappearances_violent','pf_ss_disappearances_organized','pf_ss_disappearances_fatalities','pf_ss_disappearances_injuries','pf_ss_disappearances','pf_ss_women_fgm','pf_ss_women_missing','pf_ss_women_inheritance_widows','pf_ss_women_inheritance_daughters' ,'pf_ss_women_inheritance','pf_ss_women','pf_ss')]
cor(pf_ss)
corrplot(cor(pf_ss))

pf_movement = data[, c('pf_movement_domestic', 'pf_movement_foreign','pf_movement_women','pf_movement')]
cor(pf_movement)
corrplot(cor(pf_movement))

pf_religion = data[, c('pf_religion_estop_establish', 'pf_religion_estop_operate','pf_religion_estop','pf_religion_harassment','pf_religion_restrictions','pf_religion')]
cor(pf_religion)
corrplot(cor(pf_religion))

pf_association = data[,  c('pf_association_association', 'pf_association_assembly','pf_association_political_establish','pf_association_political_operate','pf_association_political','pf_association_prof_establish','pf_association_prof_operate','pf_association_prof','pf_association_sport_establish')]
cor(pf_association)
corrplot(cor(pf_association))

pf_expression = data[, c('pf_expression_killed', 'pf_expression_jailed','pf_expression_influence','pf_expression_control','pf_expression_cable','pf_expression_newspapers','pf_expression_internet','pf_expression')]
cor(pf_expression)
corrplot(cor(pf_expression))

pf_identity = data[, c('pf_identity_legal', 'pf_identity_parental_marriage','pf_identity_parental_divorce','pf_identity_parental','pf_identity_sex_male','pf_identity_sex_female','pf_identity_sex','pf_identity_divorce','pf_identity')]
corrplot(cor(pf_identity))

ef_government = data[, c('ef_government_consumption', 'ef_government_transfers','ef_government_enterprises','ef_government_tax_income','ef_government_tax_payroll','ef_government_tax','ef_government')]
cor(ef_government)
corrplot(cor(ef_government))

ef_legal = data[, c('ef_legal_courts', 'ef_legal_protection','ef_legal_military','ef_legal_integrity','ef_legal_enforcement','ef_legal_restrictions','ef_legal_police','ef_legal_crime','ef_legal_gender','ef_legal')]
cor(ef_legal)
corrplot(cor(ef_legal))

ef_money = data[, c('ef_money_growth', 'ef_money_sd','ef_money_inflation','ef_money_currency','ef_money')]
cor(ef_money)
corrplot(cor(ef_money))

ef_trade = data[, c('ef_trade_tariffs_revenue', 'ef_trade_tariffs_mean','ef_trade_tariffs_sd','ef_trade_tariffs','ef_trade_regulatory_nontariff','ef_trade_regulatory_compliance','ef_trade_regulatory','ef_trade_black','ef_trade_movement_foreign','ef_trade_movement_capital','ef_trade_movement_visit','ef_trade_movement','ef_trade')]
cor(ef_trade)
corrplot(cor(ef_trade))

ef_regulation = data[, c('ef_regulation_business_adm', 'ef_regulation_business_bureaucracy','ef_regulation_business_start','ef_regulation_business_bribes','ef_regulation_business_licensing','ef_regulation_business_compliance','ef_regulation_business','ef_regulation','ef_regulation_credit_ownership', 'ef_regulation_credit_private','ef_regulation_credit_interest','ef_regulation_credit','ef_regulation_labor_minwage','ef_regulation_labor_firing','ef_regulation_labor_bargain','ef_regulation_labor_hours','ef_regulation_labor_dismissal','ef_regulation_labor_conscription','ef_regulation_labor')]
cor(ef_regulation)
corrplot(cor(ef_regulation))

total = data[, c('hf_score','hf_rank','hf_quartile','pf_rol','pf_ss','pf_movement','pf_religion','pf_association','pf_expression','pf_identity','pf_score','pf_rank','ef_government','ef_legal','ef_money','ef_trade','ef_regulation','ef_score','ef_rank')]
cor(total)
corrplot(cor(total))

hist(log(data$pf_rol), main = "Histogram of log(Pf_rol)", xlab="log(pf_rol)", ylab="Frequency", freq = TRUE)
hist(log(data$pf_ss), main = "Histogram of log(pf_ss)", xlab="log(pf_ss)", ylab="Frequency", freq = TRUE)
hist(log(data$pf_movement), main = "Histogram of log(pf_movement)", xlab="log(pf_movement)", ylab="Frequency", freq = TRUE)
hist(log(data$pf_religion), main = "Histogram of log(pf_religion)", xlab="log(pf_religion)", ylab="Frequency", freq = TRUE)
hist(log(data$pf_association), main = "Histogram of log(pf_association)", xlab="log(pf_association)", ylab="Frequency", freq = TRUE)
hist(log(data$pf_expression), main = "Histogram of log(pf_expression)", xlab="log(pf_expression)", ylab="Frequency", freq = TRUE)
hist(log(data$pf_identity), main = "Histogram of log(pf_identity)", xlab="log(pf_identity)", ylab="Frequency", freq = TRUE)
hist(log(data$pf_score), main = "Histogram of log(pf_score)", xlab="log(pf_score)", ylab="Frequency", freq = TRUE)
hist(log(data$pf_rank), main = "Histogram of log(pf_rank)", xlab="log(pf_rank)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_government), main = "Histogram of log(ef_government)", xlab="log(ef_government)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_legal), main = "Histogram of log(ef_legal)", xlab="log(ef_legal)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_money), main = "Histogram of log(ef_money)", xlab="log(ef_money)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_trade), main = "Histogram of log(ef_trade)", xlab="log(ef_trade)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_money), main = "Histogram of log(ef_money)", xlab="log(ef_money)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_trade), main = "Histogram of log(ef_trade)", xlab="log(ef_trade)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_regulation), main = "Histogram of log(ef_regulation)", xlab="log(ef_regulation)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_score), main = "Histogram of log(ef_score)", xlab="log(ef_score)", ylab="Frequency", freq = TRUE)
hist(log(data$ef_rank), main = "Histogram of log(ef_rank)", xlab="log(ef_rank)", ylab="Frequency", freq = TRUE)

qqnorm(data$hf_score); qqline(data$hf_score)
qqnorm(data$hf_rank); qqline(data$hf_rank)
qqnorm(data$hf_quartile); qqline(data$hf_quartile)

#data.ggpairs.colnumbers = c()
#ggpairs(data, columns = data.ggpairs.colnumbers)

    #minimize data
data['pf_ss_women_inheritance_widows'] = (data['pf_ss_women_inheritance_widows'] + data['pf_ss_women_inheritance_daughters']) / 2
data['pf_rol_civil'] = (data['pf_rol_procedural'] + data['pf_rol_civil'] + data['pf_rol_criminal']) / 3
data['pf_ss_disappearances'] = (data['pf_ss_disappearances'] + data['pf_ss_disappearances_fatalities'] + data['pf_ss_disappearances_injuries'] ) / 3
data['pf_religion_estop_operate'] = (data['pf_religion_estop_operate'] + data['pf_religion_estop_establish']) / 2
data['pf_religion'] = (data['pf_religion'] + data['pf_religion_harassment']) / 2
data['pf_association_association'] = (data['pf_association_association'] + data['pf_association_assembly']) / 2
data['pf_expression_newspapers'] = (data['pf_expression_newspapers'] + data['pf_expression_cable']) / 2
data['pf_identity_parental_marriage'] = (data['pf_identity_parental_marriage'] + data['pf_identity_parental_divorce']) / 2
data['pf_identity_sex'] = (data['pf_identity_sex'] + data['pf_identity_sex_male'] + data['pf_identity_sex_female']) / 3
data['ef_government_tax'] = (data['ef_government_tax'] + data['ef_government_tax_income']) / 2
data['ef_trade_regulatory'] = (data['ef_trade_regulatory'] + data['ef_trade_regulatory_compliance']) / 2



#3.partition data(train and test = 7:3)

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
train <- train[c(names(train)[6:124])]
test <- data[ind==2,]
test <- test[c(names(test)[6:124])]

#4. Results(get Models)

hf_score_fullModel = lm(hf_score ~ .- hf_rank - hf_quartile - pf_ss_women_inheritance_daughters - pf_rol_civil - pf_rol_criminal - pf_ss_disappearances_injuries - pf_ss_disappearances_fatalities - pf_religion_estop_establish - pf_religion_harassment - pf_association_assembly - pf_expression_cable - pf_identity_parental_divorce - pf_identity_sex_male - pf_identity_sex_female - ef_government_tax_income - ef_trade_regulatory_compliance  ,train )
summary(hf_score_fullModel)

 #final hf_score model
hf_score_reducedModel = lm(hf_score ~ pf_score + ef_score, train)
summary(hf_score_reducedModel)

hf_rank_fullModel = lm(hf_rank ~ .- hf_score - hf_quartile - pf_ss_women_inheritance_daughters - pf_rol_civil - pf_rol_criminal - pf_ss_disappearances_injuries - pf_ss_disappearances_fatalities - pf_religion_estop_establish - pf_religion_harassment - pf_association_assembly - pf_expression_cable - pf_identity_parental_divorce - pf_identity_sex_male - pf_identity_sex_female - ef_government_tax_income - ef_trade_regulatory_compliance   ,train )
summary(hf_rank_fullModel)
 #final hf_rank model
hf_rank_reducedModel = lm(hf_rank ~ pf_ss_homicide + pf_ss_disappearances_violent + pf_ss_women + pf_ss + pf_movement + pf_rank + pf_score + ef_government_enterprises + ef_legal_integrity + ef_legal_restrictions + ef_money_sd + ef_money_currency + ef_trade_black + ef_regulation_business_bureaucracy + ef_rank, train)
summary(hf_rank_reducedModel)

source('fixed-polr.R')
train$hf_quartile = as.factor(train$hf_quartile)
hf_quartilefullModel <- polr(hf_quartile ~ .- hf_score - hf_rank - pf_ss_women_inheritance_daughters - pf_rol_civil - pf_rol_criminal - pf_ss_disappearances_injuries - pf_ss_disappearances_fatalities - pf_religion_estop_establish - pf_religion_harassment - pf_association_assembly - pf_expression_cable - pf_identity_parental_divorce - pf_identity_sex_male - pf_identity_sex_female - ef_government_tax_income - ef_trade_regulatory_compliance ,train, Hess = TRUE)
summary(hf_quartilefullModel)
  #p-value calculation
(ctable = coef(summary(hf_quartilefullModel)))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctabel = cbind(ctable, "p value" =  p))
  #final hf_quartile model
hf_quartilereducedModel <- polr(hf_quartile ~ pf_rank + ef_government + ef_regulation + ef_score + ef_rank, data = train, Hess = TRUE )
summary(hf_quartilereducedModel)
(ctable = coef(summary(hf_quartilereducedModel)))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctabel = cbind(ctable, "p value" =  p))

#5. Testing the models.
  # normal  Test method.
hf_score_predict = predict(hf_score_reducedModel, test)  
data.frame( R2 = R2(hf_score_predict, test$hf_score),      # R-squard : the higher, the better model
            RMSE = RMSE(hf_score_predict, test$hf_score), # Root Mean Squared Error : the lower, the better
            MAE = MAE(hf_score_predict, test$hf_score))    #Mean Absolute Error : the lower, the better.

hf_rank_predict = predict(hf_rank_reducedModel, test)
data.frame( R2 = R2(hf_rank_predict, test$hf_rank),
            RMSE = RMSE(hf_rank_predict, test$hf_rank),
            MAE = MAE(hf_rank_predict, test$hf_rank)) 
  
hf_quartile_predict = predict(hf_quartilereducedModel, test)
print(hf_quartile_predict, digits = 1)

(tab = table(hf_quartile_predict, test$hf_quartile))
#accuray: 
sum(diag(tab)) / sum(tab)


  
   # Cross Validation Test
set.seed(123)
train.control = trainControl(method = "cv", number = 10)
hf_scoreModel = train(hf_score ~ pf_score + ef_score + ef_regulation_business_start, data = data, method = "lm", trControl = train.control)
print(hf_scoreModel)

train.control = trainControl(method = "cv", number = 10)
hf_rankModel = train(hf_rank ~ pf_ss_homicide + pf_ss_disappearances_violent + pf_ss_women + pf_ss + pf_movement + pf_religion_estop_operate + pf_religion_estop + pf_association_political_establish + pf_association_political + pf_expression_jailed + pf_identity_sex + pf_rank + pf_score + ef_government_enterprises + ef_legal_integrity + ef_legal_restrictions + ef_money_sd + ef_money_currency + ef_trade_black + ef_regulation_business_bureaucracy + ef_rank, data = data, method = "lm", trControl = train.control)
print(hf_rankModel)


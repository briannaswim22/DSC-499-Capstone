#Brianna Johnson
#DSC 499 Capstone
#Detecting Medical Errors in the Cancer Screening Process
#Last Updated: 04/17/2021

#Load Entire Dataset
lungdata <- read.csv("/Users/briannajohnson/Desktop/LungData.csv")

#Load Dataset with only Lung Cancer Patients 
lungcancer <- read.csv("/Users/briannajohnson/Desktop/LungCancer.csv")

#Create subset with just factors being used for EDA - Histograms
mortality <- data.frame(lungcancer$dth_days, lungcancer$lung_stage, lungcancer$cig_stat, lungcancer$age, lungcancer$mortality_exitage)


#Eliminate Small Cell Lung Cancer - genetic factor
smoking <- subset(lungcancer, lung_stage <= 500)

#Create Subset for nonsmokers, current, and former smokers
nonsmoker1 <- subset(smoking, cig_stat <= 0)
current_smoker1 <- subset(smoking, cig_stat >= 1 & cig_stat < 2)
former_smoker1 <- subset(smoking, cig_stat >= 2)

#Plot days lived for all three types, by cancer stage to identify disparities
plot(nonsmoker1$dth_days, nonsmoker1$lung_stage)
plot(current_smoker1$dth_days, current_smoker1$lung_stage)
plot(former_smoker1$dth_days, former_smoker1$lung_stage)

#Create subset just to analyze death days & type of smoker
nonsmoker <- subset(mortality, lungcancer$cig_stat <= 0)
stage1 <- subset(mortality, lungcancer$lung_stage <200)
stage2 <- subset(mortality, lungcancer$lung_stage >= 200 & lungcancer$lung_stage <300)
stage3 <- subset(mortality, lungcancer$lung_stage >= 300 & lungcancer$lung_stage <400)
stage4 <- subset(mortality, lungcancer$lung_stage >= 400 & lungcancer$lung_stage <500)
smallcell <- subset(mortality, lungcancer$lung_stage >500)
current_smoker <- subset(mortality, lungcancer$cig_stat >= 1 & lungcancer$cig_stat < 2)
former_smoker <- subset(mortality, lungcancer$cig_stat >= 2)
smoker <- subset(mortality, lungcancer$cig_stat > 0)

#Plot Overlapping Histogram to analyze differences among smokers
histnon <- hist(nonsmoker$lungcancer.dth_days)
histcurrentsmoker <- hist(current_smoker$lungcancer.dth_days)
histformer <- hist(former_smoker$lungcancer.dth_days)
histsmoker <- hist(smoker$lungcancer.dth_days)
plot(histsmoker, col = "purple", main = "Histogram of Days Lived by Smoking Status", xlab = "# of Days Lived")
plot(histformer, col = "gray", add = TRUE)
plot(histcurrentsmoker, col = "red", add = TRUE)
plot(histnon, col = "light blue", add = TRUE)
legend(5100, 325, legend = c("History of Smoking", "Former Smoker", "Current Smoker", "NonSmoker"), col = c("purple", "gray", "red", "light blue"), pch = c(19), pt.cex = 1.5, cex = 0.75)

#Create Histogram of Death Days by Cancer Stage
stage_1 <- hist(stage1$lungcancer.dth_days)
stage_2 <- hist(stage2$lungcancer.dth_days)
stage_3 <- hist(stage3$lungcancer.dth_days)
stage_4 <- hist(stage4$lungcancer.dth_days)

#Plot Results
plot(stage_4, col = "light blue", main = "Histogram of Days Lived by Cancer Stage", xlab = "# of Days Lived")
plot(stage_1, col = "purple", add = TRUE)
plot(stage_3, col = "red", add = TRUE)
plot(stage_2, col = "gray", add = TRUE)
legend(6000, 150, legend = c("Stage I", "Stage II", "Stage III", "Stage IV"), col = c("purple", "gray", "red", "light blue"), pch = c(19), pt.cex = 1.5, cex = 0.75)

#Create Histogram of Age at Diagnosis by Cancer Stage
stage.1 <- hist(stage1$lungcancer.age)
stage.2 <- hist(stage2$lungcancer.age)
stage.3 <- hist(stage3$lungcancer.age)
stage.4 <- hist(stage4$lungcancer.age)

#Plot Results
plot(stage.4, col = "light blue", main = "Histogram of Age at Diagnosis per Cancer Stage", xlab = "# of Days Lived")
plot(stage.1, col = "purple", add = TRUE)
plot(stage.3, col = "red", add = TRUE)
plot(stage.2, col = "gray", add = TRUE)
legend(70, 170, legend = c("Stage I", "Stage II", "Stage III", "Stage IV"), col = c("purple", "gray", "red", "light blue"), pch = c(19), pt.cex = 1.5, cex = 0.75)

#Create Histogram of Age at Death, by Cancer Stage
stage1_ <- hist(stage1$lungcancer.mortality_exitage)
stage2_ <- hist(stage2$lungcancer.mortality_exitage)
stage3_ <- hist(stage3$lungcancer.mortality_exitage)
stage4_ <- hist(stage4$lungcancer.mortality_exitage)

#Plot Results
plot(stage4_, col = "light blue", main = "Age Died by Cancer Stage", xlab = "# of Days Lived")
plot(stage1_, col = "purple", add = TRUE)
plot(stage3_, col = "red", add = TRUE)
plot(stage2_, col = "gray", add = TRUE)
legend(57, 300, legend = c("Stage I", "Stage II", "Stage III", "Stage IV"), col = c("purple", "gray", "red", "light blue"), pch = c(19), pt.cex = 1.5, cex = 0.75)

non_smoker1 <- data.frame(nonsmoker1$educat, nonsmoker1$marital, nonsmoker1$occupat, nonsmoker1$sisters, nonsmoker1$brothers, nonsmoker1$race7, nonsmoker1$state, nonsmoker1$fh_cancer, nonsmoker1$lung_fh, nonsmoker1$lung_fh_cnt, nonsmoker1$bmi_curr, nonsmoker1$asp, nonsmoker1$ibup, nonsmoker1$dth_days, nonsmoker1$lung_stage)
colnames(non_smoker1) <- c("Education", "Marital", "Occupation", "Sisters", "Brothers", "Race", "State", "Family History Cancer", "FH Lung Cancer", "# of Lung Cancer in Family", "Current BMI", "Aspirin", "Ibuprofen", "Death Days", "Stage")
ns_health <- data.frame(nonsmoker1$arthrit_f, nonsmoker1$bronchit_f, nonsmoker1$colon_comorbidity, nonsmoker1$diabetes_f, nonsmoker1$divertic_f, nonsmoker1$emphys_f, nonsmoker1$gallblad_f, nonsmoker1$hearta_f, nonsmoker1$hyperten_f, nonsmoker1$liver_comorbidity, nonsmoker1$osteopor_f, nonsmoker1$polyps_f, nonsmoker1$stroke_f, nonsmoker1$dth_days, nonsmoker1$lung_stage)
colnames(ns_health) <- c("Arthritis", "Bronchitis", "Colon", "Diabetes", "Diverticulitis", "Emphys", "GallBladder", "Heart Attack", "Hypertension", "Liver", "Osteoparosis", "Polyps", "Stroke", "Death Days", "Stage")
ns_female <- data.frame(nonsmoker1$hyster_f, nonsmoker1$hystera, nonsmoker1$ovariesr_f, nonsmoker1$tuballig, nonsmoker1$bcontr_f, nonsmoker1$bcontra, nonsmoker1$bcontrt, nonsmoker1$curhorm, nonsmoker1$horm_f, nonsmoker1$horm_stat, nonsmoker1$thorm, nonsmoker1$fchilda, nonsmoker1$livec, nonsmoker1$miscar, nonsmoker1$preg_f, nonsmoker1$prega, nonsmoker1$pregc, nonsmoker1$stillb, nonsmoker1$trypreg, nonsmoker1$tubal, nonsmoker1$fmenstr, nonsmoker1$lmenstr, nonsmoker1$menstrs, nonsmoker1$menstrs_stat_type, nonsmoker1$post_menopausal, nonsmoker1$bbd, nonsmoker1$benign_ovcyst, nonsmoker1$endometriosis, nonsmoker1$uterine_fib, nonsmoker1$dth_days, nonsmoker1$lung_stage)
colnames(ns_female) <- c("Hysterectomy", "Age of Hyst", "Removed Ovaries", "Tubes Tied", "Birth Control", "Age Start BC", "# Years on BC", "Use Hormones", "Taken Hormones", "Hormone Status", "# Years on Hormones", "Age at First Child", "# of Live Births", "# Miscarriage", "Ever Pregnant?", "Age First Pregnant", "# Pregnancies", "# Still Birth", "Tried Pregnancy", "# Tubal Pregnancies", "Age of First Period", "Age at Menopause", "Type of Menopause", "Reason for Menopause", "BBD", "Benign Cyst", "Endometriosis", "Fibroid Tumours", "Death Days", "Stage")
ns_male <- data.frame(nonsmoker1$enlpros_f, nonsmoker1$enlprosa, nonsmoker1$infpros_f, nonsmoker1$infprosa, nonsmoker1$prosprob_f, nonsmoker1$urinate_f, nonsmoker1$urinatea, nonsmoker1$surg_age, nonsmoker1$surg_any, nonsmoker1$surg_biopsy, nonsmoker1$surg_prostatectomy, nonsmoker1$surg_resection, nonsmoker1$vasect_f, nonsmoker1$vasecta, nonsmoker1$dth_days, nonsmoker1$lung_stage)
colnames(ns_male) <- c("Enlarged Prostate", "Age of EP", "Inflamed Prostate", "Age of IP", "Problems with Prostate", "Urinate Issues", "Age of Urinate Issues", "Age at Prostate Surgery", "Prostate Surgery", "Biopsy of Prostate", "Prostatectomy", "Resection", "Vesectomy", "Age at Vesectomy", "Death Days", "Stage")

corMatrix <- cor(non_smoker1, use = "pairwise.complete.obs")
corrplot.mixed(corMatrix, number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")

ns_health <- na.omit(ns_health)
corHealth <- cor(ns_health)
corrplot.mixed(corHealth, number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")

corMatrix <- cor(c_environmental, use = "pairwise.complete.obs")
corrplot.mixed(corMatrix, number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")

corHealth <- cor(c_health, use = "pairwise.complete.obs")
corrplot.mixed(corHealth, number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")

corFemale <- cor(c_female, use = "pairwise.complete.obs")
corrplot.mixed(corFemale, number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")

corMale <- cor(c_male, use = "pairwise.complete.obs")
corrplot.mixed(corMale, number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")

#######
#######
#######
#######
#Logistic Regression (Machine Learning Classification Algorithm)

lungdata <- read.csv("/Users/briannajohnson/Desktop/LungData.csv")

#Define the 27 Attributes being used
logistic <- data.frame(lungdata$educat, lungdata$marital, lungdata$occupat, lungdata$sisters, lungdata$brothers, lungdata$race7, lungdata$state, lungdata$fh_cancer, lungdata$lung_fh, lungdata$lung_fh_cnt, lungdata$bmi_curr, lungdata$asp, lungdata$ibup, lungdata$arthrit_f, lungdata$bronchit_f, lungdata$colon_comorbidity, lungdata$diabetes_f, lungdata$divertic_f, lungdata$emphys_f, lungdata$gallblad_f, lungdata$hearta_f, lungdata$hyperten_f, lungdata$liver_comorbidity, lungdata$osteopor_f, lungdata$polyps_f, lungdata$stroke_f, lungdata$lung_cancer)
head(logistic)

#Omit null data
logistic <- na.omit(logistic)

#Divide data into those with cancer and those without
cancer <- subset(logistic, lungdata.lung_cancer >= 1 )
non_cancer <- subset(logistic, lungdata.lung_cancer < 1)

#Create a representative ratio of the American Population
cancer <- cancer[sample(1:nrow(cancer), 3308),]
non_cancer <- non_cancer[sample(1:nrow(non_cancer), 46311),]

#Train 70% of data, test 30%
s_cancer <- sample(1:nrow(cancer), 0.7*nrow(cancer), replace = TRUE)
s_non_cancer <- sample(1:nrow(non_cancer), 0.7*nrow(non_cancer), replace = TRUE)

training <- rbind(cancer[s_cancer,],non_cancer[s_non_cancer,])
test <- rbind(cancer[-s_cancer,], non_cancer[-s_non_cancer,])
logistic <- rbind(cancer, non_cancer)

#indices <- sample(1:nrow(logistic), 0.7*nrow(logistic), replace = TRUE)
#training <- logistic[indices,]
#test <- logistic[-indices,]

library(ggplot2)
library(caret)
library("gower")
library("generics")
library("Rcpp")
library("ModelMetrics")
library("pROC")

summary(logistic)
#Set seed
Random.seed <- c('Mersenne-Twister', 1)
set.seed(1)

#Create Logistic Regression
mylogit <- glm(lungdata.lung_cancer ~ lungdata.educat + lungdata.marital + lungdata.occupat + lungdata.sisters + lungdata.brothers + lungdata.race7 + lungdata.state + lungdata.fh_cancer + lungdata.lung_fh + lungdata.lung_fh_cnt + lungdata.bmi_curr + lungdata.asp + lungdata.ibup + lungdata.arthrit_f + lungdata.bronchit_f + lungdata.colon_comorbidity + lungdata.diabetes_f + lungdata.divertic_f + lungdata.emphys_f + lungdata.gallblad_f + lungdata.hearta_f + lungdata.hyperten_f + lungdata.liver_comorbidity + lungdata.osteopor_f + lungdata.polyps_f + lungdata.stroke_f, data = logistic, family = "binomial")
summary(mylogit)

#Checking model fit
logLik(mylogit) #-14989.83

with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#6.164094e-261 - model fits better than empty model

#Removing nonsignificant params from Logistic Regression
newlogit <- glm(lungdata.lung_cancer ~ lungdata.educat + lungdata.marital + lungdata.occupat + lungdata.race7 + lungdata.lung_fh + lungdata.lung_fh_cnt + lungdata.bmi_curr + lungdata.asp + lungdata.ibup + lungdata.bronchit_f + lungdata.emphys_f + lungdata.gallblad_f + lungdata.hearta_f + lungdata.osteopor_f + lungdata.polyps_f, data = training, family = "binomial")
summary(newlogit)

logit_prob <- predict(newlogit, test, type = "response")
logit_prob[1:50]

#Compare the models before and after removing non significant params
summary(logit_prob)
summary(log_prob)

log_prob <- predict(mylogit, test, type = "response")

#Create Histogram of Probabliites
histProb1 <- ggplot(mapping = aes(log_prob)) + geom_histogram(bins=30, color = "darkblue", fill = "lightblue") + ggtitle("Probability Distribution")
histProb1

histProb <- ggplot(mapping = aes(logit_prob)) + geom_histogram(bins=30, color = "darkblue", fill = "lightblue") + ggtitle("Probability Distribution")
histProb

#Generate Confusion Matrix with Threshold of .5
pred <- ifelse(logit_prob > 0.5, 1, 0)
pred.table <- table(pred, test$lungdata.lung_cancer)
pred.table

#Generate Confusion Matrix with Threshold of .15
pred1 <- ifelse(logit_prob > 0.15, 1, 0)
pred1.table <- table(pred1, test$lungdata.lung_cancer)
pred1.table

#Calculate Accuracy
table1.trace <- sum(diag(pred1.table))
table1.sum <- sum(pred1.table)
acc1 <- table1.trace / table1.sum
acc1

table.trace <- sum(diag(pred.table))
table.sum <- sum(pred.table)
acc <- table.trace / table.sum
acc

err <- 1 - acc1
err

sens <- pred1.table[4]/(pred1.table[4] + pred1.table[3])
sens

spec <- pred1.table[1]/(pred1.table[1] + pred1.table[2])
spec

PPV <- pred1.table[4]/(pred1.table[2] + pred1.table[4])
PPV

NPV <- pred1.table[1] / (pred1.table[1] + pred1.table[3])
NPV

#Generate AUC / ROC Curve
test_prob <- predict(newlogit, newdata = test, type = "response")
test_roc <- roc(test$lungdata.lung_cancer, test_prob)
plot.roc(test_roc, col=par("fg"), print.auc=FALSE, legacy.axes=TRUE, asp=NA)
plot.roc(smooth(test_roc), col="blue", add=TRUE, print.auc=TRUE, legacy.axes=TRUE, asp=NA)
legend("bottomright", legend=c("Empirical", "Smoothed"), col=c(par("fg"), "blue"), lwd=2)

abline(v = -coef(newlogit)[1] / coef(newlogit)[2], lwd=3)

########
########
########
########
#CART Model

lungdata <- read.csv("/Users/briannajohnson/Desktop/LungData.csv")

#Defining our CART Model with 27 attributes
cart <- data.frame(lungdata$educat, lungdata$marital, lungdata$sisters, lungdata$brothers, lungdata$race7, lungdata$state, lungdata$fh_cancer, lungdata$lung_fh, lungdata$lung_fh_cnt, lungdata$bmi_curr, lungdata$asp, lungdata$ibup, lungdata$arthrit_f, lungdata$bronchit_f, lungdata$colon_comorbidity, lungdata$diabetes_f, lungdata$divertic_f, lungdata$emphys_f, lungdata$gallblad_f, lungdata$hearta_f, lungdata$hyperten_f, lungdata$liver_comorbidity, lungdata$osteopor_f, lungdata$polyps_f, lungdata$stroke_f, lungdata$lung_cancer)
head(cart)

#Omit Unused Data
cart <- na.omit(cart)
head(cart)

#Load Packages
library(rpart)
library(rpart.plot)

#Make data similar to Logistic Regression - representative of overall population
cancer <- subset(logistic, lungdata.lung_cancer >= 1 )
non_cancer <- subset(logistic, lungdata.lung_cancer < 1)

cancer <- cancer[sample(1:nrow(cancer), 3308),]
non_cancer <- non_cancer[sample(1:nrow(non_cancer), 46311),]

#Training on 70% of data, testing on 30%
s_cancer <- sample(1:nrow(cancer), 0.7*nrow(cancer), replace = TRUE)
s_non_cancer <- sample(1:nrow(non_cancer), 0.7*nrow(non_cancer), replace = TRUE)

#Redefine Column names so CART Models are easier to read
cart.train <- rbind(cancer[s_cancer,],non_cancer[s_non_cancer,])
cart.train
colnames(cart.train) <- c("Education", "Marital", "Occupation", "Sisters", "Brothers", "Race", "State", "FamilyHistoryCancer", "FHLungCancer", "FamilyLC", "BMI", "Aspirin", "Ibuprofen", "Arthritis", "Bronchitis", "ColonComorbidity", "Diabetes", "Diverticulitis", "Emphysema", "GallBladder", "HeartAttack", "Hypertension", "LiverComorbidity", "Osteoporosis", "Polyps", "Stroke", "LungCancer")

cart.test <- rbind(cancer[-s_cancer,], non_cancer[-s_non_cancer,])
colnames(cart.test) <- c("Education", "Marital", "Occupation", "Sisters", "Brothers", "Race", "State", "FamilyHistoryCancer", "FHLungCancer", "FamilyLC", "BMI", "Aspirin", "Ibuprofen", "Arthritis", "Bronchitis", "ColonComorbidity", "Diabetes", "Diverticulitis", "Emphysema", "GallBladder", "HeartAttack", "Hypertension", "LiverComorbidity", "Osteoporosis", "Polyps", "Stroke", "LungCancer")
cart.train

#Set Seed
set.seed(1)

#Create CART of all attributes
class.cart <- rpart(formula = LungCancer ~ Education + Marital + Sisters + Brothers + Race + State + FamilyHistoryCancer + FHLungCancer + FamilyLC + BMI + Aspirin + Ibuprofen + Arthritis + Bronchitis + ColonComorbidity + Diabetes + Diverticulitis + Emphysema + GallBladder + HeartAttack + Hypertension + LiverComorbidity + Osteoporosis + Polyps + Stroke, cart.train,control = rpart.control(minsplit=2, minbucket = 2, cp = 0.001))
class.cart
prp(class.cart, roundint = FALSE)

#Identify most important params
cp.param <- class.cart$cptable
cp.param

#Change Double Value to Results of cp.param
train.mse <- double(18)
cv.mse <- double(18)
test.mse <- double(18)

for (i in 1:18) {
  alpha <- cp.param[i, 'CP']
  train.mse[i] <- mean((cart.train$LungCancer - predict(prune(class.cart, cp=alpha
  ), newdata = cart.train))^2)
  cv.mse[i] <- cp.param[i, 'xerror'] * cp.param[i, 'rel error']
  test.mse[i] <- mean((cart.test$LungCancer - predict(prune(class.cart, cp=alpha),
                                                      newdata = cart.test))^2)
}
matplot(cp.param[,'nsplit'], cbind(train.mse, cv.mse, test.mse), pch=19, col=
          c("red", "black", "blue"), type="b", ylab="Mean Squared Error", xlab="# of Sp
lits")
legend("right", c('Train', 'CV', 'Test') ,col=seq_len(3),cex=0.8,fill=c("red"
                                                                        , "black", "blue"))

#Recreate CART Model, but with only 10 params included
prune.class.trees <- prune(class.cart, cp=cp.param[10,'CP'])
prp(prune.class.trees)

#Redefine Lung Cancer as Factor to create Confusion Matrix
cart.train$LungCancer <- factor(cart.train$LungCancer)
cart.test$LungCancer <- factor(cart.test$LungCancer)

#Generate Confusion Matrix with same threshold as Logistic Regression
pred = predict(prune.class.trees, type = "vector")
pred1 <- ifelse(pred > 0.15, 1, 0)
pred1.table <- table(pred1, cart.train$LungCancer)
pred1.table

#Confusion Matrix with original CART Model
pred2 = predict(class.cart, type = "vector")
pred2 <- ifelse(pred2 > 0.15, 1, 0)
pred2.table <- table(pred2, cart.train$LungCancer)
pred2.table

#Calculate Accuracy of Model
table1.trace <- sum(diag(pred1.table))
table1.sum <- sum(pred1.table)
acc1 <- table1.trace / table1.sum
acc1

err <- 1 - acc1
err

sens <- pred1.table[4]/(pred1.table[4] + pred1.table[3])
sens

spec <- pred1.table[1]/(pred1.table[1] + pred1.table[2])
spec

PPV <- pred1.table[4]/(pred1.table[2] + pred1.table[4])
PPV

NPV <- pred1.table[1] / (pred1.table[1] + pred1.table[3])
NPV

#Generate AUC / ROC Curve
test_prob <- predict(prune.class.trees, type = "vector")
test_roc <- roc(cart.train$LungCancer, test_prob)
plot.roc(test_roc, col=par("fg"), print.auc=FALSE, legacy.axes=TRUE, asp=NA)
plot.roc(smooth(test_roc), col="blue", add=TRUE, print.auc=TRUE, legacy.axes=TRUE, asp=NA)
legend("bottomright", legend=c("Empirical", "Smoothed"), col=c(par("fg"), "blue"), lwd=2)

abline(v = -coef(newlogit)[1] / coef(newlogit)[2], lwd=3)

######
######
######
#Random Forest

library(randomForest)
rf <- data.frame(lungdata$educat, lungdata$marital, lungdata$occupat, lungdata$sisters, lungdata$brothers, lungdata$race7, lungdata$state, lungdata$fh_cancer, lungdata$lung_fh, lungdata$lung_fh_cnt, lungdata$bmi_curr, lungdata$asp, lungdata$ibup, lungdata$arthrit_f, lungdata$bronchit_f, lungdata$colon_comorbidity, lungdata$diabetes_f, lungdata$divertic_f, lungdata$emphys_f, lungdata$gallblad_f, lungdata$hearta_f, lungdata$hyperten_f, lungdata$liver_comorbidity, lungdata$osteopor_f, lungdata$polyps_f, lungdata$stroke_f, lungdata$lung_cancer)
head(rf)
rf <- na.omit(rf)
head(rf)

#Split Data into Cancer and NonCancer
cancer <- subset(rf, lungdata.lung_cancer >= 1 )
non_cancer <- subset(rf, lungdata.lung_cancer < 1)

#Make data representative of American Population
cancer <- cancer[sample(1:nrow(cancer), 3308),]
non_cancer <- non_cancer[sample(1:nrow(non_cancer), 46311),]

#Test/Train Data Split 30/70
s_cancer <- sample(1:nrow(cancer), 0.7*nrow(cancer), replace = TRUE)
s_non_cancer <- sample(1:nrow(non_cancer), 0.7*nrow(non_cancer), replace = TRUE)

#Define Train data and redefine Columns
rf.train <- rbind(cancer[s_cancer,],non_cancer[s_non_cancer,])
colnames(rf.train) <- c("Education", "Marital", "Occupation", "Sisters", "Brothers", "Race", "State", "FamilyHistoryCancer", "FHLungCancer", "LungCancerinFamily", "BMI", "Aspirin", "Ibuprofen", "Arthritis", "Bronchitis", "ColonComorbidity", "Diabetes", "Diverticulitis", "Emphysema", "GallBladder", "HeartAttack", "Hypertension", "LiverComorbidity", "Osteoporosis", "Polyps", "Stroke", "LungCancer")
rf.train$LungCancer <- as.factor(rf.train$LungCancer)

#Define Testing data and redefine Columns
rf.test <- rbind(cancer[-s_cancer,], non_cancer[-s_non_cancer,])
rf.test$LungCancer <- as.factor(rf.test$LungCancer)
colnames(rf.test) <- c("Education", "Marital", "Occupation", "Sisters", "Brothers", "Race", "State", "FamilyHistoryCancer", "FHLungCancer", "LungCancerinFamily", "BMI", "Aspirin", "Ibuprofen", "Arthritis", "Bronchitis", "ColonComorbidity", "Diabetes", "Diverticulitis", "Emphysema", "GallBladder", "HeartAttack", "Hypertension", "LiverComorbidity", "Osteoporosis", "Polyps", "Stroke", "LungCancer")

#Set Seed
set.seed(1)

#Generate Random Forest
rf.lung <- randomForest(LungCancer ~ ., data = rf.train, importance = TRUE)
pred.rf <- predict(rf.lung, rf.train, type = "class")
pred.test <- predict(rf.lung, rf.test, type = "class")
calc_error_rate <- function(predicted.value, true.value)
{return(mean(true.value!=predicted.value))}
rf.error <-  calc_error_rate(predicted.value = pred.rf, true.value = rf.train$LungCancer)
test.error <- calc_error_rate(predicted.value = pred.test, true.value = rf.test$LungCancer)

pred.rf
rf.error
test.error

#Generate Random Forest w/ Specifications
rf.class <- randomForest(LungCancer~., data=rf.train, mtry=round(sqrt(ncol(rf.train)-1)), importance=TRUE, xtest=test[,-26], ytest=factor(rf.test$LungCancer))

#Generate Random Forest Graph
plot(rf.class, col=c("red", "black", "blue"), main = "Random Forest Class")
legend(400, 0.4, colnames(rf.class$err.rate) ,col=seq_len(3),cex=0.8,
       fill=c("red", "black", "blue"))

#Calculate OOB
p <- ncol(rf.train) - 1
oob.error.class <- double(p)
set.seed(1)

for(m in 1:p){
  fit <- randomForest(LungCancer ~ ., data = rf.train, mtry=m, ntree = 100)
  conf.mat <- fit$err.rate[100]
  oob.error.class[m] <- fit$err.rate[100, 'OOB']
}

#Plot Missclassification Error
matplot(1:p, oob.error.class, pch = 19, col = "red", type = "b", ylab = "Misclassification Error", xlab="mtry")


rf.reg <- randomForest(LungCancer~., data=rf.train, mtry=round(sqrt(ncol(rf.train) -1)), importance = TRUE)
importance(rf.reg)
#Plot Variance Importance Plot
varImpPlot(rf.reg, cex=0.5, main = "Random Forest Regression")

#Create Boosting Model
#install.packages("gbm")
library(gbm)
set.seed(1)

#Redefine Lung Cancer as a Yes or No Attribute
LungCancer.0.1 <- ifelse(rf.train$LungCancer == 1, 1, 0)

#Generate Class Model
class.boost = gbm(LungCancer.0.1 ~. - LungCancer, data = rf.train, n.trees = 5000, distribution = "bernoulli", shrinkage = 0.01)
summary(class.boost)



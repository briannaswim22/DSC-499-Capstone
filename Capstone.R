#lungdata <- read.csv("/Users/briannajohnson/Desktop/LungData.csv")
lungcancer <- read.csv("/Users/briannajohnson/Desktop/LungCancer.csv")
mortality <- data.frame(lungcancer$dth_days, lungcancer$lung_stage, lungcancer$cig_stat)
plot(mortality)
mortality

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
stage2 <- subset(mortality, lungcancer$lung_stage <= 200 & lungcancer$lung_stage <300)
stage3 <- subset(mortality, lungcancer$lung_stage <= 300 & lungcancer$lung_stage <400)
stage4 <- subset(mortality, lungcancer$lung_stage <= 400 & lungcancer$lung_stage <500)
smallcell <- subset(mortality, lungcancer$lung_stage >500)
current_smoker <- subset(mortality, lungcancer$cig_stat >= 1 & lungcancer$cig_stat < 2)
former_smoker <- subset(mortality, lungcancer$cig_stat >= 2)
smoker <- subset(mortality, lungcancer$cig_stat > 0)

#Plot Overlapping Histogram to analyze differences among smokers
histnon <- hist(nonsmoker$lungcancer.dth_days)
histcurrentsmoker <- hist(current_smoker$lungcancer.dth_days)
histformer <- hist(former_smoker$lungcancer.dth_days)
histsmoker <- hist(smoker$lungcancer.dth_days)
plot(histsmoker, col = "purple")
plot(histformer, col = "gray", add = TRUE)
plot(histcurrentsmoker, col = "red", add = TRUE)
plot(histnon, col = "light blue", add = TRUE)
legend(5100, 325, legend = c("Smoker", "Former Smoker", "Current Smoker", "NonSmoker"), col = c("purple", "gray", "red", "light blue"), pch = c(19), pt.cex = 1.5, cex = 0.75)


#
plot(hist(stage1$lungcancer.dth_days), col = "purple")
plot(hist(stage2$lungcancer.dth_days), col = "gray", add = TRUE)
plot(hist(stage3$lungcancer.dth_days), col = "red", add = TRUE)
plot(hist(stage4$lungcancer.dth_days), col = "light blue", add = TRUE)
plot(hist(smallcell$lungcancer.dth_days),col = "orange", add = TRUE)

#Plot individual histograms
library(ggplot2)
#install.packages("corrplot")
library(corrplot)
histDeath <- ggplot(nonsmoker, aes(lungcancer.dth_days)) + geom_histogram(bins=30, color = "darkblue", fill = "lightblue") + ggtitle("Days Unitl Death Distribution")
histDeath
smokerDeath <- ggplot(smoker, aes(lungcancer.dth_days)) + geom_histogram(bins=30, color = "darkblue", fill = "lightblue") + ggtitle("Days Unitl Death Distribution")
smokerDeath
currentDeath <- ggplot(current_smoker, aes(dth_days)) + geom_histogram(bins=30, color = "darkblue", fill = "lightblue") + ggtitle("Days Unitl Death Distribution")
formerDeath <- ggplot(former_smoker, aes(lungcancer.dth_days)) + geom_histogram(bins=30, color = "darkblue", fill = "lightblue") + ggtitle("Days Unitl Death Distribution")
currentDeath
formerDeath
nonsmoker

non_smoker1 <- data.frame(nonsmoker1$educat, nonsmoker1$marital, nonsmoker1$occupat, nonsmoker1$sisters, nonsmoker1$brothers, nonsmoker1$race7, nonsmoker1$state, nonsmoker1$fh_cancer, nonsmoker1$lung_fh, nonsmoker1$lung_fh_cnt, nonsmoker1$bmi_curr, nonsmoker1$asp, nonsmoker1$ibup, nonsmoker1$dth_days, nonsmoker1$lung_stage)
colnames(non_smoker1) <- c("Education", "Marital", "Occupation", "Sisters", "Brothers", "Race", "State", "Family History Cancer", "FH Lung Cancer", "# of Lung Cancer in Family", "Current BMI", "Aspirin", "Ibuprofen", "Death Days", "Stage")
ns_health <- data.frame(nonsmoker1$arthrit_f, nonsmoker1$bronchit_f, nonsmoker1$colon_comorbidity, nonsmoker1$diabetes_f, nonsmoker1$divertic_f, nonsmoker1$emphys_f, nonsmoker1$gallblad_f, nonsmoker1$hearta_f, nonsmoker1$hyperten_f, nonsmoker1$liver_comorbidity, nonsmoker1$osteopor_f, nonsmoker1$polyps_f, nonsmoker1$stroke_f, nonsmoker1$dth_days, nonsmoker1$lung_stage)
colnames(ns_health) <- c("Arthritis", "Bronchitis", "Colon", "Diabetes", "Diverticulitis", "Emphys", "GallBladder", "Heart Attack", "Hypertension", "Liver", "Osteoparosis", "Polyps", "Stroke", "Death Days", "Stage")
ns_female <- data.frame(nonsmoker1$hyster_f, nonsmoker1$hystera, nonsmoker1$ovariesr_f, nonsmoker1$tuballig, nonsmoker1$bcontr_f, nonsmoker1$bcontra, nonsmoker1$bcontrt, nonsmoker1$curhorm, nonsmoker1$horm_f, nonsmoker1$horm_stat, nonsmoker1$thorm, nonsmoker1$fchilda, nonsmoker1$livec, nonsmoker1$miscar, nonsmoker1$preg_f, nonsmoker1$prega, nonsmoker1$pregc, nonsmoker1$stillb, nonsmoker1$trypreg, nonsmoker1$tubal, nonsmoker1$fmenstr, nonsmoker1$lmenstr, nonsmoker1$menstrs, nonsmoker1$menstrs_stat_type, nonsmoker1$post_menopausal, nonsmoker1$bbd, nonsmoker1$benign_ovcyst, nonsmoker1$endometriosis, nonsmoker1$uterine_fib, nonsmoker1$dth_days, nonsmoker1$lung_stage)
colnames(ns_female) <- c("Hysterectomy", "Age of Hyst", "Removed Ovaries", "Tubes Tied", "Birth Control", "Age Start BC", "# Years on BC", "Use Hormones", "Taken Hormones", "Hormone Status", "# Years on Hormones", "Age at First Child", "# of Live Births", "# Miscarriage", "Ever Pregnant?", "Age First Pregnant", "# Pregnancies", "# Still Birth", "Tried Pregnancy", "# Tubal Pregnancies", "Age of First Period", "Age at Menopause", "Type of Menopause", "Reason for Menopause", "BBD", "Benign Cyst", "Endometriosis", "Fibroid Tumours", "Death Days", "Stage")
ns_male <- data.frame(nonsmoker1$enlpros_f, nonsmoker1$enlprosa, nonsmoker1$infpros_f, nonsmoker1$infprosa, nonsmoker1$prosprob_f, nonsmoker1$urinate_f, nonsmoker1$urinatea, nonsmoker1$surg_age, nonsmoker1$surg_any, nonsmoker1$surg_biopsy, nonsmoker1$surg_prostatectomy, nonsmoker1$surg_resection, nonsmoker1$vasect_f, nonsmoker1$vasecta, nonsmoker1$dth_days, nonsmoker1$lung_stage)
colnames(ns_male) <- c("Enlarged Prostate", "Age of EP", "Inflamed Prostate", "Age of IP", "Problems with Prostate", "Urinate Issues", "Age of Urinate Issues", "Age at Prostate Surgery", "Prostate Surgery", "Biopsy of Prostate", "Prostatectomy", "Resection", "Vesectomy", "Age at Vesectomy", "Death Days", "Stage")

ns_health
non_smoker1 <- na.omit(non_smoker1)
ns_health <- na.omit(ns_health)
ns_female <- na.omit(ns_female)
ns.male <- na.omit(ns_male)

corMatrix <- cor(non_smoker1)
corrplot.mixed(corMatrix, number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")
corrplot.mixed(cor(ns_health), number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")
corrplot.mixed(cor(ns_female), number.cex= 9/ncol(data),tl.cex= 9/ncol(data),lower.col = "black")

plot(smallcell$lungcancer.dth_days)

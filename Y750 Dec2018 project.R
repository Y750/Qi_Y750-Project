library("mice")
library("psych")
library("dplyr")
library("summarytools")
library("gmodels")
library("lavaan")

#################################### MISSING DATA ##########################################
### note: the code I used here for missing data imputation are mostly borrowed from: 
# https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/

# Descriptives
describeBy(c16rp,group= "GENDER")

# Check the presence of missings
sapply(c16rp, function(x) sum(is.na(x)))

# Check frequency distributions
# this needs summarytools package
freq(c16rp$ar6)

# missing data - transform the variables in factors (categorical) or numeric (continuous). 
# this needs the dplyr package

dat <- c16rp[,1:24] # select Situations items and PersonID variables
head(dat)

sapply(dat, function(x) sum(is.na(x)))

dat <- dat %>%
  mutate(
    CIP = as.factor(CIP),
    ACAREA = as.factor(ACAREA),
    YEARHIRE = as.factor(YEARHIRE),
    RANK = as.factor(RANK),
    TENURE = as.factor(TENURE),
    CITIZEN = as.factor(CITIZEN),
    RACE = as.factor(RACE),
    GENDER = as.factor(GENDER),
    RACE = as.factor(RACE),
    AGE = as.numeric(AGE),
    SCHOOL1 = as.factor(SCHOOL1),
    ADMIN = as.factor(ADMIN),
    ar1 = as.factor(ar1),
    ar2= as.factor(ar2),
    ar3= as.factor(ar3),
    ar4= as.factor(ar4),
    ar5= as.factor(ar5),
    ar6= as.factor(ar6),
    ar7= as.factor(ar7)
      )

# Look the dataset structure.
str(dat)

# proceed with imputation; needs mice package

init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# To impute the missing values, mice package use an algorithm in a such a way 
# that use information from other variables in the dataset to predict and impute the missing values. 
# Therefore, you may not want to use a certain variable as predictors. 
# The code below will remove the variable as a predictor but still will be imputed.

predM[, c("ID")]=0
predM[, c("SCHOOL")]=0
predM[, c("AGE")]=0
predM[, c("RT")]=0
predM[, c("RT2")]=0
predM[, c("PGRADE")]=0

# If you want to skip a variable from imputation use the code below. 
# NOTE:This variable will be used for prediction unless excluded in the previous step.

meth[c("AGE")]=""
meth[c("ADMIN")]=""
meth[c("ADMIN2")]=""

# Now let specify the methods for imputing the missing values. 
# There are specific methods for continuous, binary and ordinal variables. 
# e.g., meth[c("Cholesterol")]="norm" # continuous
# e.g., meth[c("Smoking")]="logreg" # binary

meth[c("ar1")]="polyreg"
meth[c("ar2")]="polyreg"
meth[c("ar3")]="polyreg"
meth[c("ar4")]="polyreg"
meth[c("ar5")]="polyreg"
meth[c("ar6")]="polyreg"
meth[c("ar7")]="polyreg"

# run the multiple (m=5) imputation.
set.seed(123)
imputed1 = mice(dat, method=meth, predictorMatrix=predM, m=5)

# Create a dataset after imputation.
imputed1 <- complete (imputed1)
write.csv (imputed1,"C:/Users/helen/Google Drive/_R/_Project/DATA/imputed1.csv")

# Check for missings in the imputed dataset.
sapply(imputed1, function(x) sum(is.na(x)))

############## IMPUTATION repeated for the other constructs, ommitted here ##############


############################### MEASUREMENT INVARIANCE ######################################

# NOTE: the codes below are adapted from Dr. Rutkowski's Y750 lab and Dr. Templin's online lecture (https://jonathantemplin.com/)

#recode group variable
c16rp1$GENDER[which(c16rp1$GENDER==1)] = "female"
c16rp1$GENDER[which(c16rp1$GENDER==0)] = "male"

############    Configural Invariance Model   ############

# everything is estimated separately across groups except for parameters needed to be constrained for identification.

# note: for the three-indicator model, factor loading of the first variable was fixed to 1 for both groups to avoide the degree of freedom =0

initial = "
# Factor loadings (AKA slopes/discriminations) all freely estimated in both groups with label for each group
AR =~ c(1,1)*ar1 + c(L2M, L2F)*ar2 + c(L3M, L3F)*ar4 
DP =~ c(1,1)*ar7 + c(L5M, L5F)*dpc4 + c(L6M, L6F)*dpc7 

IS =~ c(1, 1)*nwr5 + c(L8M, L8F)*nwr10 + c(L9M, L9F)*nwr11 
+ c(L10M,L10F)*nws2 + c(L11M, L11F)*nws3 + c(L12M, L12F)*nws6 
+ c(L13M, L13F)*nws7 + c(L14M, L14F)*nws8 + c(L15M, L15F)*nws9 
+ c(L16M, L16F)*nwt1 + c(L17M, L17F)*nwt2 + c(L18M, L18F)*nwt7 

# Item Thresholds all freely estimated in both groups with label for each group
ar1|c(I1T1M, I1T1F)*t1; ar1|c(I1T2M, I1T2F)*t2; ar1|c(I1T3M, I1T3F)*t3;ar1|c(I1T4M, I1T4F)*t4;
ar2|c(I2T1M, I2T1F)*t1; ar2|c(I2T2M, I2T2F)*t2; ar2|c(I2T3M, I2T3F)*t3;ar2|c(I2T4M, I2T4F)*t4;
ar4|c(I3T1M, I3T1F)*t1; ar4|c(I3T2M, I3T2F)*t2; ar4|c(I3T3M, I3T3F)*t3;ar4|c(I3T4M, I3T4F)*t4;

ar7|c(I4T1M, I4T1F)*t1; ar7|c(I4T2M, I4T2F)*t2; ar7|c(I4T3M, I4T3F)*t3;ar7|c(I4T4M, I4T4F)*t4;
dpc4|c(I5T1M, I5T1F)*t1; dpc4|c(I5T2M, I5T2F)*t2; dpc4|c(I5T3M, I5T3F)*t3;dpc4|c(I5T4M, I5T4F)*t4;
dpc7|c(I6T1M, I6T1F)*t1; dpc7|c(I6T2M, I6T2F)*t2; dpc7|c(I6T3M, I6T3F)*t3;dpc7|c(I6T4M, I6T4F)*t4;

nwr5|c(I7T1M, I7T1F)*t1; nwr5|c(I7T2M, I7T2F)*t2; nwr5|c(I7T3M, I7T3F)*t3;nwr5|c(I7T4M, I7T4F)*t4;
nwr10|c(I8T1M, I8T1F)*t1; nwr10|c(I8T2M, I8T2F)*t2; nwr10|c(I8T3M, I8T3F)*t3;nwr10|c(I8T4M, I8T4F)*t4;
nwr11|c(I9T1M, I9T1F)*t1; nwr11|c(I9T2M, I9T2F)*t2; nwr11|c(I9T3M, I9T3F)*t3;nwr11|c(I9T4M, I9T4F)*t4;

nws2|c(I10T1M, I10T1F)*t1; nws2|c(I10T2M, I10T2F)*t2; nws2|c(I10T3M, I10T3F)*t3;nws2|c(I10T4M, I10T4F)*t4;
nws3|c(I11T1M, I11T1F)*t1; nws3|c(I11T2M, I11T2F)*t2; nws3|c(I11T3M, I11T3F)*t3;nws3|c(I11T4M, I11T4F)*t4;
nws6|c(I12T1M, I12T1F)*t1; nws6|c(I12T2M, I12T2F)*t2; nws6|c(I12T3M, I12T3F)*t3;nws6|c(I12T4M, I12T4F)*t4;

nws7|c(I13T1M, I13T1F)*t1; nws7|c(I13T2M, I13T2F)*t2; nws7|c(I13T3M, I13T3F)*t3;nws7|c(I13T4M, I13T4F)*t4;
nws8|c(I14T1M, I14T1F)*t1; nws8|c(I14T2M, I14T2F)*t2; nws8|c(I14T3M, I14T3F)*t3;nws8|c(I14T4M, I14T4F)*t4;
nws9|c(I15T1M, I15T1F)*t1; nws9|c(I15T2M, I15T2F)*t2; nws9|c(I15T3M, I15T3F)*t3;nws9|c(I15T4M, I15T4F)*t4;

nwt1|c(I16T1M, I16T1F)*t1; nwt1|c(I16T2M, I16T2F)*t2; nwt1|c(I16T3M, I16T3F)*t3;nwt1|c(I16T4M, I16T4F)*t4;
nwt2|c(I17T1M, I17T1F)*t1; nwt2|c(I17T2M, I17T2F)*t2; nwt2|c(I17T3M, I17T3F)*t3;nwt2|c(I17T4M, I17T4F)*t4;
nwt7|c(I18T1M, I18T1F)*t1; nwt7|c(I18T2M, I18T2F)*t2; nwt7|c(I18T3M, I18T3F)*t3;nwt7|c(I18T4M, I18T4F)*t4;


#Redidual variances all fixed to one for each group
ar1 ~~ c(1, 1)*ar1; ar2 ~~ c(1, 1)*ar2; ar4 ~~ c(1, 1)*ar4; 
ar7 ~~ c(1, 1)*ar7; dpc4 ~~ c(1, 1)*dpc4; dpc7 ~~ c(1, 1)*dpc7;

nwr5 ~~ c(1, 1)*nwr5; nwr10 ~~ c(1, 1)*nwr10; nwr11 ~~ c(1, 1)*nwr11; 
nws2 ~~ c(1, 1)*nws2; nws3 ~~ c(1, 1)*nws3; nws6 ~~ c(1, 1)*nws6; 
nws7 ~~ c(1, 1)*nws7; nws8 ~~ c(1, 1)*nws8; nws9 ~~ c(1, 1)*nws9; 
nwt1 ~~ c(1, 1)*nwt1; nwt2 ~~ c(1, 1)*nwt2; nwt7 ~~ c(1, 1)*nwt7; 

#Factor variance fixed to 1 for identification in each group
AR ~~ c(1,1)*AR;
DP ~~ c(1,1)*DP;
IS ~~ c(1,1)*IS; 

#Factor mean fixed to zero for identification in each group

AR ~ c(0,0)*0 
DP ~ c(0,0)*0 
IS ~ c(0,0)*0 

#Regression
AR ~ IS + DP
DP ~ IS"

initialEstimates = lavaan(model = initial, data = c16rp1,
                          ordered = c("ar1", "ar2", "ar4", "ar7", "dpc4", "dpc7",
                                      "nwr5", "nwr10", "nwr11", "nws2", "nws3", "nws6", 
                                      "nws7", "nws8", "nws9", "nwt1", "nwt2", "nwt7")
                          , std.lv = TRUE, parameterization="theta", group = "GENDER")

summary(initialEstimates, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
fitmeasures(initialEstimates, fit.measures = c("chisq", "df", "pvalue", "CFI", "RMSEA", "srmr"))

############################## REVISED  CONFIGURAL MODEL #####################################

configural = "
# Factor loadings (AKA slopes/discriminations) all freely estimated in both groups with label for each group
AR =~ c(1,1)*ar1 + c(L2M, L2F)*ar2 + c(L3M, L3F)*ar4 
DP =~ c(1,1)*ar7 + c(L5M, L5F)*dpc4 + c(L6M, L6F)*dpc7 

IS =~ c(1, 1)*nwr5 + c(L8M, L8F)*nwr10 + c(L9M, L9F)*nwr11 
+ c(L10M,L10F)*nws2 + c(L11M, L11F)*nws3 + c(L12M, L12F)*nws6 
+ c(L13M, L13F)*nws7 + c(L14M, L14F)*nws8 + c(L15M, L15F)*nws9 
+ c(L16M, L16F)*nwt1 + c(L17M, L17F)*nwt2 + c(L18M, L18F)*nwt7 

dpc4~~dpc7  
nws6~~nws9
nws6~~nwt7
nws9~~nwt7

# Item Thresholds all freely estimated in both groups with label for each group
ar1|c(I1T1M, I1T1F)*t1; ar1|c(I1T2M, I1T2F)*t2; ar1|c(I1T3M, I1T3F)*t3;ar1|c(I1T4M, I1T4F)*t4;
ar2|c(I2T1M, I2T1F)*t1; ar2|c(I2T2M, I2T2F)*t2; ar2|c(I2T3M, I2T3F)*t3;ar2|c(I2T4M, I2T4F)*t4;
ar4|c(I3T1M, I3T1F)*t1; ar4|c(I3T2M, I3T2F)*t2; ar4|c(I3T3M, I3T3F)*t3;ar4|c(I3T4M, I3T4F)*t4;

ar7|c(I4T1M, I4T1F)*t1; ar7|c(I4T2M, I4T2F)*t2; ar7|c(I4T3M, I4T3F)*t3;ar7|c(I4T4M, I4T4F)*t4;
dpc4|c(I5T1M, I5T1F)*t1; dpc4|c(I5T2M, I5T2F)*t2; dpc4|c(I5T3M, I5T3F)*t3;dpc4|c(I5T4M, I5T4F)*t4;
dpc7|c(I6T1M, I6T1F)*t1; dpc7|c(I6T2M, I6T2F)*t2; dpc7|c(I6T3M, I6T3F)*t3;dpc7|c(I6T4M, I6T4F)*t4;

nwr5|c(I7T1M, I7T1F)*t1; nwr5|c(I7T2M, I7T2F)*t2; nwr5|c(I7T3M, I7T3F)*t3;nwr5|c(I7T4M, I7T4F)*t4;
nwr10|c(I8T1M, I8T1F)*t1; nwr10|c(I8T2M, I8T2F)*t2; nwr10|c(I8T3M, I8T3F)*t3;nwr10|c(I8T4M, I8T4F)*t4;
nwr11|c(I9T1M, I9T1F)*t1; nwr11|c(I9T2M, I9T2F)*t2; nwr11|c(I9T3M, I9T3F)*t3;nwr11|c(I9T4M, I9T4F)*t4;

nws2|c(I10T1M, I10T1F)*t1; nws2|c(I10T2M, I10T2F)*t2; nws2|c(I10T3M, I10T3F)*t3;nws2|c(I10T4M, I10T4F)*t4;
nws3|c(I11T1M, I11T1F)*t1; nws3|c(I11T2M, I11T2F)*t2; nws3|c(I11T3M, I11T3F)*t3;nws3|c(I11T4M, I11T4F)*t4;
nws6|c(I12T1M, I12T1F)*t1; nws6|c(I12T2M, I12T2F)*t2; nws6|c(I12T3M, I12T3F)*t3;nws6|c(I12T4M, I12T4F)*t4;

nws7|c(I13T1M, I13T1F)*t1; nws7|c(I13T2M, I13T2F)*t2; nws7|c(I13T3M, I13T3F)*t3;nws7|c(I13T4M, I13T4F)*t4;
nws8|c(I14T1M, I14T1F)*t1; nws8|c(I14T2M, I14T2F)*t2; nws8|c(I14T3M, I14T3F)*t3;nws8|c(I14T4M, I14T4F)*t4;
nws9|c(I15T1M, I15T1F)*t1; nws9|c(I15T2M, I15T2F)*t2; nws9|c(I15T3M, I15T3F)*t3;nws9|c(I15T4M, I15T4F)*t4;

nwt1|c(I16T1M, I16T1F)*t1; nwt1|c(I16T2M, I16T2F)*t2; nwt1|c(I16T3M, I16T3F)*t3;nwt1|c(I16T4M, I16T4F)*t4;
nwt2|c(I17T1M, I17T1F)*t1; nwt2|c(I17T2M, I17T2F)*t2; nwt2|c(I17T3M, I17T3F)*t3;nwt2|c(I17T4M, I17T4F)*t4;
nwt7|c(I18T1M, I18T1F)*t1; nwt7|c(I18T2M, I18T2F)*t2; nwt7|c(I18T3M, I18T3F)*t3;nwt7|c(I18T4M, I18T4F)*t4;


#Redidual variances all fixed to one for each group
ar1 ~~ c(1, 1)*ar1; ar2 ~~ c(1, 1)*ar2; ar4 ~~ c(1, 1)*ar4; 
ar7 ~~ c(1, 1)*ar7; dpc4 ~~ c(1, 1)*dpc4; dpc7 ~~ c(1, 1)*dpc7;

nwr5 ~~ c(1, 1)*nwr5; nwr10 ~~ c(1, 1)*nwr10; nwr11 ~~ c(1, 1)*nwr11; 
nws2 ~~ c(1, 1)*nws2; nws3 ~~ c(1, 1)*nws3; nws6 ~~ c(1, 1)*nws6; 
nws7 ~~ c(1, 1)*nws7; nws8 ~~ c(1, 1)*nws8; nws9 ~~ c(1, 1)*nws9; 
nwt1 ~~ c(1, 1)*nwt1; nwt2 ~~ c(1, 1)*nwt2; nwt7 ~~ c(1, 1)*nwt7; 

#Factor variance fixed to 1 for identification in each group
AR ~~ c(1,NA)*AR;
DP ~~ c(1,NA)*DP;
IS ~~ c(1,NA)*IS; 

#Factor mean fixed to zero for identification in each group

AR ~ c(0,0)*0 
DP ~ c(0,0)*0 
IS ~ c(0,0)*0 

#Regression
AR ~ IS + DP
DP ~ IS"

configuralEstimates = lavaan(model = configural, data = c16rp1,
                             ordered = c("ar1", "ar2", "ar4", "ar7", "dpc42", "dpc7",
                                         "nwr5", "nwr10", "nwr11", "nws2", "nws3", "nws6", 
                                         "nws7", "nws8", "nws9", "nwt1", "nwt2", "nwt7")
                             , std.lv = TRUE, parameterization="theta", group = "GENDER")

summary(configuralEstimates, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
fitmeasures(configuralEstimates, fit.measures = c("chisq", "df", "pvalue", "CFI", "RMSEA", "srmr"))

############################# METRIC INVARIANCE MODEL ########################

# Metric Invariance Models
# Here the IFA factor loadings are held equal across groups but the factor variance is freed.

metric = "
#Factor loadings (AKA slopes/discriminations) all constriaind across both groups with same label for both groups *****
AR =~ c(1,1)*ar1 + c(L2, L2)*ar2 + c(L3, L3)*ar4 
DP =~ c(1,1)*ar7 + c(L5, L5)*dpc4 + c(L6, L6)*dpc7 

IS =~ c(1, 1)*nwr5 + c(L8, L8)*nwr10 + c(L9, L9)*nwr11 
+ c(L10,L10)*nws2 + c(L11, L11)*nws3 + c(L12, L12)*nws6 
+ c(L13, L13F)*nws7 + c(L14, L14)*nws8 + c(L15, L15)*nws9 
+ c(L16, L16)*nwt1 + c(L17, L17)*nwt2 + c(L18, L18)*nwt7 

dpc4~~dpc7  
nws6~~nws9
nws6~~nwt7
nws9~~nwt7

#Item Thresholds all freely estimated in both groups with label for each group
ar1|c(I1T1M, I1T1F)*t1; ar1|c(I1T2M, I1T2F)*t2; ar1|c(I1T3M, I1T3F)*t3;ar1|c(I1T4M, I1T4F)*t4;
ar2|c(I2T1M, I2T1F)*t1; ar2|c(I2T2M, I2T2F)*t2; ar2|c(I2T3M, I2T3F)*t3;ar2|c(I2T4M, I2T4F)*t4;
ar4|c(I3T1M, I3T1F)*t1; ar4|c(I3T2M, I3T2F)*t2; ar4|c(I3T3M, I3T3F)*t3;ar4|c(I3T4M, I3T4F)*t4;

ar7|c(I4T1M, I4T1F)*t1; ar7|c(I4T2M, I4T2F)*t2; ar7|c(I4T3M, I4T3F)*t3;ar7|c(I4T4M, I4T4F)*t4;
dpc4|c(I5T1M, I5T1F)*t1; dpc4|c(I5T2M, I5T2F)*t2; dpc4|c(I5T3M, I5T3F)*t3;dpc4|c(I5T4M, I5T4F)*t4;
dpc7|c(I6T1M, I6T1F)*t1; dpc7|c(I6T2M, I6T2F)*t2; dpc7|c(I6T3M, I6T3F)*t3;dpc7|c(I6T4M, I6T4F)*t4;

nwr5|c(I7T1M, I7T1F)*t1; nwr5|c(I7T2M, I7T2F)*t2; nwr5|c(I7T3M, I7T3F)*t3;nwr5|c(I7T4M, I7T4F)*t4;
nwr10|c(I8T1M, I8T1F)*t1; nwr10|c(I8T2M, I8T2F)*t2; nwr10|c(I8T3M, I8T3F)*t3;nwr10|c(I8T4M, I8T4F)*t4;
nwr11|c(I9T1M, I9T1F)*t1; nwr11|c(I9T2M, I9T2F)*t2; nwr11|c(I9T3M, I9T3F)*t3;nwr11|c(I9T4M, I9T4F)*t4;

nws2|c(I10T1M, I10T1F)*t1; nws2|c(I10T2M, I10T2F)*t2; nws2|c(I10T3M, I10T3F)*t3;nws2|c(I10T4M, I10T4F)*t4;
nws3|c(I11T1M, I11T1F)*t1; nws3|c(I11T2M, I11T2F)*t2; nws3|c(I11T3M, I11T3F)*t3;nws3|c(I11T4M, I11T4F)*t4;
nws6|c(I12T1M, I12T1F)*t1; nws6|c(I12T2M, I12T2F)*t2; nws6|c(I12T3M, I12T3F)*t3;nws6|c(I12T4M, I12T4F)*t4;

nws7|c(I13T1M, I13T1F)*t1; nws7|c(I13T2M, I13T2F)*t2; nws7|c(I13T3M, I13T3F)*t3;nws7|c(I13T4M, I13T4F)*t4;
nws8|c(I14T1M, I14T1F)*t1; nws8|c(I14T2M, I14T2F)*t2; nws8|c(I14T3M, I14T3F)*t3;nws8|c(I14T4M, I14T4F)*t4;
nws9|c(I15T1M, I15T1F)*t1; nws9|c(I15T2M, I15T2F)*t2; nws9|c(I15T3M, I15T3F)*t3;nws9|c(I15T4M, I15T4F)*t4;

nwt1|c(I16T1M, I16T1F)*t1; nwt1|c(I16T2M, I16T2F)*t2; nwt1|c(I16T3M, I16T3F)*t3;nwt1|c(I16T4M, I16T4F)*t4;
nwt2|c(I17T1M, I17T1F)*t1; nwt2|c(I17T2M, I17T2F)*t2; nwt2|c(I17T3M, I17T3F)*t3;nwt2|c(I17T4M, I17T4F)*t4;
nwt7|c(I18T1M, I18T1F)*t1; nwt7|c(I18T2M, I18T2F)*t2; nwt7|c(I18T3M, I18T3F)*t3;nwt7|c(I18T4M, I18T4F)*t4;

#Redidual variances all fixed to one for each group
ar1 ~~ c(1, 1)*ar1; ar2 ~~ c(1, 1)*ar2; ar4 ~~ c(1, 1)*ar4; 
ar7 ~~ c(1, 1)*ar7; dpc4 ~~ c(1, 1)*dpc4; dpc7 ~~ c(1, 1)*dpc7;

nwr5 ~~ c(1, 1)*nwr5; nwr10 ~~ c(1, 1)*nwr10; nwr11 ~~ c(1, 1)*nwr11; 
nws2 ~~ c(1, 1)*nws2; nws3 ~~ c(1, 1)*nws3; nws6 ~~ c(1, 1)*nws6; 
nws7 ~~ c(1, 1)*nws7; nws8 ~~ c(1, 1)*nws8; nws9 ~~ c(1, 1)*nws9; 
nwt1 ~~ c(1, 1)*nwt1; nwt2 ~~ c(1, 1)*nwt2; nwt7 ~~ c(1, 1)*nwt7;

#Factor variance fixed to 1 for identification in group 1; estimated for group 2
AR ~~ c(1,NA)*AR;
DP ~~ c(1,NA)*DP;
IS ~~ c(1,NA)*IS; 

#Factor mean fixed to zero for identification in each group
AR ~ c(0,0)*0
DP ~ c(0,0)*0
IS ~ c(0,0)*0

#Regression
AR ~ IS + DP
DP ~ IS "

metricEstimates = lavaan(model = metric, data = c16rp1, 
                         ordered = c("ar1", "ar2", "ar4", "ar7", "dpc42", "dpc7",
                                     "nwr5", "nwr10", "nwr11", "nws2", "nws3", "nws6", 
                                     "nws7", "nws8", "nws9", "nwt1", "nwt2", "nwt7"),
                         parameterization="theta", group = "GENDER")

summary(metricEstimates, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
fitmeasures(metricEstimates, fit.measures = c("chisq", "df", "pvalue", "CFI", "RMSEA", "srmr"))

## PLOT PATH DIAGRAM
semPaths(metricEstimates, "std", curvePivot = TRUE, thresholds = FALSE, what="paths", style="lisrel")

# COMPARE MODELS
anova(configuralEstimates,metricEstimates )  
## OR
lavTestLRT(configuralEstimates, metricEstimates)


########################### Full Threshold Invariance Model ##########################

threshold = "

#Factor loadings (AKA slopes/discriminations) from final metric model
AR =~ c(1,1)*ar1 + c(L2, L2)*ar2 + c(L3, L3)*ar4 
DP =~ c(1,1)*ar7 + c(L5, L5)*dpc4 + c(L6, L6)*dpc7 

IS =~ c(1, 1)*nwr5 + c(L8, L8)*nwr10 + c(L9, L9)*nwr11 
+ c(L10,L10)*nws2 + c(L11, L11)*nws3 + c(L12, L12)*nws6 
+ c(L13, L13F)*nws7 + c(L14, L14)*nws8 + c(L15, L15)*nws9 
+ c(L16, L16)*nwt1 + c(L17, L17)*nwt2 + c(L18, L18)*nwt7 

dpc4~~dpc7  
nws6~~nws9
nws6~~nwt7
nws9~~nwt7 

# Item Thresholds all constrained to be equal across groups 
ar1|c(I1T1, I1T1)*t1; ar1|c(I1T2, I1T2)*t2; ar1|c(I1T3, I1T3)*t3;ar1|c(I1T4, I1T4)*t4;
ar2|c(I2T1, I2T1)*t1; ar2|c(I2T2, I2T2)*t2; ar2|c(I2T3, I2T3)*t3;ar2|c(I2T4, I2T4)*t4;
ar4|c(I3T1, I3T1)*t1; ar4|c(I3T2, I3T2)*t2; ar4|c(I3T3, I3T3)*t3;ar4|c(I3T4, I3T4)*t4;

ar7|c(I4T1, I4T1)*t1; ar7|c(I4T2, I4T2)*t2; ar7|c(I4T3, I4T3)*t3;ar7|c(I4T4, I4T4)*t4;
dpc4|c(I5T1, I5T1)*t1; dpc4|c(I5T2, I5T2)*t2; dpc4|c(I5T3, I5T3)*t3;dpc4|c(I5T4, I5T4)*t4;
dpc7|c(I6T1, I6T1)*t1; dpc7|c(I6T2, I6T2)*t2; dpc7|c(I6T3, I6T3)*t3;dpc7|c(I6T4, I6T4)*t4;

nwr5|c(I7T1, I7T1)*t1; nwr5|c(I7T2, I7T2)*t2; nwr5|c(I7T3, I7T3)*t3;nwr5|c(I7T4, I7T4)*t4;
nwr10|c(I8T1, I8T1)*t1; nwr10|c(I8T2, I8T2)*t2; nwr10|c(I8T3, I8T3)*t3;nwr10|c(I8T4, I8T4)*t4;
nwr11|c(I9T1, I9T1)*t1; nwr11|c(I9T2, I9T2)*t2; nwr11|c(I9T3, I9T3)*t3;nwr11|c(I9T4, I9T4)*t4;

nws2|c(I10T1, I10T1)*t1; nws2|c(I10T2, I10T2)*t2; nws2|c(I10T3, I10T3)*t3;nws2|c(I10T4, I10T4)*t4;
nws3|c(I11T1, I11T1)*t1; nws3|c(I11T2, I11T2)*t2; nws3|c(I11T3, I11T3)*t3;nws3|c(I11T4, I11T4)*t4;
nws6|c(I12T1, I12T1)*t1; nws6|c(I12T2, I12T2)*t2; nws6|c(I12T3, I12T3)*t3;nws6|c(I12T4, I12T4)*t4;

nws7|c(I13T1, I13T1)*t1; nws7|c(I13T2, I13T2)*t2; nws7|c(I13T3, I13T3)*t3;nws7|c(I13T4, I13T4)*t4;
nws8|c(I14T1, I14T1)*t1; nws8|c(I14T2, I14T2)*t2; nws8|c(I14T3, I14T3)*t3;nws8|c(I14T4, I14T4)*t4;
nws9|c(I15T1, I15T1)*t1; nws9|c(I15T2, I15T2)*t2; nws9|c(I15T3, I15T3)*t3;nws9|c(I15T4, I15T4)*t4;

nwt1|c(I16T1, I16T1)*t1; nwt1|c(I16T2, I16T2)*t2; nwt1|c(I16T3, I16T3)*t3;nwt1|c(I16T4, I16T4)*t4;
nwt2|c(I17T1, I17T1)*t1; nwt2|c(I17T2, I17T2)*t2; nwt2|c(I17T3, I17T3)*t3;nwt2|c(I17T4, I17T4)*t4;
nwt7|c(I18T1, I18T1)*t1; nwt7|c(I18T2, I18T2)*t2; nwt7|c(I18T3, I18T3)*t3;nwt7|c(I18T4, I18T4)*t4;

#Redidual variances all fixed to one for each group
ar1 ~~ c(1, 1)*ar1; ar2 ~~ c(1, 1)*ar2; ar4 ~~ c(1, 1)*ar4; 
ar7 ~~ c(1, 1)*ar7; dpc4 ~~ c(1, 1)*dpc4; dpc7 ~~ c(1, 1)*dpc7;

nwr5 ~~ c(1, 1)*nwr5; nwr10 ~~ c(1, 1)*nwr10; nwr11 ~~ c(1, 1)*nwr11; 
nws2 ~~ c(1, 1)*nws2; nws3 ~~ c(1, 1)*nws3; nws6 ~~ c(1, 1)*nws6; 
nws7 ~~ c(1, 1)*nws7; nws8 ~~ c(1, 1)*nws8; nws9 ~~ c(1, 1)*nws9; 
nwt1 ~~ c(1, 1)*nwt1; nwt2 ~~ c(1, 1)*nwt2; nwt7 ~~ c(1, 1)*nwt7;

#Factor variance fixed to 1 for identification in males; estimated for females
AR ~~ c(1,NA)*AR
DP ~~ c(1,NA)*DP
IS ~~ c(1,NA)*IS

#Factor mean fixed to zero for identification in males; estimated for females ****
AR ~ c(0,NA)*0 
DP ~ c(0,NA)*0 
IS ~ c(0,NA)*0 

#Regression
AR ~ IS + DP
DP ~ IS "

thresholdEstimates = lavaan(model = threshold, data = c16rp1,
                            ordered = c("ar1", "ar2", "ar4", "ar7", "dpc42", "dpc7",
                                        "nwr5", "nwr10", "nwr11", "nws2", "nws3", "nws6", 
                                        "nws7", "nws8", "nws9", "nwt1", "nwt2", "nwt7"),
                            parameterization="theta", group = "GENDER")

summary(thresholdEstimates, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
fitmeasures(thresholdEstimates, fit.measures = c("chisq", "df", "pvalue", "CFI", "RMSEA", "srmr"))

anova(thresholdEstimates, metricEstimates)
anova(configuralEstimates, thresholdEstimates)

################# look at the modification indices to see if there are any parameters to free.
thresholdMI1 = modificationindices(thresholdEstimates, free.remove = FALSE, maximum.number = 10000)

#restrict output to only factor loading parameters
thresholdMI1a = thresholdMI1[which(thresholdMI1$op == "|"),]
thresholdMI1a[order(-thresholdMI1a$mi),]

# add one parameter at a time, preferrably 

# revised threshold model
threshold1 = "

#Factor loadings (AKA slopes/discriminations) from final metric model
AR =~ c(1,1)*ar1 + c(L2, L2)*ar2 + c(L3, L3)*ar4 
DP =~ c(1,1)*ar7 + c(L5, L5)*dpc4 + c(L6, L6)*dpc7 

IS =~ c(1, 1)*nwr5 + c(L8, L8)*nwr10 + c(L9, L9)*nwr11 
+ c(L10,L10)*nws2 + c(L11, L11)*nws3 + c(L12, L12)*nws6 
+ c(L13, L13F)*nws7 + c(L14, L14)*nws8 + c(L15, L15)*nws9 
+ c(L16, L16)*nwt1 + c(L17, L17)*nwt2 + c(L18, L18)*nwt7 

dpc4~~dpc7  
nws6~~nws9
nws6~~nwt7
nws9~~nwt7 

# Item Thresholds all constrained to be equal across groups EXCEPT Non-invairant items
ar1|c(I1T1, I1T1)*t1; ar1|c(I1T2, I1T2)*t2; ar1|c(I1T3, I1T3)*t3;ar1|c(I1T4, I1T4)*t4;
ar2|c(I2T1, I2T1)*t1; ar2|c(I2T2, I2T2)*t2; ar2|c(I2T3, I2T3)*t3;ar2|c(I2T4, I2T4)*t4;
ar4|c(I3T1, I3T1)*t1; ar4|c(I3T2, I3T2)*t2; ar4|c(I3T3, I3T3)*t3;ar4|c(I3T4M, I3T4F)*t4;

ar7|c(I4T1, I4T1)*t1; ar7|c(I4T2, I4T2)*t2; ar7|c(I4T3, I4T3)*t3;ar7|c(I4T4, I4T4)*t4;
dpc4|c(I5T1, I5T1)*t1; dpc4|c(I5T2, I5T2)*t2; dpc4|c(I5T3, I5T3)*t3;dpc4|c(I5T4, I5T4)*t4;
dpc7|c(I6T1, I6T1)*t1; dpc7|c(I6T2, I6T2)*t2; dpc7|c(I6T3, I6T3)*t3;dpc7|c(I6T4, I6T4)*t4;

nwr5|c(I7T1M, I7T1F)*t1; nwr5|c(I7T2M, I7T2F)*t2; nwr5|c(I7T3M, I7T3F)*t3;nwr5|c(I7T4M, I7T4F)*t4;
nwr10|c(I8T1, I8T1)*t1; nwr10|c(I8T2, I8T2)*t2; nwr10|c(I8T3, I8T3)*t3;nwr10|c(I8T4, I8T4)*t4;
nwr11|c(I9T1, I9T1)*t1; nwr11|c(I9T2, I9T2)*t2; nwr11|c(I9T3M, I9T3F)*t3;nwr11|c(I9T4, I9T4)*t4;

nws2|c(I10T1, I10T1)*t1; nws2|c(I10T2, I10T2)*t2; nws2|c(I10T3, I10T3)*t3;nws2|c(I10T4, I10T4)*t4;
nws3|c(I11T1, I11T1)*t1; nws3|c(I11T2, I11T2)*t2; nws3|c(I11T3, I11T3)*t3;nws3|c(I11T4, I11T4)*t4;
nws6|c(I12T1, I12T1)*t1; nws6|c(I12T2M, I12T2F)*t2; nws6|c(I12T3, I12T3)*t3;nws6|c(I12T4, I12T4)*t4;

nws7|c(I13T1, I13T1)*t1; nws7|c(I13T2, I13T2)*t2; nws7|c(I13T3, I13T3)*t3;nws7|c(I13T4, I13T4)*t4;
nws8|c(I14T1, I14T1)*t1; nws8|c(I14T2, I14T2)*t2; nws8|c(I14T3, I14T3)*t3;nws8|c(I14T4, I14T4)*t4;
nws9|c(I15T1, I15T1)*t1; nws9|c(I15T2, I15T2)*t2; nws9|c(I15T3, I15T3)*t3;nws9|c(I15T4, I15T4)*t4;

nwt1|c(I16T1, I16T1)*t1; nwt1|c(I16T2M, I16T2F)*t2; nwt1|c(I16T3M, I16T3F)*t3;nwt1|c(I16T4M, I16T4F)*t4;
nwt2|c(I17T1, I17T1)*t1; nwt2|c(I17T2, I17T2)*t2; nwt2|c(I17T3, I17T3)*t3;nwt2|c(I17T4, I17T4)*t4;
nwt7|c(I18T1, I18T1)*t1; nwt7|c(I18T2, I18T2)*t2; nwt7|c(I18T3, I18T3)*t3;nwt7|c(I18T4, I18T4)*t4;


#Redidual variances all fixed to one for each group
ar1 ~~ c(1, 1)*ar1; ar2 ~~ c(1, 1)*ar2; ar4 ~~ c(1, 1)*ar4; 
ar7 ~~ c(1, 1)*ar7; dpc4 ~~ c(1, 1)*dpc4; dpc7 ~~ c(1, 1)*dpc7;

nwr5 ~~ c(1, 1)*nwr5; nwr10 ~~ c(1, 1)*nwr10; nwr11 ~~ c(1, 1)*nwr11; 
nws2 ~~ c(1, 1)*nws2; nws3 ~~ c(1, 1)*nws3; nws6 ~~ c(1, 1)*nws6; 
nws7 ~~ c(1, 1)*nws7; nws8 ~~ c(1, 1)*nws8; nws9 ~~ c(1, 1)*nws9; 
nwt1 ~~ c(1, 1)*nwt1; nwt2 ~~ c(1, 1)*nwt2; nwt7 ~~ c(1, 1)*nwt7;

#Factor variance fixed to 1 for identification in males; estimated for females
AR ~~ c(1,NA)*AR
DP ~~ c(1,NA)*DP
IS ~~ c(1,NA)*IS

#Factor mean fixed to zero for identification in males; estimated for females ****
AR ~ c(0,NA)*0 
DP ~ c(0,NA)*0 
IS ~ c(0,NA)*0 

#Regression
AR ~ IS + DP
DP ~ IS "

thresholdEstimates1 = lavaan(model = threshold1, data = c16rp1,
                             ordered = c("ar1", "ar2", "ar4", "ar7", "dpc42", "dpc7",
                                         "nwr5", "nwr10", "nwr11", "nws2", "nws3", "nws6", 
                                         "nws7", "nws8", "nws9", "nwt1", "nwt2", "nwt7"),
                             std.lv = TRUE, parameterization="theta", group = "GENDER")

summary(thresholdEstimates1, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
fitmeasures(thresholdEstimates1, fit.measures = c("chisq", "df", "pvalue", "CFI", "RMSEA", "srmr"))

anova(metricEstimates, thresholdEstimates1)
anova(configuralEstimates, thresholdEstimates1)

########################## Structural Model Invariance: Factor Variance #####################
### To test the equality of factor variance across groups,
### we use our last model and constrain the factor variance.

factorVar = "

AR =~ c(1,1)*ar1 + c(L2, L2)*ar2 + c(L3, L3)*ar4 
DP =~ c(1,1)*ar7 + c(L5, L5)*dpc4 + c(L6, L6)*dpc7 

IS =~ c(1, 1)*nwr5 + c(L8, L8)*nwr10 + c(L9, L9)*nwr11 
+ c(L10,L10)*nws2 + c(L11, L11)*nws3 + c(L12, L12)*nws6 
+ c(L13, L13F)*nws7 + c(L14, L14)*nws8 + c(L15, L15)*nws9 
+ c(L16, L16)*nwt1 + c(L17, L17)*nwt2 + c(L18, L18)*nwt7 

dpc4~~dpc7  
nws6~~nws9
nws6~~nwt7
nws9~~nwt7 

# Item Thresholds all constrained to be equal across groups EXCEPT Non-invairant items
ar1|c(I1T1, I1T1)*t1; ar1|c(I1T2, I1T2)*t2; ar1|c(I1T3, I1T3)*t3;ar1|c(I1T4, I1T4)*t4;
ar2|c(I2T1, I2T1)*t1; ar2|c(I2T2, I2T2)*t2; ar2|c(I2T3, I2T3)*t3;ar2|c(I2T4, I2T4)*t4;
ar4|c(I3T1, I3T1)*t1; ar4|c(I3T2, I3T2)*t2; ar4|c(I3T3, I3T3)*t3;ar4|c(I3T4M, I3T4F)*t4;

ar7|c(I4T1, I4T1)*t1; ar7|c(I4T2, I4T2)*t2; ar7|c(I4T3, I4T3)*t3;ar7|c(I4T4, I4T4)*t4;
dpc4|c(I5T1, I5T1)*t1; dpc4|c(I5T2, I5T2)*t2; dpc4|c(I5T3, I5T3)*t3;dpc4|c(I5T4, I5T4)*t4;
dpc7|c(I6T1, I6T1)*t1; dpc7|c(I6T2, I6T2)*t2; dpc7|c(I6T3, I6T3)*t3;dpc7|c(I6T4, I6T4)*t4;

nwr5|c(I7T1M, I7T1F)*t1; nwr5|c(I7T2M, I7T2F)*t2; nwr5|c(I7T3M, I7T3F)*t3;nwr5|c(I7T4M, I7T4F)*t4;
nwr10|c(I8T1, I8T1)*t1; nwr10|c(I8T2, I8T2)*t2; nwr10|c(I8T3, I8T3)*t3;nwr10|c(I8T4, I8T4)*t4;
nwr11|c(I9T1, I9T1)*t1; nwr11|c(I9T2, I9T2)*t2; nwr11|c(I9T3M, I9T3F)*t3;nwr11|c(I9T4, I9T4)*t4;

nws2|c(I10T1, I10T1)*t1; nws2|c(I10T2, I10T2)*t2; nws2|c(I10T3, I10T3)*t3;nws2|c(I10T4, I10T4)*t4;
nws3|c(I11T1, I11T1)*t1; nws3|c(I11T2, I11T2)*t2; nws3|c(I11T3, I11T3)*t3;nws3|c(I11T4, I11T4)*t4;
nws6|c(I12T1, I12T1)*t1; nws6|c(I12T2M, I12T2F)*t2; nws6|c(I12T3, I12T3)*t3;nws6|c(I12T4, I12T4)*t4;

nws7|c(I13T1, I13T1)*t1; nws7|c(I13T2, I13T2)*t2; nws7|c(I13T3, I13T3)*t3;nws7|c(I13T4, I13T4)*t4;
nws8|c(I14T1, I14T1)*t1; nws8|c(I14T2, I14T2)*t2; nws8|c(I14T3, I14T3)*t3;nws8|c(I14T4, I14T4)*t4;
nws9|c(I15T1, I15T1)*t1; nws9|c(I15T2, I15T2)*t2; nws9|c(I15T3, I15T3)*t3;nws9|c(I15T4, I15T4)*t4;

nwt1|c(I16T1, I16T1)*t1; nwt1|c(I16T2M, I16T2F)*t2; nwt1|c(I16T3M, I16T3F)*t3;nwt1|c(I16T4M, I16T4F)*t4;
nwt2|c(I17T1, I17T1)*t1; nwt2|c(I17T2, I17T2)*t2; nwt2|c(I17T3, I17T3)*t3;nwt2|c(I17T4, I17T4)*t4;
nwt7|c(I18T1, I18T1)*t1; nwt7|c(I18T2, I18T2)*t2; nwt7|c(I18T3, I18T3)*t3;nwt7|c(I18T4, I18T4)*t4;


#Redidual variances all fixed to one for each group
ar1 ~~ c(1, 1)*ar1; ar2 ~~ c(1, 1)*ar2; ar4 ~~ c(1, 1)*ar4; 
ar7 ~~ c(1, 1)*ar7; dpc4 ~~ c(1, 1)*dpc4; dpc7 ~~ c(1, 1)*dpc7;

nwr5 ~~ c(1, 1)*nwr5; nwr10 ~~ c(1, 1)*nwr10; nwr11 ~~ c(1, 1)*nwr11; 
nws2 ~~ c(1, 1)*nws2; nws3 ~~ c(1, 1)*nws3; nws6 ~~ c(1, 1)*nws6; 
nws7 ~~ c(1, 1)*nws7; nws8 ~~ c(1, 1)*nws8; nws9 ~~ c(1, 1)*nws9; 
nwt1 ~~ c(1, 1)*nwt1; nwt2 ~~ c(1, 1)*nwt2; nwt7 ~~ c(1, 1)*nwt7;

#Factor variance fixed to 1 for both groups
AR ~~ c(1,1)*AR
DP ~~ c(1,1)*DP
IS ~~ c(1,1)*IS

#Factor mean fixed to zero for identification in males; estimated for females
AR ~ c(0,NA)*0   
DP ~ c(0,NA)*0
IS ~ c(0,NA)*0

#Regression
AR ~ IS + DP
DP ~ IS "

factorVarEstimates = lavaan(model = factorVar, data = c16rp1,
                            ordered = c("ar1", "ar2", "ar4", "ar7", "dpc42", "dpc7",
                                        "nwr5", "nwr10", "nwr11", "nws2", "nws3", "nws6", 
                                        "nws7", "nws8", "nws9", "nwt1", "nwt2", "nwt7"),
                            std.lv = TRUE, parameterization="theta", group = "GENDER")

summary(factorVarEstimates, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
fitmeasures(factorVarEstimates, fit.measures = c("chisq", "df", "pvalue", "CFI", "RMSEA", "srmr"))

anova(thresholdEstimates1, factorVarEstimates)
anova(configuralEstimates, factorVarEstimates)

# To test the factor mean, we only need to look at the Wald test for the factor means from the last threshold model
summary(thresholdEstimates1, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

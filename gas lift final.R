################################################################################
# File:   		gas lift.R										
# Author: 		Data Science Team												
# Date:			16-NOV-2018													
# Description:	
#
# Revision History:
# ------------------------------------------------------------------------------
# Date				Author			Change
# ------------------------------------------------------------------------------
# 16-NOV-2017		Data Science 	Initial check in with design time
# 20-NOV-2017		Data Science	completed runtime and preprocessing
################################################################################
rm(list=ls());
gc();
func <- function(js_object){return(js_object);
};
js_object<-"";
is_error_occur <- FALSE;
tryCatch({
  library('log4r');
  library("RJSONIO");
  logger <- create.logger();  
  logfile(logger) <- "$pathToRLog";
  if(!file.exists(logfile(logger))){
    js_prep <- list(error_code = "R201", error_message = paste("logger block : Log File Does not exist"),status = "FAILURE", output_data = NA);
    js_object <<- toJSON(js_prep);
    js_object;
    is_error_occur <<- TRUE;
  };
  level(logger) <- 'INFO';
},error = function(c) {is_error_occur <<- TRUE;
error(logger, c);
js_prep <- list(error_code = "R201", error_message = paste("logger block : ",c,sep=""),status = "FAILURE", output_data = NA);
js_object <<- toJSON(js_prep);
js_object;
});
print(Sys.time());

if(is_error_occur == FALSE){
  tryCatch({
    library("reshape2");
    library("RJSONIO");
    library("randomForest");
    library("caTools");
    library("stringr");
    library(data.table);
    library(lubridate);
    library(ggplot2);
    library(zoo);
    library(forecast);
    library(scales);
    info(logger, paste("Libraries import completed"));
    
  },error = function(c) {
    is_error_occur<<- TRUE;
    error(logger, c);	
    js_prep <- list(error_code = "R201", error_message = paste("library block : ",c,sep=""),status = "FAILURE", output_data = NA);
    js_object <<- toJSON(js_prep);
    js_object;
    info(logger, paste("Error in library block"));
  });
  if(is_error_occur == FALSE){
    tryCatch({
      PATH <- "$pathToCsv";
      info(logger, paste("Initial input path:",PATH));
      DATA <- fread(PATH);
      DATA <- unique(DATA);
      info(logger, paste("Data unique completed, Data Dimensions:",dim(DATA)));
      TRAINING_DATA_PER <- 80;
      
      #runtime - bo/designtime -  dsw flag input
      ALGO_FLAG <- "$clientName";
      ALGO_FLAG <- tolower(ALGO_FLAG);
      
      info(logger, paste("Algorithm Flag:",ALGO_FLAG));
      
      #Input Tags
      # Dependent Tags
      
      DEPENDENT_TAG_DESC <- "$tags";
      DEPENDENT_TAG_DATA <- data.table(TAG_DATA=strsplit(DEPENDENT_TAG_DESC,",")[[1]]);
      DEPENDENT_TAG_DATA[,TAG_IDS:=strsplit(TAG_DATA,"=")[[1]][1],by="TAG_DATA"][,TAG_STORAGE:=strsplit(strsplit(TAG_DATA,"=")[[1]][2],"@")[[1]][1],by="TAG_DATA"][,TAG_TYPE:=strsplit(strsplit(TAG_DATA,"=")[[1]][2],"@")[[1]][2],by="TAG_DATA"];
      DEPENDENT_TAG_IDS <- DEPENDENT_TAG_DATA$TAG_IDS;
      info(logger, paste("Tag List:",DEPENDENT_TAG_IDS));
      
      INDEPENDENT_TAG_DESC <-"$itags";
      INDEPENDENT_TAG_DATA <- data.table(TAG_DATA=strsplit(INDEPENDENT_TAG_DESC,",")[[1]]);
      INDEPENDENT_TAG_DATA[,TAG_IDS:=strsplit(TAG_DATA,"=")[[1]][1],by="TAG_DATA"][,TAG_STORAGE:=strsplit(strsplit(TAG_DATA,"=")[[1]][2],"@")[[1]][1],by="TAG_DATA"][,TAG_TYPE:=strsplit(strsplit(TAG_DATA,"=")[[1]][2],"@")[[1]][2],by="TAG_DATA"];
      INDEPENDENT_TAG_IDS <- INDEPENDENT_TAG_DATA$TAG_IDS;
      info(logger, paste("Tag List:",INDEPENDENT_TAG_IDS));
      
      TAG_IDS <- c(INDEPENDENT_TAG_IDS,DEPENDENT_TAG_IDS);
      #TAG_IDS <- c(primary_key,INDEPENDENT_TAG_IDS,DEPENDENT_TAG_IDS);
      
      primary_key <- strsplit("$primaryTag","=")[[1]][1];
      
      #DATA <- DATA[,c("Date","Well",TAG_IDS), with=FALSE];
      #DATA <- DATA[,c("Well",TAG_IDS), with=FALSE];
      DATA <- DATA[, Date := format(ymd_hms(Date),format="%Y-%m-%d")];
      DATA <- DATA[, Date := ymd(Date)];
      
      MODEL_PATH <- "$modelPath";
      MODEL_CODE <- "$modelCode";
      RDA_MODEL_PATH <- paste(MODEL_PATH,MODEL_CODE,"/",MODEL_CODE,".rda",sep=""); 
      info(logger, paste("Input Model Path:",RDA_MODEL_PATH));
      
    },error = function(c) {
      if(is_error_occur == FALSE){
        is_error_occur<<- TRUE;
        error(logger, c);	
        js_prep <- list(error_code = "R202", error_message = paste("Input parameter and dataset Import block : ",c,sep=""),status = "FAILURE", output_data = NA);
        js_object <<- toJSON(js_prep);
        js_object;
        #func(js_object);
        info(logger, paste("Error in input parameter and dataset Import block"));
      };
    });
    
    if(ALGO_FLAG == "dsw" && is_error_occur== FALSE) { 
      #design_time
      tryCatch({
        info(logger, paste("algorithm execution flag",ALGO_FLAG));   
        set.seed(1);
        train = sample(1:nrow(DATA), nrow(DATA)*(TRAINING_DATA_PER/100));
        test = -train;
        TRAINING_DATA <- data.table(DATA[train,]);
        TEST_DATA = data.table(DATA[test,]);
        WELL_NAME <- "$wellName";
        WELL_NAME <- data.table(WELL_NAME=strsplit(WELL_NAME,split = ",")[[1]]);
        WELL_NAME <- WELL_NAME$WELL_NAME;
        TEST_DATA_BY_WELL <- DATA[Well %in% WELL_NAME];
        info(logger, paste("Dataset divided into test and train data:"));
        
        
        FORMULA <- "Injgas+ WHP+AnnulusA+AnnulusB +Choke";
        FORMULA <- paste(DEPENDENT_TAG_IDS, FORMULA, sep = "~");
        info(logger, paste("formula prepared:",FORMULA));
        
        ## Models for Oil, Water and Total Liquid Production
        MODEL<-randomForest(as.formula(FORMULA), data = TRAINING_DATA[,TAG_IDS, with=F]);
        info(logger, paste("Model prepared:"));
        
        tryCatch({	
          #Saving Model
          dir.create(paste(MODEL_PATH,MODEL_CODE,sep=""));
          save(MODEL, file = RDA_MODEL_PATH);
          info(logger, paste("Model saved at location:",MODEL_PATH));
        },error = function(c) {
          is_error_occur <<- TRUE;
          error(logger, c);	
          js_prep <- list(error_code = "R205", error_message = paste("Error while writing .rda file: ",c,sep=""),status = "FAILURE", output_data = NA);
          js_object <<- toJSON(js_prep);
          js_object;
        });
        
      },error = function(c) {
        is_error_occur<<- TRUE;			
        error(logger, c);	
        js_prep <- list(error_code = "R203", error_message = paste("Model Creation Part : ",c,sep=""),status = "FAILURE", output_data = NA);
        js_object <<- toJSON(js_prep);
        js_object;
        info(logger, paste("Error in Model Creation Part"));
      });
      if(is_error_occur == FALSE){
        tryCatch({
          PREDICTION <- predict(MODEL, TEST_DATA_BY_WELL[,TAG_IDS, with=F]);
          info(logger, paste("prediction calculation completed"));
          #prediction and actual data
          DATA_VALIDATION <- data.table(TEST_DATA_BY_WELL[,c("Date",DEPENDENT_TAG_IDS),with=FALSE],PREDICTION);
          setnames(DATA_VALIDATION,colnames(DATA_VALIDATION),c("Date","actual","predicted"));
          DATA_VALIDATION[,error_percentage := round(abs(predicted-actual)/actual*100,2)];
          ERROR_PERCENTAGE <- cbind(data.table(tag_id=sub("","",DEPENDENT_TAG_IDS)),error_percentage=DATA_VALIDATION[,round(mean(error_percentage*is.finite(error_percentage),na.rm=TRUE),2)]);
          
          RMSE_VALUE <- sqrt( mean( (PREDICTION - TEST_DATA_BY_WELL[, DEPENDENT_TAG_IDS, with=F])^2, na.rm = TRUE) );
          LINEAR_MODEL <- lm(formula = predicted ~ actual, data = DATA_VALIDATION);
          INTERCEPT <- round(as.numeric(LINEAR_MODEL$coefficients[1]),3);
          SLOPE <-round(as.numeric(LINEAR_MODEL$coefficients[2]),3);
          
          ##Filtering DATA for selected well 
          WELL_DATA <- DATA[Well==WELL_NAME];
          
          
          ## Optimal Gas Rate - simple sampling through various gas inj rates and chosing the reasonable gas inj rate
          
          WELL_AVERAGE <- data.table(t(sapply(WELL_DATA[Date <= as.Date("2017-04-01"),INDEPENDENT_TAG_IDS, with=FALSE ],function(x) mean(x))));
          
          
          ## Inj gas
          LOWER_LIMIT_OF_INJ_GAS <- $lowerLimit;
          UPPER_LIMIT_OF_INJ_GAS <- $upperLimit;
          
          Inj_gas <- seq(LOWER_LIMIT_OF_INJ_GAS,UPPER_LIMIT_OF_INJ_GAS, by=100);
          OPTIMIZATION_DATA <- data.table(Choke=WELL_DATA[["Choke"]][nrow(WELL_DATA)],AnnulusA= WELL_AVERAGE$AnnulusA,AnnulusB= WELL_AVERAGE$AnnulusB, WHP=WELL_AVERAGE$WHP, Injgas=Inj_gas );
          LIQ_PREDICTION_using_INJGAS <- predict(MODEL, OPTIMIZATION_DATA);
          INJ_GAS_VS_LIQ_PREDICTED <- data.table(INJECTION_GAS = Inj_gas, LIQ_PRODUCTION=LIQ_PREDICTION_using_INJGAS);
          
          if(DEPENDENT_TAG_IDS=="OilDay"){
            OPTIMUM_INJECTION_GAS <-as.numeric(INJ_GAS_VS_LIQ_PREDICTED[which.max(INJ_GAS_VS_LIQ_PREDICTED$LIQ_PRODUCTION),])[1];
          };
          if(DEPENDENT_TAG_IDS=="WaterDay"){
            OPTIMUM_INJECTION_GAS <-as.numeric(INJ_GAS_VS_LIQ_PREDICTED[which.min(INJ_GAS_VS_LIQ_PREDICTED$LIQ_PRODUCTION),])[1];
          };
          
          ggplot()+geom_line(data=INJ_GAS_VS_LIQ_PREDICTED, aes(INJECTION_GAS, LIQ_PRODUCTION,color=LIQ_PRODUCTION));
          
          
          ## Prediciton and data for plot
          time_interval<-data.table(Date=seq(as.Date("2017-07-01"),as.Date("2018-01-01") , by = "months"));
          PREDICTION_DATA<-data.table(time_interval, Choke=WELL_DATA[["Choke"]][nrow(WELL_DATA)],AnnulusA= WELL_AVERAGE$AnnulusA,AnnulusB= WELL_AVERAGE$AnnulusB, WHP=WELL_AVERAGE$WHP,Injgas=OPTIMUM_INJECTION_GAS);
          WELL_DATA_FOR_PREDICTION <- WELL_DATA[,c(names(PREDICTION_DATA)), with=FALSE];
          WELL_DATA_FOR_PREDICTION <- rbind.data.frame(WELL_DATA_FOR_PREDICTION,PREDICTION_DATA);
          
          
          FINAL_PREDICTION<-predict(MODEL, WELL_DATA_FOR_PREDICTION);
          
          ############INJECTION GAS RATE###########
          
          HISTORICAL_IGR <- c(WELL_DATA[, Injgas],rep(OPTIMUM_INJECTION_GAS,nrow(time_interval)));
          FINAL_PREDICTION <-data.table(Date=as.character(WELL_DATA_FOR_PREDICTION$Date), PREDICTED_VALUE=FINAL_PREDICTION, INJECTION_GAS=HISTORICAL_IGR);
          ggplot(data = FINAL_PREDICTION,mapping = aes(x=Date))+geom_line(aes(y=PREDICTED_VALUE,color=PREDICTED_VALUE));
          
          ACTUAL_AND_PREDICTED_DATA  <- list(FINAL_PREDICTION,ACTUAL <- WELL_DATA[, c("Date",DEPENDENT_TAG_IDS), with=FALSE]);
          
          OUTPUT_PREDICTION_CAPABILITY <- list(prediction_data=DATA_VALIDATION,error_percentage=ERROR_PERCENTAGE, RMSE= RMSE_VALUE,intercept=INTERCEPT,slope=SLOPE);
          OUTPUT_PREDICTION_CAPABILITY$prediction_data$Date <- as.character(OUTPUT_PREDICTION_CAPABILITY$prediction_data$Date);
          
          FINAL_OUTPUT <- list(OUTPUT_PREDICTION_CAPABILITY=OUTPUT_PREDICTION_CAPABILITY,INJ_GAS_VS_LIQ_PREDICTED=INJ_GAS_VS_LIQ_PREDICTED,OPTIMUM_INJECTION_GAS=OPTIMUM_INJECTION_GAS, ACTUAL_AND_PREDICTED_DATA=ACTUAL_AND_PREDICTED_DATA); 
          
          js_prep <- list(error_code = NA, error_message = NA,status = "SUCCESS", output_data = toJSON(FINAL_OUTPUT));
          js_object <<- toJSON(js_prep);
          
          info(logger, paste("json output returned and regression analysis completed successfully:"));
          info(logger, paste(js_object));
          
          js_object;
          
        },error = function(c) {
          error(logger, c);	
          js_prep <- list(error_code = "R204", error_message = paste("Model Output : ",c,sep=""),status = "FAILURE", output_data = NA);
          js_object <<- toJSON(js_prep);
          js_object;
          info(logger, paste("Error in Model Output"));
        });
      };
      
    }; 
  };
};
# print current system time
print(Sys.time());

func(js_object);

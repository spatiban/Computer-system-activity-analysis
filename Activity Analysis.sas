/*Importing dataset into SAS*/
Proc import OUT = CompActiv 
DATAFILE="E:\School\Sem 1\Multivariate Data Analysis I\Final Project\ComputerActivity.csv"
dbms=csv replace; 
GETNAMES=YES;
RUN;
/* Standardising the dataset*/
proc stdize data=CompActiv out=stdized_comp method=range;
var lread lwrite scall scread swrite fork exec rchar wchar pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freemem freeswap;
run;
/*Dividing the dataset into training and test set*/

data temp;
set stdized_comp;
n=ranuni(8);
proc sort data=temp;
  by n;
  data training testing;
   set temp nobs=nobs;
   if _n_<=.7*nobs then output training;
    else output testing;
   run;
/* Principal Component Analysis*/
proc corr data = stdized_comp  noprob;
var lread lwrite scall scread swrite exec rchar wchar pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freemem freeswap;
run;

proc factor data=Work.training method=prin n=7 out=training2;
var lread lwrite scall scread swrite exec rchar wchar pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freemem freeswap;
run;
proc factor data=Work.testing method=prin n=7 out=testing2;
var lread lwrite scall scread swrite exec rchar wchar pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freemem freeswap;
run;
proc reg data=Work.training2 outest=regpca_usr;
model usr = Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 / STB VIF selection=stepwise sle=0.05;
run;
quit;

proc reg data=Work.training2 outest=regpca_sys;
model sys = Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 / STB VIF selection=stepwise sle=0.05;
run;
quit;
/*Predicting usr on test set*/
Proc score data = Work.Testing2 score = regpca_usr type = parms predict out=predictpca_usr;
	var Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7;
	run;
/*Predicting sys on test set*/
Proc score data = Work.Testing2 score = regpca_sys type = parms predict out=predictpca_sys;
	var Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7;
	run;


/* Factor analysis */
/* before rotation */
Proc Factor Data = Work.Training Method=Principal Rotate=None NFactors=7 Simple MSA Plots = Scree MINEIGEN=0 Reorder;
Var lread lwrite scall scread swrite fork exec rchar wchar pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freemem freeswap;
run;
/* varimax rotation */
Proc Factor Data = Work.Training Method=Principal Rotate=varimax NFactors=7 print score Simple corr MSA Plots = Scree MINEIGEN=0 Reorder;
Var lread lwrite scall scread swrite fork exec rchar wchar pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freemem freeswap;
run;
/* After removing the factor freemem*/
Proc Factor Data = Work.Training Method=Principal Rotate=varimax NFactors=7 print score Simple corr MSA Plots = Scree MINEIGEN=0 Reorder;
Var lread lwrite scall scread swrite fork exec rchar wchar pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freeswap;
run;
/* After removing the factor rchar*/
Proc Factor Data = Work.Training Method=Principal Rotate=varimax NFactors=7 print score Simple corr MSA Plots = Scree MINEIGEN=0 Reorder;
Var lread lwrite scall scread swrite fork exec wchar pgout ppgout pgfree pgscan pgin ppgin pflt vflt runqsz atch runocc freeswap;
run;
/* After removing the factor wchar*/
Proc Factor Data = Work.Training Method=Principal Rotate=varimax NFactors=7 print score Simple corr MSA Plots = Scree MINEIGEN=0 Reorder;
Var lread lwrite scall scread swrite fork exec pgout ppgout pgfree pgscan pgin ppgin pflt vflt runqsz atch runocc freeswap;
run;

/* Building regression model using stepwise for usr */
proc reg data=Work.Training outest = regout_usr;
model usr = lread lwrite scall scread swrite fork exec pgout ppgout pgfree pgscan pgin ppgin pflt vflt runqsz atch runocc  freeswap  / STB VIF selection=stepwise sle=0.05;
run;
/* Building regression model using stepwise for sys */
proc reg data=Work.Training outest = regout_sys;
model sys = lread lwrite scall scread swrite fork exec pgout ppgout pgfree pgscan pgin ppgin pflt vflt runqsz atch runocc  freeswap  / STB VIF selection=stepwise sle=0.05;
run;
/*Predicting usr on test set*/
Proc score data = Work.Testing score = regout_usr type = parms predict out=predict_factor_usr;
	var lread lwrite scall scread swrite fork exec pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freeswap;
	run;
/*Predicting sys on test set*/
Proc score data = Work.Testing score = regout_sys type = parms predict out=predict_factor_sys;
	var lread lwrite scall scread swrite fork exec pgout ppgout pgfree pgscan atch pgin ppgin pflt vflt runqsz runocc freeswap;
	run;
/* Macro to calculate Mean Absolute Eror and Root Mean Squared Error */
/* Outputs to data set, log, and macro variable */
%macro mae_rmse(
        dataset /* Data set which contains the actual and predicted values */, 
        actual /* Variable which contains the actual or observed valued */, 
        predicted /* Variable which contains the predicted value */
        );
%global mae rmse; /* Make the scope of the macro variables global */
data &dataset;
    retain square_error_sum abs_error_sum; 
    set &dataset 
        end=last /* Flag for the last observation */
        ;
    error = &actual - &predicted; /* Calculate simple error */
    square_error = error * error; /* error^2 */
    if _n_ eq 1 then do;
        /* Initialize the sums */
        square_error_sum = square_error; 
        abs_error_sum = abs(error); 
        end;
    else do;
        /* Add to the sum */
        square_error_sum = square_error_sum + square_error; 
        abs_error_sum = abs_error_sum + abs(error);
    end;
    if last then do;
        /* Calculate RMSE and MAE and store in SAS data set. */
        mae = abs_error_sum/_n_;
        rmse = sqrt(square_error_sum/_n_); 
        /* Write to SAS log */
        put 'NOTE: ' mae= rmse=; 
        /* Store in SAS macro variables */
        call symput('mae', put(mae, 20.10)); 
        call symput('rmse', put(rmse, 20.10)); 
        end;
run;
%mend;
%mae_rmse(predictpca_usr, usr, MODEL1);
%put NOTE: mae=&mae rmse=&rmse;
%mae_rmse(predictpca_sys, sys, MODEL1);
%put NOTE: mae=&mae rmse=&rmse;
%mae_rmse(predict_factor_usr, usr, MODEL1);
%put NOTE: mae=&mae rmse=&rmse;
%mae_rmse(predict_factor_sys, sys, MODEL1);
%put NOTE: mae=&mae rmse=&rmse;

%put NOTE: mae=&mae rmse=&rmse;

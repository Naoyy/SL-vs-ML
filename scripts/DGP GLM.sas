proc iml;
PF=0; /* Perfect Fitting */
UF=0; /* Underfitting */
OF=0; /* Overfitting */
SF=0; /* Semi Failure */
TF=0; /* Total Failure */
X1=0;
X2=0;
X3=0;
X4=0;
X5=0;
/* DGP */
do rep=1 to 10000;
N = 100;
Mean =j(50,1,0);
Cov =I(50); /*identity matrix*/
x = RandNormal(N, Mean, Cov);
eps=normal(j(N,1,0))*0.25;
beta={1,2,0.7,-1.2,3};
y=X[,1:5]*beta+eps;
rnames='y'//('X1':'X50')`;
d=y||X;
create tableau from d[colname=rnames];
append from d;
close tableau;
submit;
/*Procedure Glmselect*/
proc glmselect data = tableau outdesign=names noprint;
model Y= X1-X50 / selection=Elasticnet(choose=CV);
run;
proc contents data=names out=toto noprint;
run;
endsubmit;
use toto; read all;
close toto;
Xglm=name[2:nrow(name)-1];
Xtrue=('X1':'X5');
D1=setdif(Xglm,Xtrue);
D2=union(Xtrue,Xglm);
D3=setdif(Xtrue,Xglm);
if ncol (D2)=ncol(Xtrue)& ncol(Xtrue)=nrow(Xglm)then /* Perfect Fitting */
PF=PF+1;else PF=PF;
if ncol(D2)=ncol(Xtrue) & nrow(Xglm)< ncol(Xtrue) then /* Underfitting */
UF=UF+1;else UF=UF;
if ncol(D2)>ncol(Xtrue) & ncol(D3)<5 & ncol(D3)>0 then /* Semi Failure */
SF=SF+1;else SF=SF;
if ncol(D2)>ncol(Xtrue) & ncol(D3)=0 then OF=OF+1;/*Overfitting */
else OF=OF;
if ncol(D2)>ncol(Xtrue) & ncol(D3)=5 then TF=TF+1;/* Total Failure */
else TF=TF;
if element('X1',Xglm)=1 then X1=X1+1;
else X1=X1;
if element('X2',Xglm)=1 then X2=X2+1;
else X2=X2;
if element('X3',Xglm)=1 then X3=X3+1;
else X3=X3;
if element('X4',Xglm)=1 then X4=X4+1;
else X4=X4;
if element('X5',Xglm)=1 then X5=X5+1;
else X5=X5;
end;
*print PF UF OF SF TF X1 X2 X3 X4 X5;
TXPF=(PF/(rep-1))*100;
TXUF=(UF/(rep-1))*100;
TXOF=(OF/(rep-1))*100;
TXSF=(SF/(rep-1))*100;
TXTF=(TF/(rep-1))*100;
TX1=(X1/(rep-1))*100;
TX2=(X2/(rep-1))*100;
TX3=(X3/(rep-1))*100;
TX4=(X4/(rep-1))*100;
TX5=(X5/(rep-1))*100;
/* We get the rate values and put them in a data with the name of "resultat" */
Res=TXPF||TXUF ||TXOF ||TXSF ||TXTF ||TX1 ||TX2 ||TX3 ||TX4 ||TX5;
create resultat from
res[colname={TXPF, TXUF ,TXOF ,TXSF ,TXTF ,TX1 ,TX2 ,TX3 ,TX4 ,TX5}];
append from res;
close resultat;
print TXPF TXUF TXOF TXSF TXTF TX1 TX2 TX3 TX4 TX5;
/* Histogram */
PROC TRANSPOSE DATA = resultat out= table1; RUN ;
DATA TABLE2 ; SET TABLE1 ;
RENAME _NAME_ = NAME_TAUX;
label _NAME_ = Metrics and variables;
RENAME COL1 = VALEUR ;
label COL1 = Percentage;
Title1 "Success Rate";
RUN ;
PROC SGPLOT DATA=TABLE2 ;
HBAR NAME_TAUX / STAT=SUM RESPONSE=VALEUR ;
RUN ;

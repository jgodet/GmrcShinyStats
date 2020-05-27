# logist.r
# written by JuG
# May 27 2020


#' Technical function  logistic regression on quali quali
#' @author JuG
#' @description logistic regression quali quali
#' @param y binomial variable
#' @param x binomial variable
#' @details
#' @examples
#'x  <- rbinom(n = 10, size = 1, prob = .4)
#'y  <- rbinom(n = 10, size = 1, prob = .4)
#'logis(x,y)
#' @return
#' @export


logist<-function(y,x){
  situation=99
  if(nlevels(as.factor(x))!=2){situation=0;print("Variable non binaire")}
  if(nlevels(as.factor(y))!=2){situation=0;print("Variable non binaire")}

  if(situation==99){
    m				<-glm(y~as.factor(x),family="binomial")
    coefs			<-cbind(summary(m)$coefficients[,1],suppressMessages(confint(m)),summary(m)$coefficients[,-1])
    AIC				<-summary(m)$aic
    OR				<-round(exp(coefs[,1:3]),3)
    p.values		<-round(coefs[,6],2)
    p.values		<-noquote(ifelse(p.values>0.01,round(p.values,2),"<0.01"))
    coefs			<-cbind(round(coefs[,1:5],3),p.values)

    Response		<-y
    T				<-table(x,Response)
    VP				<-T[2,2]
    VN				<-T[1,1]
    FP				<-T[2,1]
    FN				<-T[1,2]
    V1				<-cbind(VP,VN,FP,FN);colnames(V1)=c("VP","VN","FP","FN")

    Sens			<-VP/(VP+FN)
    Spec			<-VN/(VN+FP)
    VPP				<-VP/(VP+FP)
    VPN				<-VN/(VN+FN)
    Exact			<-(VP+VN)/(VP+VN+FP+FN)
    Erreur			<-(FP+FN)/(VP+VN+FP+FN)
    V2				<-round(cbind(Sens,Spec,VPP,VPN,Exact,Erreur),2)
    colnames(V2)	=c("Sens","Spec","VPP","VPN","Exactitude","Taux d erreur")

    R<-list(Table=T,Valeurs=V1,Performance=V2,Regression.Logistique=coefs,OR=OR)
    return(R)}
}

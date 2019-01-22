#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(read);
  DATA_VECTOR(len);
  DATA_VECTOR(len0);
  DATA_FACTOR(sex);
  DATA_ARRAY(age);
  DATA_SCALAR(CVe);
  DATA_VECTOR(pred_age_f);
  DATA_VECTOR(pred_age_m);
  int n = len.size();
  
  PARAMETER_VECTOR(Linf); 
  PARAMETER_VECTOR(K);
  PARAMETER(L0);
  PARAMETER(CV_L);
  //PARAMETER(logShape);
  //PARAMETER(logScale);
  //PARAMETER(logRate);
  PARAMETER_VECTOR(age_re);
  
  Type eps=1e-5;
  vector<Type> Lt(n);
  vector<Type> resid(n);
  
  for(int i=0;i<n;i++){
    Lt(i) = L0+(Linf(sex(i))-L0)*(1.0-exp(-K(sex(i))*(age_re(i))));
  }
  
  vector<Type> logSigma_Lt=log(CV_L)+log(Lt+eps);
  vector<Type> logSigma_L0=log(CV_L)+log(len0+eps);
  vector<Type> Sigma_t=CVe*(age_re+eps);
  //vector<Type> age_re2=age_re+eps;
  
  // Predicted growth curves
  vector<Type> pred_len_f=L0+(Linf(0)-L0)*(Type(1)-exp(-K(0)*pred_age_f));
  vector<Type> pred_len_m=L0+(Linf(1)-L0)*(Type(1)-exp(-K(1)*pred_age_m));
  
  // Residuals
  resid=Lt-len;
  
  Type ans=0;
  
  for(int i=1;i<read;i++){
    ans+=-sum(dnorm(age_re,age.col(i),Sigma_t,true));
  }
  
  ans+=-sum(dnorm(len,Lt,exp(logSigma_Lt),true));
  ans+=-sum(dnorm(L0,len0,exp(logSigma_L0),true));
  //ans+=-sum(dgamma(age_re2,exp(logShape),exp(logScale),true));
  //ans+=-sum(dexp(age_re2,exp(logRate),true));


  ADREPORT(pred_len_f);
  ADREPORT(pred_len_m);
  ADREPORT(resid);
  REPORT(Lt);
  return ans;
}

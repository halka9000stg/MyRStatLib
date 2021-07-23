#2-way anova
#params:
#   dataSheet<'data.frame'<chr,chr,num>>:data table
#   facA_levels<int>:levels of factor A
#   facB_levels<int>:levels of factor B
#   meas_colName<chr>: column name of measured values
#   y<num>:reliability (y*100)%
#   ret_kined<vec<logi>>:
anlss.tw_anova.test = function(dataSheet,facA_levels,facB_levels,meas_colName,y,ret_kind){
  
  #need interim static values?
  #need interim standard values?
  #need calculated data sheet?
  
  #significance level
  alp = 1 - y
  
  #lines of data sheet
  ln = nrow(dataSheet)
  
  #means of assigned data
  all_m = mean(dataSheet[,meas_colName])
  for (level_a in 1:facA_levels) {
    a_m[level_a] = mean(dataSheet[dataSheet$Factor_A==paste("A",level_a,sep = ""),meas_colName])  
  }
  for (level_b in 1:facB_levels) {
    b_m[level_b] = mean(dataSheet[dataSheet$Factor_B==paste("B",level_b,sep = ""),meas_colName])  
  }
  
  #degrees of freedom
  dof_t = ln - 1
  dof_a = facB_levels - 1
  dof_b = facA_levels - 1
  
  #F values
  dof_e = dof_t - dof_a - dof_b
  fval_a = qf(alp,dof_a,dof_e)
  fval_b = qf(alp,dof_b,dof_e)
  
  for (rowt in 1:ln) {
    dataSheet[rowt,"W_mi_All_m"]=dataSheet[rowt,meas_colName] - all_m
    for (level_a in 1:facA_levels) {
      if(dataSheet$Factor_A[rowt]==paste("A",level_a,sep = "")){
        dataSheet[rowt,"W_mi_A_m"]=dataSheet[rowt,meas_colName] - a_m[level_a]
      }
    }
    for (level_b in 1:facB_levels) {
      if(dataSheet$Factor_B[rowt]==paste("B",level_a,sep = "")){
        dataSheet[rowt,"W_mi_A_m"]=dataSheet[rowt,meas_colName] - b_m[level_b]
      }
    }
    dataSheet[rowt,"W_Sub_AB"] = dataSheet[rowt,"W_mi_All_m"] - (dataSheet[rowt,"W_mi_A_m"] + dataSheet[rowt,"W_mi_B_m"])
   }
  s_t = var(dataSheet[,"W_mi_All_m"])
  s_a = var(dataSheet[,"W_mi_A_m"])
  s_b = var(dataSheet[,"W_mi_B_m"])
  s_e = var(dataSheet[,"W_Sub_AB"])

  sta_a = (s_a/dof_a) / (s_e/dof_e)
  sta_b = (s_b/dof_b) / (s_e/dof_e)

  isInRjaAtFactA = sta_a > fval_a
  isInRjaAtFactB = sta_b > fval_b
  
  stand_dof = c("DoF_of_A"=dof_a,"DoF_of_B"=dof_b,"DoF_of_E"=dof_e)
  stand_F_val = c("F_val_of_A" = fval_b,"F_val_of_B" = fval_b)
  stand=list("DoFs"=stand_dof,"F_vals"=stand_F_val)
  
  stas_F_val = c("F_sta_of_A" = sta_b,"F_sta_of_B" = sta_b)
  stas_s = c("s_var_of_A"=s_a,"s_var_of_B"=s_b,"s_var_of_E"=s_e)
  stas=list("F_stas"=stas_F_val,"s_vars"=stas_s)
  
  isInRja = c("On_Factor_A" = isInRjaAtFactA,"On_Factor_B" = isInRjaAtFactB)
  
  list("Standard_vals"=stand,"Static_vals" = stas,"Is_in_Reject_Area" = isInRja,"Calced_DataTable" = dataSheet)
}
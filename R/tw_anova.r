tw_anova.test = function(datum,facA_levels,facB_levels,meas_colName,y){
  alp = 1 - y

  all_m = mean(datum[,meas_colName])
  for (level_a in 1:facA_levels) {
    a_m[level_a] = mean(datum[datum$Factor_A==paste("A",level_a,sep = ""),meas_colName])  
  }
  for (level_b in 1:facB_levels) {
    b_m[level_b] = mean(datum[datum$Factor_B==paste("B",level_b,sep = ""),meas_colName])  
  }
  ln = nrow(datum)
  
  dof_t = ln - 1
  dof_a = facB_levels - 1
  dof_b = facA_levels - 1
  dof_e = dof_t - dof_a - dof_b
  fval_a = qf(alp,dof_a,dof_e)
  fval_b = qf(alp,dof_b,dof_e)
  gcprint(list(c(all_m,a1_m,a2_m,a3_m,b1_m,b2_m,b3_m)))
  for (rowt in 1:ln) {
    datum[rowt,"W_mi_All_m"]=datum[rowt,meas_colName] - all_m
    for (level_a in 1:facA_levels) {
      if(datum$Factor_A[rowt]==paste("A",level_a,sep = "")){
        datum[rowt,"W_mi_A_m"]=datum[rowt,meas_colName] - a_m[level_a]
      }
    }
    for (level_b in 1:facB_levels) {
      if(datum$Factor_B[rowt]==paste("B",level_a,sep = "")){
        datum[rowt,"W_mi_A_m"]=datum[rowt,meas_colName] - b_m[level_b]
      }
    }
    datum[rowt,"W_Sub_AB"] = datum[rowt,"W_mi_All_m"] - (datum[rowt,"W_mi_A_m"] + datum[rowt,"W_mi_B_m"])
   }
  print(datum)
  s_t = var(datum[,"W_mi_All_m"])
  s_a = var(datum[,"W_mi_A_m"])
  s_b = var(datum[,"W_mi_B_m"])
  s_e = var(datum[,"W_Sub_AB"])

  sta_a = (s_a/dof_a) / (s_e/dof_e)
  sta_b = (s_b/dof_b) / (s_e/dof_e)

  isInRjaAtFactA = sta_a > fval_a
  isInRjaAtFactB = sta_b > fval_b
  stas = c("F_sta_of_A" = sta_b,"F_stal_of_B" = sta_b, "F_val_of_A" = fval_b,"F_val_of_B" = fval_b)
  isInRja = c("On_Factor_A" = isInRjaAtFactA,"On_Factor_B" = isInRjaAtFactB)
  list("Statics" = stas,"Is_in_Reject_Area" = isInRja,"Calced_DataTable" = datum)
}
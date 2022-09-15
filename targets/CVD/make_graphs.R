time4a1c_SL_iccensor_new <- rio::import("./export/results_CVD_2022-08-06_time4a1c_SL_iccensor_new_.txt")
time4a1c_SL_iccensor_new$TCP_type <- "Censoring unmeasured A1C"
# data <- rio::import("./export/results_CVD_2022-08-06_time5iccensor_MACE_lvcf.txt")
time4_lvcfa1c_SLglm <- rio::import("./export/results_CVD_2022-08-06_time4_lvcfa1c_SLglm_ic.txt")
time4_lvcfa1c_SLglm$TCP_type <- "LVCF A1C"

fp <- ggplot(data=df, aes(x=label, y=mean, ymin=lower, ymax=upper)) +
  geom_pointrange() +
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(fp)

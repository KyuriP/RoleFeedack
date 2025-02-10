![License](https://img.shields.io/github/license/KyuriP/RoleFeedack?cacheBust=1)
![Stars](https://img.shields.io/github/stars/KyuriP/RoleFeedack)
<img src="https://github.com/KyuriP/RoleFeedack/blob/main/figure/feedback_logo.png" alt="Feedbaack Logo" width="100" align="left" style="margin-right: 15px;">

# The Role of Feedback Loops in Dynamical Symptom Networks


## Overview

This repository provides the computational tools and analysis scripts used to study the role of feedback loops in dynamical symptom networks. The focus is on simulating and analyzing various network configurations to explore the influence of feedback loop structures on symptom dynamics.

## Repository Structure

### 1. [`code/`](https://github.com/KyuriP/RoleFeedack/tree/main/code)
- Contains script files. Each script includes a brief description at the top detailing its purpose and functionality.

### 2. [`data/`](https://github.com/KyuriP/RoleFeedack/tree/main/data)
- Stores generated data for publication and analysis.

### 3. [`figure/`](https://github.com/KyuriP/RoleFeedack/tree/main/figure)
- Stores generated plots for publication and analysis.

## Software & Dependencies 
<details>
<summary><b><i>R session & Pacakge information</i></b></summary>

           
```
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20
Running under: macOS Sonoma 14.6.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Amsterdam
tzcode source: internal

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] pcalg_2.7-12        qgraph_1.9.8        patchwork_1.3.0     gtable_0.3.6        ggExtra_0.10.1     
 [6] plot3D_1.4.1        ggthemes_5.1.0      PupillometryR_0.0.5 rlang_1.1.4         ggridges_0.5.6     
[11] ggpubr_0.6.0        stringr_1.5.1       deSolve_1.40        rootSolve_1.8.2.4   bootnet_1.6        
[16] ggplot2_3.5.1       haven_2.5.4         magrittr_2.0.3      tidyr_1.3.1         purrr_1.0.2        
[21] dplyr_1.1.4         modelr_0.1.11      

loaded via a namespace (and not attached):
  [1] splines_4.4.1        later_1.3.2          tibble_3.2.1         R.oo_1.27.0          graph_1.82.0        
  [6] rpart_4.1.23         lifecycle_1.0.4      tcltk_4.4.1          rstatix_0.7.2        doParallel_1.0.17   
 [11] lattice_0.22-6       MASS_7.3-61          backports_1.5.0      Hmisc_5.2-0          rmarkdown_2.29      
 [16] plotrix_3.8-4        IsingFit_0.4         httpuv_1.6.15        pbapply_1.7-2        minqa_1.2.8         
 [21] RColorBrewer_1.1-3   abind_1.4-8          quadprog_1.5-8       sfsmisc_1.1-20       R.utils_2.12.3      
 [26] BiocGenerics_0.50.0  nnet_7.3-19          misc3d_0.9-1         gdata_3.0.1          mgm_1.2-14          
 [31] ellipse_0.5.0        codetools_0.2-20     tidyselect_1.2.1     shape_1.4.6.1        farver_2.1.2        
 [36] lme4_1.1-35.5        IsingSampler_0.2.3   stats4_4.4.1         base64enc_0.1-3      eigenmodel_1.11     
 [41] e1071_1.7-16         mitml_0.4-5          Formula_1.2-5        survival_3.7-0       iterators_1.0.14    
 [46] foreach_1.5.2        tools_4.4.1          snow_0.4-4           Rcpp_1.0.13-1        NetworkToolbox_1.4.2
 [51] glue_1.8.0           mnormt_2.1.1         gridExtra_2.3        pan_1.9              xfun_0.49           
 [56] withr_3.0.2          BiocManager_1.30.25  fastmap_1.2.0        boot_1.3-31          fansi_1.0.6         
 [61] digest_0.6.37        R6_2.5.1             mime_0.12            mice_3.16.0          colorspace_2.1-1    
 [66] gtools_3.9.5         jpeg_0.1-10          weights_1.0.4        R.methodsS3_1.8.2    utf8_1.2.4          
 [71] generics_0.1.3       data.table_1.16.2    corpcor_1.6.10       robustbase_0.99-4-1  class_7.3-22        
 [76] htmlwidgets_1.6.4    pkgconfig_2.0.3      htmltools_0.5.8.1    lavaan_0.6-19        carData_3.0-5       
 [81] RBGL_1.80.0          clue_0.3-66          scales_1.3.0         png_0.1-8            wordcloud_2.6       
 [86] knitr_1.48           rstudioapi_0.17.1    ggm_2.5.1            reshape2_1.4.4       checkmate_2.3.2     
 [91] nlme_3.1-166         nloptr_2.1.1         bdsmatrix_1.3-7      proxy_0.4-27         parallel_4.4.1      
 [96] miniUI_0.1.1.1       foreign_0.8-87       fastICA_1.2-7        pillar_1.9.0         vctrs_0.6.5         
[101] promises_1.3.0       car_3.1-3            jomo_2.7-6           xtable_1.8-4         cluster_2.1.6       
[106] htmlTable_2.4.3      evaluate_1.0.1       pbivnorm_0.6.0       mvtnorm_1.3-2        cli_3.6.3           
[111] compiler_4.4.1       smacof_2.1-7         ggsignif_0.6.4       fdrtool_1.2.18       plyr_1.8.9          
[116] forcats_1.0.0        stringi_1.8.4        psych_2.4.6.26       nnls_1.6             networktools_1.5.2  
[121] munsell_0.5.1        glmnet_4.1-8         Matrix_1.7-1         hms_1.1.3            glasso_1.11         
[126] shiny_1.9.1          igraph_2.1.2         broom_1.0.7          DEoptimR_1.1-3-1     polynom_1.4-1      

──────────────────────────────────────────────────────────────────────────────────────────────────────────────
```

</details>

<details>
<summary><b><i>Python session & Pacakge information</i></b></summary>

```
-----
matplotlib          3.9.2
numpy               2.1.3
pandas              2.2.3
sklearn             1.6.0
tigramite           NA
-----
Python 3.12.2 (v3.12.2:6abddd9f6a, Feb  6 2024, 17:02:06) [Clang 13.0.0 (clang-1300.0.29.30)]
macOS-14.6.1-arm64-arm-64bit
-----
Session information updated at 2025-02-11 00:17
```

</details>

<!--## Citation

If you use this repository or find the work helpful, please cite:

> Kyuri Park, Lourens Waldorp, and Vítor V. Vasconcelos.  
> "The Individual- and Population-level Mechanistic Implications of Statistical Networks of Symptoms (2024)"  
> [Link to Preprint](#) (update link)-->



## Contact

For questions, or feedback, please contact [Kyuri Park](mailto:kyurheep@gmail.com).

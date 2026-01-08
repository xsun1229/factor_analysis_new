---
title: "Summarizing ECT results -- ≥ 5 supporting SNPs & ≥ 20 genes, CEDAR, bin AF"
author: "XSun"
date: "2026-01-07"
output:
  workflowr::wflow_html:
    code_folding: hide
    toc: true
---

<p>
<button type="button" class="btn btn-default btn-workflowr btn-workflowr-report"
  data-toggle="collapse" data-target="#workflowr-report">
  <span class="glyphicon glyphicon-list" aria-hidden="true"></span>
  workflowr
  <span class="glyphicon glyphicon-exclamation-sign text-danger" aria-hidden="true"></span>
</button>
</p>

<div id="workflowr-report" class="collapse">
<ul class="nav nav-tabs">
  <li class="active"><a data-toggle="tab" href="#summary">Summary</a></li>
  <li><a data-toggle="tab" href="#checks">
  Checks <span class="glyphicon glyphicon-exclamation-sign text-danger" aria-hidden="true"></span>
  </a></li>
  <li><a data-toggle="tab" href="#versions">Past versions</a></li>
</ul>

<div class="tab-content">
<div id="summary" class="tab-pane fade in active">
  <p><strong>Last updated:</strong> 2026-01-08</p>
  <p><strong>Checks:</strong>
  <span class="glyphicon glyphicon-ok text-success" aria-hidden="true"></span>
  6
  <span class="glyphicon glyphicon-exclamation-sign text-danger" aria-hidden="true"></span>
  1
  </p>
  <p><strong>Knit directory:</strong>
  <code>factor_analysis_new/</code>
  <span class="glyphicon glyphicon-question-sign" aria-hidden="true"
  title="This is the local directory in which the code in this file was executed.">
  </span>
  </p>
  <p>
  This reproducible <a href="https://rmarkdown.rstudio.com">R Markdown</a>
  analysis was created with <a
  href="https://github.com/workflowr/workflowr">workflowr</a> (version
  1.7.0). The <em>Checks</em> tab describes the
  reproducibility checks that were applied when the results were created.
  The <em>Past versions</em> tab lists the development history.
  </p>
<hr>
</div>
<div id="checks" class="tab-pane fade">
  <div class="panel-group" id="workflowr-checks">
  <div class="panel panel-default">
<div class="panel-heading">
<p class="panel-title">
<a data-toggle="collapse" data-parent="#workflowr-checks" href="#strongRMarkdownfilestronguncommittedchanges">
  <span class="glyphicon glyphicon-exclamation-sign text-danger" aria-hidden="true"></span>
  <strong>R Markdown file:</strong> uncommitted changes
</a>
</p>
</div>
<div id="strongRMarkdownfilestronguncommittedchanges" class="panel-collapse collapse">
<div class="panel-body">
  The R Markdown file has unstaged changes. 
To know which version of the R Markdown file created these
results, you'll want to first commit it to the Git repo. If
you're still working on the analysis, you can ignore this
warning. When you're finished, you can run
<code>wflow_publish</code> to commit the R Markdown file and
build the HTML.

</div>
</div>
</div>
<div class="panel panel-default">
<div class="panel-heading">
<p class="panel-title">
<a data-toggle="collapse" data-parent="#workflowr-checks" href="#strongEnvironmentstrongempty">
  <span class="glyphicon glyphicon-ok text-success" aria-hidden="true"></span>
  <strong>Environment:</strong> empty
</a>
</p>
</div>
<div id="strongEnvironmentstrongempty" class="panel-collapse collapse">
<div class="panel-body">
  
Great job! The global environment was empty. Objects defined in the global
environment can affect the analysis in your R Markdown file in unknown ways.
For reproduciblity it's best to always run the code in an empty environment.

</div>
</div>
</div>
<div class="panel panel-default">
<div class="panel-heading">
<p class="panel-title">
<a data-toggle="collapse" data-parent="#workflowr-checks" href="#strongSeedstrongcodesetseed20221201code">
  <span class="glyphicon glyphicon-ok text-success" aria-hidden="true"></span>
  <strong>Seed:</strong> <code>set.seed(20221201)</code>
</a>
</p>
</div>
<div id="strongSeedstrongcodesetseed20221201code" class="panel-collapse collapse">
<div class="panel-body">
  
The command <code>set.seed(20221201)</code> was run prior to running the code in the R Markdown file.
Setting a seed ensures that any results that rely on randomness, e.g.
subsampling or permutations, are reproducible.

</div>
</div>
</div>
<div class="panel panel-default">
<div class="panel-heading">
<p class="panel-title">
<a data-toggle="collapse" data-parent="#workflowr-checks" href="#strongSessioninformationstrongrecorded">
  <span class="glyphicon glyphicon-ok text-success" aria-hidden="true"></span>
  <strong>Session information:</strong> recorded
</a>
</p>
</div>
<div id="strongSessioninformationstrongrecorded" class="panel-collapse collapse">
<div class="panel-body">
  
Great job! Recording the operating system, R version, and package versions is
critical for reproducibility.

</div>
</div>
</div>
<div class="panel panel-default">
<div class="panel-heading">
<p class="panel-title">
<a data-toggle="collapse" data-parent="#workflowr-checks" href="#strongCachestrongnone">
  <span class="glyphicon glyphicon-ok text-success" aria-hidden="true"></span>
  <strong>Cache:</strong> none
</a>
</p>
</div>
<div id="strongCachestrongnone" class="panel-collapse collapse">
<div class="panel-body">
  
Nice! There were no cached chunks for this analysis, so you can be confident
that you successfully produced the results during this run.

</div>
</div>
</div>
<div class="panel panel-default">
<div class="panel-heading">
<p class="panel-title">
<a data-toggle="collapse" data-parent="#workflowr-checks" href="#strongFilepathsstrongrelative">
  <span class="glyphicon glyphicon-ok text-success" aria-hidden="true"></span>
  <strong>File paths:</strong> relative
</a>
</p>
</div>
<div id="strongFilepathsstrongrelative" class="panel-collapse collapse">
<div class="panel-body">
  
Great job! Using relative paths to the files within your workflowr project
makes it easier to run your code on other machines.

</div>
</div>
</div>
<div class="panel panel-default">
<div class="panel-heading">
<p class="panel-title">
<a data-toggle="collapse" data-parent="#workflowr-checks" href="#strongRepositoryversionstrongahrefhttpsgithubcomxsun1229factoranalysisnewtree768f8ff3767501763e71e0d18d2533cc380b1f7btargetblank768f8ffa">
  <span class="glyphicon glyphicon-ok text-success" aria-hidden="true"></span>
  <strong>Repository version:</strong> <a href="https://github.com/xsun1229/factor_analysis_new/tree/768f8ff3767501763e71e0d18d2533cc380b1f7b" target="_blank">768f8ff</a>
</a>
</p>
</div>
<div id="strongRepositoryversionstrongahrefhttpsgithubcomxsun1229factoranalysisnewtree768f8ff3767501763e71e0d18d2533cc380b1f7btargetblank768f8ffa" class="panel-collapse collapse">
<div class="panel-body">
  
<p>
Great! You are using Git for version control. Tracking code development and
connecting the code version to the results is critical for reproducibility.
</p>

<p>
The results in this page were generated with repository version <a href="https://github.com/xsun1229/factor_analysis_new/tree/768f8ff3767501763e71e0d18d2533cc380b1f7b" target="_blank">768f8ff</a>.
See the <em>Past versions</em> tab to see a history of the changes made to the
R Markdown and HTML files.
</p>

<p>
Note that you need to be careful to ensure that all relevant files for the
analysis have been committed to Git prior to generating the results (you can
use <code>wflow_publish</code> or <code>wflow_git_commit</code>). workflowr only
checks the R Markdown file, but you know if there are other scripts or data
files that it depends on. Below is the status of the Git repository when the
results were generated:
</p>

<pre><code>
Unstaged changes:
	Modified:   analysis/revision_num_pair_pass_5supp_permut.Rmd
	Modified:   analysis/revision_summarizing_ECT_CEDAR_binAF.Rmd

</code></pre>

<p>
Note that any generated files, e.g. HTML, png, CSS, etc., are not included in
this status report because it is ok for generated content to have uncommitted
changes.
</p>

</div>
</div>
</div>
</div>
<hr>
</div>
<div id="versions" class="tab-pane fade">
  
<p>
These are the previous versions of the repository in which changes were made
to the R Markdown (<code>analysis/revision_summarizing_ECT_CEDAR_binAF.Rmd</code>) and HTML (<code>docs/revision_summarizing_ECT_CEDAR_binAF.html</code>)
files. If you've configured a remote Git repository (see
<code>?wflow_git_remote</code>), click on the hyperlinks in the table below to
view the files as they were in that past version.
</p>
<div class="table-responsive">
<table class="table table-condensed table-hover">
<thead>
<tr>
<th>File</th>
<th>Version</th>
<th>Author</th>
<th>Date</th>
<th>Message</th>
</tr>
</thead>
<tbody>
<tr>
<td>Rmd</td>
<td><a href="https://github.com/xsun1229/factor_analysis_new/blob/768f8ff3767501763e71e0d18d2533cc380b1f7b/analysis/revision_summarizing_ECT_CEDAR_binAF.Rmd" target="_blank">768f8ff</a></td>
<td>XSun</td>
<td>2026-01-07</td>
<td>update</td>
</tr>
<tr>
<td>html</td>
<td><a href="https://rawcdn.githack.com/xsun1229/factor_analysis_new/768f8ff3767501763e71e0d18d2533cc380b1f7b/docs/revision_summarizing_ECT_CEDAR_binAF.html" target="_blank">768f8ff</a></td>
<td>XSun</td>
<td>2026-01-07</td>
<td>update</td>
</tr>
</tbody>
</table>
</div>

<hr>
</div>
</div>
</div>






# Introduction

We applied cutoffs of 
+
- `supporting SNP ≥ 5` and 
- `gene number in pathway ≥ 20` 

to filter the pairs before performing FDR control. All results presented below are based on this setting.

**ECT strategy: When generating “random” GWAS effects of a SNP, we sample from GWAS SNPs with comparable AF with that SNP.**



```r
library(dplyr)
library(ggplot2)
library(gridExtra)
library(forcats)
library(tidyr)

# library(ensembldb)
# library(EnsDb.Hsapiens.v75)
# library(ggrepel)
# library(ggrastr)

celltypes <- c("B_cell","CD14_positive_monocyte","CD15_positive_leukocyte","platelet","T_cell","thymocyte")
types <- c("B cell (CD19+)","Monocyte (CD14+)","Leukocyte (CD15+)","Platelet","T cell (CD4+)","T cell (CD8+)")
names(types) <- celltypes
names(celltypes) <- types

source("/project/xinhe/xsun/pathway_factor/data_v2/traits_finalselection_SLEremoved.R")

folder_ect_summary <- "/project/xinhe/xsun/pathway_factor/analysis/1.ECT_v2/ECT_summary_binAF/"

DT::datatable(matrix())
```

```{=html}
<div id="htmlwidget-0c923cdc0df2ab45b6f5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0c923cdc0df2ab45b6f5">{"x":{"filter":"none","vertical":false,"data":[[null]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>V1<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

# Number of supporting variants


```r
p <- list()
for(celltype in celltypes){

  df <- readRDS(paste0(folder_ect_summary, celltype,"_ECT_summary_poolalltraits_selected.RDS"))
  df <- df[df$trait_id %in% traits_EUR,]

  summary <- df %>%
    count(num_supp_SNP, name = "Freq")

  # Plot
  p[[celltype]] <- ggplot(summary, aes(x = num_supp_SNP, y = Freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_bw(base_line_size = 0.3) +
    ggtitle(types[celltype]) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "# of supporting SNPs in each pair", y = "# of factor–trait pairs") +
    geom_text(
      aes(label = ifelse(num_supp_SNP %% 5 == 0, as.character(Freq), "")),
      colour = "black", size = 2, vjust = -0.1, family = "Times"
    ) +
    annotate(
      geom = "text",
      x = Inf, y = Inf,
      label = paste0("# of candidate pairs = ", sum(summary$Freq[summary$num_supp_SNP >= 5])),
      hjust = 1.1, vjust = 1.5, size = 3, family = "Times", color = "black"
    ) +
    theme(
      axis.title.x = element_text(size = 10),
      axis.text.x  = element_text(size = 8, color = "black"),
      axis.title.y = element_text(size = 10),
      axis.text.y  = element_text(size = 8, color = "black"),
      text = element_text(family = "Times")
    ) +
    scale_x_continuous(breaks = seq(0, max(summary$num_supp_SNP), 5))

}


all <- grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], nrow = 2)
```

<img src="figure/revision_summarizing_ECT_CEDAR_binAF.Rmd/unnamed-chunk-2-1.png" width="864" style="display: block; margin: auto;" />

  <p>
  <button type="button" class="btn btn-default btn-xs btn-workflowr btn-workflowr-fig"
  data-toggle="collapse" data-target="#fig-unnamed-chunk-2-1">
  Past versions of unnamed-chunk-2-1.png
  </button>
  </p>

  <div id="fig-unnamed-chunk-2-1" class="collapse">
  <div class="table-responsive">
  <table class="table table-condensed table-hover">
  <thead>
  <tr>
  <th>Version</th>
  <th>Author</th>
  <th>Date</th>
  </tr>
  </thead>
  <tbody>
  <tr>
  <td><a href="https://github.com/xsun1229/factor_analysis_new/blob/768f8ff3767501763e71e0d18d2533cc380b1f7b/docs/figure/revision_summarizing_ECT_CEDAR_binAF.Rmd/unnamed-chunk-2-1.png" target="_blank">768f8ff</a></td>
  <td>XSun</td>
  <td>2026-01-07</td>
  </tr>
  </tbody>
  </table>
  </div>
  </div>
  


# Histogram for ECT p-values


```r
p <- list()
for(celltype in celltypes){

  df <- readRDS(paste0(folder_ect_summary, celltype,"_ECT_summary_poolalltraits_selected.RDS"))
  df <- df[df$trait_id %in% traits_EUR,]

  df <- df[complete.cases(df$p_ECT),]

  summary_p <- df %>%
    mutate(bin = cut(p_ECT, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)) %>%
    count(bin, name = "Freq")

  # Correctly handle both [ and ( at the start of interval labels
  summary_p <- summary_p %>%
    mutate(
      bin_low  = as.numeric(sub("[^0-9\\.]*([0-9\\.]+),.*", "\\1", bin)),
      bin_high = as.numeric(sub(".*,([0-9\\.]+)\\]", "\\1", bin)),
      bin_mid  = (bin_low + bin_high) / 2
    )

  p[[celltype]] <- ggplot(summary_p, aes(x = bin_mid, y = Freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_bw(base_line_size = 0.3) +
    ggtitle(types[celltype]) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "ECT p-value", y = "Count") +
    theme(
      axis.title.x = element_text(size = 10),
      axis.text.x  = element_text(size = 8, color = "black"),
      axis.title.y = element_text(size = 10),
      axis.text.y  = element_text(size = 8, color = "black"),
      text = element_text(family = "Times")
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1))

}

all <- grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], nrow = 2)
```

<img src="figure/revision_summarizing_ECT_CEDAR_binAF.Rmd/unnamed-chunk-3-1.png" width="864" style="display: block; margin: auto;" />

  <p>
  <button type="button" class="btn btn-default btn-xs btn-workflowr btn-workflowr-fig"
  data-toggle="collapse" data-target="#fig-unnamed-chunk-3-1">
  Past versions of unnamed-chunk-3-1.png
  </button>
  </p>

  <div id="fig-unnamed-chunk-3-1" class="collapse">
  <div class="table-responsive">
  <table class="table table-condensed table-hover">
  <thead>
  <tr>
  <th>Version</th>
  <th>Author</th>
  <th>Date</th>
  </tr>
  </thead>
  <tbody>
  <tr>
  <td><a href="https://github.com/xsun1229/factor_analysis_new/blob/768f8ff3767501763e71e0d18d2533cc380b1f7b/docs/figure/revision_summarizing_ECT_CEDAR_binAF.Rmd/unnamed-chunk-3-1.png" target="_blank">768f8ff</a></td>
  <td>XSun</td>
  <td>2026-01-07</td>
  </tr>
  </tbody>
  </table>
  </div>
  </div>
  

# Number of significant pairs at different cutoffs, for each cell type


```r
num_fdr01 <- c()
num_fdr02 <- c()
for(celltype in celltypes){

  df <- readRDS(paste0(folder_ect_summary, celltype,"_ECT_summary_poolalltraits_selected.RDS"))

  df_fdr01 <- df[df$ECT_FDR_5suppSNP < 0.1,]
  df_fdr01 <- df_fdr01[complete.cases(df_fdr01$p_ECT),]
  num_fdr01 <- c(num_fdr01, nrow(df_fdr01))

  df_fdr02 <- df[df$ECT_FDR_5suppSNP < 0.2 & df$ECT_FDR_5suppSNP >= 0.1,]
  df_fdr02 <- df_fdr02[complete.cases(df_fdr02$p_ECT),]
  num_fdr02 <- c(num_fdr02, nrow(df_fdr02))

}

sum <- data.frame(celltype = types,
                  num_fdr01 = num_fdr01,
                  num_fdr02 = num_fdr02)


df <- sum %>%
  pivot_longer(cols = starts_with("num_fdr"), names_to = "event", values_to = "total") %>%
  mutate(
    event = recode(event,
                   "num_fdr01" = "FDR < 0.1",
                   "num_fdr02" = "0.1 < FDR < 0.2")
  )

df$event <- factor(df$event, levels = c("FDR < 0.1", "0.1 < FDR < 0.2"))

# Plot
p1 <- ggplot(df, aes(x = fct_inorder(celltype), y = total, fill = event)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.9) +
  scale_fill_manual(
    name = "group",
    values = c("#F8766D", "darkturquoise"),
    labels = c("FDR < 0.1", "0.1 < FDR < 0.2")
  ) +
  geom_text(
    aes(label = total),
    position = position_dodge(width = 0.9),
    color = "black",
    size = 3,
    vjust = -0.3
  ) +
  labs(x = "Cell types", y = "Count") +
  ggtitle("# of pairs") +
  ylim(0, 30) +
  theme_bw(base_line_size = 0.3) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, vjust = 1.1),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12, color = "black"),
    text = element_text(family = "Times")
  )

p1
```

<img src="figure/revision_summarizing_ECT_CEDAR_binAF.Rmd/unnamed-chunk-4-1.png" width="384" style="display: block; margin: auto;" />

  <p>
  <button type="button" class="btn btn-default btn-xs btn-workflowr btn-workflowr-fig"
  data-toggle="collapse" data-target="#fig-unnamed-chunk-4-1">
  Past versions of unnamed-chunk-4-1.png
  </button>
  </p>

  <div id="fig-unnamed-chunk-4-1" class="collapse">
  <div class="table-responsive">
  <table class="table table-condensed table-hover">
  <thead>
  <tr>
  <th>Version</th>
  <th>Author</th>
  <th>Date</th>
  </tr>
  </thead>
  <tbody>
  <tr>
  <td><a href="https://github.com/xsun1229/factor_analysis_new/blob/768f8ff3767501763e71e0d18d2533cc380b1f7b/docs/figure/revision_summarizing_ECT_CEDAR_binAF.Rmd/unnamed-chunk-4-1.png" target="_blank">768f8ff</a></td>
  <td>XSun</td>
  <td>2026-01-07</td>
  </tr>
  </tbody>
  </table>
  </div>
  </div>
  



# Detailed results for all pairs at ECT FDR < 0.2


```r
df_all_fdr02 <- c()
for(celltype in celltypes){

  df <- readRDS(paste0(folder_ect_summary, celltype,"_ECT_summary_poolalltraits_selected.RDS"))
  df <- df[complete.cases(df$ECT_FDR_5suppSNP),]

  df_fdr02 <- df[df$ECT_FDR_5suppSNP < 0.2,]
  df_all_fdr02 <- rbind(df_all_fdr02,df_fdr02)

}

rownames(df_all_fdr02) <- NULL

#df_all_fdr02$celltype <- types[df_all_fdr02$celltype]
df_all_fdr02 <- df_all_fdr02[order(df_all_fdr02$ECT_FDR_5suppSNP),]

df_all_fdr01 <- df_all_fdr02[df_all_fdr02$ECT_FDR_5suppSNP < 0.1,]

DT::datatable(df_all_fdr02,caption = htmltools::tags$caption( style = 'caption-side: left; text-align: left; color:black;  font-size:150% ;','Pairs with ECT FDR <= 0.2 '),options = list(pageLength = 10) )
```

```{=html}
<div id="htmlwidget-ad41175326dc68f35292" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ad41175326dc68f35292">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"caption-side: left; text-align: left; color:black;  font-size:150% ;\">Pairs with ECT FDR &lt;= 0.2 <\/caption>","data":[["1","2","3"],["B cell (CD19+)","Monocyte (CD14+)","Leukocyte (CD15+)"],["00600_PC1","04911_PC2","04910_PC4"],["00600","04911","04910"],["Sphingolipid metabolism","Insulin secretion","Insulin signaling pathway"],["AS-finn-b-M13_ANKYLOSPON","RA-ukb-a-105","MCH-ebi-a-GCST90002390"],["Ankylosing spondylitis ","Rheumatoid arthritis ","Mean Corpuscular Hemoglobin "],[6,5,8],[1e-06,1e-06,1.1e-05],[0.002208,0.003709,0.015818],[47,86,137],[24,37,87]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>celltype<\/th>\n      <th>factor<\/th>\n      <th>pathwayID<\/th>\n      <th>pathwayName<\/th>\n      <th>trait_id<\/th>\n      <th>trait_name<\/th>\n      <th>num_supp_SNP<\/th>\n      <th>p_ECT<\/th>\n      <th>ECT_FDR_5suppSNP<\/th>\n      <th>n_genes_inpathway<\/th>\n      <th>n_genes_expressed<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[7,8,9,10,11]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```






# Comparing the ECT p-values from old and new ECT strategy


```r
library(DT)
library(htmltools)
library(knitr)

p <- list()
tables <- list()
for(celltype in celltypes){
  
  df_new <- readRDS(paste0(folder_ect_summary, celltype,"_ECT_summary_poolalltraits_selected.RDS"))
  df_new$id <- paste0(df_new$factor,"-", df_new$trait_id)
  df_new <- df_new[df_new$num_supp_SNP >=5,]
  df_new <- df_new[c("id","p_ECT","ECT_FDR_5suppSNP")]
  
  df_old <- readRDS(paste0("/project/xinhe/xsun/pathway_factor/analysis/1.ECT_v2/ECT_summary_originalECT/", celltype,"_ECT_summary_poolalltraits_selected.RDS"))
  df_old$id <- paste0(df_old$factor,"-", df_old$trait_id)
  df_old <- df_old[df_old$num_supp_SNP >=5,]
  df_old <- df_old[c("id","p_ECT","ECT_FDR_5suppSNP","pathwayName","trait_id","trait_name")]
  
  df_merge <- merge(df_new,df_old, by = "id")
  colnames(df_merge)[1:5] <- c("id","p_ECT_new","FDR_ECT_new" ,"p_ECT_old","FDR_ECT_old")
  
  df_plot <- df_merge %>%
    filter(!is.na(p_ECT_new), !is.na(p_ECT_old)) %>%
    mutate(
      logp_old = -log10(p_ECT_old),
      logp_new = -log10(p_ECT_new)
    )
  
  p[[celltype]] <- ggplot(df_plot, aes(x = logp_old, y = logp_new)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "red") +
    ggtitle(types[celltype]) +
    labs(
      x = expression(-log[10](p[ECT~old_strategy])),
      y = expression(-log[10](p[ECT~new_binAF]))
    ) +
    theme_classic()
  
  df_show <- df_merge[order(df_merge$p_ECT_new, decreasing = F),]
  df_show <- df_show[1:20,]
  
  tables[[celltype]] <- DT::datatable(
    df_show,
    caption = tags$caption(
      style = 'caption-side: left; text-align: left; color:black; font-size:150%;',
      paste0('Pairs with lowest new ECT p-values - ', types[celltype])
    ),
    options = list(pageLength = 10)
  )
  
  tables[[celltype]] <- datatable(
    df_show,
    caption = tags$caption(
      style = 'caption-side:left; text-align:left; color:black; font-size:140%;',
      paste0('Pairs with lowest new ECT p-values - ', types[celltype])
    ),
    options = list(pageLength = 10)
  )
  
  # print(
  #   DT::datatable(
  #     df_show,
  #     caption = tags$caption(
  #       style = 'caption-side: left; text-align: left; color:black; font-size:150%;',
  #       paste0('Pairs with lowest new ECT p-values - ', types[celltype])
  #     ),
  #     options = list(pageLength = 10)
  #   )
  # )
  # 
  # cat("\n\n")
  
  
}

all <- grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], nrow = 2)
```

<img src="figure/revision_summarizing_ECT_CEDAR_binAF.Rmd/unnamed-chunk-6-1.png" width="864" style="display: block; margin: auto;" />

```r
for(celltype in celltypes) {
 
  cat(knit_print(tables[[celltype]]))
}
```

```{=html}
<div id="htmlwidget-66aaeb19fe992dfb51c2" style="width:960px;height:500px;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-66aaeb19fe992dfb51c2">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"caption-side:left; text-align:left; color:black; font-size:140%;\">Pairs with lowest new ECT p-values - B cell (CD19+)<\/caption>","data":[["167","992","1005","122","219","1072","1213","186","182","543","316","511","619","318","328","1846","1659","1965","510","1836"],["00600_PC1-AS-finn-b-M13_ANKYLOSPON","04211_PC3-T1D-ebi-a-GCST90014023","04213_PC2-RET-ebi-a-GCST90002405","00510_PC2-lymphc-ebi-a-GCST90002388","03010_PC2-MCH-ebi-a-GCST90002390","04218_PC4-WBC-ebi-a-GCST90002407","04380_PC3-RET-ebi-a-GCST90002405","00640_PC1-T1D-ebi-a-GCST90014023","00630_PC4-lymphc-ebi-a-GCST90002388","04062_PC2-Psoriasis-ebi-a-GCST90038681","03040_PC4-MCV-ebi-a-GCST90002392","04060_PC2-HCT-ebi-a-GCST90002383","04068_PC3-WBC-ebi-a-GCST90002407","03040_PC4-RBC-ebi-a-GCST90002403","03050_PC3-RET-ebi-a-GCST90002405","04730_PC5-MCH-ebi-a-GCST90002390","04668_PC2-RET-ebi-a-GCST90002405","04917_PC2-RET-ebi-a-GCST90002405","04060_PC2-HB-ebi-a-GCST90002384","04728_PC4-MPV-ebi-a-GCST90002395"],[1e-06,0.000203,0.000616,0.000883,0.0011999,0.0012999,0.0015998,0.0017998,0.0024998,0.0028997,0.0030997,0.0036996,0.0038996,0.0040996,0.0043996,0.0044996,0.0050995,0.0051995,0.0059994,0.0060994],[0.002208,0.224112,0.453376,0.4783632,0.4783632,0.4783632,0.4967448,0.4967448,0.613284266666667,0.6209448,0.6209448,0.6209448,0.6209448,0.6209448,0.6209448,0.6209448,0.629623466666667,0.629623466666667,0.629623466666667,0.629623466666667],[0.0458954,0.000195,0.000221,0.000278,0.00045,0.3866134,0.001153,0.0012999,0.0018998,4e-05,0.0016998,0.000565,0.0029997,0.0017998,0.0018998,0.0020998,0.0021998,0.001053,0.000489,0.0030997],[0.37685869010989,0.138613333333333,0.138613333333333,0.138613333333333,0.138613333333333,0.706728471676301,0.173469942857143,0.173469942857143,0.173469942857143,0.08832,0.173469942857143,0.138613333333333,0.189238217142857,0.173469942857143,0.173469942857143,0.173469942857143,0.173469942857143,0.173469942857143,0.138613333333333,0.190114933333333],["Sphingolipid metabolism","Longevity regulating pathway","Longevity regulating pathway - multiple species","N-Glycan biosynthesis","Ribosome","Cellular senescence","Osteoclast differentiation","Propanoate metabolism","Glyoxylate and dicarboxylate metabolism","Chemokine signaling pathway","Spliceosome","Cytokine-cytokine receptor interaction","FoxO signaling pathway","Spliceosome","Proteasome","Long-term depression","TNF signaling pathway","Prolactin signaling pathway","Cytokine-cytokine receptor interaction","Dopaminergic synapse"],["AS-finn-b-M13_ANKYLOSPON","T1D-ebi-a-GCST90014023","RET-ebi-a-GCST90002405","lymphc-ebi-a-GCST90002388","MCH-ebi-a-GCST90002390","WBC-ebi-a-GCST90002407","RET-ebi-a-GCST90002405","T1D-ebi-a-GCST90014023","lymphc-ebi-a-GCST90002388","Psoriasis-ebi-a-GCST90038681","MCV-ebi-a-GCST90002392","HCT-ebi-a-GCST90002383","WBC-ebi-a-GCST90002407","RBC-ebi-a-GCST90002403","RET-ebi-a-GCST90002405","MCH-ebi-a-GCST90002390","RET-ebi-a-GCST90002405","RET-ebi-a-GCST90002405","HB-ebi-a-GCST90002384","MPV-ebi-a-GCST90002395"],["Ankylosing spondylitis ","Type 1 diabetes ","Reticulocyte count","Lymphocyte count  lymphc","Mean Corpuscular Hemoglobin ","White blood cell count ","Reticulocyte count","Type 1 diabetes ","Lymphocyte count  lymphc","Psoriasis","Mean Corpuscular Volume","Hematocrit","White blood cell count ","Red blood cell count ","Reticulocyte count","Mean Corpuscular Hemoglobin ","Reticulocyte count","Reticulocyte count","Hemoglobin ","Mean Platelet Volume"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>p_ECT_new<\/th>\n      <th>FDR_ECT_new<\/th>\n      <th>p_ECT_old<\/th>\n      <th>FDR_ECT_old<\/th>\n      <th>pathwayName<\/th>\n      <th>trait_id<\/th>\n      <th>trait_name<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

```{=html}
<div id="htmlwidget-1814506231684700bf53" style="width:960px;height:500px;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1814506231684700bf53">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"caption-side:left; text-align:left; color:black; font-size:140%;\">Pairs with lowest new ECT p-values - Monocyte (CD14+)<\/caption>","data":[["3184","1047","1089","3449","1283","2002","3709","2446","1463","2141","1328","3154","2707","911","2354","3707","2533","2543","1739","117"],["04911_PC2-RA-ukb-a-105","04068_PC1-RBC-ebi-a-GCST90002403","04070_PC2-asthma_pmid36778051","04924_PC3-asthma_pmid36778051","04120_PC2-asthma_pmid36778051","04340_PC4-celiac-ieu-a-1058","04979_PC5-lymphc-ebi-a-GCST90002388","04622_PC1-MCH-ebi-a-GCST90002390","04142_PC3-WBC-ebi-a-GCST90002407","04390_PC2-MPV-ebi-a-GCST90002395","04136_PC3-UC-ebi-a-GCST003045","04810_PC5-UC-ebi-a-GCST003045","04664_PC1-asthma_pmid36778051","04020_PC5-MCH-ebi-a-GCST90002390","04611_PC1-HCT-ebi-a-GCST90002383","04979_PC1-lymphc-ebi-a-GCST90002388","04630_PC5-lymphc-ebi-a-GCST90002388","04650_PC1-RBC-ebi-a-GCST90002403","04211_PC4-MPV-ebi-a-GCST90002395","00230_PC3-CD-ebi-a-GCST003044"],[1e-06,0.000189,0.000228,0.000231,0.000406,0.000822,0.00101,0.0010999,0.0011999,0.0015998,0.0018998,0.0018998,0.0023998,0.0025997,0.0026997,0.0027997,0.0029997,0.0031997,0.0036996,0.0037996],[0.003709,0.21419475,0.21419475,0.21419475,0.3011708,0.494492122222222,0.494492122222222,0.494492122222222,0.494492122222222,0.587196516666667,0.587196516666667,0.587196516666667,0.64900545625,0.64900545625,0.64900545625,0.64900545625,0.654463958823529,0.659315961111111,0.70463582,0.70463582],[0.0183982,7.1e-05,0.000309,0.000105,0.000361,0.000659,0.000527,0.0010999,0.00046,4.3e-05,0.000761,0.0018998,0.0022998,0.000895,0.0027997,0.0012999,0.0017998,0.0027997,0.000349,0.0028997],[0.454606147741936,0.129815,0.201345714285714,0.129815,0.201345714285714,0.222202818181818,0.2121548,0.226640505555556,0.2121548,0.129815,0.226640505555556,0.250881123529412,0.250881123529412,0.226640505555556,0.256071126190476,0.247235758333333,0.247239192592593,0.256071126190476,0.201345714285714,0.256071126190476],["Insulin secretion","FoxO signaling pathway","Phosphatidylinositol signaling system","Renin secretion","Ubiquitin mediated proteolysis","Hedgehog signaling pathway","Cholesterol metabolism","RIG-I-like receptor signaling pathway","Lysosome","Hippo signaling pathway","Autophagy - other","Regulation of actin cytoskeleton","Fc epsilon RI signaling pathway","Calcium signaling pathway","Platelet activation","Cholesterol metabolism","JAK-STAT signaling pathway","Natural killer cell mediated cytotoxicity","Longevity regulating pathway","Purine metabolism"],["RA-ukb-a-105","RBC-ebi-a-GCST90002403","asthma_pmid36778051","asthma_pmid36778051","asthma_pmid36778051","celiac-ieu-a-1058","lymphc-ebi-a-GCST90002388","MCH-ebi-a-GCST90002390","WBC-ebi-a-GCST90002407","MPV-ebi-a-GCST90002395","UC-ebi-a-GCST003045","UC-ebi-a-GCST003045","asthma_pmid36778051","MCH-ebi-a-GCST90002390","HCT-ebi-a-GCST90002383","lymphc-ebi-a-GCST90002388","lymphc-ebi-a-GCST90002388","RBC-ebi-a-GCST90002403","MPV-ebi-a-GCST90002395","CD-ebi-a-GCST003044"],["Rheumatoid arthritis ","Red blood cell count ","Asthma","Asthma","Asthma","Celiac disease","Lymphocyte count  lymphc","Mean Corpuscular Hemoglobin ","White blood cell count ","Mean Platelet Volume","Ulcerative colitis ","Ulcerative colitis ","Asthma","Mean Corpuscular Hemoglobin ","Hematocrit","Lymphocyte count  lymphc","Lymphocyte count  lymphc","Red blood cell count ","Mean Platelet Volume","Crohn<U+2019>s disease "]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>p_ECT_new<\/th>\n      <th>FDR_ECT_new<\/th>\n      <th>p_ECT_old<\/th>\n      <th>FDR_ECT_old<\/th>\n      <th>pathwayName<\/th>\n      <th>trait_id<\/th>\n      <th>trait_name<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

```{=html}
<div id="htmlwidget-43c5071fe8a890b96ecd" style="width:960px;height:500px;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-43c5071fe8a890b96ecd">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"caption-side:left; text-align:left; color:black; font-size:140%;\">Pairs with lowest new ECT p-values - Leukocyte (CD15+)<\/caption>","data":[["1202","913","351","751","239","1412","823","946","74","1313","306","126","128","345","648","688","548","115","1044","835"],["04910_PC4-MCH-ebi-a-GCST90002390","04630_PC2-WBC-ebi-a-GCST90002407","04068_PC2-lymphc-ebi-a-GCST90002388","04371_PC4-MCH-ebi-a-GCST90002390","04010_PC2-MCV-ebi-a-GCST90002392","04971_PC1-MCV-ebi-a-GCST90002392","04550_PC2-lymphc-ebi-a-GCST90002388","04658_PC4-HB-ebi-a-GCST90002384","00510_PC3-LDL-ukb-d-30780_irnt","04922_PC2-MCV-ebi-a-GCST90002392","04024_PC4-RBC-ebi-a-GCST90002403","03008_PC2-allergy-ebi-a-GCST005038","03008_PC4-LDL-ukb-d-30780_irnt","04068_PC1-celiac-ieu-a-1058","04217_PC4-MCH-ebi-a-GCST90002390","04270_PC1-HT-ebi-a-GCST90018855","04146_PC3-HT-ebi-a-GCST90018855","00590_PC5-CD-ebi-a-GCST003044","04714_PC4-IBD-ebi-a-GCST003043","04611_PC3-lymphc-ebi-a-GCST90002388"],[1.1e-05,0.0022998,0.0026997,0.0026997,0.0050995,0.0059994,0.0063994,0.0064994,0.0074993,0.0077992,0.0107989,0.0114989,0.0122988,0.0132987,0.0140986,0.0145985,0.0169983,0.0170983,0.0178982,0.020298],[0.015818,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528,0.914792517924528],[1e-06,0.0010999,0.0048995,0.000528,0.0012999,0.0013999,0.0026997,0.0011999,0.0028997,0.0109989,0.0023998,0.0091991,0.0063994,0.0105989,0.0047995,0.039996,0.0035996,0.0051995,0.0105989,0.0044996],[0.001438,0.287579457142857,0.302976328571429,0.287579457142857,0.287579457142857,0.287579457142857,0.302976328571429,0.287579457142857,0.302976328571429,0.337457514285714,0.302976328571429,0.337457514285714,0.306744573333333,0.337457514285714,0.302976328571429,0.456462285714286,0.302976328571429,0.302976328571429,0.337457514285714,0.302976328571429],["Insulin signaling pathway","JAK-STAT signaling pathway","FoxO signaling pathway","Apelin signaling pathway","MAPK signaling pathway","Gastric acid secretion","Signaling pathways regulating pluripotency of stem cells","Th1 and Th2 cell differentiation","N-Glycan biosynthesis","Glucagon signaling pathway","cAMP signaling pathway","Ribosome biogenesis in eukaryotes","Ribosome biogenesis in eukaryotes","FoxO signaling pathway","Necroptosis","Vascular smooth muscle contraction","Peroxisome","Arachidonic acid metabolism","Thermogenesis","Platelet activation"],["MCH-ebi-a-GCST90002390","WBC-ebi-a-GCST90002407","lymphc-ebi-a-GCST90002388","MCH-ebi-a-GCST90002390","MCV-ebi-a-GCST90002392","MCV-ebi-a-GCST90002392","lymphc-ebi-a-GCST90002388","HB-ebi-a-GCST90002384","LDL-ukb-d-30780_irnt","MCV-ebi-a-GCST90002392","RBC-ebi-a-GCST90002403","allergy-ebi-a-GCST005038","LDL-ukb-d-30780_irnt","celiac-ieu-a-1058","MCH-ebi-a-GCST90002390","HT-ebi-a-GCST90018855","HT-ebi-a-GCST90018855","CD-ebi-a-GCST003044","IBD-ebi-a-GCST003043","lymphc-ebi-a-GCST90002388"],["Mean Corpuscular Hemoglobin ","White blood cell count ","Lymphocyte count  lymphc","Mean Corpuscular Hemoglobin ","Mean Corpuscular Volume","Mean Corpuscular Volume","Lymphocyte count  lymphc","Hemoglobin ","LDL cholesterol ","Mean Corpuscular Volume","Red blood cell count ","Allergy","LDL cholesterol ","Celiac disease","Mean Corpuscular Hemoglobin ","Hashimoto thyroiditis","Hashimoto thyroiditis","Crohn<U+2019>s disease ","Inflammatory bowel disease ","Lymphocyte count  lymphc"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>p_ECT_new<\/th>\n      <th>FDR_ECT_new<\/th>\n      <th>p_ECT_old<\/th>\n      <th>FDR_ECT_old<\/th>\n      <th>pathwayName<\/th>\n      <th>trait_id<\/th>\n      <th>trait_name<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

```{=html}
<div id="htmlwidget-3f22443f93eeeda03f7b" style="width:960px;height:500px;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3f22443f93eeeda03f7b">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"caption-side:left; text-align:left; color:black; font-size:140%;\">Pairs with lowest new ECT p-values - Platelet<\/caption>","data":[["935","36","931","1308","1767","821","1137","736","1572","136","129","905","274","1460","728","1541","801","708","326","311"],["04390_PC5-RET-ebi-a-GCST90002405","00230_PC1-RET-ebi-a-GCST90002405","04390_PC3-LDL-ukb-d-30780_irnt","04714_PC4-RET-ebi-a-GCST90002405","04971_PC4-HB-ebi-a-GCST90002384","04270_PC4-UC-ebi-a-GCST003045","04625_PC3-RET-ebi-a-GCST90002405","04217_PC3-HCT-ebi-a-GCST90002383","04918_PC4-MCV-ebi-a-GCST90002392","03018_PC2-RET-ebi-a-GCST90002405","03013_PC5-UC-ebi-a-GCST003045","04371_PC5-HCT-ebi-a-GCST90002383","04022_PC4-WBC-ebi-a-GCST90002407","04750_PC3-WBC-ebi-a-GCST90002407","04216_PC3-RET-ebi-a-GCST90002405","04915_PC5-WBC-ebi-a-GCST90002407","04261_PC4-RET-ebi-a-GCST90002405","04211_PC5-MCH-ebi-a-GCST90002390","04062_PC2-WBC-ebi-a-GCST90002407","04061_PC2-MCH-ebi-a-GCST90002390"],[0.000574,0.000784,0.000785,0.0011999,0.0021998,0.0022998,0.0029997,0.0032997,0.0052995,0.0059994,0.0069993,0.0070993,0.0085991,0.0089991,0.009599,0.009699,0.010099,0.0121988,0.0123988,0.0132987],[0.478326666666667,0.478326666666667,0.478326666666667,0.5483543,0.7006724,0.7006724,0.75398145,0.75398145,0.8396540975,0.8396540975,0.8396540975,0.8396540975,0.8396540975,0.8396540975,0.8396540975,0.8396540975,0.8396540975,0.8396540975,0.8396540975,0.8396540975],[6.7e-05,0.000177,0.00064,9.4e-05,0.000677,0.000803,0.000595,0.0040996,0.0013999,0.0027997,0.0032997,0.0042996,0.0024998,0.0056994,0.0039996,0.010199,0.0017998,0.0079992,0.0018998,0.0039996],[0.0559926666666667,0.07907475,0.172828428571429,0.0559926666666667,0.172828428571429,0.179370125,0.172828428571429,0.201591928205128,0.201591928205128,0.201591928205128,0.201591928205128,0.201591928205128,0.201591928205128,0.2165844,0.201591928205128,0.239810697368421,0.201591928205128,0.2165844,0.201591928205128,0.201591928205128],["Hippo signaling pathway","Purine metabolism","Hippo signaling pathway","Thermogenesis","Gastric acid secretion","Vascular smooth muscle contraction","C-type lectin receptor signaling pathway","Necroptosis","Thyroid hormone synthesis","RNA degradation","RNA transport","Apelin signaling pathway","cGMP-PKG signaling pathway","Inflammatory mediator regulation of TRP channels","Ferroptosis","Estrogen signaling pathway","Adrenergic signaling in cardiomyocytes","Longevity regulating pathway","Chemokine signaling pathway","Viral protein interaction with cytokine and cytokine receptor"],["RET-ebi-a-GCST90002405","RET-ebi-a-GCST90002405","LDL-ukb-d-30780_irnt","RET-ebi-a-GCST90002405","HB-ebi-a-GCST90002384","UC-ebi-a-GCST003045","RET-ebi-a-GCST90002405","HCT-ebi-a-GCST90002383","MCV-ebi-a-GCST90002392","RET-ebi-a-GCST90002405","UC-ebi-a-GCST003045","HCT-ebi-a-GCST90002383","WBC-ebi-a-GCST90002407","WBC-ebi-a-GCST90002407","RET-ebi-a-GCST90002405","WBC-ebi-a-GCST90002407","RET-ebi-a-GCST90002405","MCH-ebi-a-GCST90002390","WBC-ebi-a-GCST90002407","MCH-ebi-a-GCST90002390"],["Reticulocyte count","Reticulocyte count","LDL cholesterol ","Reticulocyte count","Hemoglobin ","Ulcerative colitis ","Reticulocyte count","Hematocrit","Mean Corpuscular Volume","Reticulocyte count","Ulcerative colitis ","Hematocrit","White blood cell count ","White blood cell count ","Reticulocyte count","White blood cell count ","Reticulocyte count","Mean Corpuscular Hemoglobin ","White blood cell count ","Mean Corpuscular Hemoglobin "]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>p_ECT_new<\/th>\n      <th>FDR_ECT_new<\/th>\n      <th>p_ECT_old<\/th>\n      <th>FDR_ECT_old<\/th>\n      <th>pathwayName<\/th>\n      <th>trait_id<\/th>\n      <th>trait_name<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

```{=html}
<div id="htmlwidget-198b30f56771bc600b2c" style="width:960px;height:500px;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-198b30f56771bc600b2c">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"caption-side:left; text-align:left; color:black; font-size:140%;\">Pairs with lowest new ECT p-values - T cell (CD4+)<\/caption>","data":[["1859","66","1825","441","1011","2040","858","640","1826","337","1575","2275","969","1299","2401","2244","2501","779","2342","444"],["04666_PC2-RBC-ebi-a-GCST90002403","00190_PC5-MPV-ebi-a-GCST90002395","04662_PC4-lymphc-ebi-a-GCST90002388","03040_PC5-PLT-ebi-a-GCST90002402","04142_PC5-WBC-ebi-a-GCST90002407","04724_PC1-IBD-ebi-a-GCST003043","04110_PC3-MCV-ebi-a-GCST90002392","04022_PC5-WBC-ebi-a-GCST90002407","04662_PC5-CD-ebi-a-GCST003044","03013_PC3-MCV-ebi-a-GCST90002392","04611_PC2-lymphc-ebi-a-GCST90002388","04919_PC3-CD-ebi-a-GCST003044","04140_PC5-RBC-ebi-a-GCST90002403","04261_PC3-UC-ebi-a-GCST003045","04927_PC4-IBD-ebi-a-GCST003043","04917_PC2-CD-ebi-a-GCST003044","04971_PC1-IBD-ebi-a-GCST003043","04068_PC2-PLT-ebi-a-GCST90002402","04923_PC1-WBC-ebi-a-GCST90002407","03050_PC3-WBC-ebi-a-GCST90002407"],[0.000781,0.0013999,0.0016998,0.0017998,0.0019998,0.0022998,0.0024998,0.0029997,0.0031997,0.0032997,0.0037996,0.0037996,0.0038996,0.0048995,0.0051995,0.0055994,0.0056994,0.0058994,0.0065993,0.0068993],[0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182,0.666888181818182],[0.000212,0.000216,0.0010999,0.0013999,0.0189981,0.0014999,0.000402,0.0028997,0.0016998,0.0016998,0.000445,0.0046995,0.0023998,0.0035996,0.0012999,0.0025997,0.0043996,0.0061994,0.0044996,0.0011999],[0.255469941176471,0.255469941176471,0.255469941176471,0.255469941176471,0.369225474452555,0.255469941176471,0.255469941176471,0.266827166666667,0.255469941176471,0.255469941176471,0.255469941176471,0.266827166666667,0.255478708333333,0.266827166666667,0.255469941176471,0.265297442307692,0.266827166666667,0.311212936363636,0.266827166666667,0.255469941176471],["Fc gamma R-mediated phagocytosis","Oxidative phosphorylation","B cell receptor signaling pathway","Spliceosome","Lysosome","Glutamatergic synapse","Cell cycle","cGMP-PKG signaling pathway","B cell receptor signaling pathway","RNA transport","Platelet activation","Thyroid hormone signaling pathway","Autophagy - animal","Adrenergic signaling in cardiomyocytes","Cortisol synthesis and secretion","Prolactin signaling pathway","Gastric acid secretion","FoxO signaling pathway","Regulation of lipolysis in adipocytes","Proteasome"],["RBC-ebi-a-GCST90002403","MPV-ebi-a-GCST90002395","lymphc-ebi-a-GCST90002388","PLT-ebi-a-GCST90002402","WBC-ebi-a-GCST90002407","IBD-ebi-a-GCST003043","MCV-ebi-a-GCST90002392","WBC-ebi-a-GCST90002407","CD-ebi-a-GCST003044","MCV-ebi-a-GCST90002392","lymphc-ebi-a-GCST90002388","CD-ebi-a-GCST003044","RBC-ebi-a-GCST90002403","UC-ebi-a-GCST003045","IBD-ebi-a-GCST003043","CD-ebi-a-GCST003044","IBD-ebi-a-GCST003043","PLT-ebi-a-GCST90002402","WBC-ebi-a-GCST90002407","WBC-ebi-a-GCST90002407"],["Red blood cell count ","Mean Platelet Volume","Lymphocyte count  lymphc","Platelet count ","White blood cell count ","Inflammatory bowel disease ","Mean Corpuscular Volume","White blood cell count ","Crohn<U+2019>s disease ","Mean Corpuscular Volume","Lymphocyte count  lymphc","Crohn<U+2019>s disease ","Red blood cell count ","Ulcerative colitis ","Inflammatory bowel disease ","Crohn<U+2019>s disease ","Inflammatory bowel disease ","Platelet count ","White blood cell count ","White blood cell count "]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>p_ECT_new<\/th>\n      <th>FDR_ECT_new<\/th>\n      <th>p_ECT_old<\/th>\n      <th>FDR_ECT_old<\/th>\n      <th>pathwayName<\/th>\n      <th>trait_id<\/th>\n      <th>trait_name<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

```{=html}
<div id="htmlwidget-b544a055166914a18edc" style="width:960px;height:500px;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b544a055166914a18edc">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"caption-side:left; text-align:left; color:black; font-size:140%;\">Pairs with lowest new ECT p-values - T cell (CD8+)<\/caption>","data":[["1768","1833","2377","1161","2251","2075","1177","2313","1121","2344","2363","1136","1349","625","1255","1485","977","524","610","1878"],["04670_PC1-IBD-ebi-a-GCST003043","04714_PC4-RBC-ebi-a-GCST90002403","04972_PC3-WBC-ebi-a-GCST90002407","04217_PC5-IBD-ebi-a-GCST003043","04925_PC3-RET-ebi-a-GCST90002405","04911_PC3-MCV-ebi-a-GCST90002392","04218_PC5-lymphc-ebi-a-GCST90002388","04935_PC1-MPV-ebi-a-GCST90002395","04211_PC5-MPV-ebi-a-GCST90002395","04962_PC2-RET-ebi-a-GCST90002405","04971_PC1-IBD-ebi-a-GCST003043","04215_PC2-lymphc-ebi-a-GCST90002388","04390_PC4-PLT-ebi-a-GCST90002402","04060_PC2-IBD-ebi-a-GCST003043","04340_PC4-MCV-ebi-a-GCST90002392","04612_PC3-MPV-ebi-a-GCST90002395","04144_PC3-MCV-ebi-a-GCST90002392","04014_PC3-PLT-ebi-a-GCST90002402","04024_PC4-MCV-ebi-a-GCST90002392","04722_PC3-MCV-ebi-a-GCST90002392"],[0.000554,0.0023998,0.0026997,0.0027997,0.0028997,0.0029997,0.0037996,0.0042996,0.0047995,0.0047995,0.0050995,0.0052995,0.0056994,0.0058994,0.0062994,0.0070993,0.0071993,0.0072993,0.0073993,0.0078992],[0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431,0.775869396078431],[8.3e-05,0.0012999,0.0012999,0.0011999,0.0013999,0.0019998,0.001328,0.0018998,0.00018,0.000346,0.000189,0.0037996,0.000436,0.000527,0.0023998,0.0016998,0.010099,0.0010999,0.0038996,0.0044996],[0.1001395,0.179230745714286,0.179230745714286,0.179230745714286,0.179230745714286,0.179230745714286,0.179230745714286,0.179230745714286,0.11401425,0.1669796,0.11401425,0.1993138,0.175344666666667,0.179230745714286,0.179230745714286,0.179230745714286,0.282875712643678,0.179230745714286,0.200207123404255,0.212892839215686],["Leukocyte transendothelial migration","Thermogenesis","Pancreatic secretion","Necroptosis","Aldosterone synthesis and secretion","Insulin secretion","Cellular senescence","Growth hormone synthesis, secretion and action","Longevity regulating pathway","Vasopressin-regulated water reabsorption","Gastric acid secretion","Apoptosis - multiple species","Hippo signaling pathway","Cytokine-cytokine receptor interaction","Hedgehog signaling pathway","Antigen processing and presentation","Endocytosis","Ras signaling pathway","cAMP signaling pathway","Neurotrophin signaling pathway"],["IBD-ebi-a-GCST003043","RBC-ebi-a-GCST90002403","WBC-ebi-a-GCST90002407","IBD-ebi-a-GCST003043","RET-ebi-a-GCST90002405","MCV-ebi-a-GCST90002392","lymphc-ebi-a-GCST90002388","MPV-ebi-a-GCST90002395","MPV-ebi-a-GCST90002395","RET-ebi-a-GCST90002405","IBD-ebi-a-GCST003043","lymphc-ebi-a-GCST90002388","PLT-ebi-a-GCST90002402","IBD-ebi-a-GCST003043","MCV-ebi-a-GCST90002392","MPV-ebi-a-GCST90002395","MCV-ebi-a-GCST90002392","PLT-ebi-a-GCST90002402","MCV-ebi-a-GCST90002392","MCV-ebi-a-GCST90002392"],["Inflammatory bowel disease ","Red blood cell count ","White blood cell count ","Inflammatory bowel disease ","Reticulocyte count","Mean Corpuscular Volume","Lymphocyte count  lymphc","Mean Platelet Volume","Mean Platelet Volume","Reticulocyte count","Inflammatory bowel disease ","Lymphocyte count  lymphc","Platelet count ","Inflammatory bowel disease ","Mean Corpuscular Volume","Mean Platelet Volume","Mean Corpuscular Volume","Platelet count ","Mean Corpuscular Volume","Mean Corpuscular Volume"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>p_ECT_new<\/th>\n      <th>FDR_ECT_new<\/th>\n      <th>p_ECT_old<\/th>\n      <th>FDR_ECT_old<\/th>\n      <th>pathwayName<\/th>\n      <th>trait_id<\/th>\n      <th>trait_name<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

<br>
<p>
<button type="button" class="btn btn-default btn-workflowr btn-workflowr-sessioninfo"
  data-toggle="collapse" data-target="#workflowr-sessioninfo"
  style = "display: block;">
  <span class="glyphicon glyphicon-wrench" aria-hidden="true"></span>
  Session information
</button>
</p>

<div id="workflowr-sessioninfo" class="collapse">

```r
sessionInfo()
```

```
R version 4.2.0 (2022-04-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: CentOS Linux 7 (Core)

Matrix products: default
BLAS/LAPACK: /software/openblas-0.3.13-el7-x86_64/lib/libopenblas_haswellp-r0.3.13.so

locale:
[1] C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] knitr_1.39      htmltools_0.5.2 DT_0.22         tidyr_1.3.0    
[5] forcats_0.5.1   gridExtra_2.3   ggplot2_4.0.0   dplyr_1.1.4    

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.12        highr_0.9          RColorBrewer_1.1-3 pillar_1.9.0      
 [5] compiler_4.2.0     bslib_0.3.1        later_1.3.0        jquerylib_0.1.4   
 [9] git2r_0.30.1       workflowr_1.7.0    tools_4.2.0        digest_0.6.29     
[13] gtable_0.3.6       jsonlite_1.8.0     evaluate_0.15      lifecycle_1.0.4   
[17] tibble_3.2.1       pkgconfig_2.0.3    rlang_1.1.2        cli_3.6.1         
[21] rstudioapi_0.13    crosstalk_1.2.0    yaml_2.3.5         xfun_0.41         
[25] fastmap_1.1.0      withr_2.5.0        stringr_1.5.1      htmlwidgets_1.5.4 
[29] generics_0.1.4     fs_1.5.2           vctrs_0.6.5        sass_0.4.1        
[33] grid_4.2.0         rprojroot_2.0.3    tidyselect_1.2.0   glue_1.6.2        
[37] R6_2.5.1           fansi_1.0.3        rmarkdown_2.25     purrr_1.0.2       
[41] farver_2.1.0       magrittr_2.0.3     whisker_0.4        scales_1.4.0      
[45] promises_1.2.0.1   dichromat_2.0-0.1  httpuv_1.6.5       labeling_0.4.2    
[49] S7_0.2.0           utf8_1.2.2         stringi_1.7.6     
```
</div>

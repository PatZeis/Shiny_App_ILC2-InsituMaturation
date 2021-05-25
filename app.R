library(shiny)
library(shinybusy)
library(RaceID)
library(RColorBrewer)
library(dplyr)
library(DT)


source("fracdotplotmapleg_one.R")
source("plotmap_specific_cluster.RData.R")
source("plot3marker.R")
source("plotexpression_multipe_genes_samp.R")
source("plotsymbolmap.R")


set2 <- brewer.pal(7, "Set2")
set1 <- brewer.pal(7, "Set1")
set1 <- c(set1[7], set2[6],set1[5], set1[3], set1[4],  set1[1], set1[2]) 
set4 <- c(set1[6], set1[7])

marker_col <- set1[c(3, 6, 7, 4)]

# Define UI ----
ui <- navbarPage("In situ maturation and tissue adaptation of type 2 innate lymphoid cell progenitors (select tabs to navigate)",
 #tabsetPanel( id="tabs",
  tabPanel(value = "tab1", title = "Cover Page",
    a(href="https://imprint.ie-freiburg.mpg.de/imprint/imprint.php?ref=12","Inprint"),
    br(),
    a(href="https://imprint.ie-freiburg.mpg.de/privacy/privacy.php?ref=12", "Privacy Notice" ),
    h4("Shiny App Contact:", a(href="mailto:zeis@ie-freiburg.mpg.de","Patrice Zeis")),
    h1("In situ maturation and tissue adaptation of type 2 innate lymphoid cell progenitors"),
    br(),
    h4("Patrice Zeis, Mi Lian, Xiying Fan, Josip S. Herman, Daniela C. Hernandez, Rebecca Gentek, Shlomo Elias, Cornelia Symowski, Konrad Knöpper, Nina Peltokangas, Christin Friedrich, Remi Doucet-Ladeveze, Agnieszka M. Kabat, Richard M. Locksley, David Voehringer, Marc Bajenoff, Alexander Y. Rudensky, Chiara Romagnani, Dominic Grün, Georg Gasteiger"),
    br(),
    h5("To investigate how ILCs are locally maintained, Zeis et al. generated a single cell atlas of lung ILCs and tracked Il18r1+ progenitor and effector ILC2s. Their work identifies tissue-resident and circulating ILCPs, and highlights in situ differentiation and tissue adaptation as a mechanism of ILC renewal and phenotypic diversification."),
    br(),
    img(src = "grahical_abstract_v4.2.png", heigh = 1000, width = 1000)
    ),
  tabPanel(value = "tab2", title = "Resting Lung",
    h4("Shiny App Contact:", a("Patrice Zeis")),
    em("If red progress bar keeps appearing, shiny app is loading. Please wait patiently!"),
    br(),
    h1("Resting Lung Dataset"),
    add_busy_bar(color = "red", height = "16px"),
    fluidRow(
      column(6, h3("Resting Lung Cluster"), helpText("")),
      column(6, textInput("plotmapcluster", h5("which cluster to depict ( if more than one, enter comma delimited vector)"), value = "1,5,11"))
    ),
    fluidRow(
      column(6, img(src = "cluster_new.png", heigh = 600, width = 600)),
      column(6, plotOutput("plot_specfic_cluster", width = "100%", height = "600px"))
    ),
    fluidRow(
      textInput("Gene", h3("Gene to Plot"), value = "Gata3" )
    ),
    fluidRow(
      column(6, h3(""), helpText("log2 norm. expression")),
      column(6, h3(""), helpText("norm. expression"))
    ),
    fluidRow(
      column(6, plotOutput("expression_plotlogT", width = "100%", height = "600px")),
      column(6, plotOutput("expression_plotlogF", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("FractionDotPlot for Clusters" )
    ),
    fluidRow(
      textInput("Genes", h5("Genes to Plot (  comma delimited )"), value = "Tbx21,Rorc,Gata3,Il1rl1,Il17rb,Il2ra,Il18r1,Icos,Thy1,Il7r,Zbtb16,Tcf7,Cd7,Cd3e,S100a6,Id2,Arg1,Klrg1,Mki67,Vim,Cd69,S1pr1,Klf2,Cxcl2,Csf2,Il2,Calca,Il5,Il13,Areg,Klf4,Rgs2")
    ),
    fluidRow(
      textInput("Clusters", h5("clusters to include ( comma delimited )"), value = "1,5,11,9,8,4,10,12,3,6,7,2")
    ),
    fluidRow(
      column(6, h5("zscore of mean expression"), helpText("")),
      column(6, h5("log2 mean expression"), helpText(""))
    ),
    fluidRow(
      column(6, plotOutput("fracdotplot_zscT", width = "100%", height = "600px")),
      column(6, plotOutput("fracdotplot_log2mean", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("3 Genes Scatterplot" )
    ),
    fluidRow(
      column(6, textInput("three_genes", h5("Input 3 genes  ( comma delimited)"), value = "Il17rb,Rorc,Zbtb16")),
      column(6, textInput("three_genes2", h5("Input 3 genes  ( comma delimited)"), value = "Il17rb,Rorc,Zbtb16"))
    ),
    fluidRow(
      column(6, textInput("three_genes_cluster", h5("Input Cluster"), value = 1)),
      column(6, textInput("three_genes_cluster2", h5("Input Cluster"), value = 5))
    
    ),
    fluidRow(
      column(6, plotOutput("plot3marker", width = "100%", height = "600px")),
      column(6, plotOutput("plot3marker2", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("Lineage Tree")
    ),
    fluidRow(
      column(6, offset = 3, img(src = "plottree_nmodeT.png", heigh = 600, width = 600))
    ),
    fluidRow(
      h3("Trajectory Analysis")
    ),
    fluidRow(
      column(6, textInput("branch_cxcl2", h5("Plot genes along Cxcl2 trajectory"), value = "Gata3,Il1rl1,Arg1,Klrg1,Bcl11b,Cxcl2")),
      column(6, textInput("branch_areg", h5("Plot genes along Areg trajectory"), value = "Gata3,Il1rl1,Arg1,Klrg1,Bcl11b,Areg"))
    ),
    fluidRow(
      column(6, h5("trajectory 5>11>9>8>4>10"), helpText("")),
      column(6, h5("trajectory 5>11>9>8>4>12"), helpText(""))
    ),
    
    fluidRow(
      column(6, plotOutput("plotcxcl2branch", width = "100%", height = "600px")),
      column(6, plotOutput("plotaregbranch", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("Differential Expression Cluster versus all other")
    ),
    fluidRow(
      column(6, textInput("cluster1", h5("Enter a cluster number"), value = 1)),
      column(6, textInput("cluster2", h5("Enter a cluster number"), value = 2))
    ),
    fluidRow(
      p("mean.cl = mean expression within cluster, mean.other.cl = mean expression in all other cluster, log2FC = log2 Foldchange selected clusters versus all other", align = "center")
    ),
    fluidRow(
      p("pval = p-value, padj = adjusted p-value", align = "center")
    ),
    fluidRow(
      column(6, dataTableOutput("table1")),
      column(6, dataTableOutput("table2"))
    ),
    br(),
    br(),
    br(),
    fluidRow(
      h3("Differential Expression Cluster1 against Cluster2")
    ),
    fluidRow(
      column(6, selectInput("clustcompare", h5("select Cluster 1"), choices= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16), selected = "1")),
      column(6, selectInput("clustcompare2", h5("select Cluster 2"), choices= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16), selected = "9"))
    ),
    fluidRow(
      column(6, offset = 3, dataTableOutput("diffexpclus"))
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(6, offset = 3, img(src = "resting_lung_heatmapAsset 1@2x.png", heigh = 800, width = 600))
    ),
    fluidRow(
      strong("Gene expression signature of resting lung ILCs."),
      p("Heatmap showing the log2 normalized expression of the top 10 differentially expressed genes (P<0.05, Methods) in increasing order of p-values across clusters (y-axis) and cells (x-axis). Duplicated genes and ensemble of Gm-, RP-, Rik-, Rpl- and Rps genes were removed if present. Differentially expressed genes were obtained by comparing gene expression of cells of the respective cluster to all other clusters. Color bars indicate RaceID3 cluster and sample-of-origin for each cell.")
    )),
 
 tabPanel(value = "tab3", title ="Neonatal Lung",
          h4("Shiny App Contact:", a("Patrice Zeis")),
          em("If red progress bar keeps appearing, shiny app is loading. Please wait patiently!"),
          br(),         
          h1("Neonatal Lung Dataset"),
          fluidRow(
            column(6, h3("Neonatal Cluster"), helpText("")),
            column(6, textInput("plotmapclusterNL", h5("which cluster to depict ( if more than one, enter comma delimited vector)"), value = "3,5,9"))
          ),
          fluidRow(
            column(6, img(src = "cluster_neonat.png", heigh = 600, width = 600)),
            column(6, plotOutput("plot_specfic_clusterNL", width = "100%", height = "600px"))
          ),
          fluidRow(
            textInput("GeneNL", h3("Gene to Plot"), value = "Zbtb16" )
          ),
          fluidRow(
            column(6, h5("zscore of mean expression"), helpText("")),
            column(6, h5("log2 mean expression"), helpText(""))
          ),
          fluidRow(
            column(6, plotOutput("expression_plotlogTNL", width = "100%", height = "600px")),
            column(6, plotOutput("expression_plotlogFNL", width = "100%", height = "600px"))
          ),
          fluidRow(
            h3("FractionDotPlot for Clusters" )
          ),
          fluidRow(
            textInput("GenesNL", h5("Genes to Plot (  comma delimited )"), value = "Tbx21,Rorc,Gata3,Il1rl1,Il17rb,Il2ra,Il18r1,Icos,Thy1,Il7r,Zbtb16,Tcf7,Cd7,Cd3e,S100a6,Id2,Arg1,Klrg1,Mki67,Vim,Cd69,S1pr1,Klf2,Cxcl2,Csf2,Il2,Calca,Il5,Il13,Areg,Klf4,Rgs2")
          ),
          fluidRow(
            textInput("ClustersNL", h5("clusters to include ( comma delimited )"), value = "7,4,6,12,9,2,3,5,1,10")
          ),
          fluidRow(
            column(6, h5("zscore of mean expression"), helpText("")),
            column(6, h5("log2 mean expression"), helpText(""))
          ),
          fluidRow(
            column(6, plotOutput("fracdotplot_zscTNL", width = "100%", height = "600px")),
            column(6, plotOutput("fracdotplot_log2meanNL", width = "100%", height = "600px"))
          ),
          fluidRow(
            h3("3 Genes Scatterplot" )
          ),
          fluidRow(
            column(6, textInput("three_genesNL", h5("Input 3 genes  ( comma delimited)"), value = "Il17rb,Rorc,Zbtb16")),
            column(6, textInput("three_genes2NL", h5("Input 3 genes  ( comma delimited)"), value = "Il17rb,Rorc,Zbtb16"))
          ),
          fluidRow(
            column(6, textInput("three_genes_clusterNL", h5("Input Cluster"), value = 9)),
            column(6, textInput("three_genes_cluster2NL", h5("Input Cluster"), value = 4))
            
          ),
          fluidRow(
            column(6, plotOutput("plot3markerNL", width = "100%", height = "600px")),
            column(6, plotOutput("plot3marker2NL", width = "100%", height = "600px"))
          ),
          fluidRow(
            h3("Differential Expression Cluster versus all other")
          ),
          fluidRow(
            column(6, textInput("cluster1NL", h5("Enter a cluster number"), value = 9)),
            column(6, textInput("cluster2NL", h5("Enter a cluster number"), value = 4))
          ),
          fluidRow(
            p("mean.cl = mean expression within cluster, mean.other.cl = mean expression in all other cluster, log2FC = log2 Foldchange selected clusters versus all other", align = "center")
          ),
          fluidRow(
            p("pval = p-value, padj = adjusted p-value", align = "center")
          ),
          fluidRow(
            column(6, dataTableOutput("table1NL")),
            column(6, dataTableOutput("table2NL"))
          ),
          br(),
          br(),
          br(),
          fluidRow(
            h3("Differential Expression Cluster1 against Cluster2")
          ),
          fluidRow(
            column(6, selectInput("clustcompareNL", h5("select Cluster 1"), choices= c(1:14), selected = "9")),
            column(6, selectInput("clustcompare2NL", h5("select Cluster 2"), choices= c(1:14), selected = "12"))
          ),
          fluidRow(
            column(6, offset = 3, dataTableOutput("diffexpclusNL"))
          ),
          br(),
          br(),
          br(),
          fluidRow(
            column(6, offset = 3, img(src = "neonatal_lung_heatmapAsset 1@2x.png", heigh = 800, width = 600))
          ),
          fluidRow(
            strong("Gene expression signature of neonatal lung ILCs."),
            p("Heatmap showing the log2 normalized expression of the top 10 differentially expressed genes (P<0.05, Methods) in increasing order of p-values across clusters (y-axis) and cells (x-axis) for neonatal lung. Duplicated genes, Mid1, Malat1, Xist, Kcnq1ot1 and the ensemble of all Gm-, RP, Hsp, A4300 genes were removed. Differentially expressed genes were obtained by comparing gene expression of cells of the respective cluster to all other clusters. Color bars indicate RaceID3 cluster and sample-of-origin for each cell.")
          )),
  
  tabPanel(value = "tab4", title ="BM",
    h4("Shiny App Contact:", a("Patrice Zeis")),
    em("If red progress bar keeps appearing, shiny app is loading. Please wait patiently!"),
    br(),
    h1("BM Dataset"),
    fluidRow(
      column(6, h3("Resting BM Cluster"), helpText("")),
      column(6, textInput("plotmapclusterBM", h5("which cluster to depict ( if more than one, enter comma delimited vector)"), value = "1,2,7"))
      ),
    fluidRow(
      column(6, img(src = "cluster_new_BM.png", heigh = 600, width = 600)),
      column(6, plotOutput("plot_specfic_clusterBM", width = "100%", height = "600px"))
    ),
    fluidRow(
      textInput("GeneBM", h3("Gene to Plot"), value = "Il17rb" )
    ),
    fluidRow(
      column(6, h5("zscore of mean expression"), helpText("")),
      column(6, h5("log2 mean expression"), helpText(""))
    ),
    fluidRow(
      column(6, plotOutput("expression_plotlogTBM", width = "100%", height = "600px")),
      column(6, plotOutput("expression_plotlogFBM", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("FractionDotPlot for Clusters" )
    ),
    fluidRow(
      textInput("GenesBM", h5("Genes to Plot (  comma delimited )"), value = "Tbx21,Rorc,Gata3,Il1rl1,Il17rb,Il2ra,Il18r1,Icos,Thy1,Il7r,Zbtb16,Tcf7,Cd7,Cd3e,S100a6,Id2,Arg1,Klrg1,Mki67,Vim,Cd69,S1pr1,Klf2,Cxcl2,Csf2,Il2,Calca,Il5,Il13,Areg,Klf4,Rgs2")
    ),
    fluidRow(
      textInput("ClustersBM", h5("clusters to include ( comma delimited )"), value = "1,4,5,6,3,7,2")
    ),
    fluidRow(
      column(6, h5("zscore of mean expression"), helpText("")),
      column(6, h5("log2 mean expression"), helpText(""))
    ),
    fluidRow(
      column(6, plotOutput("fracdotplot_zscTBM", width = "100%", height = "600px")),
      column(6, plotOutput("fracdotplot_log2meanBM", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("3 Genes Scatterplot" )
    ),
    fluidRow(
      column(6, textInput("three_genesBM", h5("Input 3 genes  ( comma delimited)"), value = "Il17rb,Rorc,Zbtb16")),
      column(6, textInput("three_genes2BM", h5("Input 3 genes  ( comma delimited)"), value = "Il17rb,Rorc,Zbtb16"))
    ),
    fluidRow(
      column(6, textInput("three_genes_clusterBM", h5("Input Cluster"), value = 1)),
      column(6, textInput("three_genes_cluster2BM", h5("Input Cluster"), value = 5))
      
    ),
    fluidRow(
      column(6, plotOutput("plot3markerBM", width = "100%", height = "600px")),
      column(6, plotOutput("plot3marker2BM", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("Differential Expression Cluster versus all other")
    ),
    fluidRow(
      column(6, textInput("cluster1BM", h5("Enter a cluster number"), value = 1)),
      column(6, textInput("cluster2BM", h5("Enter a cluster number"), value = 2))
    ),
    fluidRow(
      p("mean.cl = mean expression within cluster, mean.other.cl = mean expression in all other cluster, log2FC = log2 Foldchange selected clusters versus all other", align = "center")
    ),
    fluidRow(
      p("pval = p-value, padj = adjusted p-value", align = "center")
    ),
    fluidRow(
      column(6, dataTableOutput("table1BM")),
      column(6, dataTableOutput("table2BM"))
    ),
    br(),
    br(),
    br(),
    fluidRow(
      h3("Differential Expression Cluster1 against Cluster2")
    ),
    fluidRow(
      column(6, selectInput("clustcompareBM", h5("select Cluster 1"), choices= c(1:8), selected = "1")),
      column(6, selectInput("clustcompare2BM", h5("select Cluster 2"), choices= c(1:8), selected = "5"))
    ),
    fluidRow(
      column(6, offset = 3, dataTableOutput("diffexpclusBM"))
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(6, offset = 3, img(src = "BM_heatmapAsset 2@2x.png", heigh = 600, width = 600))
    ),
    fluidRow(
      strong("Gene expression signature of Il18r1+Icos+ BM ILCs."),
      p("Heatmap showing the log2 normalized expression of the top 10 differentially expressed genes (P<0.05, Methods) in increasing order of p-values across clusters (y-axis) and cells (x-axis) for Il18r1+Icos+ Bm ILCs. Duplicated genes, Mid1, Malat1, Xist, Kcnq1ot1 and the ensemble of all Gm-, RP, Hsp, A4300 genes were removed. Differentially expressed genes were obtained by comparing gene expression of cells of the respective cluster to all other clusters. Color bars indicate RaceID3 cluster and sample-of-origin for each cell.")
    )),

  tabPanel(value = "tab5", title = "Lung Culture/Culture Input",
     h4("Shiny App Contact:", a("Patrice Zeis")),
     em("If red progress bar keeps appearing, shiny app is loading. Please wait patiently!"),
     br(),        
     h1("Il18r1+Icos+ Cultured Subsets and Il18r1+Icos+ST2-Il17rb- Input cells"),
     fluidRow(
       column(6, h3("Culture/Input Cluster"), helpText("")),
       column(6, textInput("plotmapclusterCL", h5("which cluster to depict ( if more than one, enter comma delimited vector)"), value = "1,2,8"))
     ),
     fluidRow(
       column(6, img(src = "cluster_new_input_culture.png", heigh = 600, width = 600)),
       column(6, plotOutput("plot_specfic_clusterCL", width = "100%", height = "600px"))
     ),
     fluidRow(
       column(6, h3("Culture/Input Il18r1+Icos+ Sample information "), helpText("")),
       column(6, checkboxGroupInput("plotsymbolCL", h3( "Il18r1+Icos+ Samples to depict"), choices = list("OP9_Pan"="a.OP9.Pan","OP9_ST2-"="b.OP9.ST2.n","OP9_ST2-Il17rb-"="c.OP9.ST2.n.Il17rb.n", "Input_ST2-Il17rb-"="d.Input.ST2.n.Il17rb.n"), selected = "d.Input.ST2.n.Il17rb.n"))
     ),
     fluidRow(
       column(6, img(src = "symbols_map_culture.png", heigh = 600, width = 600)),
       column(6, plotOutput("plot_specfic_sampleCL", width = "100%", height = "600px"))
     ),
     fluidRow(
       textInput("GeneCL", h3("Gene to Plot"), value = "Gata3" )
     ),
     fluidRow(
       column(6, h5("zscore of mean expression"), helpText("")),
       column(6, h5("log2 mean expression"), helpText(""))
     ),
     fluidRow(
       column(6, plotOutput("expression_plotlogTCL", width = "100%", height = "600px")),
       column(6, plotOutput("expression_plotlogFCL", width = "100%", height = "600px"))
     ),
     fluidRow(
       h3("FractionDotPlot for Clusters" )
     ),
     fluidRow(
       textInput("GenesCL", h5("Genes to Plot (  comma delimited )"), value = "Gata3,Rorc,Tbx21,Il1rl1,Il17rb,Il18r1,Zbtb16,Tcf7,Mki67,Id2,Klrg1,Il13,Il5,Areg,Cxcl2,Calca,Cxcr3,Gzmc")
     ),
     fluidRow(
       textInput("ClustersCL", h5("clusters to include ( comma delimited )"), value = "5,6,7,1,2,4,3,8")
     ),
     fluidRow(
       column(6, h5("zscore of mean expression"), helpText("")),
       column(6, h5("log2 mean expression"), helpText(""))
     ),
     fluidRow(
       column(6, plotOutput("fracdotplot_zscTCL", width = "100%", height = "600px")),
       column(6, plotOutput("fracdotplot_log2meanCL", width = "100%", height = "600px"))
     ),
     fluidRow(
       h3("FractionDotPlot for Samples" )
     ),
     fluidRow(
       textInput("GenesSCL", h5("Genes to Plot (  comma delimited )"), value = "Gata3,Rorc,Tbx21,Il1rl1,Il17rb,Il18r1,Zbtb16,Tcf7,Mki67,Id2,Klrg1,Il13,Il5,Areg,Cxcl2,Calca,Cxcr3,Gzmc")
     ),
     fluidRow(
       textInput("SamplesCL", h5("samples to include ( comma delimited )"), value = "a.OP9.Pan,b.OP9.ST2.n,c.OP9.ST2.n.Il17rb.n,d.Input.ST2.n.Il17rb.n")
     ),
     fluidRow(
       h5("Il18r1+Icos+ ILC subsets", align = "center")
     ),
     br(),
     fluidRow(
       h5("OP9_Pan = a.OP9.Pan, OP9_ST2- = b.OP9.ST2.n, OP9_ST2-Il17rb -= c.OP9.ST2.n.Il17rb.n, Input_ST2-Il17rb- = d.Input.ST2.n.Il17rb.n", align = "center")
     ),
     fluidRow(
       column(6, h5("zscore of mean expression"), helpText("")),
       column(6, h5("log2 mean expression"), helpText(""))
     ),
     fluidRow(
       column(6, plotOutput("fracdotplotS_zscTCL", width = "100%", height = "600px")),
       column(6, plotOutput("fracdotplotS_log2meanCL", width = "100%", height = "600px"))
     ),
     fluidRow(
       h3("3 Genes Scatterplot" )
     ),
     fluidRow(
       column(6, textInput("three_genesCL", h5("Input 3 genes  ( comma delimited)"), value = "Gata3,Rorc,Cxcr3")),
       column(6, textInput("three_genes2CL", h5("Input 3 genes  ( comma delimited)"), value = "Gata3,Rorc,Cxcr3"))
     ),
     fluidRow(
       column(6, textInput("three_genes_clusterCL", h5("Input Cluster"), value = 3)),
       column(6, textInput("three_genes_cluster2CL", h5("Input Cluster"), value = 4))
     ),
     fluidRow(
       column(6, plotOutput("plot3markerCL", width = "100%", height = "600px")),
       column(6, plotOutput("plot3marker2CL", width = "100%", height = "600px"))
     ),
     fluidRow(
       h3("Differential Expression Cluster versus all other")
     ),
     fluidRow(
       column(6, textInput("cluster1CL", h5("Enter a cluster number"), value = 1)),
       column(6, textInput("cluster2CL", h5("Enter a cluster number"), value = 8))
     ),
     fluidRow(
       p("mean.cl = mean expression within cluster, mean.other.cl = mean expression in all other cluster, log2FC = log2 Foldchange selected clusters versus all other", align = "center")
     ),
     fluidRow(
       p("pval = p-value, padj = adjusted p-value", align = "center")
     ),
     fluidRow(
       column(6, dataTableOutput("table1CL")),
       column(6, dataTableOutput("table2CL"))
     ),
     br(),
     br(),
     br(),
     fluidRow(
       h3("Differential Expression Cluster1 against Cluster2")
     ),
     fluidRow(
       column(6, selectInput("clustcompareCL", h5("select Cluster 1"), choices= c(1:8), selected = "1")),
       column(6, selectInput("clustcompare2CL", h5("select Cluster 2"), choices= c(1:8), selected = "8"))
     ),
     fluidRow(
       column(6, offset = 3, dataTableOutput("diffexpclusCL"))
     )
    ),
 
  tabPanel(value = "tab6", title ="Time-Course",
    h4("Shiny App Contact:", a("Patrice Zeis")),
    em("If red progress bar keeps appearing, shiny app is loading. Please wait patiently!"),
    br(),      
    h1("Nippostrongylus brasiliensis infection time-course"),
    fluidRow(
      column(6, h3("Time-course Cluster"), helpText("")),
      column(6, textInput("plotmapclusterTC", h5("which cluster to depict ( if more than one, enter comma delimited vector)"), value = "12,5,2"))
    ),
    fluidRow(
      column(6, img(src = "cluster_new_time-course.png", heigh = 500, width = 600)),
      column(6, plotOutput("plot_specfic_clusterTC", width = "100%", height = "600px"))
    ),
    fluidRow(
      column(6, h3("Time-course Cluster Composition(normalised)"), helpText("")),
      #column(6, textInput("plotsymbolTC", h5("which sample to depict ( if more than one, enter comma delimited vector)"), value = "a.uninfected,b.d4,c.d7"))
      column(6, checkboxGroupInput("plotsymbolTC", h3( "Samples to depict"), choices = list("a.uninfected","b.d4","c.d7", "d.d10", "e.d10ivCD45", "f.d15Blood", "g.d15"), selected = "g.d15"))
    ),
    fluidRow(
      column(6, img(src = "cluster_compositionAsset 3@2x.png", heigh = 400, width = 400)),
      column(6, plotOutput("plot_specfic_sample", width = "100%", height = "600px"))
    ),
    fluidRow(
      textInput("GeneTC", h3("Gene to Plot"), value = "Il18r1" )
    ),
    fluidRow(
      column(6, h3(""), helpText("log2 norm. expression")),
      column(6, h3(""), helpText("norm. expression"))
    ),
    fluidRow(
      column(6, plotOutput("expression_plotlogTTC", width = "100%", height = "600px")),
      column(6, plotOutput("expression_plotlogFTC", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("FractionDotPlot for Clusters" )
    ),
    fluidRow(
      textInput("GenesTC", h5("Genes to Plot (  comma delimited )"), value = "Tbx21,Rorc,Gata3,Il1rl1,Il17rb,Il2ra,Il18r1,Icos,Thy1,Il7r,Zbtb16,Tcf7,Cd7,Cd3e,S100a6,Id2,Arg1,Klrg1,Mki67,Vim,Cd69,S1pr1,Klf2,Cxcl2,Csf2,Il2,Calca,Il5,Il13,Pdcd1,Ctla4,Il1r2,H2-Ab1,Areg,Klf4,Rgs2")
    ),
    fluidRow(
      textInput("ClustersTC", h5("clusters to include ( comma delimited )"), value = "12,1,3,2,15,5,8,4,10,23,14,6,16,17,7,19,29,9")
    ),
    fluidRow(
      column(6, h5("zscore of mean expression"), helpText("")),
      column(6, h5("log2 mean expression"), helpText(""))
    ),
    fluidRow(
      column(6, plotOutput("fracdotplot_zscTTC", width = "100%", height = "600px")),
      column(6, plotOutput("fracdotplot_log2meanTC", width = "100%", height = "600px"))
    ),
    ##
    fluidRow(
      h3("FractionDotPlot for Samples" )
    ),
    fluidRow(
      textInput("GenesSTC", h5("Genes to Plot (  comma delimited )"), value = "Tbx21,Rorc,Gata3,Il1rl1,Il17rb,Il2ra,Il18r1,Icos,Thy1,Il7r,Zbtb16,Tcf7,Cd7,Cd3e,S100a6,Id2,Arg1,Klrg1,Mki67,Vim,Cd69,S1pr1,Klf2,Cxcl2,Csf2,Il2,Calca,Il5,Il13,Pdcd1,Ctla4,Il1r2,H2-Ab1,Areg,Klf4,Rgs2")
    ),
    fluidRow(
      textInput("SamplesTC", h5("samples to include ( comma delimited )"), value = "a.uninfected,b.d4,c.d7,d.d10,e.d10ivCD45,f.d15Blood,g.d15")
    ),
    fluidRow(
      column(6, h5("zscore of mean expression"), helpText("")),
      column(6, h5("log2 mean expression"), helpText(""))
    ),
    fluidRow(
      column(6, plotOutput("fracdotplotS_zscTTC", width = "100%", height = "600px")),
      column(6, plotOutput("fracdotplotS_log2meanTC", width = "100%", height = "600px"))
    ),
    fluidRow(
      h3("3 Genes Scatterplot" )
    ),
    fluidRow(
      column(6, textInput("three_genesTC", h5("Input 3 genes  ( comma delimited)"), value = "Il18r1,Tcf7,Il1rl1")),
      column(6, textInput("three_genes2TC", h5("Input 3 genes  ( comma delimited)"), value = "Il18r1,Tcf7,Il1rl1"))
    ),
    fluidRow(
      column(6, textInput("three_genes_clusterTC", h5("Input Cluster"), value = 12)),
      column(6, textInput("three_genes_cluster2TC", h5("Input Cluster"), value = 6))
    ),
    fluidRow(
      column(6, plotOutput("plot3markerTC", width = "100%", height = "600px")),
      column(6, plotOutput("plot3marker2TC", width = "100%", height = "600px"))
    ), 
    ##
    fluidRow(
      h3("Lineage Tree")
    ),
    fluidRow(
      column(6, offset = 3, img(src = "cluster_composition_treeAsset 1@2x.png", heigh = 400, width = 400))
      
    ),
    fluidRow(
      h3("Trajectory Analysis")
    ),
    fluidRow(
      column(6, textInput("branch_il13", h5("Plot genes along Il13 trajectory"), value = "Gata3,Il1rl1,Arg1,Klrg1,Bcl11b,Il13,Neurl3,Furin")),
      column(6, textInput("branch_cxcl2_TC", h5("Plot genes along Cxcl2 trajectory"), value = "Gata3,Il1rl1,Arg1,Klrg1,Bcl11b,Csf2,Cxcl2"))
    ),
    fluidRow(
      column(6, h5("trajectory 12>5>2>8>6>16"), helpText("")),
      column(6, h5("trajectory 12>5>2>8>4>23"), helpText(""))
    ),
    fluidRow(
      column(6, plotOutput("plotil13branch", width = "100%", height = "600px")),
      column(6, plotOutput("plotcxcl2branchTC", width = "100%", height = "600px"))
    ),
    ##
    fluidRow(
      h3("Differential Expression Cluster versus all other")
    ),
    fluidRow(
      column(6, textInput("cluster1TC", h5("Enter a cluster number"), value = 16)),
      column(6, textInput("cluster2TC", h5("Enter a cluster number"), value = 23))
    ),
    fluidRow(
      p("mean.cl = mean expression within cluster, mean.other.cl = mean expression in all other cluster, log2FC = log2 Foldchange selected clusters versus all other", align = "center")
    ),
    fluidRow(
      p("pval = p-value, padj = adjusted p-value", align = "center")
    ),
    fluidRow(
      column(6, dataTableOutput("table1TC")),
      column(6, dataTableOutput("table2TC"))
    ),
    br(),
    br(),
    br(),
    fluidRow(
      h3("Differential Expression Cluster1 against Cluster2")
    ),
    fluidRow(
      column(6, selectInput("clustcompareTC", h5("select Cluster 1"), choices= c(1:12,14:23,29), selected = "16")),
      column(6, selectInput("clustcompare2TC", h5("select Cluster 2"), choices= c(1:12,14:23,29), selected = "23"))
    ),
    fluidRow(
      column(6, offset = 3, dataTableOutput("diffexpclusTC"))
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(6, img(src = "timecourse_heatmapAsset 5@2x.png", heigh = 1000, width = 800))
    ),
    fluidRow(
      strong("Gene expression signature of combined Nippostrongylus brasiliensis infection time-course and normal lung ILC clusters."),
      p("Heatmap showing the log2 normalized expression of the top 10 differentially expressed genes (Benjamini Hochberg corrected P<0.05, Methods) in increasing order of p-values across clusters (y-axis) and cells (x-axis) for different sets of clusters with highest relative contribution of the respective sample (after normalization for sample size). Duplicated genes, Mid1, Malat1, Xist, Kcnq1ot1 and the ensemble of all Gm-, RP, Hsp, A4300 genes were removed. Differentially expressed genes were obtained by comparing gene expression of cells of the respective cluster to all other clusters. Color bars indicate RaceID3 cluster and sample-of-origin for each cell.")
    )),
 
  tabPanel(value = "tab7", title ="Parabiosis",
    h4("Shiny App Contact:", a("Patrice Zeis")),
    em("If red progress bar keeps appearing, shiny app is loading. Please wait patiently!"),
    br(),
    h1("Parabiosis d15 Nippostrongylus brasiliensis infection"),
    fluidRow(
      column(6, h3("Parabiosis Cluster"), helpText("")),
      column(6, textInput("plotmapclusterPara", h5("which cluster to depict ( if more than one, enter comma delimited vector)"), value = "11,4,17,7,2,10,18"))
      ),
    fluidRow(
      column(6, img(src = "cluster_new_parabiosis.png", heigh = 600, width = 600)),
      column(6, plotOutput("plot_specfic_clusterPara", width = "100%", height = "600px"))
      ),
   fluidRow(
     textInput("GenePara", h3("Gene to Plot"), value = "Gata3" )
     ),
    fluidRow(
      column(6, h3(""), helpText("log2 norm. expression")),
      column(6, h3(""), helpText("norm. expression"))
      ),
     fluidRow(
       column(6, plotOutput("expression_plotlogTPara", width = "100%", height = "600px")),
       column(6, plotOutput("expression_plotlogFPara", width = "100%", height = "600px"))
      ),
    fluidRow(
     column(6, h3("Parabiosis Cluster Composition(normalised)"), helpText("")),
     column(6, selectInput("plotsymbolPara", h5("select Host or Donor sample"), choices= c("Host", "Donor"), selected = "Donor"))
    ),
    fluidRow(
      column(6, img(src = "parabiosis_treeAsset 3@2x.png", heigh = 400, width = 400)),
      column(6, plotOutput("plot_specfic_samplePara", width = "100%", height = "600px"))     
    ),
    fluidRow(
     h3("FractionDotPlot for Clusters" )
    ),
    fluidRow(
      textInput("GenesPara", h5("Genes to Plot (  comma delimited )"), value = "Tbx21,Rorc,Gata3,Il1rl1,Il17rb,Il2ra,Il18r1,Icos,Thy1,Il7r,Zbtb16,Tcf7,Cd7,Cd3e,S100a6,Id2,Arg1,Klrg1,Mki67,Vim,Cd69,S1pr1,Klf2,Cxcl2,Csf2,Il2,Calca,Il5,Il13,Pdcd1,Ctla4,Il1r2,H2-Ab1,Areg,Klf4,Rgs2")
    ),
    fluidRow(
     textInput("ClustersPara", h5("clusters to include ( comma delimited )"), value = "11,4,17,3,5,14,10,18,8,9,2,1,6,7,12,13,15")
    ),
    fluidRow(
      column(6, h5("zscore of mean expression"), helpText("")),
      column(6, h5("log2 mean expression"), helpText(""))
    ),
    fluidRow(
      column(6, plotOutput("fracdotplot_zscTPara", width = "100%", height = "600px")),
      column(6, plotOutput("fracdotplot_log2meanPara", width = "100%", height = "600px"))
    ),
   fluidRow(
     h3("Differential Expression Cluster versus all other")
   ),
   fluidRow(
     column(6, textInput("cluster1para", h5("Enter a cluster number"), value = 4)),
     column(6, textInput("cluster2para", h5("Enter a cluster number"), value = 11))
   ),
   fluidRow(
     p("mean.cl = mean expression within cluster, mean.other.cl = mean expression in all other cluster, log2FC = log2 Foldchange selected clusters versus all other", align = "center")
   ),
   fluidRow(
     p("pval = p-value, padj = adjusted p-value", align = "center")
   ),
    fluidRow(
      column(6, dataTableOutput("table1para")),
      column(6, dataTableOutput("table2para"))
    ),
   br(),
   br(),
   br(),
   fluidRow(
     h3("Differential Expression Cluster1 against Cluster2")
   ),
   fluidRow(
     column(6, selectInput("clustcomparePara", h5("select Cluster 1"), choices= c(1:18,20), selected = "4")),
     column(6, selectInput("clustcompare2Para", h5("select Cluster 2"), choices= c(1:18,20), selected = "5"))
   ),
   fluidRow(
     column(6, offset = 3, dataTableOutput("diffexpclusPara"))
   )
  ),
 
  #### chimera
  tabPanel(value = "tab8", title ="Shield Chimera",
   h4("Shiny App Contact:", a("Patrice Zeis")),
   em("If red progress bar keeps appearing, shiny app is loading. Please wait patiently!"),
   br(),  
   h1("Shield Chimera d15 Nippostrongylus brasiliensis infection"),
   fluidRow(
      column(6, h3("Shield Chimera Cluster"), helpText("")),
      column(6, textInput("plotmapclusterChim", h5("which cluster to depict ( if more than one, enter comma delimited vector)"), value = "11,3,6,16,17"))
   ),
   fluidRow(
      column(6, img(src = "cluster4_new_chimera.png", heigh = 600, width = 600)),
      column(6, plotOutput("plot_specfic_clusterChim", width = "100%", height = "600px"))
    ),
   fluidRow(
      textInput("GeneChim", h3("Gene to Plot"), value = "Gata3" )
   ),
   fluidRow(
      column(6, h3(""), helpText("log2 norm. expression")),
      column(6, h3(""), helpText("norm. expression"))
   ),
   fluidRow(
      column(6, plotOutput("expression_plotlogTChim", width = "100%", height = "600px")),
      column(6, plotOutput("expression_plotlogFChim", width = "100%", height = "600px"))
   ),
   fluidRow(
      column(6, h3("Shield Chimera Cluster Composition(normalised)"), helpText("")),
      column(6, selectInput("plotsymbolChim", h5("select Host or Donor sample"), choices= c("Host", "Donor"), selected = "Donor"))
   ),
   fluidRow(
      column(6, img(src = "chimera_treeAsset 1@2x.png", heigh = 400, width = 400)),
      column(6, plotOutput("plot_specfic_sampleChim", width = "100%", height = "600px"))     
   ),
   fluidRow(
     h3("FractionDotPlot for Clusters" )
   ),
   fluidRow(
      textInput("GenesChim", h5("Genes to Plot (  comma delimited )"), value = "Tbx21,Rorc,Gata3,Il1rl1,Il17rb,Il2ra,Il18r1,Icos,Thy1,Il7r,Zbtb16,Tcf7,Cd7,Cd3e,S100a6,Id2,Arg1,Klrg1,Mki67,Vim,Cd69,S1pr1,Klf2,Cxcl2,Csf2,Il2,Calca,Il5,Il13,Pdcd1,Ctla4,Il1r2,H2-Ab1,Areg,Klf4,Rgs2")
   ),
   fluidRow(
      textInput("ClustersChim", h5("clusters to include ( comma delimited )"), value = "16,17,6,10,11,13,3,2,4,5,12,8,7,14,15,1,19,9")
   ),
   fluidRow(
      column(6, h5("zscore of mean expression"), helpText("")),
      column(6, h5("log2 mean expression"), helpText(""))
   ),
   fluidRow(
      column(6, plotOutput("fracdotplot_zscTChim", width = "100%", height = "600px")),
      column(6, plotOutput("fracdotplot_log2meanChim", width = "100%", height = "600px"))
   ),
   fluidRow(
     h3("Differential Expression Cluster versus all other")
   ),
   fluidRow(
      column(6, textInput("cluster1chim", h5("Enter a cluster number"), value = 16)),
      column(6, textInput("cluster2chim", h5("Enter a cluster number"), value = 17))
  ),
   fluidRow(
      p("mean.cl = mean expression within cluster, mean.other.cl = mean expression in all other cluster, log2FC = log2 Foldchange selected clusters versus all other", align = "center")
   ),
   fluidRow(
      p("pval = p-value, padj = adjusted p-value", align = "center")
   ),
   fluidRow(
      column(6, dataTableOutput("table1chim")),
      column(6, dataTableOutput("table2chim"))
   ),
  br(),
  br(),
  br(),
  fluidRow(
    h3("Differential Expression Cluster1 against Cluster2")
  ),
  fluidRow(
    column(6, selectInput("clustcompareChim", h5("select Cluster 1"), choices= c(1:17,19), selected = "12")),
    column(6, selectInput("clustcompare2Chim", h5("select Cluster 2"), choices= c(1:17,19), selected = "16"))
  ),
  fluidRow(
    column(6, offset = 3, dataTableOutput("diffexpclusChim"))
  )
  )
 #)
)

# Define server logic ----
server <- function(input, output) {
  
  #observeEvent(input$tabs, {
    #if (input$tabs == "tab1") {
    sc_wt <- reactive({ 
     readRDS("sc_objects/sc_wtlung.RData") 
    })
     diff_exp_wt <- reactive({ 
      readRDS("diff_exp_tables/wt_diff_exp_table_filt.RData")
   })
    branch <- reactive({ 
       readRDS("StemID_Cxcl_Areg_branches.RData")}
   )
  
  output$plot_specfic_cluster <- renderPlot({
    sc_wt <- sc_wt()
    z <- as.numeric(unlist(strsplit(input$plotmapcluster, split = ",")))
    plotmap2(sc_wt, final = T, cluster = z)
  })
  output$expression_plotlogT <- renderPlot({
    sc_wt <- sc_wt()
    plotexpmap(sc_wt, input$Gene, logsc = T)
  })
  output$expression_plotlogF <- renderPlot({
    sc_wt <- sc_wt()
    plotexpmap(sc_wt, input$Gene, logsc = F)
  })
  output$fracdotplot_zscT <- renderPlot({
    sc_wt <- sc_wt()
    x <- unlist(strsplit(input$Genes, split = ","))
    y <- as.numeric(unlist(strsplit(input$Clusters, split = ",")))
    fracdotplot(sc_wt, genes = x, cluster = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplot_log2mean <- renderPlot({
    sc_wt <- sc_wt()
    x <- unlist(strsplit(input$Genes, split = ","))
    y <- as.numeric(unlist(strsplit(input$Clusters, split = ",")))
  fracdotplot(sc_wt, genes = x, cluster = y, zsc = F, limup = 4, limdown = -4)
  })
  output$plot3marker <- renderPlot({
    sc_wt <- sc_wt()
    genes1 <- unlist(strsplit(input$three_genes, split = ","))
    plot3marker(sc_wt, cluster = input$three_genes_cluster, gene1 = genes1[1], gene2 = genes1[2], gene3 = genes1[3]) 
  })
  output$plot3marker2 <- renderPlot({
    sc_wt <- sc_wt()
    genes2 <- unlist(strsplit(input$three_genes2, split = ","))
    plot3marker(sc_wt, cluster = input$three_genes_cluster2, gene1 = genes2[1], gene2 = genes2[2], gene3 = genes2[3]) 
  })

  output$plotcxcl2branch <- renderPlot({
    branch <- branch()
    cxcl2_genes <- unlist(strsplit(input$branch_cxcl2, split = ","))
    plotexpression(branch$ndata_cxcl2_branch, branch$cluspar_cxcl2_branch, cxcl2_genes, branch$pseudoorder_cxcl2_branch, col = branch$fcol, cluster = F, alpha=.5,types=NULL, cex = 3, lwd = 2.5, leg = T, map = F, ylim = range(-0.5:10))
  })
  output$plotaregbranch <- renderPlot({
    branch <- branch()
    areg_genes <- unlist(strsplit(input$branch_areg, split = ","))
    plotexpression(branch$ndata_areg_branch, branch$cluspar_areg_branch, areg_genes, branch$pseudoorder_areg_branch, col = branch$fcol, cluster = F, alpha=.5,types=NULL, cex = 3, lwd = 2.5, leg = T, map = F, ylim = range(-0.5:10))
  })
  
  output$diffexpclus <- renderDataTable({
    sc_wt <- sc_wt()
    tab <-  as.matrix(sc_wt@ndata * min(sc_wt@counts)) + 0.1
    tab <- data.frame(tab)
    l1 <- names(sc_wt@cpart[sc_wt@cpart == input$clustcompare])
    l2 <- names(sc_wt@cpart[sc_wt@cpart == input$clustcompare2])
    x <- diffexpnb(tab, l1,l2,method="per-condition",norm=F,vfit=sc_wt@background$vfit)
    x <- x$res[rownames(x$res) %in% sc_wt@genes,]
    x <- x[order(x$pval),]
    x <- x[x$pval <0.05,]
    x <- x[,-c(1,4)]
    colnames(x) <- c(paste("mean.cl", input$clustcompare, sep=""), paste("mean.other.cl",input$clustcompare2, sep = ""), "log2FC", "pval", "padj")
    x[,"log2FC"] <- x[,"log2FC"] * -1
    x <- apply(x, 2, function(x) round(x, digits = 5))
    x
  })
  
  output$table1 <- renderDataTable({
    diff_exp_wt <- diff_exp_wt()
    eval(parse( text = paste("diff_exp_wt$clust_", input$cluster1, "_vs_all", sep = ""))) })
  output$table2 <- renderDataTable({
    diff_exp_wt <- diff_exp_wt()
    eval(parse( text = paste("diff_exp_wt$clust_", input$cluster2, "_vs_all", sep = ""))) })
  #}
  
##neonat
  sc_neo <- reactive({ 
    readRDS("sc_objects/sc_neonat_proliferation_out.RData") 
  })
  diff_exp_neo <- reactive({ 
    readRDS("diff_exp_tables/neonat_diff_exp_table_filt.RData")
  })  
  
  output$plot_specfic_clusterNL <- renderPlot({
    z <- as.numeric(unlist(strsplit(input$plotmapclusterNL, split = ",")))
    plotmap2(sc_neo(), final = T, cluster = z, cex = 1.25)
  })
  output$expression_plotlogTNL <- renderPlot({
    plotexpmap(sc_neo(), input$GeneNL, logsc = T, cex = 1.25)
  })
  
  output$expression_plotlogFNL <- renderPlot({
    plotexpmap(sc_neo(), input$GeneNL, logsc = F, cex = 1.25)
  })
  output$fracdotplot_zscTNL <- renderPlot({
    x <- unlist(strsplit(input$GenesNL, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersNL, split = ",")))
    fracdotplot(sc_neo(), genes = x, cluster = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplot_log2meanNL <- renderPlot({
    x <- unlist(strsplit(input$GenesNL, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersNL, split = ",")))
    fracdotplot(sc_neo(), genes = x, cluster = y, zsc = F, limup = 4, limdown = -4)
  })
  output$plot3markerNL <- renderPlot({
    genes1 <- unlist(strsplit(input$three_genesNL, split = ","))
    plot3marker(sc_neo(), cluster = input$three_genes_clusterNL, gene1 = genes1[1], gene2 = genes1[2], gene3 = genes1[3]) 
  })
  output$plot3marker2NL <- renderPlot({
    genes2 <- unlist(strsplit(input$three_genes2NL, split = ","))
    plot3marker(sc_neo(), cluster = input$three_genes_cluster2NL, gene1 = genes2[1], gene2 = genes2[2], gene3 = genes2[3]) 
  })
  output$table1NL <- renderDataTable({
    diff_exp_neo <- diff_exp_neo()
    eval(parse( text = paste("diff_exp_neo$clust_", input$cluster1NL, "_vs_all", sep = ""))) })
  output$table2NL <- renderDataTable({
    diff_exp_neo <- diff_exp_neo()
    eval(parse( text = paste("diff_exp_neo$clust_", input$cluster2NL, "_vs_all", sep = ""))) })
  output$diffexpclusNL <- renderDataTable({
    sc_neo <- sc_neo()
    tab <-  as.matrix(sc_neo@ndata * min(sc_neo@counts)) + 0.1
    tab <- data.frame(tab)
    l1 <- names(sc_neo@cpart[sc_neo@cpart == input$clustcompareNL])
    l2 <- names(sc_neo@cpart[sc_neo@cpart == input$clustcompare2NL])
    x <- diffexpnb(tab, l1,l2,method="per-condition",norm=F,vfit=sc_neo@background$vfit)
    x <- x$res[rownames(x$res) %in% sc_neo@genes,]
    x <- x[order(x$pval),]
    x <- x[x$pval <0.05,]
    x <- x[,-c(1,4)]
    colnames(x) <- c(paste("mean.cl", input$clustcompareNL, sep=""), paste("mean.other.cl",input$clustcompare2NL, sep = ""), "log2FC", "pval", "padj")
    x[,"log2FC"] <- x[,"log2FC"] * -1
    x <- apply(x, 2, function(x) round(x, digits = 5))
    x
  })
  
  ## BM 
  sc_bm <- reactive({ 
    readRDS("sc_objects/BM_cln6_scobject.RData") 
  })
  diff_exp_bm <- reactive({ 
    readRDS("diff_exp_tables/bm_diff_exp_table_filt.RData")
  })  
  
  output$plot_specfic_clusterBM <- renderPlot({
    z <- as.numeric(unlist(strsplit(input$plotmapclusterBM, split = ",")))
    plotmap2(sc_bm(), final = T, cluster = z, cex = 1.25)
  })
  output$expression_plotlogTBM <- renderPlot({
    plotexpmap(sc_bm(), input$GeneBM, logsc = T, cex = 1.25)
  })
  
  output$expression_plotlogFBM <- renderPlot({
    plotexpmap(sc_bm(), input$GeneBM, logsc = F, cex = 1.25)
  })
  output$fracdotplot_zscTBM <- renderPlot({
    x <- unlist(strsplit(input$GenesBM, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersBM, split = ",")))
    fracdotplot(sc_bm(), genes = x, cluster = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplot_log2meanBM <- renderPlot({
    x <- unlist(strsplit(input$GenesBM, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersBM, split = ",")))
    fracdotplot(sc_bm(), genes = x, cluster = y, zsc = F, limup = 4, limdown = -4)
  })
  output$plot3markerBM <- renderPlot({
    genes1 <- unlist(strsplit(input$three_genesBM, split = ","))
    plot3marker(sc_bm(), cluster = input$three_genes_clusterBM, gene1 = genes1[1], gene2 = genes1[2], gene3 = genes1[3]) 
  })
  output$plot3marker2BM <- renderPlot({
    genes2 <- unlist(strsplit(input$three_genes2BM, split = ","))
    plot3marker(sc_bm(), cluster = input$three_genes_cluster2BM, gene1 = genes2[1], gene2 = genes2[2], gene3 = genes2[3]) 
  })
  output$table1BM <- renderDataTable({
    diff_exp_bm <- diff_exp_bm()
    eval(parse( text = paste("diff_exp_bm$clust_", input$cluster1BM, "_vs_all", sep = ""))) })
  output$table2BM <- renderDataTable({
    diff_exp_bm <- diff_exp_bm()
    eval(parse( text = paste("diff_exp_bm$clust_", input$cluster2BM, "_vs_all", sep = ""))) })
  output$diffexpclusBM <- renderDataTable({
    sc_bm <- sc_bm()
    tab <-  as.matrix(sc_bm@ndata * min(sc_bm@counts)) + 0.1
    tab <- data.frame(tab)
    l1 <- names(sc_bm@cpart[sc_bm@cpart == input$clustcompareBM])
    l2 <- names(sc_bm@cpart[sc_bm@cpart == input$clustcompare2BM])
    x <- diffexpnb(tab, l1,l2,method="per-condition",norm=F,vfit=sc_bm@background$vfit)
    x <- x$res[rownames(x$res) %in% sc_bm@genes,]
    x <- x[order(x$pval),]
    x <- x[x$pval <0.05,]
    x <- x[,-c(1,4)]
    colnames(x) <- c(paste("mean.cl", input$clustcompareBM, sep=""), paste("mean.other.cl",input$clustcompare2BM, sep = ""), "log2FC", "pval", "padj")
    x[,"log2FC"] <- x[,"log2FC"] * -1
    x <- apply(x, 2, function(x) round(x, digits = 5))
    x
  })
  ### culture 
  sc_cuinp <- reactive({ 
    readRDS("sc_objects/sc_culture_input.RData") 
  })
  diff_exp_culture <- reactive({ 
    readRDS("diff_exp_tables/culture_diff_exp_table_filt.RData")
  })  
  output$plot_specfic_clusterCL <- renderPlot({
    z <- as.numeric(unlist(strsplit(input$plotmapclusterCL, split = ",")))
    plotmap2(sc_cuinp(), final = T, cluster = z, cex = 1.25)
  })
  output$plot_specfic_sampleCL <- renderPlot({
    sc_cuinp <- sc_cuinp()
    #z <- as.character(unlist(strsplit(input$plotsymbolTC, split = ",")))
    z <- paste(input$plotsymbolCL)
    plotsymbolsmap(sc_cuinp, types = sub("\\_.+", "", colnames(sc_cuinp@ndata)), subset = z, samples_col = marker_col, leg = F, um = F, cex = 1.25)
  })
  output$expression_plotlogTCL <- renderPlot({
    plotexpmap(sc_cuinp(), input$GeneCL, logsc = T, cex = 1.25)
  })
  
  output$expression_plotlogFCL <- renderPlot({
    plotexpmap(sc_cuinp(), input$GeneCL, logsc = F, cex = 1.25)
  })
  output$fracdotplot_zscTCL <- renderPlot({
    x <- unlist(strsplit(input$GenesCL, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersCL, split = ",")))
    fracdotplot(sc_cuinp(), genes = x, cluster = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplot_log2meanCL <- renderPlot({
    x <- unlist(strsplit(input$GenesCL, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersCL, split = ",")))
    fracdotplot(sc_cuinp(), genes = x, cluster = y, zsc = F, limup = 4, limdown = -4)
  })
  output$fracdotplotS_zscTCL <- renderPlot({
    sc_cuinp <- sc_cuinp()
    x <- unlist(strsplit(input$GenesSCL, split = ","))
    y <- as.character(unlist(strsplit(input$SamplesCL, split = ",")))
    fracdotplot(sc_cuinp, genes = x, samples  = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplotS_log2meanCL <- renderPlot({
    sc_cuinp <- sc_cuinp()
    x <- unlist(strsplit(input$GenesSCL, split = ","))
    y <- as.character(unlist(strsplit(input$SamplesCL, split = ",")))
    fracdotplot(sc_cuinp, genes = x, samples  = y, zsc = F, limup = 4, limdown = -4)
  })
  output$plot3markerCL <- renderPlot({
    genes1 <- unlist(strsplit(input$three_genesCL, split = ","))
    plot3marker(sc_cuinp(), cluster = input$three_genes_clusterCL, gene1 = genes1[1], gene2 = genes1[2], gene3 = genes1[3]) 
  })
  output$plot3marker2CL <- renderPlot({
    genes2 <- unlist(strsplit(input$three_genes2CL, split = ","))
    plot3marker(sc_cuinp(), cluster = input$three_genes_cluster2CL, gene1 = genes2[1], gene2 = genes2[2], gene3 = genes2[3]) 
  })
  output$table1CL <- renderDataTable({
    diff_exp_culture <- diff_exp_culture()
    eval(parse( text = paste("diff_exp_culture$clust_", input$cluster1CL, "_vs_all", sep = ""))) })
  output$table2CL <- renderDataTable({
    diff_exp_culture <- diff_exp_culture()
    eval(parse( text = paste("diff_exp_culture$clust_", input$cluster2CL, "_vs_all", sep = ""))) })
  
  output$diffexpclusCL <- renderDataTable({
    sc_cuinp <- sc_cuinp()
    tab <-  as.matrix(sc_cuinp@ndata * min(sc_cuinp@counts)) + 0.1
    tab <- data.frame(tab)
    l1 <- names(sc_cuinp@cpart[sc_cuinp@cpart == input$clustcompareCL])
    l2 <- names(sc_cuinp@cpart[sc_cuinp@cpart == input$clustcompare2CL])
    x <- diffexpnb(tab, l1,l2,method="per-condition",norm=F,vfit=sc_cuinp@background$vfit)
    x <- x$res[rownames(x$res) %in% sc_cuinp@genes,]
    x <- x[order(x$pval),]
    x <- x[x$pval <0.05,]
    x <- x[,-c(1,4)]
    colnames(x) <- c(paste("mean.cl", input$clustcompareCL, sep=""), paste("mean.other.cl",input$clustcompare2CL, sep = ""), "log2FC", "pval", "padj")
    x[,"log2FC"] <- x[,"log2FC"] * -1
    x <- apply(x, 2, function(x) round(x, digits = 5))
    x
  })

  ### timecourse
  sc_timecourse <- reactive({ 
      readRDS("sc_objects/sc_timecourse.RData") 
  })
  diff_exp_timecourse <- reactive({ 
      readRDS("diff_exp_tables/timecourse_diff_exp_table_filt.RData")
  })  
  branch_tc <- reactive({
    readRDS("StemID_Il13_Cxcl_branches_timecourse.RData")
  }) 
  
  output$plot_specfic_clusterTC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    z <- as.numeric(unlist(strsplit(input$plotmapclusterTC, split = ",")))
    plotmap2(sc_timecourse, final = T, cluster = z)
  })
  output$plot_specfic_sample <- renderPlot({
    sc_timecourse <- sc_timecourse()
    #z <- as.character(unlist(strsplit(input$plotsymbolTC, split = ",")))
    z <- paste(input$plotsymbolTC)
    plotsymbolsmap(sc_timecourse, types = sub("\\_.+", "", colnames(sc_timecourse@ndata)), subset = z, samples_col = set1, leg = F)
  })
  ##
  output$expression_plotlogTTC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    plotexpmap(sc_timecourse, input$GeneTC, logsc = T)
  })
  output$expression_plotlogFTC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    plotexpmap(sc_timecourse, input$GeneTC, logsc = F)
  })
  output$fracdotplot_zscTTC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    x <- unlist(strsplit(input$GenesTC, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersTC, split = ",")))
    fracdotplot(sc_timecourse, genes = x, cluster = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplot_log2meanTC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    x <- unlist(strsplit(input$GenesTC, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersTC, split = ",")))
    fracdotplot(sc_timecourse, genes = x, cluster = y, zsc = F, limup = 4, limdown = -4)
  })
  ##
  output$fracdotplotS_zscTTC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    x <- unlist(strsplit(input$GenesSTC, split = ","))
    y <- as.character(unlist(strsplit(input$SamplesTC, split = ",")))
    fracdotplot(sc_timecourse, genes = x, samples  = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplotS_log2meanTC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    x <- unlist(strsplit(input$GenesSTC, split = ","))
    y <- as.character(unlist(strsplit(input$SamplesTC, split = ",")))
    fracdotplot(sc_timecourse, genes = x, samples  = y, zsc = F, limup = 4, limdown = -4)
  })
  output$plot3markerTC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    genes1 <- unlist(strsplit(input$three_genesTC, split = ","))
    plot3marker(sc_timecourse, cluster = input$three_genes_clusterTC, gene1 = genes1[1], gene2 = genes1[2], gene3 = genes1[3]) 
  })
  output$plot3marker2TC <- renderPlot({
    sc_timecourse <- sc_timecourse()
    genes2 <- unlist(strsplit(input$three_genes2TC, split = ","))
    plot3marker(sc_timecourse, cluster = input$three_genes_cluster2TC, gene1 = genes2[1], gene2 = genes2[2], gene3 = genes2[3]) 
  })
  output$plotcxcl2branchTC <- renderPlot({
    branch_tc <- branch_tc()
    cxcl2_genes <- unlist(strsplit(input$branch_cxcl2_TC, split = ","))
    plotexpression(branch_tc$ndata_cxcl2_branch, branch_tc$cluspar_cxcl2_branch, cxcl2_genes, branch_tc$pseudoorder_cxcl2_branch, col = branch_tc$fcol, cluster = F, alpha=.5,types=NULL, cex = 3, lwd = 2.5, leg = T, map = F, ylim = range(-0.5:10), samp = T, samppart = branch_tc$samplepar_cxcl2_branch)
  })
  output$plotil13branch <- renderPlot({
    branch_tc <- branch_tc()
    areg_genes <- unlist(strsplit(input$branch_il13, split = ","))
    plotexpression(branch_tc$ndata_il13_branch, branch_tc$cluspar_il13_branch, areg_genes, branch_tc$pseudoorder_il13_branch, col = branch_tc$fcol, cluster = F, alpha=.5,types=NULL, cex = 3, lwd = 2.5, leg = T, map = F, ylim = range(-0.5:10), samp = T, samppart = branch_tc$samplepar_il13_branch)
  })
  output$table1TC <- renderDataTable({
    diff_exp_timecourse <- diff_exp_timecourse()
    eval(parse( text = paste("diff_exp_timecourse$clust_", input$cluster1TC, "_vs_all", sep = ""))) })
  output$table2TC <- renderDataTable({
    diff_exp_timecourse <- diff_exp_timecourse()
    eval(parse( text = paste("diff_exp_timecourse$clust_", input$cluster2TC, "_vs_all", sep = ""))) })
  output$diffexpclusTC <- renderDataTable({
    sc_timecourse <- sc_timecourse()
    tab <-  as.matrix(sc_timecourse@ndata * min(sc_timecourse@counts)) + 0.1
    tab <- data.frame(tab)
    l1 <- names(sc_timecourse@cpart[sc_timecourse@cpart == input$clustcompareTC])
    l2 <- names(sc_timecourse@cpart[sc_timecourse@cpart == input$clustcompare2TC])
    x <- diffexpnb(tab, l1,l2,method="per-condition",norm=F,vfit=sc_timecourse@background$vfit)
    x <- x$res[rownames(x$res) %in% sc_timecourse@genes,]
    x <- x[order(x$pval),]
    x <- x[x$pval <0.05,]
    x <- x[,-c(1,4)]
    colnames(x) <- c(paste("mean.cl", input$clustcompareTC, sep=""), paste("mean.other.cl",input$clustcompare2TC, sep = ""), "log2FC", "pval", "padj")
    x[,"log2FC"] <- x[,"log2FC"] * -1
    x <- apply(x, 2, function(x) round(x, digits = 5))
    x
  })
  ####
  sc_para <- reactive({ 
      readRDS("sc_objects/sc_parabiosis.RData") 
  })
  diff_exp_para <- reactive({ 
      readRDS("diff_exp_tables/parabios_diff_exp_table_filt.RData")
  })  
  
  output$plot_specfic_clusterPara <- renderPlot({
    sc_para <- sc_para()
    z <- as.numeric(unlist(strsplit(input$plotmapclusterPara, split = ",")))
    plotmap2(sc_para, final = T, cluster = z, cex = 1)
  })
  output$plot_specfic_samplePara <- renderPlot({
    sc_para <- sc_para()
    plotsymbolsmap(sc_para, types = sub("\\_.+", "", colnames(sc_para@ndata)), subset = input$plotsymbolPara, samples_col = set4, leg = F)
  })
  output$expression_plotlogTPara <- renderPlot({
    sc_para <- sc_para()
    plotexpmap(sc_para, input$GenePara, logsc = T, cex = 1)
  })
  
  output$expression_plotlogFPara <- renderPlot({
    sc_para <- sc_para()
    plotexpmap(sc_para, input$GenePara, logsc = F, cex = 1)
  })
  output$fracdotplot_zscTPara <- renderPlot({
    sc_para <- sc_para()
    x <- unlist(strsplit(input$GenesPara, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersPara, split = ",")))
    fracdotplot(sc_para, genes = x, cluster = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplot_log2meanPara <- renderPlot({
    sc_para <- sc_para()
    x <- unlist(strsplit(input$GenesPara, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersPara, split = ",")))
    fracdotplot(sc_para, genes = x, cluster = y, zsc = F, limup = 4, limdown = -4)
  })
  output$table1para <- renderDataTable({
    diff_exp_para <- diff_exp_para()
    eval(parse( text = paste("diff_exp_para$clust_", input$cluster1para, "_vs_all", sep = ""))) })
  output$table2para <- renderDataTable({
    diff_exp_para <- diff_exp_para()
    eval(parse( text = paste("diff_exp_para$clust_", input$cluster2para, "_vs_all", sep = ""))) })
  output$diffexpclusPara <- renderDataTable({
    sc_para  <- sc_para()
    tab <-  as.matrix(sc_para@ndata * min(sc_para@counts)) + 0.1
    tab <- data.frame(tab)
    l1 <- names(sc_para@cpart[sc_para@cpart == input$clustcomparePara])
    l2 <- names(sc_para@cpart[sc_para@cpart == input$clustcompare2Para])
    x <- diffexpnb(tab, l1,l2,method="per-condition",norm=F,vfit=sc_para@background$vfit)
    x <- x$res[rownames(x$res) %in% sc_para@genes,]
    x <- x[order(x$pval),]
    x <- x[x$pval <0.05,]
    x <- x[,-c(1,4)]
    colnames(x) <- c(paste("mean.cl", input$clustcomparePara, sep=""), paste("mean.other.cl",input$clustcompare2Para, sep = ""), "log2FC", "pval", "padj")
    x[,"log2FC"] <- x[,"log2FC"] * -1
    x <- apply(x, 2, function(x) round(x, digits = 5))
    x
  })
  ### 
  sc_chim <- reactive({ 
      readRDS("sc_objects/sc_chimera.RData") 
  })
  diff_exp_chim <- reactive({ 
      readRDS("diff_exp_tables/chimera_diff_exp_table_filt.RData")
  })  
  

  output$plot_specfic_clusterChim <- renderPlot({
    sc_chim <- sc_chim()
    z <- as.numeric(unlist(strsplit(input$plotmapclusterChim, split = ",")))
    plotmap2(sc_chim, final = T, cluster = z, cex = 1)
  })
  output$plot_specfic_sampleChim <- renderPlot({
    sc_chim <- sc_chim()
    plotsymbolsmap(sc_chim, types = sub("\\_.+", "", colnames(sc_chim@ndata)), subset = input$plotsymbolChim, samples_col = set4, leg = F)
  })
  output$expression_plotlogTChim <- renderPlot({
    sc_chim <- sc_chim()
    plotexpmap(sc_chim, input$GeneChim, logsc = T, cex = 1)
  })
  
  output$expression_plotlogFChim <- renderPlot({
    sc_chim <- sc_chim()
    plotexpmap(sc_chim, input$GeneChim, logsc = F, cex = 1)
  })
  output$fracdotplot_zscTChim <- renderPlot({
    sc_chim <- sc_chim()
    x <- unlist(strsplit(input$GenesChim, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersChim, split = ",")))
    fracdotplot(sc_chim, genes = x, cluster = y, zsc = T, limup = 1, limdown = -1)
    
  })
  output$fracdotplot_log2meanChim <- renderPlot({
    sc_chim <- sc_chim()
    x <- unlist(strsplit(input$GenesChim, split = ","))
    y <- as.numeric(unlist(strsplit(input$ClustersChim, split = ",")))
    fracdotplot(sc_chim, genes = x, cluster = y, zsc = F, limup = 4, limdown = -4)
  })
  output$table1chim <- renderDataTable({
    diff_exp_chim <- diff_exp_chim()
    eval(parse( text = paste("diff_exp_chim$clust_", input$cluster1chim, "_vs_all", sep = ""))) })
  output$table2chim <- renderDataTable({
    diff_exp_chim <- diff_exp_chim()
    eval(parse( text = paste("diff_exp_chim$clust_", input$cluster2chim, "_vs_all", sep = ""))) })
  output$diffexpclusChim <- renderDataTable({
    sc_chim  <- sc_chim()
    tab <-  as.matrix(sc_chim@ndata * min(sc_chim@counts)) + 0.1
    tab <- data.frame(tab)
    l1 <- names(sc_chim@cpart[sc_chim@cpart == input$clustcompareChim])
    l2 <- names(sc_chim@cpart[sc_chim@cpart == input$clustcompare2Chim])
    x <- diffexpnb(tab, l1,l2,method="per-condition",norm=F,vfit=sc_chim@background$vfit)
    x <- x$res[rownames(x$res) %in% sc_chim@genes,]
    x <- x[order(x$pval),]
    x <- x[x$pval <0.05,]
    x <- x[,-c(1,4)]
    colnames(x) <- c(paste("mean.cl", input$clustcompareChim, sep=""), paste("mean.other.cl",input$clustcompare2Chim, sep = ""), "log2FC", "pval", "padj")
    x[,"log2FC"] <- x[,"log2FC"] * -1
    x <- apply(x, 2, function(x) round(x, digits = 5))
    x
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

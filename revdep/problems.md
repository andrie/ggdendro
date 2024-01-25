# chemodiv

<details>

* Version: 0.3.0
* GitHub: https://github.com/hpetren/chemodiv
* Source code: https://github.com/cran/chemodiv
* Date/Publication: 2023-08-17 17:52:33 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::revdep_details(, "chemodiv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'chemodiv-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: chemoDivPlot
    > ### Title: Plot chemodiversity
    > ### Aliases: chemoDivPlot
    > 
    > ### ** Examples
    > 
    > minimalDiv <- calcDiv(minimalSampData, minimalCompDis, type = "FuncHillDiv")
    ...
     20. │                 └─ggplot2 (local) compute_aesthetics(..., self = self)
     21. │                   └─ggplot2:::scales_add_defaults(...)
     22. │                     └─base::lapply(aesthetics[new_aesthetics], eval_tidy, data = data)
     23. │                       └─rlang (local) FUN(X[[i]], ...)
     24. ├─label
     25. ├─rlang:::`$.rlang_data_pronoun`(.data, label)
     26. │ └─rlang:::data_pronoun_get(...)
     27. └─rlang:::abort_data_pronoun(x, call = y)
     28.   └─rlang::abort(msg, "rlang_error_data_pronoun_not_found", call = call)
    Execution halted
    ```

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
      ══ Skipped tests (3) ═══════════════════════════════════════════════════════════
      • On CRAN (3): 'test-NPCTable.R:16:3', 'test-NPCTable.R:24:3',
        'test-compDis.R:28:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-chemoDivPlot.R:17:1'): (code run outside of `test_that()`) ─────
      Error in `ggplot2::geom_text(data = compDisMatClustDendData$labels, ggplot2::aes(x = .data$x, 
          y = .data$y, label = .data$label), hjust = -0.1, angle = 0)`: Problem while computing aesthetics.
      ℹ Error occurred in the 2nd layer.
      Caused by error in `.data$label`:
      ! Column `label` not found in `.data`.
      
      [ FAIL 1 | WARN 48 | SKIP 3 | PASS 81 ]
      Error: Test failures
      Execution halted
    ```

# fairmodels

<details>

* Version: 1.2.1
* GitHub: https://github.com/ModelOriented/fairmodels
* Source code: https://github.com/cran/fairmodels
* Date/Publication: 2022-08-23 19:50:06 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::revdep_details(, "fairmodels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'fairmodels-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: fairness_heatmap
    > ### Title: Fairness heatmap
    > ### Aliases: fairness_heatmap
    > 
    > ### ** Examples
    > 
    > 
    ...
    [32m Fairness object created succesfully [39m 
    > 
    > 
    > fh <- fairness_heatmap(fobject)
    > 
    > plot(fh)
    Error in `[.data.frame`(dendro_data1$labels, "label") : 
      undefined columns selected
    Calls: plot ... plot.fairness_heatmap -> unlist -> [ -> [.data.frame
    Execution halted
    ```

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        2. │ └─base::withCallingHandlers(...)
        3. ├─fairmodels:::expect_s3_class(...)
        4. │ ├─testthat::expect(...) at C:\Users\apdev\Documents\github\R-packages\maintainer\ggdendro\ggdendro\revdep\checks\fairmodels\new\fairmodels.Rcheck\tests\testthat\helper_objects.R:70:19
        5. │ └─base::class(object) %in% class
        6. ├─fairmodels::plot_fairmodels(fc, type = "fairness_heatmap")
        7. └─fairmodels:::plot_fairmodels.fairness_object(fc, type = "fairness_heatmap")
        8.   └─fairmodels:::plot_fairmodels.default(x, type, ...)
        9.     └─fairmodels:::plot.fairness_heatmap(fairness_heatmap(x, ...))
       10.       ├─base::unlist(dendro_data1$labels["label"])
       11.       ├─dendro_data1$labels["label"]
       12.       └─base::`[.data.frame`(dendro_data1$labels, "label")
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 300 ]
      Error: Test failures
      Execution halted
    ```

# forestmangr

<details>

* Version: 0.9.5
* GitHub: https://github.com/sollano/forestmangr
* Source code: https://github.com/cran/forestmangr
* Date/Publication: 2023-02-15 22:20:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::revdep_details(, "forestmangr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'forestmangr-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: similarity_matrix
    > ### Title: Get the similarity matrix of an area
    > ### Aliases: similarity_matrix
    > 
    > ### ** Examples
    > 
    > library(forestmangr)
    ...
     15. │   └─forestmangr (local) .f(.x[[i]], ...)
     16. │     ├─base::merge(dendr[["labels"]], clust.df, by = "label")
     17. │     └─base::merge.data.frame(dendr[["labels"]], clust.df, by = "label")
     18. │       └─base (local) fix.by(by.x, x)
     19. │         └─base::stop(...)
     20. └─base::.handleSimpleError(...)
     21.   └─purrr (local) h(simpleError(msg, call))
     22.     └─cli::cli_abort(...)
     23.       └─rlang::abort(...)
    Execution halted
    ```

# ggh4x

<details>

* Version: 0.2.6
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2023-08-30 19:10:06 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::revdep_details(, "ggh4x")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'ggh4x-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: guide_dendro
    > ### Title: Dendrogram guide
    > ### Aliases: guide_dendro
    > 
    > ### ** Examples
    > 
    > clust <- hclust(dist(USArrests), "ave")
    ...
    +   geom_raster() +
    +   scale_y_dendrogram(hclust = clust,
    +                      guide = guide_dendro(n.dodge = 2))
    > 
    > # The looks of the dendrogram are controlled through ticks
    > g + theme(axis.ticks = element_line(colour = "red"))
    Error in `$<-.data.frame`(`*tmp*`, "label", value = character(0)) : 
      replacement has 0 rows, data has 50
    Calls: <Anonymous> ... guide_train -> guide_train.dendroguide -> $<- -> $<-.data.frame
    Execution halted
    ```

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        6.         └─base::lapply(...)
        7.           └─ggplot2 (local) FUN(X[[i]], ...)
        8.             └─ggplot2 (local) train_panel_guides(..., self = self)
        9.               └─base::lapply(...)
       10.                 └─ggplot2 (local) FUN(X[[i]], ...)
       11.                   ├─ggplot2::guide_train(guide, panel_params[[aesthetic]])
       12.                   └─ggh4x:::guide_train.dendroguide(guide, panel_params[[aesthetic]])
       13.                     ├─base::`$<-`(`*tmp*`, "label", value = `<chr>`)
       14.                     └─base::`$<-.data.frame`(`*tmp*`, "label", value = `<chr>`)
      
      [ FAIL 4 | WARN 0 | SKIP 21 | PASS 774 ]
      Deleting unused snapshots:
      • facet_manual/removable-whitespace.svg
      Error: Test failures
      Execution halted
    ```

# iheatmapr

<details>

* Version: 0.7.0
* GitHub: https://github.com/ropensci/iheatmapr
* Source code: https://github.com/cran/iheatmapr
* Date/Publication: 2023-08-30 17:00:17 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::revdep_details(, "iheatmapr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
      Component "layout": Component "shapes": Component 3: Component 4: Mean relative difference: 0.3955314
      Component "layout": Component "shapes": Component 4: Component 1: Mean relative difference: 0.7857143
      Component "layout": Component "shapes": Component 4: Component 2: Mean relative difference: 0.7857143
      Component "layout": Component "shapes": Component 4: Component 3: Mean relative difference: 0.3955314
      Component "layout": Component "shapes": Component 4: Component 4: Mean relative difference: 0.2759312
      Component "layout": Component "shapes": Component 5: Component 1: Mean relative difference: 0.4
      ...
      Backtrace:
          ▆
       1. └─iheatmapr:::expect_iheatmap(...) at test_dendro.R:22:2
       2.   └─iheatmapr:::expect_ihm_equal_to_reference(...) at C:\Users\apdev\Documents\github\R-packages\maintainer\ggdendro\ggdendro\revdep\checks\iheatmapr\new\iheatmapr.Rcheck\tests\testthat\helper_expectation.R:56:2
      
      [ FAIL 14 | WARN 0 | SKIP 0 | PASS 348 ]
      Error: Test failures
      Execution halted
    ```

# Linnorm

<details>

* Version: 2.24.1
* GitHub: NA
* Source code: https://github.com/cran/Linnorm
* Date/Publication: 2023-05-02
* Number of recursive dependencies: 107

Run `revdepcheck::revdep_details(, "Linnorm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'Linnorm-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: Linnorm.HClust
    > ### Title: Linnorm-hierarchical clustering analysis.
    > ### Aliases: Linnorm.HClust
    > ### Keywords: CPM Clustering Count Expression FPKM Linnorm Parametric
    > ###   RNA-seq RPKM Raw TPM hierarchical normalization transformation
    > 
    > ### ** Examples
    ...
    > 
    > #Obtain example matrix:
    > data(Islam2011)
    > #Example:
    > HClust.results <- Linnorm.HClust(Islam2011, Group=c(rep("ESC",48), rep("EF",44), rep("NegCtrl",4)))
    Warning in Linnorm.HClust(Islam2011, Group = c(rep("ESC", 48), rep("EF",  :
      NAs introduced by coercion
    Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
    Calls: Linnorm.HClust -> merge -> merge.data.frame -> fix.by
    Execution halted
    ```

## In both

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
      <packageNotFoundError/error/condition>
      Error in `library(matrixStats)`: there is no package called 'matrixStats'
      Backtrace:
          ▆
       1. └─base::library(matrixStats) at test_Misc_Stats.R:3:0
      ── Error ('test_SkewVar.R:3:1'): (code run outside of `test_that()`) ───────────
      <packageNotFoundError/error/condition>
      Error in `library(matrixStats)`: there is no package called 'matrixStats'
      Backtrace:
          ▆
       1. └─base::library(matrixStats) at test_SkewVar.R:3:0
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   2.3Mb
        doc    1.9Mb
        libs   1.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    Linnorm.HClust: no visible binding for global variable 'x'
    Linnorm.HClust: no visible binding for global variable 'y'
    Linnorm.HClust: no visible binding for global variable 'xend'
    Linnorm.HClust: no visible binding for global variable 'yend'
    Linnorm.HClust: no visible binding for global variable 'cluster'
    Linnorm.HClust: no visible binding for global variable 'X1'
    Linnorm.HClust: no visible binding for global variable 'X2'
    Linnorm.HVar: no visible binding for global variable 'SD'
    Linnorm.HVar: no visible binding for global variable 'group'
    Undefined global functions or variables:
      SD X1 X2 cluster group x xend y yend
    ```

# microeco

<details>

* Version: 1.1.0
* GitHub: https://github.com/ChiLiubio/microeco
* Source code: https://github.com/cran/microeco
* Date/Publication: 2023-09-13 23:00:02 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::revdep_details(, "microeco")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'microeco-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: trans_beta
    > ### Title: Create 'trans_beta' object for beta-diversity analysis based on
    > ###   the distance matrix
    > ### Aliases: trans_beta
    > 
    > ### ** Examples
    > 
    ...
     1. └─t1$plot_clustering(...)
     2.   ├─base::suppressWarnings(...)
     3.   │ └─base::withCallingHandlers(...)
     4.   ├─dplyr::left_join(...)
     5.   └─dplyr:::left_join.data.frame(...)
     6.     └─dplyr:::join_mutate(...)
     7.       └─dplyr:::join_cols(...)
     8.         └─dplyr:::check_join_vars(by$x, x_names, by$condition, "x", error_call = error_call)
     9.           └─rlang::abort(bullets, call = error_call)
    Execution halted
    ```

# mosaic

<details>

* Version: 1.8.4.2
* GitHub: https://github.com/ProjectMOSAIC/mosaic
* Source code: https://github.com/cran/mosaic
* Date/Publication: 2022-09-20 18:10:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::revdep_details(, "mosaic")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
      • plotDist/plotdist5.svg
      • plotDist/plotdist6.svg
      • plotDist/plotdist7.svg
      • plotDist/plotdist8.svg
      • plotDist/plotdist9.svg
      • plotModel/plotmodel2.svg
      • plotModel/plotmodel3.svg
      • plotPoints/plotpoints2.svg
      • plotPoints/plotpoints3.svg
      • rfun/rfun2.svg
      • statTally/stattally2.svg
      • statTally/stattally3.svg
      • xpnorm/xpnorm2.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: 'manipulate'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: 'cubature'
    ```

# qPLEXanalyzer

<details>

* Version: 1.18.0
* GitHub: https://github.com/crukci-bioinformatics/qPLEXanalyzer
* Source code: https://github.com/cran/qPLEXanalyzer
* Date/Publication: 2023-04-25
* Number of recursive dependencies: 150

Run `revdepcheck::revdep_details(, "qPLEXanalyzer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'qPLEXanalyzer-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: hierarchicalPlot
    > ### Title: Hierarchical clustering plot
    > ### Aliases: hierarchicalPlot
    > 
    > ### ** Examples
    > 
    > 
    ...
      6. ├─dplyr:::mutate.data.frame(., SampleName = as.character(label))
      7. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
      8. │   ├─base::withCallingHandlers(...)
      9. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
     10. │     └─mask$eval_all_mutate(quo)
     11. │       └─dplyr (local) eval()
     12. └─base::.handleSimpleError(...)
     13.   └─dplyr (local) h(simpleError(msg, call))
     14.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: 'qPLEXdata'
    ```

*   checking R code for possible problems ... NOTE
    ```
    convertToMSnset: no visible binding for global variable 'SampleName'
    corrPlot: no visible binding for global variable 'X'
    corrPlot: no visible binding for global variable 'AddValues'
    corrPlot: no visible binding for global variable 'Cor'
    corrPlot: no visible binding for global variable 'Y'
    corrPlot: no visible binding for global variable 'CorTxt'
    coveragePlot: no visible binding for global variable 'Accessions'
    coveragePlot: no visible binding for global variable 'Sequences'
    getContrastResults: no visible binding for global variable 'B'
    getContrastResults: no visible binding for global variable 'AveExpr'
    ...
    summarizeIntensities: no visible binding for global variable
      'Sequences'
    summarizeIntensities: no visible global function definition for 'where'
    summarizeIntensities: no visible binding for global variable 'Count'
    Undefined global functions or variables:
      Accessions AddValues AveExpr B Cor CorTxt Count GeneSymbol Intensity
      Mean Modifications PeptideID RLI RowID SampleName Seq_Acc Sequences
      Sites Sites_Acc SymbolLab Type Variance X Y adj.P.Val group logFC
      logInt logIntensity meanscaledIntensity medianLogInt sInt where x
      xend y yend
    ```

# SPARTAAS

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/SPARTAAS
* Date/Publication: 2023-06-06 13:10:09 UTC
* Number of recursive dependencies: 191

Run `revdepcheck::revdep_details(, "SPARTAAS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'SPARTAAS-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: hclustcompro
    > ### Title: hclustcompro
    > ### Aliases: hclustcompro perioclust
    > 
    > ### ** Examples
    > 
    > library(SPARTAAS)
    ...
      9.         │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
     10.         │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     11.         │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     12.         │ └─base::withCallingHandlers(...)
     13.         └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     14.           └─l$compute_aesthetics(d, plot)
     15.             └─ggplot2 (local) compute_aesthetics(..., self = self)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

# statVisual

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/statVisual
* Date/Publication: 2020-02-20 19:30:02 UTC
* Number of recursive dependencies: 190

Run `revdepcheck::revdep_details(, "statVisual")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'statVisual-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: Dendro
    > ### Title: Compare Groups Based on Dendrogram
    > ### Aliases: Dendro
    > ### Keywords: method
    > 
    > ### ** Examples
    > 
    ...
    > 
    > pDat$grp = factor(pDat$grp)
    > 
    > statVisual(type = 'Dendro', 
    +            x = pDat[, c(3:8)], 
    +            group = pDat$grp)
    Error in `$<-.data.frame`(`*tmp*`, "group", value = integer(0)) : 
      replacement has 0 rows, data has 20
    Calls: statVisual -> Dendro -> $<- -> $<-.data.frame
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      'gbm' 'ggfortify' 'tibble' 'tidyverse'
      All declared Imports should be used.
    ```

# triplot

<details>

* Version: 1.3.0
* GitHub: https://github.com/ModelOriented/triplot
* Source code: https://github.com/cran/triplot
* Date/Publication: 2020-07-13 17:00:03 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::revdep_details(, "triplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'triplot-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: hierarchical_importance
    > ### Title: Calculates importance of hierarchically grouped aspects
    > ### Aliases: hierarchical_importance plot.hierarchical_importance
    > 
    > ### ** Examples
    > 
    > library(DALEX)
    ...
      8.       │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_aesthetics(d, plot)
     14.           └─ggplot2 (local) compute_aesthetics(..., self = self)
     15.             └─cli::cli_abort(...)
     16.               └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_group_variables.R:23:3'): check plot.cluster_variables function ──
      `plot(p)` threw an error.
      Message: Problem while computing aesthetics.
      ℹ Error occurred in the 3rd layer.
      Caused by error in `compute_aesthetics()`:
      ! Aesthetics are not valid data columns.
      ✖ The following aesthetics are invalid:
      ✖ `label = label`
      ℹ Did you mistype the name of a data column or forget to add `after_stat()`?
      Class:   rlang_error/error/condition
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 83 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```


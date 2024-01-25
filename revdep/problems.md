# iheatmapr

<details>

* Version: 0.7.0
* GitHub: https://github.com/ropensci/iheatmapr
* Source code: https://github.com/cran/iheatmapr
* Date/Publication: 2023-08-30 17:00:17 UTC
* Number of recursive dependencies: 93

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
       1. └─iheatmapr:::expect_iheatmap(...) at test_dendro.R:22:3
       2.   └─iheatmapr:::expect_ihm_equal_to_reference(...) at C:\Users\apdev\Documents\github\R-packages\maintainer\ggdendro\ggdendro\revdep\checks\iheatmapr\new\iheatmapr.Rcheck\tests\testthat\helper_expectation.R:56:3
      
      [ FAIL 14 | WARN 0 | SKIP 0 | PASS 348 ]
      Error: Test failures
      Execution halted
    ```

# mosaic

<details>

* Version: 1.9.0
* GitHub: https://github.com/ProjectMOSAIC/mosaic
* Source code: https://github.com/cran/mosaic
* Date/Publication: 2023-11-10 00:10:13 UTC
* Number of recursive dependencies: 133

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


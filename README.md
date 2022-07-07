# Regression Analysis of Relative CPU Performance Data
The CPU is the work engine of a system and it keeps everything running. All the different calculations required for gathering and delivering data, maintaining the system, and ordering access are performed by the CPU. It has different characteristics like frequency, cache size, memory bandwidth, and core count. Better configurations usually cost more. Consumers should be able to estimate whether investing in a costly configuration helps to meet their particular goals. Hence, performance analysis is really important at present.

The questions explored in this notebook are as follows:

1. Does any pair of variables correlated?
2. Can the use of transformations on the response and predictor variables improve the regression models?
3. Can weighted least square method improve the model?
4. Can variable selection methods improve the model?
5. Is the improved model effective?

The goals of this study:
1. Regression analysis to create a mathematical model that can be used to predict the valuesof estimated relative performance.
2. Use Box-Cox method and square root transformations.
3. Apply Weighted Least Square method.
4. Apply variable selection methods. Specifically, forward and backward methods.
5. Use 5-fold cross validation to measure the model effectiveness.
6. Use R libraries such as alr4, caret and MASS.

![My Image](reg.jpg.webp)

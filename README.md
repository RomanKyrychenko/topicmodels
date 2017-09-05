Topic modeling
================

Модель
------

Система базується на моделі **LDA** (Latent Dirichlet Allocation). Конкретно реалізовано **WarpLDA** модель, яка дозволяє достатньо швидко зробити класифікацію текстів на велику кількість тем.

Код базується на бібліотеці [text2vec](http://text2vec.org/). Векторне представлення слів пришвидшує обробку масивів. Альтернатива - робота з матрицями в рамках бібліотеки **tm**, однак у неї є недоліки:

-   більше використання ресурсів машини
-   некоректна робота з кирилицею

Ядро
----

Основа моделі:

``` r
library(tidyverse)
library(stringr)
library(text2vec)
data("movie_review")
it = itoken(movie_review$review, progressbar = FALSE, ids=movie_review$ids)
v = create_vocabulary(it) %>% 
  prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)
lda_model = LDA$new(n_topics = 10)

doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 1000, 
                              convergence_tol = 0.001, n_check_convergence = 25,progressbar = F)
```

    ## INFO [2017-09-05 08:28:50] iter 25 loglikelihood = -3287623.371
    ## INFO [2017-09-05 08:28:52] iter 50 loglikelihood = -3207258.004
    ## INFO [2017-09-05 08:28:54] iter 75 loglikelihood = -3176885.052
    ## INFO [2017-09-05 08:28:56] iter 100 loglikelihood = -3162532.061
    ## INFO [2017-09-05 08:28:59] iter 125 loglikelihood = -3154170.709
    ## INFO [2017-09-05 08:29:01] iter 150 loglikelihood = -3150266.555
    ## INFO [2017-09-05 08:29:03] iter 175 loglikelihood = -3149750.609
    ## INFO [2017-09-05 08:29:03] early stopping at 175 iteration

``` r
gammaDF <- as_tibble(doc_topic_distr)
names(gammaDF) <- c(1:10)
    
data_frame(ID = attr(doc_topic_distr,"dimnames")[[1]],
       Тема = as.numeric(apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))][1])),
       Відповідність = apply(gammaDF,1,max))
```

    ## # A tibble: 5,000 x 3
    ##       ID  Тема Відповідність
    ##    <chr> <dbl>         <dbl>
    ##  1     1     5     0.2621951
    ##  2     2     6     0.3750000
    ##  3     3     4     0.2822086
    ##  4     4     6     0.1826087
    ##  5     5     4     0.2455090
    ##  6     6     5     0.1600000
    ##  7     7     4     0.2857143
    ##  8     8     9     0.3018868
    ##  9     9     3     0.2933333
    ## 10    10     1     0.2222222
    ## # ... with 4,990 more rows

Вхідний файл
------------

На вхід береться таблиця Excel


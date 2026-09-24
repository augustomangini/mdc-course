# MDC — Complex Data Mining (Unicamp)

Code and assignments from **MDC** (*Mineração de Dados Complexos*), the professional extension program in
Data Science / Machine Learning / Big Data offered by the **Institute of Computing, Unicamp** (Universidade
Estadual de Campinas). In progress, expected completion in 2026.

> **Note on language:** course materials, variable names and comments are mostly in **Portuguese** (the
> language of instruction). This README is in English for portfolio purposes.
>
> **Note on data:** raw datasets, slide decks (PDF) and generated reports have been intentionally left out of
> this repository to keep it lean and focused on code — see [`.gitignore`](.gitignore). Notebooks/scripts
> reference the original filenames from each assignment (e.g. `bakery.csv`, `speech.csv`), available on the
> course's Moodle page.

## Disciplines

| Discipline | Folder | Topics covered | Tools |
|---|---|---|---|
| **Data Analysis** (INF-0612) | [`analise-dados/`](analise-dados) | Data cleaning (missing values, duplicates, outliers), date/time handling, exploratory analysis and visualization on real-world weather station data ([CEPAGRI](analise-dados/trabalhos/trabalho%202)) | R, `ggplot2` |
| **Unsupervised Machine Learning** (INF-0613) | [`aprendizado-nao-supervisionado/`](aprendizado-nao-supervisionado) | [Association rule mining](aprendizado-nao-supervisionado/trabalhos/trabalho%201) on retail transactions; [dimensionality reduction & clustering](aprendizado-nao-supervisionado/trabalhos/trabalho%202) (PCA, t-SNE, UMAP, k-means, hierarchical clustering, DBSCAN) on speech/phoneme data | R, `arules`, `factoextra`, `Rtsne`, `umap`, `dbscan`, `NbClust`, `cluster` |
| **Supervised Machine Learning I** (INF-0615) | [`aprendizado-supervisionado-I/`](aprendizado-supervisionado-I) | [Regression](aprendizado-supervisionado-I/Trabalhos/Trabalho%201) on clinical voice biomarkers to predict Parkinson's disease severity (UPDRS score); [classification](aprendizado-supervisionado-I/Trabalhos/Trabalho%202) of fake vs. real news with class-imbalance handling. Regularized regression, trees, ensembles, model evaluation | R, `glmnet`, `randomForest`, `rpart`, `xgboost`, `caret`, `SMOTE`, `pROC` |
| **Supervised Machine Learning II** (INF-0616) | [`aprendizado-supervisionado-II/`](aprendizado-supervisionado-II) | Python/NumPy/pandas foundations; SVMs (linear/RBF, from-scratch implementation) and SVR with grid search; neural networks from scratch and with Keras/PyTorch on MNIST, IMDB sentiment and a bank customer-complaints classifier | Python, `scikit-learn`, `PyTorch`, `Keras`/`TensorFlow`, `pandas`, `NumPy` |
| **Big Data** (INF-0617) | [`big-data/`](big-data) | Distributed processing fundamentals (MapReduce), Spark RDD transformations, ETL and Spark SQL, Structured Streaming; [large-scale sentiment analysis](big-data/Trabalhos) of movie dialogue using a pre-trained HuggingFace Transformers model over PySpark | PySpark (Spark SQL, Structured Streaming), `transformers` (HuggingFace) |
| **Information Retrieval** (INF-0611) | [`recuperacao-informacao/`](recuperacao-informacao) | Ranking models ([tf-idf and BM25](recuperacao-informacao/trabalho%201)) evaluated with ranking-quality metrics (precision/recall, MAP); [content-based image retrieval](recuperacao-informacao/trabalho%202) and rank aggregation | R, `tm`, `tokenizers`, `udpipe`, `imager`, `proxy` |
| **Information Visualization** (INF-0614) | [`vis-informacao/`](vis-informacao) | Multivariate data analysis (MovieLens dataset) and multidimensional projection (MDS), parallel coordinates and scatterplot matrices on the periodic table and biblical text corpora. Built with no-code visualization tools rather than scripted code | Tableau, Orange Data Mining |

## Tech stack across the program

- **Languages:** R, Python
- **ML / stats:** scikit-learn, caret, glmnet, randomForest, xgboost, rpart, arules
- **Deep learning:** PyTorch, Keras/TensorFlow
- **Big data:** Apache Spark (PySpark), Structured Streaming
- **NLP / IR:** tf-idf, BM25, tm, tokenizers, HuggingFace Transformers
- **Unsupervised learning:** PCA, t-SNE, UMAP, k-means, hierarchical clustering, DBSCAN
- **Visualization:** ggplot2, matplotlib/seaborn, Tableau, Orange

## Collaboration

Several assignments (data analysis, unsupervised learning, supervised learning I, information visualization)
were completed in groups as part of the course requirements; group members' individual files are kept as-is
(e.g. files suffixed with a name) to preserve the original submission history.

## Disclaimer

This repository contains coursework produced for academic purposes as part of Unicamp's MDC extension
program. It is shared as a portfolio reference and is not intended for redistribution of course materials
(slides, datasets) subject to the institution's own terms.

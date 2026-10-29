# The Causal Effects of Counseling Quality: Experimental Evidence from Urban Malawi

Replication code for the paper by Marie C. Montás, Sarah Vincent, and Mahesh V. Karra. Contact: Marie C. Montás, Department of Global Health and Population, Harvard T.H. Chan School of Public Health (mariemontas@fas.harvard.edu).

## Abstract

Across many primary health services in low- and middle-income countries, counseling is often the first point of entry and engagement with the health system, yet the empirical literature offers little causal evidence on how its quality, as distinct from access, shapes health care utilization and outcomes. We exploit variation in first counselor assignment in a randomized controlled trial in urban Malawi, in which 901 postpartum and pregnant women were each randomly allocated to one of six family planning counselors at their first home-visit session. We construct an examiner-design instrument from each counselor's leave-one-out average of patient-reported first-session quality on two dimensions: satisfaction with the counselor and the breadth of topics covered. Higher-quality counseling compresses session dose and expands per-session content. Family planning take-up rises sharply, with particularly large effects on injectable contraception around delivery. Reproductive outcomes diverge by baseline pregnancy status and concentrate among women closest to the peri-partum window. The family planning take-up effects remain positive under every leave-one-counselor-out check, while the reproductive divergence does not survive dropping the highest-quality counselor.

*JEL:* I12, I15, J13, O15

## Contents

```
code/                       Stata do-files (cleaning, estimation, robustness), R figure scripts,
                            and the cost-effectiveness builder; master.do runs Stages 1-3 in order
code/build_latex_tables/    Python builders that turn the Stage 2 results into the paper's tables
```

Data (`dta/`), logs, tables, and figures are not included.

## Pipeline

Set `global MFPS_ROOT` to the project folder (the folder that holds `Analysis/`) before running `master.do`; if it is not set, Stata uses the current working directory. The R and Python scripts read the `MFPS_ROOT` environment variable in the same way.

| Stage | What it does |
|---|---|
| 1 | Cleaning: builds the counseling-quality instrument (leave-one-out, satisfaction plus topics engaged), session- and woman-level quality indices, take-up indicators, and all outcome variables |
| 2 | Estimation: first stage, main IV results, ITT and reduced form, timing mechanism, leave-one-counselor-out, four-component quality index, autonomy, per-panel summary indices (Kling, Liebman and Katz 2007; Anderson 2008), wild-cluster bootstrap with Webb weights clustered on the six counselors, take-up balance, attrition, ITT balance on the 991 approached women, observation counts, and topic coverage |
| 2 (CEA) | `results_cea_itt.do` and `build_cea_costs.py`: provider-perspective costing of the counseling component and cost per couple-year of protection |
| 3 | `build_latex_tables/*.py` typeset the tables from the Stage 2 outputs |
| 4 | `export_data_for_R.do`, `export_km_data.do`, then the R scripts: balance and descriptive figures, forest plots, Kaplan-Meier curves, interval coefficient plot, leave-one-counselor-out plot, and the topic-coverage heatmap |

Randomization inference was tested and dropped: with six counselors, the permuted instrument is weak, so the wild-cluster bootstrap is used as the small-cluster complement.

## Software

StataNow 19.5 (MP) with `estout`, `coefplot`, `ivreg2`, `boottest`, and `winsor2`. R 4.3.3 with `haven` 2.5.5, `dplyr` 1.1.4, `tidyr` 1.3.1, `ggplot2` 3.5.2, `patchwork` 1.3.0, `broom` 1.0.5, `scales` 1.4.0, `car` 3.1.3, and `readr` 2.1.5. Python 3 (standard library only).

## Data access

The data come from the Malawi Family Planning Study (MFPS), a randomized controlled trial in Lilongwe (PI: Mahesh V. Karra). They contain individual health records and cannot be redistributed. Researchers interested in access should contact the study team.

## Citation

Montás, M. C., Vincent, S., and Karra, M. V. (2026). *The Causal Effects of Counseling Quality: Experimental Evidence from Urban Malawi.* Working paper, Harvard T.H. Chan School of Public Health.

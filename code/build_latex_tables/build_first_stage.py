"""Build LaTeX first-stage table (4 columns)."""

# Hardcoded numbers from /tmp/firststage_table.txt (rerun on the maximum
# first-stage analytic sample, with no outcome-specific sample restriction).
cols = {
    1: dict(name='Unadj.\\ pooled',     b=0.342, se=0.011, p=0.000, klm=200.88, kpf=921.41, cdf=286.25, r2=0.283, n=727),
    2: dict(name='Adj.\\ pooled',        b=0.346, se=0.012, p=0.000, klm=201.59, kpf=869.47, cdf=290.54, r2=0.308, n=724),
    3: dict(name='Adj.\\ Postpartum',    b=0.338, se=0.018, p=0.000, klm=94.62,  kpf=359.06, cdf=132.80, r2=0.306, n=370),
    4: dict(name='Adj.\\ Pregnant',      b=0.365, se=0.017, p=0.000, klm=108.44, kpf=456.89, cdf=158.44, r2=0.342, n=354),
}

def stars(p):
    if p < 0.01: return '$^{***}$'
    if p < 0.05: return '$^{**}$'
    if p < 0.10: return '$^{*}$'
    return ''

out = []
out.append(r"\begin{table}[h!]")
out.append(r"\centering")
out.append(r"\caption{First-Stage Estimates: CQT Instrument on High-Quality Counseling}")
out.append(r"\label{tab:first_stage}")
out.append(r"\small")
out.append(r"\begin{tabular}{lcccc}")
out.append(r"\toprule")
out.append(r" & (1) & (2) & (3) & (4) \\")
header = " & ".join([cols[c]['name'] for c in [1,2,3,4]])
out.append(f" & {header} \\\\")
out.append(r"\midrule")

# Row: CQT coefficient
brow = ["CQT (sat+topics)"]
serow = ["\\quad Robust SE"]
for c in [1,2,3,4]:
    brow.append(f"{cols[c]['b']:.3f}{stars(cols[c]['p'])}")
    serow.append(f"({cols[c]['se']:.3f})")
out.append(" & ".join(brow) + r" \\")
out.append(" & ".join(serow) + r" \\")
out.append(r"\addlinespace[3pt]")

# Diagnostics
diags = [
    ('Kleibergen-Paap rk LM $\\chi^2$', 'klm', '{:.2f}'),
    ('Kleibergen-Paap rk Wald $F$',     'kpf', '{:.2f}'),
    ('Cragg-Donald $F$',                 'cdf', '{:.2f}'),
]
for label, key, fmt in diags:
    row = [label]
    for c in [1,2,3,4]:
        row.append(fmt.format(cols[c][key]))
    out.append(" & ".join(row) + r" \\")

out.append(r"\addlinespace[3pt]")
# R²
r2row = ["$R^2$"]
nrow = ["$N$"]
for c in [1,2,3,4]:
    r2row.append(f"{cols[c]['r2']:.3f}")
    nrow.append(f"{cols[c]['n']:,}")
out.append(" & ".join(r2row) + r" \\")
out.append(" & ".join(nrow) + r" \\")

out.append(r"\bottomrule")
out.append(r"\end{tabular}")
out.append(r"\begin{minipage}{\linewidth}")
out.append(r"\footnotesize \textit{Notes:} First-stage OLS regression of the binary endogenous variable (an above-median indicator for the woman's average satisfaction-and-topics quality across all attended sessions) on the leave-one-out, scheduling-residualized Counselor Quality Tendency (CQT) of the woman's first counselor. Column (1) reports the unadjusted pooled regression; columns (2)--(4) include baseline covariates. Columns (3) and (4) restrict to postpartum and pregnant women respectively. Heteroskedasticity-robust standard errors in parentheses. The Stock-Yogo (2005) 10\\%\\ maximal-IV-size critical value is 16.38; first-stage $F$-statistics in every column far exceed this threshold. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
out.append(r"\end{minipage}")
out.append(r"\end{table}")

with open('tables/first_stage_4col.tex', 'w') as f:
    f.write('\n'.join(out) + '\n')
print("Wrote tables/first_stage_4col.tex")

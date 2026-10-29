"""Build the per-panel summary-index robustness table (Anderson 2008 / KLK 2007).

Reads:  /tmp/summary_index_results.csv  (from results_summary_index.do)
Writes: tables/iv_summary_index.tex
"""
import csv

with open('/tmp/summary_index_results.csv') as f:
    rows = {r['panel']: r for r in csv.DictReader(f)}

PANELS = [
    ('A', r'\makecell{Panel A \\ Continuity}'),
    ('B', r'\makecell{Panel B \\ Topics}'),
    ('C', r'\makecell{Panel C \\ FP coverage}'),
    ('D', r'\makecell{Panel D \\ Reproductive}'),
]

def stars(p):
    p = float(p)
    if p < 0.01: return '$^{***}$'
    if p < 0.05: return '$^{**}$'
    if p < 0.10: return '$^{*}$'
    return ''

def b(panel, bk, pk):
    r = rows[panel]
    return f"{float(r[bk]):.3f}{stars(r[pk])}"

def se(panel, sk):
    return f"({float(rows[panel][sk]):.3f})"

def p(panel, pk):
    return f"{float(rows[panel][pk]):.3f}"

def n(panel):
    return f"{int(float(rows[panel]['n'])):,}"

out = []
out.append(r"\begin{table}[H]")
out.append(r"\centering")
out.append(r"\caption{Per-Panel Summary-Index IV Estimates}")
out.append(r"\label{tab:iv_summary_index}")
out.append(r"\small")
out.append(r"\begin{tabular}{lcccc}")
out.append(r"\toprule")
out.append(" & " + " & ".join(lab for _, lab in PANELS) + r" \\")
out.append(r"\midrule")

def block(label, bk, sk, pk):
    out.append(f"\\textit{{{label}}} & & & & \\\\")
    out.append(r"\quad Estimate & " + " & ".join(b(P, bk, pk) for P, _ in PANELS) + r" \\")
    out.append(r"\quad & " + " & ".join(se(P, sk) for P, _ in PANELS) + r" \\")

block("Pooled", 'pool_b', 'pool_se', 'pool_p')
out.append(r"\addlinespace[2pt]")
block("Postpartum (preg=0)", 'pp_b', 'pp_se', 'pp_p')
out.append(r"\addlinespace[2pt]")
block("Pregnant (preg=1)", 'pr_b', 'pr_se', 'pr_p')
out.append(r"\quad p(PP=Preg) & " + " & ".join(p(P, 'pdiff') for P, _ in PANELS) + r" \\")
out.append(r"\midrule")
out.append(r"N & " + " & ".join(n(P) for P, _ in PANELS) + r" \\")
out.append(r"\bottomrule")
out.append(r"\end{tabular}")
out.append(r"\begin{minipage}{\linewidth}")
out.append(
    r"\footnotesize \textit{Notes:} Each column reports 2SLS estimates of higher counseling "
    r"quality on a single summary index for the panel, constructed as the equal-weighted mean of "
    r"the standardized (mean~0, SD~1) outcomes in that panel, following \citet{kling2007experimental} "
    r"and \citet{anderson2008multiple}. Indices are in standard-deviation units. Aggregating each "
    r"panel into one index controls the family-wise error rate across the nineteen outcomes and gains "
    r"power by averaging out idiosyncratic noise. Endogenous variable, instrument, covariates, and "
    r"samples are identical to Tables~\ref{tab:iv_mechanism} and~\ref{tab:iv_outcomes} (Panels~A and~B at the woman level; Panels~C "
    r"and~D at endline). Within Panel~C the four FP-service outcomes are strongly positively "
    r"correlated ($r = 0.55$ to $0.93$), while husband's awareness of FP use is near-orthogonal; "
    r"Panel~A bundles less-coherent continuity components that are aligned by the direction of the "
    r"treatment effect rather than by strong mutual correlation. Heteroskedasticity-robust standard "
    r"errors in parentheses. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01."
)
out.append(r"\end{minipage}")
out.append(r"\end{table}")

with open('tables/iv_summary_index.tex', 'w') as f:
    f.write('\n'.join(out) + '\n')
print("Wrote tables/iv_summary_index.tex")

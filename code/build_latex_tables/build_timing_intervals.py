"""Build the interval-hazard IV LaTeX table (Table 4 in the paper).

Outcome columns = non-overlapping pregnancy-timing intervals (0-6, 6-12, 12-18,
18-24 months from index birth). Stacked panels = Pooled / PP / Preg estimates,
plus a p(PP=Preg) row and three Mean DV rows.

Reads:
  /tmp/timing_intervals_results.csv         (IV results from results_main_iv.do)
  /tmp/subgroup_means_intervals.csv         (PP and Preg subgroup means)

Writes:
  tables/iv_timing_intervals.tex
"""
import csv

# Load IV results
with open('/tmp/timing_intervals_results.csv') as f:
    iv_rows = list(csv.DictReader(f))
for r in iv_rows:
    for k in ['pool_b','pool_se','pool_p','pp_b','pp_se','pp_p','pr_b','pr_se','pr_p','pdiff','n','mn']:
        try: r[k] = float(r[k])
        except: r[k] = None
iv = {r['outcome']: r for r in iv_rows}

# Load subgroup means
sub_means = {}
with open('/tmp/subgroup_means_intervals.csv') as f:
    for r in csv.DictReader(f):
        sub_means[r['outcome']] = {
            'mn_pool': float(r['mn_pool']),
            'mn_pp':   float(r['mn_pp']),
            'mn_preg': float(r['mn_preg']),
        }

OUTCOMES = [
    ('preg_0_6mo',   r'\makecell{0--6 \\ months}'),
    ('preg_6_12mo',  r'\makecell{6--12 \\ months}'),
    ('preg_12_18mo', r'\makecell{12--18 \\ months}'),
    ('preg_18_24mo', r'\makecell{18--24 \\ months}'),
]
N_COLS = len(OUTCOMES)

def stars(p):
    if p is None: return ''
    if p < 0.01: return '$^{***}$'
    if p < 0.05: return '$^{**}$'
    if p < 0.10: return '$^{*}$'
    return ''

def fmt_b(b, p, dec=3):
    return f"{b:.{dec}f}{stars(p)}" if b is not None else ''

def fmt_se(se, dec=3):
    return f"({se:.{dec}f})" if se is not None else ''

def fmt_p(p, dec=3):
    return f"{p:.{dec}f}" if p is not None else ''

def fmt_n(n):
    return f"{int(n):,}" if n is not None else ''

def fmt_mn(m, dec=3):
    return f"{m:.{dec}f}" if m is not None else ''

out = []
out.append(r"\begin{table}[H]")
out.append(r"\centering")
out.append(r"\caption{Interval-Hazard IV: Effect of Higher Counseling Quality on Pregnancy Occurrence by Months since Index Birth}")
out.append(r"\label{tab:iv_timing_intervals}")
out.append(r"\small")
out.append(r"\setlength{\tabcolsep}{6pt}")
out.append(r"\renewcommand{\arraystretch}{1.05}")
out.append(r"\begin{tabular}{l" + "c"*N_COLS + "}")
out.append(r"\toprule")
out.append(r" & \multicolumn{" + str(N_COLS) + r"}{c}{\emph{Months since index birth (non-overlapping intervals)}} \\")
out.append(r"\cmidrule(lr){2-" + str(N_COLS+1) + "}")
out.append(" & " + " & ".join(lab for _, lab in OUTCOMES) + r" \\")
out.append(r"\midrule")

def block(label, b_key, se_key, p_key):
    out.append(f"\\multicolumn{{{N_COLS+1}}}{{l}}{{\\textit{{{label}}}}} \\\\")
    row_b  = ["\\quad Estimate"]
    row_se = ["\\quad"]
    for outc, _ in OUTCOMES:
        r = iv.get(outc)
        if r is None:
            row_b.append('');  row_se.append('')
        else:
            row_b.append(fmt_b(r[b_key], r[p_key]))
            row_se.append(fmt_se(r[se_key]))
    out.append(" & ".join(row_b) + r" \\")
    out.append(" & ".join(row_se) + r" \\")

block("Pooled", 'pool_b', 'pool_se', 'pool_p')
out.append(r"\addlinespace[3pt]")
block("Postpartum (preg=0)", 'pp_b', 'pp_se', 'pp_p')
out.append(r"\addlinespace[3pt]")
block("Pregnant (preg=1)", 'pr_b', 'pr_se', 'pr_p')

# p(PP=Preg)
row_diff = [r"\quad p(PP=Preg)"]
for outc, _ in OUTCOMES:
    r = iv.get(outc)
    row_diff.append(fmt_p(r['pdiff']) if r else '')
out.append(" & ".join(row_diff) + r" \\")
out.append(r"\midrule")

# N
row_n = ["N"]
for outc, _ in OUTCOMES:
    r = iv.get(outc)
    row_n.append(fmt_n(r['n']) if r else '')
out.append(" & ".join(row_n) + r" \\")

# Mean DV (pooled, PP, Preg)
row_mn_pool = ["Mean DV (pooled)"]
row_mn_pp   = ["Mean DV (PP)"]
row_mn_pr   = ["Mean DV (Preg)"]
for outc, _ in OUTCOMES:
    m = sub_means.get(outc, {})
    row_mn_pool.append(fmt_mn(m.get('mn_pool')))
    row_mn_pp.append(fmt_mn(m.get('mn_pp')))
    row_mn_pr.append(fmt_mn(m.get('mn_preg')))
out.append(" & ".join(row_mn_pool) + r" \\")
out.append(" & ".join(row_mn_pp) + r" \\")
out.append(" & ".join(row_mn_pr) + r" \\")

out.append(r"\bottomrule")
out.append(r"\end{tabular}")
out.append(r"\begin{minipage}{\linewidth}")
out.append(
    r"\footnotesize \textit{Notes:} Two-stage least squares estimates of higher counseling quality "
    r"on the probability that a woman's next pregnancy event occurred within each non-overlapping "
    r"interval from her index birth. Endogenous variable: an above-median indicator for the woman's "
    r"average satisfaction-and-topics quality across all attended sessions. Instrument: the "
    r"leave-one-out, scheduling-residualized Counselor Quality Tendency (CQT) of the woman's first "
    r"counselor. The four intervals partition the 24-month follow-up window and together sum to the "
    r"pregnancy-within-24-months outcome reported in Table~\ref{tab:iv_outcomes}. Each pregnancy "
    r"outcome counts any subsequent pregnancy regardless of whether it ended in a live birth, "
    r"miscarriage, abortion, or stillbirth, matching the Panel D at-risk population. "
    r"Heteroskedasticity-robust standard errors in parentheses. The row "
    r"\emph{p(PP=Preg)} reports the p-value for testing equality of the two subgroup effects. "
    r"Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01."
)
out.append(r"\end{minipage}")
out.append(r"\end{table}")

with open('tables/iv_timing_intervals.tex', 'w') as f:
    f.write('\n'.join(out) + '\n')
print("Wrote tables/iv_timing_intervals.tex")

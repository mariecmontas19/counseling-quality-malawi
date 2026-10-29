"""Build the autonomy-spillover LaTeX table for Section 5.4.X.

Reads:  /tmp/autonomy_iv_results.csv  (built by autonomy_flip_test.do)
Writes: tables/iv_autonomy_spillover.tex

Three outcomes with significant PP-Preg heterogeneity:
  - husband_knows_fp        (anchor)
  - w8_w823_binary          (healthcare decisions)
  - w8_w824_binary          (major HH purchases)
"""
import csv

OUTCOMES = [
    ('husband_knows_fp',   r'\makecell{Husband knows FP \\ use (binary)}'),
    ('w8_w823_binary',     r'\makecell{Healthcare decisions \\ (woman involved)}'),
    ('w8_w824_binary',     r'\makecell{Major HH purchases \\ (woman involved)}'),
]

def stars(p):
    try:
        p = float(p)
    except (ValueError, TypeError):
        return ''
    if p < 0.01: return r'$^{***}$'
    if p < 0.05: return r'$^{**}$'
    if p < 0.10: return r'$^{*}$'
    return ''

def fmt(b, p, dec=3):
    try:
        x = float(b)
        return f'{x:.{dec}f}{stars(p)}'
    except (ValueError, TypeError):
        return ''

def fmt_se(se, dec=3):
    try:
        x = float(se)
        return f'({x:.{dec}f})'
    except (ValueError, TypeError):
        return ''

# Load IV results
results = {}
with open('/tmp/autonomy_iv_results.csv') as f:
    r = csv.DictReader(f)
    for row in r:
        results[row['outcome']] = row

# Build table
out = []
out.append(r'\begin{table}[H]')
out.append(r'\centering')
out.append(r'\caption{Spillover Effects on Autonomy: IV Estimates by Pregnancy Status}')
out.append(r'\label{tab:iv_autonomy}')
out.append(r'\small')
out.append(r'\setlength{\tabcolsep}{6pt}')
out.append(r'\renewcommand{\arraystretch}{1.10}')
out.append(r'\begin{tabular}{lccc}')
out.append(r'\toprule')
out.append(r' & ' + ' & '.join([o[1] for o in OUTCOMES]) + r' \\')
out.append(r'\midrule')

# Pooled panel
out.append(r'\multicolumn{4}{l}{\textit{Panel: Pooled}} \\')
b_cells = [r'\quad Estimate']
se_cells = [r'\quad ']
for key, _ in OUTCOMES:
    r_ = results.get(key, {})
    b_cells.append(fmt(r_.get('pool_b'), r_.get('pool_p')))
    se_cells.append(fmt_se(r_.get('pool_se')))
out.append(' & '.join(b_cells) + r' \\')
out.append(' & '.join(se_cells) + r' \\')

# PP panel
out.append(r'\addlinespace[3pt]')
out.append(r'\multicolumn{4}{l}{\textit{Panel: Postpartum at baseline}} \\')
b_cells = [r'\quad Estimate']
se_cells = [r'\quad ']
for key, _ in OUTCOMES:
    r_ = results.get(key, {})
    b_cells.append(fmt(r_.get('pp_b'), r_.get('pp_p')))
    se_cells.append(fmt_se(r_.get('pp_se')))
out.append(' & '.join(b_cells) + r' \\')
out.append(' & '.join(se_cells) + r' \\')

# Preg panel
out.append(r'\addlinespace[3pt]')
out.append(r'\multicolumn{4}{l}{\textit{Panel: Pregnant at baseline}} \\')
b_cells = [r'\quad Estimate']
se_cells = [r'\quad ']
for key, _ in OUTCOMES:
    r_ = results.get(key, {})
    b_cells.append(fmt(r_.get('pr_b'), r_.get('pr_p')))
    se_cells.append(fmt_se(r_.get('pr_se')))
out.append(' & '.join(b_cells) + r' \\')
out.append(' & '.join(se_cells) + r' \\')

# pdiff row
out.append(r'\addlinespace[3pt]')
diff_cells = [r'\quad $p$(PP=Preg)']
for key, _ in OUTCOMES:
    r_ = results.get(key, {})
    try:
        p = float(r_.get('pdiff', ''))
        diff_cells.append(f'{p:.3f}')
    except (ValueError, TypeError):
        diff_cells.append('')
out.append(' & '.join(diff_cells) + r' \\')

# N and means
out.append(r'\midrule')
n_cells = ['$N$']
mn_cells = ['Mean DV (pooled)']
for key, _ in OUTCOMES:
    r_ = results.get(key, {})
    try:
        n = int(float(r_.get('n', 0)))
        n_cells.append(str(n))
    except (ValueError, TypeError):
        n_cells.append('')
    try:
        mn = float(r_.get('mn', 0))
        mn_cells.append(f'{mn:.3f}')
    except (ValueError, TypeError):
        mn_cells.append('')
out.append(' & '.join(n_cells) + r' \\')
out.append(' & '.join(mn_cells) + r' \\')

out.append(r'\bottomrule')
out.append(r'\end{tabular}')
out.append(r'\begin{minipage}{0.92\linewidth}')
out.append(r"\footnotesize \textit{Notes:} Two-stage least squares estimates of higher counseling quality on three autonomy-domain outcomes measured at endline (year 2018). Endogenous variable: an above-median indicator for the woman's average satisfaction-and-topics quality across all attended sessions. Instrument: the leave-one-out, scheduling-residualized Counselor Quality Tendency (CQT) of the woman's first counselor. Heteroskedasticity-robust standard errors in parentheses. The row $p$(PP=Preg) reports the p-value for testing equality of the postpartum and pregnant subgroup coefficients. \emph{Husband knows FP use}: whether the husband is aware the woman uses family planning. \emph{Healthcare decisions} and \emph{Major HH purchases}: binary indicators (1 = woman is involved in the decision either alone or jointly with husband, 0 = woman not involved), constructed from MFPS Wave 1 Section 8 questions Q823 and Q824. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
out.append(r'\end{minipage}')
out.append(r'\end{table}')

# Write to Analysis/tables/
with open('tables/iv_autonomy_spillover.tex', 'w') as f:
    f.write('\n'.join(out) + '\n')
print('Wrote tables/iv_autonomy_spillover.tex')

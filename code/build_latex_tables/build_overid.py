"""Build the new identification-section tables (July 2026, per David Canning):
  1. tables/overid_rf.tex  -- reduced-form ITT: joint F of the 5 counselor dummies
  2. tables/overid_iv.tex  -- 2SLS: continuous first-session q instrumented by the
                              counselor dummies, with the Hansen J test
  3. tables/overid_ji.tex  -- just-identified estimates, one counselor contrast at
                              a time (Panels C and D)
Reads /tmp/overid_rf.csv, /tmp/overid_iv.csv, /tmp/overid_ji.csv (from
dofile/results_overid.do). Writes both the Analysis/tables and the
Main paper/CQ_manuscript/tables copies.
"""
import csv
import os

MANUSCRIPT_TABLES = os.path.join('..', 'Main paper', 'CQ_manuscript', 'tables')

def read_csv(path):
    with open(path) as f:
        return list(csv.DictReader(f))

rf_rows = read_csv('/tmp/overid_rf.csv')
iv_rows = read_csv('/tmp/overid_iv.csv')
ji_rows = read_csv('/tmp/overid_ji.csv')

def num(x):
    try:
        return float(x)
    except (TypeError, ValueError):
        return None

for r in rf_rows + iv_rows + ji_rows:
    for k in r:
        if k not in ('outcome', 'panel'):
            r[k] = num(r[k])

rf = {r['outcome']: r for r in rf_rows}
iv = {r['outcome']: r for r in iv_rows}
ji = {}
for r in ji_rows:
    ji.setdefault(r['outcome'], {})[int(r['k'])] = r

PANELS = [
    ('C', 'FP coverage', [
        ('admin_fp_any',         'Any FP service'),
        ('admin_inject_any',     'Any injectable'),
        ('n_admin_fp',           'Number of FP services'),
        ('fp_inj_coverage_frac', 'Injectable coverage fraction'),
        ('husband_knows_fp',     'Husband knows FP use'),
    ]),
    ('D', 'Reproductive', [
        ('birth_within_24mo',    'Birth within 24 months'),
        ('preg_within_12mo',     'Pregnancy within 12 months'),
        ('preg_within_24mo',     'Pregnancy within 24 months'),
        ('mistimed_preg_expost', 'Mistimed pregnancy'),
    ]),
    ('A', 'Counseling continuity', [
        ('total_sessions',       'Total sessions'),
        ('share_same_counselor', 'Share same counselor'),
        ('visit_rate_mo',        'Sessions per month'),
        ('husband_present',      'Husband present'),
    ]),
    ('B', 'Counseling topics (post-1st-session)', [
        ('etp1_fp',              'FP information'),
        ('etp1_spacing',         'Birth spacing'),
        ('etp1_preg',            'Pregnancy info'),
        ('etp1_partner',         'Partner engagement'),
        ('etp1_childhealth',     'Child health'),
        ('etp1_breast',          'Breastfeeding'),
    ]),
]

def stars(p):
    if p is None:
        return ''
    if p < 0.01:
        return '$^{***}$'
    if p < 0.05:
        return '$^{**}$'
    if p < 0.10:
        return '$^{*}$'
    return ''

def f3(x):
    return f"{x:.3f}" if x is not None else ''

def f2(x):
    return f"{x:.2f}" if x is not None else ''

def fN(x):
    return f"{int(x):,}" if x is not None else ''

def write_both(fname, content):
    for d in ('tables', MANUSCRIPT_TABLES):
        path = os.path.join(d, fname)
        with open(path, 'w') as f:
            f.write(content)
        print(f"Wrote {path}")

# ---------------------------------------------------------------------------
# Table 1: reduced-form ITT joint F
# ---------------------------------------------------------------------------
out = []
out.append(r"\begin{table}[H]")
out.append(r"\centering")
out.append(r"\caption{Reduced-Form Counselor Effects: Joint Significance of Counselor Assignment (ITT)}")
out.append(r"\label{tab:overid_rf}")
out.append(r"\small")
out.append(r"\setlength{\tabcolsep}{8pt}")
out.append(r"\renewcommand{\arraystretch}{1.02}")
out.append(r"\begin{tabular}{lcccc}")
out.append(r"\toprule")
out.append(r"Outcome & Mean DV & Joint $F$ & $p$ & $N$ \\")
out.append(r"\midrule")
for pk, pname, outs in PANELS:
    out.append(f"\\multicolumn{{5}}{{l}}{{\\emph{{Panel {pk}: {pname}}}}} \\\\")
    for var, lab in outs:
        r = rf[var]
        out.append(f"\\quad {lab} & {f3(r['mn'])} & {f2(r['rf_F'])}{stars(r['rf_p'])} & {f3(r['rf_p'])} & {fN(r['n'])} \\\\")
    out.append(r"\addlinespace[3pt]")
out.append(r"\bottomrule")
out.append(r"\end{tabular}")
out.append(r"\begin{minipage}{0.86\linewidth}")
out.append(r"\smallskip")
out.append(r"\footnotesize \textit{Notes:} Each row reports a separate intention-to-treat regression of the outcome on five first-assigned-counselor indicators (counselor 1 omitted) and the full baseline covariate vector $X_i$, with heteroskedasticity-robust standard errors. \emph{Joint $F$} tests the joint null that all five counselor indicators are zero. Panels~C and~D are estimated on the endline sample; Panels~A and~B at the woman level (one observation per woman). Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
out.append(r"\end{minipage}")
out.append(r"\end{table}")
write_both('overid_rf.tex', '\n'.join(out) + '\n')

# ---------------------------------------------------------------------------
# Table 2: overidentified 2SLS with Hansen J
# ---------------------------------------------------------------------------
out = []
out.append(r"\begin{table}[H]")
out.append(r"\centering")
out.append(r"\caption{2SLS Estimates with Counselor Indicators as Instruments and Overidentification Tests}")
out.append(r"\label{tab:overid_iv}")
out.append(r"\small")
out.append(r"\setlength{\tabcolsep}{7pt}")
out.append(r"\renewcommand{\arraystretch}{1.02}")
out.append(r"\begin{tabular}{lcccccc}")
out.append(r"\toprule")
out.append(r" & \multicolumn{2}{c}{2SLS on first-session $q$} & & \multicolumn{2}{c}{Hansen $J$} & \\")
out.append(r"\cmidrule(lr){2-3} \cmidrule(lr){5-6}")
out.append(r"Outcome & Coef. & (SE) & KP $F$ & $J$ & $p_J$ & $N$ \\")
out.append(r"\midrule")
for pk, pname, outs in PANELS:
    out.append(f"\\multicolumn{{7}}{{l}}{{\\emph{{Panel {pk}: {pname}}}}} \\\\")
    for var, lab in outs:
        r = iv[var]
        out.append(
            f"\\quad {lab} & {f3(r['b'])}{stars(r['p'])} & ({f3(r['se'])}) & "
            f"{r['kpF']:.0f} & {f2(r['jstat'])} & {f3(r['jp'])} & {fN(r['n'])} \\\\"
        )
    out.append(r"\addlinespace[3pt]")
out.append(r"\bottomrule")
out.append(r"\end{tabular}")
out.append(r"\begin{minipage}{0.9\linewidth}")
out.append(r"\smallskip")
out.append(r"\footnotesize \textit{Notes:} Each row reports a separate two-stage least squares regression instrumenting the continuous first-session quality index $q$ (satisfaction plus engaged topics; SD $\approx 0.87$) with the five first-assigned-counselor indicators, controlling for the baseline covariate vector $X_i$, with heteroskedasticity-robust standard errors. \emph{KP $F$} is the Kleibergen--Paap rk Wald first-stage statistic. \emph{Hansen $J$} tests the four overidentifying restrictions; under the null, counselor assignment affects the outcome only through measured first-session quality. With six counselors, a rejection cannot distinguish an exclusion-restriction violation from counselor-specific reporting of $q$, and the test has limited power. Panels~C and~D are estimated on the endline sample; Panels~A and~B at the woman level. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
out.append(r"\end{minipage}")
out.append(r"\end{table}")
write_both('overid_iv.tex', '\n'.join(out) + '\n')

# ---------------------------------------------------------------------------
# Table 3: just-identified, one counselor contrast at a time (Panels C and D)
# ---------------------------------------------------------------------------
out = []
out.append(r"\begin{table}[H]")
out.append(r"\centering")
out.append(r"\caption{Just-Identified 2SLS Estimates, One Counselor Contrast at a Time (Panels C and D)}")
out.append(r"\label{tab:overid_ji}")
out.append(r"\small")
out.append(r"\setlength{\tabcolsep}{5pt}")
out.append(r"\renewcommand{\arraystretch}{1.0}")
out.append(r"\begin{tabular}{lcccccc}")
out.append(r"\toprule")
out.append(r" & \multicolumn{6}{c}{Instrument: indicator for first-assigned counselor $k$ vs.\ all others} \\")
out.append(r"\cmidrule(lr){2-7}")
out.append(r"Outcome & C1 & C2 & C3 & C4 & C5 & C6 \\")
out.append(r"\midrule")
for pk, pname, outs in PANELS[:2]:  # C and D only
    out.append(f"\\multicolumn{{7}}{{l}}{{\\emph{{Panel {pk}: {pname}}}}} \\\\")
    for var, lab in outs:
        row_b = [f"\\quad {lab}"]
        row_se = ["\\quad"]
        for k in range(1, 7):
            r = ji[var][k]
            row_b.append(f"{f3(r['b'])}{stars(r['p'])}")
            row_se.append(f"({f3(r['se'])})")
        out.append(" & ".join(row_b) + r" \\")
        out.append(" & ".join(row_se) + r" \\")
    out.append(r"\addlinespace[3pt]")

# first-stage F row (endline sample; take from a Panel C outcome)
fs_row = [r"First-stage $F$ (endline)"]
for k in range(1, 7):
    fs_row.append(f"{ji['admin_fp_any'][k]['fsF']:.0f}")
out.append(r"\midrule")
out.append(" & ".join(fs_row) + r" \\")
out.append(r"\bottomrule")
out.append(r"\end{tabular}")
out.append(r"\begin{minipage}{0.9\linewidth}")
out.append(r"\smallskip")
out.append(r"\footnotesize \textit{Notes:} Each cell reports a separate just-identified two-stage least squares regression instrumenting the continuous first-session quality index $q$ with a single indicator for being first assigned to counselor $k$ (versus any other counselor), controlling for the baseline covariate vector $X_i$, with heteroskedasticity-robust standard errors in parentheses. Under the exclusion restriction that counselor assignment affects outcomes only through measured $q$, all six contrasts estimate the same parameter; the Hansen $J$ statistics in Table~\ref{tab:overid_iv} formalize this comparison. The bottom row reports the first-stage $F$ for each single-indicator instrument on the endline sample; the counselor-2 contrast is weakly identified ($F \approx 12$) and its estimates should be discounted accordingly. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
out.append(r"\end{minipage}")
out.append(r"\end{table}")
write_both('overid_ji.tex', '\n'.join(out) + '\n')

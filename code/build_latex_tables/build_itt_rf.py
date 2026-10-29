"""Build the ITT / reduced-form tables (August 2026 restructure):
  tables/itt_rf.tex      -- main text: RF of each outcome on the worst-to-best-
                            scaled CQT (pooled/PP/Preg) + joint F of the five
                            counselor dummies (robust inference)
  tables/itt_rf_wcb.tex  -- appendix twin: same layout, wild-cluster-bootstrap
                            p-values (Webb, 9,999 reps, 6 counselor clusters)
                            for the CQT coefficients; joint F stays conventional
Reads /tmp/itt_rf.csv (from dofile/results_itt_rf.do). Writes both the
Analysis/tables and the Main paper/CQ_manuscript/tables copies.
"""
import csv
import os

MANUSCRIPT_TABLES = os.path.join('..', 'Main paper', 'CQ_manuscript', 'tables')

rows = list(csv.DictReader(open('/tmp/itt_rf.csv')))
for r in rows:
    for k in r:
        if k not in ('outcome', 'panel'):
            try:
                r[k] = float(r[k])
            except ValueError:
                r[k] = None
D = {r['outcome']: r for r in rows}

PANELS = [
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

def build(wcb):
    """wcb=False -> main-text table (robust); wcb=True -> appendix WCB twin."""
    label = 'tab:itt_rf_wcb' if wcb else 'tab:itt_rf'
    caption = ('Reduced-Form (ITT) Effects of Counselor Assignment: '
               'Wild-Cluster Bootstrap Inference'
               if wcb else
               'Reduced-Form (ITT) Effects of Counselor Assignment on All Outcomes')
    out = []
    out.append(r"\begin{table}[H]")
    out.append(r"\centering")
    out.append(r"\caption{" + caption + "}")
    out.append(r"\label{" + label + "}")
    out.append(r"\small")
    out.append(r"\setlength{\tabcolsep}{5pt}")
    out.append(r"\renewcommand{\arraystretch}{1.0}")
    out.append(r"\resizebox{\textwidth}{!}{%")
    out.append(r"\begin{tabular}{lcccccc}")
    out.append(r"\toprule")
    out.append(r" & \multicolumn{3}{c}{RF: worst-to-best counselor (CQT)} & & "
               r"\multicolumn{1}{c}{Counselor dummies} & \\")
    out.append(r"\cmidrule(lr){2-4} \cmidrule(lr){6-6}")
    out.append(r"Outcome & Pooled & Postpartum & Pregnant & $p_{\text{PP}=\text{Preg}}$ & Joint $F$ ($p$) & $N$ \\")
    out.append(r"\midrule")
    for pk, pname, outs in PANELS:
        out.append(f"\\multicolumn{{7}}{{l}}{{\\emph{{Panel {pk}: {pname}}}}} \\\\")
        for var, lab in outs:
            r = D[var]
            if wcb:
                pb_p, pp_p, pr_p, pd = r['pool_pwcb'], r['pp_pwcb'], r['pr_pwcb'], r['pdiff_wcb']
                sub = lambda key_p: f"[{f3(key_p)}]"
                row2 = [f"\\quad", sub(pb_p), sub(pp_p), sub(pr_p), '', '', '']
            else:
                pb_p, pp_p, pr_p, pd = r['pool_p'], r['pp_p'], r['pr_p'], r['pdiff']
                row2 = [f"\\quad", f"({f3(r['pool_se'])})", f"({f3(r['pp_se'])})",
                        f"({f3(r['pr_se'])})", '', '', '']
            row1 = [f"\\quad {lab}",
                    f"{f3(r['pool_b'])}{stars(pb_p)}",
                    f"{f3(r['pp_b'])}{stars(pp_p)}",
                    f"{f3(r['pr_b'])}{stars(pr_p)}",
                    f3(pd),
                    f"{f2(r['dumF'])} ({f3(r['dumFp'])})",
                    fN(r['n'])]
            out.append(" & ".join(row1) + r" \\")
            out.append(" & ".join(row2) + r" \\")
        out.append(r"\addlinespace[3pt]")
    out.append(r"\bottomrule")
    out.append(r"\end{tabular}")
    out.append(r"}")
    out.append(r"\begin{minipage}{0.94\linewidth}")
    out.append(r"\smallskip")
    if wcb:
        note = (r"\footnotesize \textit{Notes:} Same reduced-form (intention-to-treat) regressions as "
                r"Table~\ref{tab:itt_rf}, with inference by wild-cluster bootstrap "
                r"(\texttt{boottest}, Webb six-point weights, 9,999 replications, clustering on the six "
                r"counselors); bootstrap $p$-values in brackets below each coefficient, and stars based on "
                r"them. The counselor-dummy joint $F$ cannot be wild-cluster bootstrapped because the "
                r"dummies coincide with the cluster identities; the reported $F$ ($p$) uses "
                r"heteroskedasticity-robust inference as in Table~\ref{tab:itt_rf}. With six clusters the "
                r"bootstrap is conservative, particularly for the session-record outcomes in Panels~A "
                r"and~B. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
    else:
        note = (r"\footnotesize \textit{Notes:} Each row reports intention-to-treat regressions with "
                r"heteroskedasticity-robust standard errors (in parentheses) and the full baseline "
                r"covariate vector $X_i$. The first three columns report the reduced form of the outcome "
                r"on the leave-one-out Counselor Quality Tendency (CQT), scaled by the observed "
                r"counselor-level CQT range so that each coefficient is the effect of moving from the "
                r"lowest- to the highest-quality-tendency counselor; pooled and subgroup coefficients come "
                r"from the pooled and fully interacted specifications, and $p_{\text{PP}=\text{Preg}}$ "
                r"tests equality of the subgroup effects. \emph{Joint $F$} tests the joint significance "
                r"of the five first-assigned-counselor indicators in a regression of the outcome on "
                r"counselor dummies and $X_i$. Panels~A and~B are estimated at the woman level (one "
                r"observation per woman); Panels~C and~D on the endline sample. Appendix "
                r"Table~\ref{tab:itt_rf_wcb} repeats this table with wild-cluster bootstrap inference. "
                r"Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
    out.append(note)
    out.append(r"\end{minipage}")
    out.append(r"\end{table}")
    fname = 'itt_rf_wcb.tex' if wcb else 'itt_rf.tex'
    write_both(fname, '\n'.join(out) + '\n')

build(wcb=False)
build(wcb=True)

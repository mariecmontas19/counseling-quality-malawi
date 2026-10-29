"""Build the continuous first-session-q appendix IV table (August 2026):
tables/iv_contq.tex -- the main IV panels re-estimated with the continuous
first-session quality index as the endogenous variable (CQT instrument,
same samples/covariates as the main table). Stacked layout: Panels A+B on
top, C+D below, matching iv_main_combined.tex.
Reads /tmp/iv_contq.csv (from dofile/results_iv_contq.do). Writes both the
Analysis/tables and Main paper/CQ_manuscript/tables copies.
"""
import csv
import os

MANUSCRIPT_TABLES = os.path.join('..', 'Main paper', 'CQ_manuscript', 'tables')

rows = list(csv.DictReader(open('/tmp/iv_contq.csv')))
for r in rows:
    for k in r:
        if k not in ('outcome', 'panel'):
            try:
                r[k] = float(r[k])
            except ValueError:
                r[k] = None
D = {r['outcome']: r for r in rows}

PANELS = {
    'A': {'name': 'Counseling continuity',
          'outcomes': ['total_sessions', 'share_same_counselor', 'visit_rate_mo', 'husband_present'],
          'labels': [r'\makecell{Total \\ sessions}', r'\makecell{Same \\ counselor}',
                     r'\makecell{Couns sessions \\ per month}', r'\makecell{Husband \\ present}']},
    'B': {'name': 'Counseling topics (post-1st-session)',
          'outcomes': ['etp1_fp', 'etp1_spacing', 'etp1_preg', 'etp1_partner', 'etp1_childhealth', 'etp1_breast'],
          'labels': [r'\makecell{FP \\ information}', r'\makecell{Birth \\ spacing}',
                     r'\makecell{Pregnancy \\ info}', r'\makecell{Partner \\ engagement}',
                     r'\makecell{Child \\ health}', r'Breastfeeding']},
    'C': {'name': 'FP coverage',
          'outcomes': ['admin_fp_any', 'admin_inject_any', 'n_admin_fp', 'fp_inj_coverage_frac', 'husband_knows_fp'],
          'labels': [r'\makecell{Any FP \\ service}', r'\makecell{Any \\ injectable}',
                     r'\makecell{Number of \\ FP services}', r'\makecell{Injectable \\ coverage}',
                     r'\makecell{Husband \\ knows FP}']},
    'D': {'name': 'Reproductive',
          'outcomes': ['birth_within_24mo', 'preg_within_12mo', 'preg_within_24mo', 'mistimed_preg_expost'],
          'labels': [r'\makecell{Birth within \\ 24 months}', r'\makecell{Pregnancy within \\ 12 months}',
                     r'\makecell{Pregnancy within \\ 24 months}', r'\makecell{Mistimed \\ pregnancy}']},
}

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

def fmt_b(b, p):
    return f"{b:.3f}{stars(p)}" if b is not None else ''

def fmt_se(se):
    return f"({se:.3f})" if se is not None else ''

def fmt_p(p):
    return f"{p:.3f}" if p is not None else ''

def fmt_n(n):
    return f"{int(n):,}" if n is not None else ''

def subtabular(panel_keys):
    all_outcomes, all_labels, breaks = [], [], []
    for pk in panel_keys:
        p = PANELS[pk]
        breaks.append((pk, len(p['outcomes']), p['name']))
        all_outcomes.extend(p['outcomes'])
        all_labels.extend(p['labels'])
    n_cols = len(all_outcomes)
    out = []
    out.append(r"\resizebox{\textwidth}{!}{%")
    out.append(r"\begin{tabular}{l" + "c" * n_cols + "}")
    out.append(r"\toprule")
    hdr = [f"\\multicolumn{{{n}}}{{c}}{{\\emph{{Panel {pk}: {nm}}}}}" for pk, n, nm in breaks]
    out.append(" & " + " & ".join(hdr) + r" \\")
    cmid, cum = [], 2
    for pk, n, _ in breaks:
        cmid.append(f"\\cmidrule(lr){{{cum}-{cum + n - 1}}}")
        cum += n
    out.append(" ".join(cmid))
    out.append(" & " + " & ".join(all_labels) + r" \\")
    out.append(r"\midrule")

    def block(label, bk, sek, pk_):
        out.append(f"\\multicolumn{{{n_cols + 1}}}{{l}}{{\\textit{{{label}}}}} \\\\")
        r1, r2 = ["\\quad Estimate"], ["\\quad"]
        for o in all_outcomes:
            r = D[o]
            r1.append(fmt_b(r[bk], r[pk_]))
            r2.append(fmt_se(r[sek]))
        out.append(" & ".join(r1) + r" \\")
        out.append(" & ".join(r2) + r" \\")

    block("Pooled", 'pool_b', 'pool_se', 'pool_p')
    out.append(r"\addlinespace[2pt]")
    block("Postpartum (preg=0)", 'pp_b', 'pp_se', 'pp_p')
    out.append(r"\addlinespace[2pt]")
    block("Pregnant (preg=1)", 'pr_b', 'pr_se', 'pr_p')
    rd = ["\\quad p(PP=Preg)"] + [fmt_p(D[o]['pdiff']) for o in all_outcomes]
    out.append(" & ".join(rd) + r" \\")
    out.append(r"\midrule")
    rn = ["N"] + [fmt_n(D[o]['n']) for o in all_outcomes]
    rm = ["Mean DV (pooled)"] + [f"{D[o]['mn']:.3f}" for o in all_outcomes]
    out.append(" & ".join(rn) + r" \\")
    out.append(" & ".join(rm) + r" \\")
    out.append(r"\bottomrule")
    out.append(r"\end{tabular}")
    out.append(r"}")
    return out

out = []
out.append(r"\begin{table}[H]")
out.append(r"\centering")
out.append(r"\caption{Robustness: IV Estimates with the Continuous First-Session Quality Index as the Endogenous Variable}")
out.append(r"\label{tab:iv_contq}")
out.append(r"\footnotesize")
out.append(r"\setlength{\tabcolsep}{4pt}")
out.append(r"\renewcommand{\arraystretch}{0.92}")
out.extend(subtabular(['A', 'B']))
out.append(r"")
out.append(r"\medskip")
out.append(r"")
out.extend(subtabular(['C', 'D']))
out.append(r"\begin{minipage}{\linewidth}")
out.append(r"\scriptsize \textit{Notes:} Two-stage least squares estimates replacing the binary "
           r"above-median endogenous variable of the main tables with the \emph{continuous} "
           r"first-session quality index (satisfaction plus engaged topics; SD $\approx 0.87$), "
           r"instrumented by the same leave-one-out, scheduling-residualized Counselor Quality "
           r"Tendency (CQT), with the same covariates and samples as the main specification. "
           r"Coefficients are per unit of the first-session index. Heteroskedasticity-robust "
           r"standard errors in parentheses. The row \emph{p(PP=Preg)} reports the p-value testing "
           r"equality of the postpartum and pregnant subgroup effects. Stars: $^{*}$ p$<$0.10, "
           r"$^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
out.append(r"\end{minipage}")
out.append(r"\end{table}")

content = '\n'.join(out) + '\n'
for d in ('tables', MANUSCRIPT_TABLES):
    path = os.path.join(d, 'iv_contq.tex')
    with open(path, 'w') as f:
        f.write(content)
    print(f"Wrote {path}")

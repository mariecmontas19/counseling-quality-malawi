"""Build leave-one-counselor-out robustness LaTeX table."""
import csv

with open('/tmp/phase7a_results.csv') as f:
    reader = csv.DictReader(f)
    rows = list(reader)
for r in rows:
    for k in ['b','se','p','n']:
        try: r[k] = float(r[k])
        except: r[k] = None

idx = {(r['outcome'], r['spec'], r['subgroup']): r for r in rows}

OUTCOMES = [
    # Panel A: Counseling continuity (4 outcomes)
    ('total_sessions',          'Total sessions'),
    ('share_same_counselor',    'Same counselor'),
    ('visit_rate_mo',           'Couns sessions per month'),
    ('husband_present',         'Husband present'),
    # Panel B: Counseling topics (6 outcomes, post-1st-session)
    ('etp1_fp',                 'Discussed FP info'),
    ('etp1_spacing',            'Discussed birth spacing'),
    ('etp1_preg',               'Discussed pregnancy'),
    ('etp1_partner',            'Discussed partner engagement'),
    ('etp1_childhealth',        'Discussed child health'),
    ('etp1_breast',             'Discussed breastfeeding'),
    # Panel C: FP coverage (4 outcomes)
    ('admin_fp_any',            'Any FP service'),
    ('admin_inject_any',        'Any injectable'),
    ('n_admin_fp',              'Number of FP services'),
    ('fp_inj_coverage_frac',    'Injectable coverage'),
    # Panel D: Reproductive (5 outcomes)
    ('birth_within_12mo',       'Birth within 12 months'),
    ('birth_within_24mo',       'Birth within 24 months'),
    ('preg_within_12mo',        'Pregnancy within 12 months'),
    ('preg_within_24mo',        'Pregnancy within 24 months'),
    ('mistimed_preg_expost',    'Mistimed pregnancy'),
]

SPECS = [('Main','Main'), ('dC1','Drop C1'), ('dC2','Drop C2'), ('dC3','Drop C3'),
         ('dC4','Drop C4'), ('dC5','Drop C5'), ('dC6','Drop C6')]

def stars(p):
    if p is None: return ''
    if p < 0.01: return '$^{***}$'
    if p < 0.05: return '$^{**}$'
    if p < 0.10: return '$^{*}$'
    return ''

def fmt_b(b, p, dec=3):
    if b is None: return ''
    return f"{b:.{dec}f}{stars(p)}"

def build_rows(subgroup):
    rows_out = []
    for outc, lab in OUTCOMES:
        cells = [lab]
        for spec_key, _ in SPECS:
            r = idx.get((outc, spec_key, subgroup))
            if r is None:
                cells.append('')
            else:
                cells.append(fmt_b(r['b'], r['p']))
        rows_out.append(" & ".join(cells) + r" \\")
    return rows_out

NOTES = (r"\footnotesize \textit{Notes:} Two-stage least squares estimates of higher counseling "
         r"quality on each outcome under the main specification (column 1) and after sequentially "
         r"dropping each of the six counselors (columns 2--7). Panels A and B (continuity and topics) "
         r"are estimated at the woman level (one observation per woman); Panels C and D (FP coverage "
         r"and reproductive) at endline. Endogenous variable: an above-median indicator for the "
         r"woman's average satisfaction-and-topics quality across all attended sessions. Instrument: "
         r"the leave-one-out, scheduling-residualized Counselor Quality Tendency (CQT) of the woman's "
         r"first counselor (recomputed within each leave-one-out subsample). Heteroskedasticity-robust "
         r"standard errors. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")

# Three separate tables (pooled / postpartum / pregnant), matching the manuscript's
# Appendix references tab:iv_robust_dropcouns_{pool,pp,preg}.
TABLES = [
    ('Pool', 'pool', 'Pooled Sample'),
    ('PP',   'pp',   'Postpartum Sample (preg=0)'),
    ('Preg', 'preg', 'Pregnant Sample (preg=1)'),
]

out = []
for sub_key, suffix, sub_caption in TABLES:
    out.append(r"\begin{table}[H]")
    out.append(r"\centering")
    out.append(r"\caption{Robustness: Leave-One-Counselor-Out IV Estimates --- " + sub_caption + r"}")
    out.append(r"\label{tab:iv_robust_dropcouns_" + suffix + r"}")
    out.append(r"\small")
    out.append(r"\setlength{\tabcolsep}{4pt}")
    out.append(r"\renewcommand{\arraystretch}{1.05}")
    out.append(r"\resizebox{\textwidth}{!}{%")
    out.append(r"\begin{tabular}{lccccccc}")
    out.append(r"\toprule")
    header = ["Outcome"] + [s[1] for s in SPECS]
    out.append(" & ".join(header) + r" \\")
    out.append(r"\midrule")
    out.extend(build_rows(sub_key))
    out.append(r"\bottomrule")
    out.append(r"\end{tabular}")
    out.append(r"}")  # close resizebox
    out.append(r"\begin{minipage}{0.95\linewidth}")
    out.append(NOTES)
    out.append(r"\end{minipage}")
    out.append(r"\end{table}")
    out.append("")

with open('tables/iv_robust_dropcouns.tex', 'w') as f:
    f.write('\n'.join(out) + '\n')
print("Wrote tables/iv_robust_dropcouns.tex (3 tables: pool/pp/preg)")

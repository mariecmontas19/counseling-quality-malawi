"""
build_cea_costs.py  -- reproducible cost-effectiveness computations (Scope A).

Scope A = the counseling-component program cost (Mahesh Karra's itemized budgets)
plus the FP-service reimbursement recomputed from the clinic reimbursement log.
Produces the program cost table, the per-method service costs, the CYP totals, and
the headline cost-effectiveness and throughput numbers.

DOCUMENTED CONSTANTS (sources):
  * Program budget: Mahesh's updated budget tables in "Mahesh's comments_JHE
    paper.docx" -- Table 3 (Initial Training & Costs, 2016-2017) and Table 4
    (Counselor Extension / Year 2). Per his June 2026 email: counselor split is
    10 + 2 = 12 months/counselor (dollar figures kept as written); the FM line is
    CORRECTED to $26,000 ($500 / FM / month x 2 FMs x 26 months); the old $20,000
    admin aggregate is DROPPED (superseded by the itemized tables); RA/PhD dropped;
    women's taxi and broader admin/ops are EXCLUDED (Scope A = counseling component).
  * Exchange rate: World Bank PA.NUS.FCRF (official, period average), Malawi:
    2016 = 718.00, 2017 = 730.27, 2018 = 732.33 MWK/USD. Representative = 730.27
    (2017, mid-intervention; ~58% of the log MKW is undated, total is ~2% of cost).
  * CYP conversion factors: USAID / FHI 360 / Avenir 2022 update
    (data4impactproject.org): injectable DMPA 0.25/dose; Jadelle 5-yr implant
    3.8/insertion; copper IUD 4.6/insertion; female sterilization 10/procedure;
    combined oral pills 0.0667/pack (15 cycles per CYP).
  * Analysis quantities (from the paper's tables): 3,898 counseling sessions;
    mean 4.643 total sessions/woman (Table 5 Panel A); IV dose reduction pooled
    -0.422, postpartum -0.623 (Table 5); 1,027 treated women.

INPUT:  Analysis/CEA/COST EFFECTIVENESS ANALYSIS/CLINIC REIMBURSEMENT LOG DEIDENTIFIED - 7-19-16.xlsx
        Analysis/CEA/outputs/cea_itt.csv  (written by results_cea_itt.do; the
        parent-RCT ITT effects on contraceptive use, used for the causal
        cost-per-additional-user metric -- the full-bundle treatment-vs-control
        effect from Karra-Maggio-Canning 2022 PNAS Table 2)
OUTPUT: Analysis/CEA/outputs/{cea_cost_table,cea_per_method,cea_metrics}.csv
"""
import os, re, csv
import openpyxl

BASE = os.environ.get("MFPS_ROOT", os.getcwd())  # project folder (holds Analysis/)
LOG  = os.path.join(BASE, "Analysis/CEA/COST EFFECTIVENESS ANALYSIS/CLINIC REIMBURSEMENT LOG DEIDENTIFIED - 7-19-16.xlsx")
OUT  = os.path.join(BASE, "Analysis/CEA/outputs")
os.makedirs(OUT, exist_ok=True)

# ---- documented constants ----
FX = 730.27                      # World Bank 2017 period-average MWK/USD
WOMEN_TREATED = 1027             # full randomized treatment arm. Per Mahesh (2026-06-23),
                                 # all assigned women count as "treated" because counselors
                                 # engaged them regardless of how many sessions they attended;
                                 # the first visit is the treatment. (For reference: 920 had
                                 # >=1 recorded session -- 901 with counselors 1-6, 15 with the
                                 # excluded counselor 7, 4 unidentified -- and 107 had none.)
COUNS_SALARY  = 17160 + 3600     # counselor salaries only (Tables 3 + 4)
COUNS_MONTHS  = 6 * 12           # 72 counselor-months (10 + 2 per counselor)
TOTAL_SESSIONS = 3898
MEAN_DOSE = 4.643                # mean total sessions/woman (Table 5)
DD_POOL, DD_PP = -0.422, -0.623  # IV dose reduction (Table 5)

# program cost components (USD), Scope A
COST_TABLE = [
    ("Counselor salary (6 counselors, 10 mo initial)", 17160, "Table 3"),
    ("Counselor phone credit (initial)",                 840,  "Table 3"),
    ("Counselor transport: van rental + fuel (initial)", 8000, "Table 3"),
    ("Counselor materials",                               150,  "Table 3"),
    ("MOH-RHD trainer (FP/RH content)",                   600,  "Table 3"),
    ("Counselor training per diem",                       900,  "Table 3"),
    ("Counselor salary (6 counselors, 2 mo extension)",  3600,  "Table 4"),
    ("Counselor phone credit (extension)",                180,  "Table 4"),
    ("Counselor transport (extension)",                  1600,  "Table 4"),
    ("Field manager salary ($500 x 2 FMs x 12 mo)",     12000,  "Table 4; 12 mo working with counselors"),
    # FP-service reimbursement is EXCLUDED from the cost (Mahesh, 2026-06-23):
    # the cost is the counseling component only; the reimbursement log is still
    # used below as the service record from which CYP (the output) is computed.
]

# CYP factors (USAID 2022); side-effects / consults generate no CYP
CYP_FACTOR = {"Injectable": 0.25, "Implant": 3.8, "IUD": 4.6,
              "Sterilization": 10.0, "Pills": 0.0667}

# ---- read the reimbursement log ----
def num(x):
    try: return float(x)
    except: return None

def is_removal(s):
    # spaces stripped so "R emoval" / "remov al" still match
    return bool(re.search(r"remov", s.lower().replace(" ", "")))

def switch_method(s):
    """For an implant-REMOVAL visit, the new method the woman switched to, if
    the row names one. A removal ends protection (0 CYP), but the same visit
    often starts a new method; we credit that method's CYP instead of losing it."""
    s = s.lower()
    if "inject" in s: return "Injectable"
    if "pill"   in s: return "Pills"
    if "iud" in s or "coil" in s: return "IUD"
    if "steril" in s: return "Sterilization"
    return None

def classify(s):
    sl = s.lower()
    if "implant" in sl or "jadelle" in sl:
        if is_removal(s):
            # removal+switch -> credit the new method's CYP; pure removal -> Implant (0 CYP)
            return switch_method(s) or "Implant"
        return "Implant"                       # insertion
    if "inject" in sl: return "Injectable"
    if "pill" in sl: return "Pills"
    if "steril" in sl: return "Sterilization"
    if "side effect" in sl: return "Side effects"
    if "iud" in sl or "coil" in sl: return "IUD"
    return "Other/consult"

wb = openpyxl.load_workbook(LOG, read_only=True, data_only=True)
ws = wb["Sheet1"]
n_svc = 0; total_mkw = 0.0; women = set()
mcount = {}; mmkw = {}
implant_ins = 0; implant_rem_pure = 0; implant_rem_switch = 0
for r in ws.iter_rows(min_row=4, values_only=True):
    if r is None: continue
    pid = r[5]; svc = r[6]; cost = num(r[8]); fp = str(r[9]).strip().lower() if r[9] else ""
    if svc is None or not fp.startswith("y") or cost is None: continue
    svc = str(svc)
    n_svc += 1; total_mkw += cost
    if pid: women.add(str(pid).strip())
    m = classify(svc); mcount[m] = mcount.get(m, 0) + 1; mmkw[m] = mmkw.get(m, 0) + cost
    # Implant-event accounting is on the RAW description (independent of the CYP
    # bucket, since removal+switch rows are re-credited to the switched method):
    if "implant" in svc.lower() or "jadelle" in svc.lower():
        if is_removal(svc):
            if switch_method(svc): implant_rem_switch += 1
            else:                  implant_rem_pure += 1
        else:
            implant_ins += 1

# ---- data integrity checks ----
assert n_svc == 472, n_svc
assert len(women) == 209, len(women)
assert round(total_mkw) == 815600, total_mkw
# 69 implant-mentioning events = insertions + pure removals + removal-switch events
assert implant_ins + implant_rem_pure + implant_rem_switch == 69, \
    (implant_ins, implant_rem_pure, implant_rem_switch)
assert implant_ins == 62, implant_ins

reimb_usd = total_mkw / FX

# ---- program cost table (counseling component only; reimbursement excluded) ----
rows = list(COST_TABLE)
total_cost = sum(v for _, v, _ in rows)

# ---- CYP (implants: insertions only) ----
cyp_rows = []
total_cyp = 0.0
for m, f in CYP_FACTOR.items():
    n = implant_ins if m == "Implant" else mcount.get(m, 0)
    c = n * f; total_cyp += c
    cyp_rows.append((m, n, f, c))
cost_per_cyp = total_cost / total_cyp

# ---- headline metrics ----
fp_users = len(women)
cost_per_woman = total_cost / WOMEN_TREATED
cost_per_user  = total_cost / fp_users

# ---- causal cost per additional user (PNAS ITT; full-bundle effect) ----
# Reads cea_itt.csv written by results_cea_itt.do (the parent-RCT ITT effects).
# The paper's own design is within-treatment, so the treatment-vs-control effect
# comes from Karra-Maggio-Canning 2022 (PNAS) Table 2. This is the BUNDLE effect;
# we divide the counseling-component cost (Scope A) by the bundle-induced number of
# additional users, which is stated as the relevant scope caveat in the CEA text.
ITT_CSV = os.path.join(OUT, "cea_itt.csv")
itt = {}
if os.path.exists(ITT_CSV):
    with open(ITT_CSV) as fh:
        for row in csv.DictReader(fh):
            itt[(row["outcome"], row["spec"])] = {k: float(row[k]) for k in ("b", "se", "pval", "n", "cmean")}

def cost_per_add_user(outcome, spec="adj"):
    rec = itt.get((outcome, spec))
    if not rec:
        return None
    return cost_per_woman / rec["b"], rec  # cost/woman divided by per-woman ITT effect

cpau_modern = cost_per_add_user("curr_use_inj_adj", "adj")
cpau_modern_un = cost_per_add_user("curr_use_inj_adj", "unadj")
cpau_larc = cost_per_add_user("more_eff_method", "adj")

# ---- throughput (quality-efficiency: women served per fixed counselor-month) ----
cost_cm = COUNS_SALARY / COUNS_MONTHS          # counselor-salary cost per counselor-month
cap = TOTAL_SESSIONS / COUNS_MONTHS            # session capacity per counselor-month
def served(dose): return cap / dose
w_base = served(MEAN_DOSE)                      # women served at the observed mean dose
thr = {}
for lbl, dd in [("pooled", DD_POOL), ("postpartum", DD_PP)]:
    dose_hq = MEAN_DOSE + dd                    # dd is negative (dose reduction)
    w_hq = served(dose_hq)
    thr[lbl] = (w_base, w_hq, 100*(w_hq/w_base - 1), cost_cm/w_base, cost_cm/w_hq, dose_hq)

# scenario rows for the throughput table (reference = observed mean dose)
thr_rows = [("Mean counseling dose (reference)", MEAN_DOSE, w_base, None, cost_cm/w_base, None)]
for lbl, dd in [("pooled", DD_POOL), ("postpartum", DD_PP)]:
    _, w_hq, pc, _, ch, dose_hq = thr[lbl]
    thr_rows.append((lbl, dose_hq, w_hq, pc, ch, dd))

# ---- write CSVs ----
with open(os.path.join(OUT, "cea_cost_table.csv"), "w", newline="") as f:
    w = csv.writer(f); w.writerow(["component", "usd", "source"])
    for k, v, s in rows: w.writerow([k, round(v), s])
    w.writerow(["TOTAL (counselor program)", round(total_cost), ""])
    w.writerow(["Cost per treated woman", round(cost_per_woman, 2), "/1027 arm"])

with open(os.path.join(OUT, "cea_per_method.csv"), "w", newline="") as f:
    w = csv.writer(f); w.writerow(["method", "n_services", "total_mkw", "mean_mkw_per_svc", "usd_per_svc", "cyp_factor", "cyp"])
    for m in ["Injectable", "Implant", "Pills", "Sterilization", "IUD", "Side effects", "Other/consult"]:
        n = mcount.get(m, 0); mk = mmkw.get(m, 0)
        fac = CYP_FACTOR.get(m, 0.0)
        ncyp = (implant_ins if m == "Implant" else n) if m in CYP_FACTOR else 0
        w.writerow([m, n, round(mk), round(mk/max(n,1)), round(mk/max(n,1)/FX, 2), fac, round(ncyp*fac, 1)])

with open(os.path.join(OUT, "cea_metrics.csv"), "w", newline="") as f:
    w = csv.writer(f); w.writerow(["metric", "value"])
    w.writerow(["program_total_usd", round(total_cost)])
    w.writerow(["cost_per_treated_woman", round(cost_per_woman, 2)])
    w.writerow(["cost_per_fp_service_user", round(cost_per_user, 2)])
    w.writerow(["total_cyp", round(total_cyp, 1)])
    w.writerow(["cost_per_cyp", round(cost_per_cyp, 2)])
    w.writerow(["implant_insertions", implant_ins])
    w.writerow(["implant_removals_total", implant_rem_pure + implant_rem_switch])
    w.writerow(["implant_removals_switch_recredited", implant_rem_switch])
    w.writerow(["reimbursement_usd", round(reimb_usd)])
    w.writerow(["cost_per_unique_woman_seeking_usd", round(total_mkw/fp_users/FX, 2)])
    if cpau_modern:
        v, rec = cpau_modern
        w.writerow(["itt_modern_use_effect", round(rec["b"], 4)])
        w.writerow(["itt_modern_use_se", round(rec["se"], 4)])
        w.writerow(["itt_modern_use_p", round(rec["pval"], 4)])
        w.writerow(["cost_per_additional_modern_user_usd", round(v, 2)])
    if cpau_modern_un:
        w.writerow(["cost_per_additional_modern_user_unadj_usd", round(cpau_modern_un[0], 2)])
    if cpau_larc:
        v, rec = cpau_larc
        w.writerow(["itt_larc_effect", round(rec["b"], 4)])
        w.writerow(["itt_larc_se", round(rec["se"], 4)])
        w.writerow(["cost_per_additional_larc_user_usd", round(v, 2)])
    w.writerow(["counselor_cost_per_month_usd", round(cost_cm, 2)])
    w.writerow(["counselor_month_capacity_sessions", round(cap, 1)])
    w.writerow(["mean_dose_sessions", MEAN_DOSE])
    for lbl, (wl, wh, pc, cl, ch, dose_hq) in thr.items():
        w.writerow([f"throughput_{lbl}_women_per_cm_low", round(wl, 1)])
        w.writerow([f"throughput_{lbl}_women_per_cm_high", round(wh, 1)])
        w.writerow([f"throughput_{lbl}_pct_gain", round(pc, 1)])
        w.writerow([f"throughput_{lbl}_cost_per_woman_high", round(ch, 2)])

# ---- throughput table CSV (one row per scenario) ----
with open(os.path.join(OUT, "cea_throughput.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["scenario", "dose", "women_per_cm", "pct_gain", "cost_per_woman", "dose_reduction"])
    for scen, dose, wpc, pc, cpw, dd in thr_rows:
        w.writerow([scen, round(dose, 3), round(wpc, 1),
                    "" if pc is None else round(pc, 1), round(cpw, 2),
                    "" if dd is None else round(dd, 3)])

# ---- console report ----
print(f"=== DATA CHECKS PASSED: 472 services, 209 women, 815,600 MKW; "
      f"{implant_ins} implant insertions, {implant_rem_pure} pure removals, "
      f"{implant_rem_switch} removal+switch re-credited to new method ===\n")
print("PROGRAM COST TABLE (Scope A, USD)")
for k, v, s in rows: print(f"  {k:48s} {v:>8,.0f}  [{s}]")
print(f"  {'TOTAL':48s} {total_cost:>8,.0f}")
print(f"  cost per treated woman = ${cost_per_woman:,.2f}   |  cost per FP-service user = ${cost_per_user:,.2f}")
print(f"\nCYP (implants = insertions only):")
for m, n, fac, c in cyp_rows: print(f"  {m:14s} {n:3d} x {fac:<6} = {c:7.1f}")
print(f"  TOTAL CYP = {total_cyp:.1f}   ->  COST PER CYP = ${cost_per_cyp:,.2f}")
if cpau_modern:
    v, rec = cpau_modern
    print(f"\nCAUSAL cost per additional user (PNAS bundle ITT, cost/woman ${cost_per_woman:,.2f}):")
    print(f"  modern use  ITT +{rec['b']:.4f} (se {rec['se']:.4f}, p {rec['pval']:.3f})  ->  ${v:,.0f} per additional modern user")
    if cpau_modern_un:
        print(f"     (unadjusted ITT +{cpau_modern_un[1]['b']:.4f}  ->  ${cpau_modern_un[0]:,.0f})")
    if cpau_larc:
        v2, rec2 = cpau_larc
        print(f"  LARC/effec. ITT +{rec2['b']:.4f} (se {rec2['se']:.4f}, p {rec2['pval']:.3f})  ->  ${v2:,.0f} per additional LARC user")
print(f"\nThroughput (counselor cost/month = ${cost_cm:,.2f}; capacity {cap:.1f} sessions/cm):")
for lbl, (wl, wh, pc, cl, ch, dose_hq) in thr.items():
    print(f"  {lbl:11s}: dose {MEAN_DOSE:.2f}->{dose_hq:.2f}  {wl:.1f}->{wh:.1f} women/cm (+{pc:.1f}%)   cost/woman ${cl:,.2f}->${ch:,.2f}")
print(f"\nWrote CSVs to {OUT}")

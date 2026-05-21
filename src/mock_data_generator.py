#!/usr/bin/env python
# coding: utf-8

# In[4]:


# Generate Mock Data 

# Libraries 
import random
import csv
from datetime import date, timedelta
from pathlib import Path 


# Working directory 
BASE_DIR = Path.cwd().parent


# In[5]:


### Study data ###

# --- Configuration ---
NUM_SUBJECTS = 50

OUTPUT_FILE = BASE_DIR / "data" / "mock_study_data.csv"
random.seed(42)  # For reproducibility

# --- Helper data ---
GENDERS = ["Male", "Female", "Non-binary"]
CONDITIONS = ["Control", "Treatment A", "Treatment B"]
EDUCATION_LEVELS = ["High School", "Some College", "Bachelor's", "Master's", "Doctorate"]
ETHNICITIES = ["White", "Black or African American", "Hispanic or Latino",
               "Asian", "Native American", "Pacific Islander", "Multiracial", "Prefer not to say"]

SITES = [
    {"site_code": "SITE-001", "site_name": "Boston Medical Center",      "city": "Boston",      "state": "MA", "pi": "Dr. Harmon",   "coordinator": "Lisa Tran"},
    {"site_code": "SITE-002", "site_name": "UCLA Health Sciences",        "city": "Los Angeles", "state": "CA", "pi": "Dr. Okafor",   "coordinator": "James Wu"},
    {"site_code": "SITE-003", "site_name": "UT Southwestern Medical",     "city": "Dallas",      "state": "TX", "pi": "Dr. Patel",    "coordinator": "Maria Santos"},
    {"site_code": "SITE-004", "site_name": "Johns Hopkins Research Ctr",  "city": "Baltimore",   "state": "MD", "pi": "Dr. Müller",   "coordinator": "Aisha Brown"},
    {"site_code": "SITE-005", "site_name": "University of Chicago Med",   "city": "Chicago",     "state": "IL", "pi": "Dr. Rivera",   "coordinator": "Tom Nguyen"},
]

VISIT_NAMES = {
    1: "Baseline",
    2: "Week 4 Follow-up",
    3: "Week 8 Follow-up",
    4: "Week 12 Follow-up",
    5: "Final / Week 16",
}

MONITORING_VISIT_TYPES = [
    "Site Initiation Visit (SIV)",
    "Interim Monitoring Visit",
    "Interim Monitoring Visit",
    "Interim Monitoring Visit",
    "Close-Out Visit",
]

MONITORING_STATUS_WEIGHTS = {
    "Scheduled": 50,
    "Completed": 35,
    "Rescheduled": 10,
    "Cancelled": 5,
}

CRA_NAMES = ["Sarah Kim", "David Reyes", "Priya Nair", "Ben Okonkwo", "Claire Fontaine"]

site_pool = (SITES * ((NUM_SUBJECTS // len(SITES)) + 1))[:NUM_SUBJECTS]
random.shuffle(site_pool)

def random_date(start_year=1950, end_year=2005):
    start = date(start_year, 1, 1)
    end = date(end_year, 12, 31)
    delta = end - start
    return start + timedelta(days=random.randint(0, delta.days))

def generate_subject(subject_id, site):
    dob = random_date()
    age = (date.today() - dob).days // 365

    gender = random.choices(GENDERS, weights=[48, 48, 4])[0]
    condition = random.choice(CONDITIONS)
    education = random.choice(EDUCATION_LEVELS)
    ethnicity = random.choice(ETHNICITIES)

    # Physiological data
    height_cm = round(random.gauss(170, 10), 1)       # Normal distribution
    weight_kg = round(random.gauss(75, 15), 1)
    bmi = round(weight_kg / ((height_cm / 100) ** 2), 1)
    systolic_bp = random.randint(100, 160)             # Blood pressure
    diastolic_bp = random.randint(60, 100)
    heart_rate = random.randint(55, 95)                # Resting heart rate

    # Survey / psychological scores (1–10 scales)
    stress_score = round(random.gauss(5, 2), 1)
    wellbeing_score = round(random.gauss(6, 1.5), 1)
    satisfaction_score = round(random.uniform(1, 10), 1)

    # Clip scores to valid range [1, 10]
    stress_score = max(1.0, min(10.0, stress_score))
    wellbeing_score = max(1.0, min(10.0, wellbeing_score))

    # Behavioral data
    sleep_hours = round(random.gauss(7, 1), 1)
    exercise_days_per_week = random.randint(0, 7)
    smoker = random.choices(["Yes", "No"], weights=[15, 85])[0]

    # Dropout / completion flag
    completed_study = random.choices(["Yes", "No"], weights=[85, 15])[0]

    return {
        "subject_id": f"SUBJ_{subject_id:03d}",
        "date_of_birth": dob.isoformat(),
        "age": age,
        "gender": gender,
        "ethnicity": ethnicity,
        "education_level": education,
        "study_condition": condition,
        "height_cm": height_cm,
        "weight_kg": weight_kg,
        "bmi": bmi,
        "systolic_bp": systolic_bp,
        "diastolic_bp": diastolic_bp,
        "resting_heart_rate": heart_rate,
        "stress_score_1_10": stress_score,
        "wellbeing_score_1_10": wellbeing_score,
        "satisfaction_score_1_10": satisfaction_score,
        "avg_sleep_hours": sleep_hours,
        "exercise_days_per_week": exercise_days_per_week,
        "smoker": smoker,
        "completed_study": completed_study,
        "site_code": site["site_code"],
        "site_name": site["site_name"],
        "site_city": site["city"],
        "site_state": site["state"],
        "principal_investigator": site["pi"],
        "site_coordinator": site["coordinator"],
    }

# --- Generate subjects ---
subjects = [generate_subject(i + 1, site_pool[i]) for i in range(NUM_SUBJECTS)]

# --- Write to CSV ---
with open(OUTPUT_FILE, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=subjects[0].keys())
    writer.writeheader()
    writer.writerows(subjects)

print(f"✅ Generated {NUM_SUBJECTS} subjects → {OUTPUT_FILE}")

# --- Preview first 3 ---
for s in subjects[:3]:
    print(s)


# In[31]:


### Visit data ###

# --- Configuration ---
INPUT_FILE = BASE_DIR / "data" / "mock_study_data.csv"
OUTPUT_FILE = BASE_DIR / "data" / "mock_visit_data.csv"
NUM_VISITS = 5
random.seed(42)

# --- Load subjects from existing CSV ---
with open(INPUT_FILE, newline="") as f:
    reader = csv.DictReader(f)
    subjects = list(reader)

# --- Visit structure ---
VISIT_NAMES = {
    1: "Baseline",
    2: "Week 4 Follow-up",
    3: "Week 8 Follow-up",
    4: "Week 12 Follow-up",
    5: "Final / Week 16"
}

VISIT_INTERVALS_DAYS = {
    1: 0,
    2: 28,
    3: 56,
    4: 84,
    5: 112
}

VISIT_TYPES = ["In-Person", "Telehealth"]
STAFF_MEMBERS = ["Dr. Rivera", "Dr. Patel", "Nurse Chen", "Nurse Okafor", "Dr. Müller"]

def random_start_date():
    """Random study enrollment date in the past ~2 years"""
    start = date.today() - timedelta(days=730)
    end = date.today() - timedelta(days=120)
    return start + timedelta(days=random.randint(0, (end - start).days))

def generate_visits(subject):
    subject_id = subject["subject_id"]
    completed_study = subject["completed_study"]
    enrollment_date = random_start_date()
    visits = []

    # Determine how many visits this subject completed
    if completed_study == "Yes":
        num_completed = NUM_VISITS  # All 5
    else:
        # Dropped out after visit 1, 2, 3, or 4
        num_completed = random.randint(1, NUM_VISITS - 1)

    conducting_staff = random.choice(STAFF_MEMBERS)  # Consistent staff per subject

    for visit_num in range(1, NUM_VISITS + 1):
        visit_name = VISIT_NAMES[visit_num]
        scheduled_date = enrollment_date + timedelta(days=VISIT_INTERVALS_DAYS[visit_num])

        if visit_num <= num_completed:
            status = "Completed"
            # Small chance of being a few days early/late
            actual_date = scheduled_date + timedelta(days=random.randint(-5, 7))
            visit_type = random.choices(VISIT_TYPES, weights=[70, 30])[0]
            staff = conducting_staff
            notes = random.choices(
                ["No concerns noted.", "Participant reported mild fatigue.",
                 "Follow-up required.", "Participant in good spirits.",
                 "Minor scheduling conflict resolved.", ""],
                weights=[40, 15, 10, 20, 10, 5]
            )[0]

            # Measurements taken at each visit
            systolic_bp = random.randint(100, 160)
            diastolic_bp = random.randint(60, 100)
            heart_rate = random.randint(55, 95)
            weight_kg = round(float(subject["weight_kg"]) + random.gauss(0, 1.5), 1)
            stress_score = round(max(1, min(10, float(subject["stress_score_1_10"]) + random.gauss(0, 1))), 1)
            wellbeing_score = round(max(1, min(10, float(subject["wellbeing_score_1_10"]) + random.gauss(0, 1))), 1)

        else:
            # Visit not yet reached or subject dropped out
            status = "Missed" if visit_num == num_completed + 1 and completed_study == "No" else "Not Reached"
            actual_date = ""
            visit_type = ""
            staff = ""
            notes = "Participant did not attend." if status == "Missed" else ""
            systolic_bp = systolic_bp = heart_rate = weight_kg = stress_score = wellbeing_score = ""

        visits.append({
            "subject_id": subject_id,
            "visit_number": visit_num,
            "visit_name": visit_name,
            "scheduled_date": scheduled_date.isoformat(),
            "actual_date": actual_date if actual_date else "",
            "status": status,
            "visit_type": visit_type,
            "staff_member": staff,
            "systolic_bp": systolic_bp,
            "diastolic_bp": diastolic_bp,
            "resting_heart_rate": heart_rate,
            "weight_kg": weight_kg,
            "stress_score_1_10": stress_score,
            "wellbeing_score_1_10": wellbeing_score,
            "notes": notes,
        })

    return visits

# --- Generate all visit records ---
all_visits = []
for subject in subjects:
    all_visits.extend(generate_visits(subject))

# --- Write to CSV ---
with open(OUTPUT_FILE, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=all_visits[0].keys())
    writer.writeheader()
    writer.writerows(all_visits)

print(f"✅ Generated {len(all_visits)} visit records for {len(subjects)} subjects → {OUTPUT_FILE}")

# --- Preview first subject's visits ---
print(f"\nSample visits for {subjects[0]['subject_id']}:")
for v in all_visits[:NUM_VISITS]:
    print(f"  Visit {v['visit_number']} ({v['visit_name']}): {v['status']} — {v['actual_date'] or 'N/A'}")


# In[32]:


### Lab data ###

OUTPUT_FILE = BASE_DIR / "data" / "mock_lab_results.csv"
NUM_VISITS = 5          # ✅ Defined at the top now
random.seed(42)

with open(INPUT_FILE, newline="") as f:
    subjects = list(csv.DictReader(f))

VISIT_NAMES = {1: "Baseline", 2: "Week 4", 3: "Week 8", 4: "Week 12", 5: "Week 16"}
LAB_PANELS = ["Complete Blood Count", "Metabolic Panel", "Lipid Panel"]

def flag(value, low, high):
    if value < low: return "LOW"
    if value > high: return "HIGH"
    return "Normal"

def generate_lab_results(subject):
    records = []
    completed = subject["completed_study"]
    num_visits = NUM_VISITS if completed == "Yes" else random.randint(1, NUM_VISITS - 1)
    base_date = date.today() - timedelta(days=random.randint(120, 730))

    for visit_num in range(1, num_visits + 1):
        visit_date = base_date + timedelta(days=(visit_num - 1) * 28)
        for panel in LAB_PANELS:
            if panel == "Complete Blood Count":
                wbc = round(random.gauss(7.0, 1.5), 2)
                rbc = round(random.gauss(4.8, 0.5), 2)
                hemoglobin = round(random.gauss(14.0, 1.5), 2)
                hematocrit = round(random.gauss(42.0, 4.0), 2)
                platelets = round(random.gauss(250, 50), 0)
                for test, val, low, high, ref in [
                    ("WBC (x10³/µL)", wbc, 4.5, 11.0, "4.5–11.0"),
                    ("RBC (x10⁶/µL)", rbc, 4.2, 5.4, "4.2–5.4"),
                    ("Hemoglobin (g/dL)", hemoglobin, 12.0, 17.5, "12.0–17.5"),
                    ("Hematocrit (%)", hematocrit, 36.0, 50.0, "36.0–50.0"),
                    ("Platelets (x10³/µL)", platelets, 150, 400, "150–400"),
                ]:
                    records.append({
                        "subject_id": subject["subject_id"],
                        "visit_number": visit_num,
                        "visit_name": VISIT_NAMES[visit_num],
                        "lab_date": visit_date.isoformat(),
                        "panel": panel,
                        "test": test,
                        "value": val,
                        "reference_range": ref,
                        "flag": flag(val, low, high)
                    })

            elif panel == "Metabolic Panel":
                glucose = round(random.gauss(95, 15), 1)
                sodium = round(random.gauss(140, 3), 1)
                potassium = round(random.gauss(4.0, 0.4), 2)
                creatinine = round(random.gauss(1.0, 0.2), 2)
                for test, val, low, high, ref in [
                    ("Glucose (mg/dL)", glucose, 70, 100, "70–100"),
                    ("Sodium (mEq/L)", sodium, 136, 145, "136–145"),
                    ("Potassium (mEq/L)", potassium, 3.5, 5.0, "3.5–5.0"),
                    ("Creatinine (mg/dL)", creatinine, 0.7, 1.3, "0.7–1.3"),
                ]:
                    records.append({
                        "subject_id": subject["subject_id"],
                        "visit_number": visit_num,
                        "visit_name": VISIT_NAMES[visit_num],
                        "lab_date": visit_date.isoformat(),
                        "panel": panel,
                        "test": test,
                        "value": val,
                        "reference_range": ref,
                        "flag": flag(val, low, high)
                    })

            elif panel == "Lipid Panel":
                total_chol = round(random.gauss(190, 30), 1)
                ldl = round(random.gauss(110, 25), 1)
                hdl = round(random.gauss(55, 12), 1)
                triglycerides = round(random.gauss(130, 40), 1)
                for test, val, low, high, ref in [
                    ("Total Cholesterol (mg/dL)", total_chol, 0, 200, "<200"),
                    ("LDL (mg/dL)", ldl, 0, 130, "<130"),
                    ("HDL (mg/dL)", hdl, 40, 999, ">40"),
                    ("Triglycerides (mg/dL)", triglycerides, 0, 150, "<150"),
                ]:
                    records.append({
                        "subject_id": subject["subject_id"],
                        "visit_number": visit_num,
                        "visit_name": VISIT_NAMES[visit_num],
                        "lab_date": visit_date.isoformat(),
                        "panel": panel,
                        "test": test,
                        "value": val,
                        "reference_range": ref,
                        "flag": flag(val, low, high)
                    })
    return records

all_labs = []
for subject in subjects:
    all_labs.extend(generate_lab_results(subject))

with open(OUTPUT_FILE, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=all_labs[0].keys())
    writer.writeheader()
    writer.writerows(all_labs)

print(f"✅ Generated {len(all_labs)} lab records → {OUTPUT_FILE}")


# In[33]:


### Adverse events ###

OUTPUT_FILE = BASE_DIR / "data" / "mock_adverse_events.csv"
random.seed(42)

with open(INPUT_FILE, newline="") as f:
    subjects = list(csv.DictReader(f))

ADVERSE_EVENTS = [
    "Headache", "Nausea", "Fatigue", "Dizziness", "Insomnia",
    "Mild rash", "Gastrointestinal discomfort", "Elevated blood pressure",
    "Palpitations", "Anxiety episode", "Joint pain", "Loss of appetite"
]
SEVERITY_LEVELS = ["Mild", "Moderate", "Severe"]
SEVERITY_WEIGHTS = [65, 28, 7]
OUTCOMES = ["Resolved", "Ongoing", "Resolved with treatment", "Withdrawn from study"]
OUTCOME_WEIGHTS = [55, 20, 20, 5]
RELATEDNESS = ["Unrelated", "Possibly related", "Probably related", "Definitely related"]
RELATEDNESS_WEIGHTS = [40, 30, 20, 10]
ACTION_TAKEN = ["None", "Dose adjusted", "Medication prescribed", "Visit scheduled", "Withdrawn"]
ACTION_WEIGHTS = [50, 15, 20, 10, 5]

def generate_adverse_events(subject):
    records = []
    # ~60% of subjects experience at least one AE
    if random.random() > 0.60:
        return records

    num_events = random.randint(1, 4)
    base_date = date.today() - timedelta(days=random.randint(120, 700))

    for i in range(num_events):
        event_date = base_date + timedelta(days=random.randint(0, 112))
        severity = random.choices(SEVERITY_LEVELS, weights=SEVERITY_WEIGHTS)[0]
        outcome = random.choices(OUTCOMES, weights=OUTCOME_WEIGHTS)[0]
        resolution_date = ""
        if "Resolved" in outcome:
            resolution_date = (event_date + timedelta(days=random.randint(1, 21))).isoformat()

        records.append({
            "subject_id": subject["subject_id"],
            "ae_id": f"{subject['subject_id']}_AE{i+1:02d}",
            "event_date": event_date.isoformat(),
            "adverse_event": random.choice(ADVERSE_EVENTS),
            "severity": severity,
            "relatedness_to_study": random.choices(RELATEDNESS, weights=RELATEDNESS_WEIGHTS)[0],
            "action_taken": random.choices(ACTION_TAKEN, weights=ACTION_WEIGHTS)[0],
            "outcome": outcome,
            "resolution_date": resolution_date,
            "serious_adverse_event": "Yes" if severity == "Severe" else "No",
            "reported_by": random.choice(["Participant", "Clinician", "Caregiver"]),
        })
    return records

all_aes = []
for subject in subjects:
    all_aes.extend(generate_adverse_events(subject))

with open(OUTPUT_FILE, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=all_aes[0].keys())
    writer.writeheader()
    writer.writerows(all_aes)

print(f"✅ Generated {len(all_aes)} adverse event records → {OUTPUT_FILE}")


# In[36]:


### Consent / screening ###

OUTPUT_FILE = BASE_DIR / "data" / "mock_consent_screening.csv"
random.seed(42)

with open(INPUT_FILE, newline="") as f:
    subjects = list(csv.DictReader(f))

REFERRING_SOURCES = ["Primary Care Physician", "Self-referral", "Online Ad",
                     "Community Flyer", "Hospital Referral", "Word of Mouth"]
EXCLUSION_CONDITIONS = ["None", "None", "None",  # Weighted toward None
                        "Active cancer treatment", "Pregnancy",
                        "Severe psychiatric disorder", "Recent surgery"]
LANGUAGES = ["English", "English", "English", "Spanish", "Mandarin", "French", "Arabic"]
CONSENT_VERSIONS = ["v1.0", "v1.1", "v1.2"]

def generate_consent_screening(subject):
    screening_date = date.today() - timedelta(days=random.randint(130, 750))
    consent_date = screening_date + timedelta(days=random.randint(0, 5))

    age = int(subject["age"])
    bmi = float(subject["bmi"])
    smoker = subject["smoker"]

    # Eligibility criteria checks
    age_eligible = 18 <= age <= 75
    bmi_eligible = 18.5 <= bmi <= 40.0
    smoker_eligible = smoker == "No"  # Hypothetical exclusion
    exclusion_condition = random.choice(EXCLUSION_CONDITIONS)
    exclusion_eligible = exclusion_condition == "None"

    overall_eligible = all([age_eligible, bmi_eligible, smoker_eligible, exclusion_eligible])

    # A few screened-out subjects still got enrolled (data entry error simulation)
    enrolled = overall_eligible or (not overall_eligible and random.random() < 0.05)

    return {
        "subject_id": subject["subject_id"],
        "screening_date": screening_date.isoformat(),
        "referring_source": random.choice(REFERRING_SOURCES),
        "preferred_language": random.choice(LANGUAGES),
        "interpreter_required": random.choices(["Yes", "No"], weights=[10, 90])[0],

        # Eligibility
        "age_eligible": "Yes" if age_eligible else "No",
        "bmi_eligible": "Yes" if bmi_eligible else "No",
        "non_smoker_eligible": "Yes" if smoker_eligible else "No",
        "exclusion_condition": exclusion_condition,
        "exclusion_condition_eligible": "Yes" if exclusion_eligible else "No",
        "overall_eligible": "Yes" if overall_eligible else "No",

        # Consent
        "consent_obtained": "Yes" if enrolled else "No",
        "consent_date": consent_date.isoformat() if enrolled else "",
        "consent_version": random.choice(CONSENT_VERSIONS) if enrolled else "",
        "consent_witness": random.choice(["Yes", "No"]) if enrolled else "",
        "capacity_confirmed": "Yes" if enrolled else "",
        "right_to_withdraw_explained": "Yes" if enrolled else "",

        # Enrollment
        "enrolled": "Yes" if enrolled else "No",
        "enrollment_date": consent_date.isoformat() if enrolled else "",
        "screening_staff": random.choice(["Dr. Rivera", "Dr. Patel", "Nurse Chen",
                                          "Nurse Okafor", "Dr. Müller"]),
        "screening_notes": random.choices(
            ["No concerns.", "Participant asked clarifying questions.",
             "Consent re-explained at participant request.", ""],
            weights=[60, 20, 15, 5]
        )[0],
    }

all_consents = [generate_consent_screening(s) for s in subjects]

with open(OUTPUT_FILE, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=all_consents[0].keys())
    writer.writeheader()
    writer.writerows(all_consents)

print(f"✅ Generated {len(all_consents)} consent/screening records → {OUTPUT_FILE}")


# In[37]:


### Site and visit data ###

# ── Generate site visit schedule ──────────────────────────────────────────────
schedule_rows = []
today = date.today()

for subject in subjects:
    sid         = subject["subject_id"]
    site_code   = subject["site_code"]
    site        = next(s for s in SITES if s["site_code"] == site_code)
    completed   = subject["completed_study"]

    # Enrollment date: random point in the past ~2 years
    enroll_date = today - timedelta(days=random.randint(60, 730))

    num_visits = 5 if completed == "Yes" else random.randint(1, 4)

    for visit_num in range(1, 6):
        scheduled = enroll_date + timedelta(days=(visit_num - 1) * 28)
        offset    = timedelta(days=random.randint(-3, 7))

        if visit_num <= num_visits:
            actual    = scheduled + offset
            status    = "Completed" if actual <= today else "Scheduled"
            actual_dt = actual.isoformat()
        else:
            status    = "Not Reached" if completed == "No" else "Scheduled"
            actual    = scheduled + offset
            actual_dt = actual.isoformat() if status == "Scheduled" else ""

        schedule_rows.append({
            "visit_type":       "Participant Visit",
            "site_code":        site_code,
            "site_name":        site["site_name"],
            "city":             site["city"],
            "state":            site["state"],
            "pi":               site["pi"],
            "coordinator":      site["coordinator"],
            "subject_id":       sid,
            "visit_number":     visit_num,
            "visit_name":       VISIT_NAMES[visit_num],
            "scheduled_date":   scheduled.isoformat(),
            "actual_date":      actual_dt,
            "status":           status,
            "visit_modality":   random.choices(["In-Person", "Telehealth"], weights=[70, 30])[0],
            "cra":              "",
            "monitoring_notes": "",
        })

# ── Generate monitoring visit schedule (one set per site) ────────────────────
for site in SITES:
    # Spread monitoring visits roughly across the study timeline
    base = today - timedelta(days=400)
    cra  = random.choice(CRA_NAMES)

    for i, mv_type in enumerate(MONITORING_VISIT_TYPES):
        mv_date = base + timedelta(days=i * 90 + random.randint(-7, 7))
        status  = random.choices(
            list(MONITORING_STATUS_WEIGHTS.keys()),
            weights=list(MONITORING_STATUS_WEIGHTS.values())
        )[0]
        actual_dt = (mv_date + timedelta(days=random.randint(-2, 5))).isoformat() if status == "Completed" else ""

        schedule_rows.append({
            "visit_type":       "Monitoring Visit",
            "site_code":        site["site_code"],
            "site_name":        site["site_name"],
            "city":             site["city"],
            "state":            site["state"],
            "pi":               site["pi"],
            "coordinator":      site["coordinator"],
            "subject_id":       "",
            "visit_number":     i + 1,
            "visit_name":       mv_type,
            "scheduled_date":   mv_date.isoformat(),
            "actual_date":      actual_dt,
            "status":           status,
            "visit_modality":   "In-Person",
            "cra":              cra,
            "monitoring_notes": random.choices(
                ["", "SDV required", "Query raised", "Protocol deviation noted", "All clear"],
                weights=[40, 20, 15, 10, 15]
            )[0],
        })

# Sort by scheduled date
schedule_rows.sort(key=lambda r: r["scheduled_date"])


OUTPUT_FILE = BASE_DIR / "data" / "mock_site_visit_schedule.csv"

with open(OUTPUT_FILE, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=schedule_rows[0].keys())
    writer.writeheader()
    writer.writerows(schedule_rows)

participant_rows = sum(1 for r in schedule_rows if r["visit_type"] == "Participant Visit")
monitoring_rows  = sum(1 for r in schedule_rows if r["visit_type"] == "Monitoring Visit")

print(f"✅ mock_site_visit_schedule.csv — {len(schedule_rows)} rows")
print(f"   ├── {participant_rows} participant visit rows")
print(f"   └── {monitoring_rows} monitoring visit rows")
print()
print("📋 Sites enrolled:")
from collections import Counter
site_counts = Counter(s["site_code"] for s in subjects)
for site in SITES:
    print(f"   {site['site_code']} | {site['site_name']:<35} | {site_counts[site['site_code']]} participants")


# In[ ]:





# In[ ]:





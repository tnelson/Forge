
/* Students
 *  - id
 *  - email e.g. "tdelvecc@cs.brown.edu"
 */
CREATE TABLE students (
    id SERIAL PRIMARY KEY,
    email TEXT
);

/* Projects
 *  - id
 *  - name e.g. "Forge 1"
 */
CREATE TABLE projects (
    id SERIAL PRIMARY KEY,
    name TEXT
);

/* Files
 *  - id
 *  - student_id
 *  - project_id
 *  - name e.g. "forge1.rkt"
 *  - current_contents e.g. "#lang forge\n\n..."
 */
CREATE TABLE files (
    id SERIAL PRIMARY KEY,
    student_id INTEGER REFERENCES students(id),
    project_id INTEGER REFERENCES projects(id),
    name TEXT,
    current_contents TEXT
);

/* Executions
 *  - id
 *  - file_id
 *  - time
 *  - mode ("forge", "forge/core", "forge/check-ex-spec", ...)
 */
CREATE TABLE executions (
    id SERIAL PRIMARY KEY,
    file_id INTEGER REFERENCES files(id),
    snapshot TEXT,
    time TIME,
    mode TEXT
);

/* Commands
 *  - id
 *  - execution_id
 *  - command e.g. "(run my-run #:preds [...])"
 */
CREATE TABLE commands (
    id SERIAL PRIMARY KEY,
    execution_id INTEGER REFERENCES executions(id),
    command TEXT
);

/* Runs
 *  - command_id
 *  - result ("sat", "unsat")
 */
CREATE TABLE runs (
    command_id INTEGER REFERENCES commands(id),
    result TEXT
);

/* Cores
 *  - command_id
 *  - core e.g. "x and y"
 */
CREATE TABLE cores (
    command_id INTEGER REFERENCES commands(id),
    core TEXT
);

/* Instances
 *  - id
 *  - command_id
 *  - model e.g. '{"A": ["A0", "A1", "A2"], ...}'::JSONB
 */
CREATE TABLE instances (
    id SERIAL PRIMARY KEY,
    command_id INTEGER REFERENCES commands(id),
    model JSONB
);

/* Tests
 *  - command_id
 *  - expected ("sat", "unsat", "theorem")
 *  - passed (true, false)
 */
CREATE TABLE tests (
    command_id INTEGER REFERENCES commands(id),
    expected TEXT,
    passed BOOL
);


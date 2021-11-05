from pathlib import Path

from invoke import UnexpectedExit, task

from utils.markdown import renderDir, renderFile

TOP_DIR = Path(__file__).parent
SRC_DIR = TOP_DIR / "src"
OUT_DIR = TOP_DIR / "output"
TEST_DIR = TOP_DIR / "tests" / "unit"
UTILS_DIR = TOP_DIR / "utils"


@task
def docs(c):
    """Build documentation."""
    (OUT_DIR / "docs").mkdir(parents=True, exist_ok=True)
    renderFile(TOP_DIR / "README.md", OUT_DIR / "docs" / "README.html")
    renderDir(TOP_DIR / "docs", OUT_DIR / "docs")


@task
def test(c):
    """Run tests."""
    with c.cd(str(TOP_DIR)):
        c.run("pytest", pty=True)


@task
def types(c, report=False):
    """Type-check sources with mypy."""
    cmd = ["mypy"]
    if report:
        cmd.append("--html-report output/mypy-report.html")
    cmd.append(str(SRC_DIR))
    cmd.append(str(TEST_DIR))
    print("Type-checking...")
    with c.cd(str(TOP_DIR)):
        try:
            c.run(" ".join(cmd), pty=True)
        except UnexpectedExit as ex:
            if ex.result.exited < 0:
                print(ex)


@task
def lint(c, src=None):
    """Check sources with PyLint."""
    print("Linting...")
    sources = (
        (SRC_DIR / "retroasm").glob("**/*.py") if src is None else Path.cwd().glob(src)
    )
    with c.cd(str(TOP_DIR)):
        c.run("pylint %s" % " ".join(str(path) for path in sources), pty=True)


@task
def isort(c):
    """Sort imports."""
    print("Sorting imports...")
    c.run(f"isort {SRC_DIR} {TEST_DIR} {UTILS_DIR} {__file__}", pty=True)


@task
def upgrade(c):
    """Upgrade sources to take advantage of new Python features."""
    print("Upgrading sources...")
    sources = (SRC_DIR / "retroasm").glob("**/*.py")
    c.run(f"pyupgrade --py39-plus {' '.join(str(path) for path in sources)}")


@task
def unused(c):
    """Find unused code."""
    print("Scanning sources for unused code...")
    c.run(f"vulture --ignore-names '*_' {SRC_DIR} {TEST_DIR}")

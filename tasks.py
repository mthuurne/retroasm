from pathlib import Path

from invoke import UnexpectedExit, task

TOP_DIR = Path(__file__).parent
SRC_DIR = TOP_DIR / "src"
OUT_DIR = TOP_DIR / "build"
TEST_DIR = TOP_DIR / "tests" / "unit"


@task
def sphinx(c, builder="html"):
    """Build documentation using Sphinx."""
    dest = OUT_DIR / "docs"
    cmd = ["sphinx-build", "-j auto", f"-d {OUT_DIR}/sphinx-cache", f"-b {builder}"]
    cmd.append(str(TOP_DIR / "docs"))
    cmd.append(str(dest))
    with c.cd(str(TOP_DIR)):
        c.run(" ".join(cmd), pty=True)


@task(post=[sphinx])
def docs(c):
    """Build documentation."""
    print("Rendering README...")
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    cmd = ["rst2html", "README.rst", f"-d {OUT_DIR}/README.html"]
    with c.cd(str(TOP_DIR)):
        c.run(" ".join(cmd), pty=True)
    print()


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
        cmd.append(f"--html-report {OUT_DIR}/mypy-report.html")
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

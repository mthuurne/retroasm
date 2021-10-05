from pathlib import Path

from invoke import UnexpectedExit, task

from utils.markdown import renderDir, renderFile

TOP_DIR = Path(__file__).parent
SRC_DIR = TOP_DIR / 'src'
OUT_DIR = TOP_DIR / 'output'
TEST_DIR = TOP_DIR / 'tests' / 'unit'

@task
def docs(c):
    """Build documentation."""
    (OUT_DIR / 'docs').mkdir(parents=True, exist_ok=True)
    renderFile(TOP_DIR / 'README.md', OUT_DIR / 'docs' / 'README.html')
    renderDir(TOP_DIR / 'docs', OUT_DIR / 'docs')

@task
def test(c):
    """Run tests."""
    with c.cd(str(TOP_DIR)):
        c.run('pytest', pty=True)

@task
def types(c, report=False):
    """Type-check sources with mypy."""
    cmd = ['mypy']
    if report:
        cmd.append('--html-report output/mypy-report.html')
    cmd.append('src/retroasm/*.py')
    print('Type-checking...')
    with c.cd(str(TOP_DIR)):
        try:
            c.run(' '.join(cmd), pty=True)
        except UnexpectedExit as ex:
            if ex.result.exited < 0:
                print(ex)

@task
def lint(c, src=None):
    """Check sources with PyLint."""
    print('Linting...')
    sources = (
        (TOP_DIR / 'src' / 'retroasm').glob('**/*.py')
        if src is None
        else Path.cwd().glob(src)
        )
    with c.cd(str(TOP_DIR)):
        c.run('pylint %s' % ' '.join(str(path) for path in sources), pty=True)

@task
def isort(c):
    """Sort imports."""
    print('Sorting imports...')
    c.run(f'isort {SRC_DIR} {TEST_DIR}', pty=True)

from pathlib import Path

from invoke import UnexpectedExit, task

from utils.markdown import renderDir

TOP_DIR = Path(__file__).parent
SRC_DIR = TOP_DIR / 'src'

@task
def docs(c):
    """Build documentation."""
    renderDir(TOP_DIR / 'docs', TOP_DIR / 'output' / 'docs')

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
def isort(c):
    """Sort imports."""
    print('Sorting imports...')
    with c.cd(str(SRC_DIR)):
        c.run('isort --apply', pty=True)

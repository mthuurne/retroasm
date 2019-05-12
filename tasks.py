from pathlib import Path

from invoke import task

TOP_DIR = Path(__file__).parent
SRC_DIR = TOP_DIR / 'src'

@task
def isort(c):
    """Sort imports."""
    print('Sorting imports...')
    with c.cd(str(SRC_DIR)):
        c.run('isort --apply', pty=True)

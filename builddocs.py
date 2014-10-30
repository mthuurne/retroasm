#!/usr/bin/env python3

from markdown import markdownFromFile

from os import makedirs, walk
from os.path import isdir
from shutil import rmtree

extensions = (
    'markdown.extensions.def_list',
    'markdown.extensions.smarty',
    'markdown.extensions.tables',
    )

def header(title):
    yield '<html>'
    yield '<head>'
    yield '<title>%s</title>' % title
    yield '</head>'
    yield '<body>'

def footer():
    yield ''
    yield '</body>'
    yield '</html>'

def writeLines(out, lines):
    out.write(''.join(line + '\n' for line in lines).encode('utf8'))

def renderFile(inFileName, outFileName):
    print(inFileName, '->', outFileName)
    with open(inFileName, 'rb') as inp:
        title = inp.readline().decode('utf8').strip()
        inp.seek(0)
        with open(outFileName, 'wb') as out:
            writeLines(out, header(title))
            markdownFromFile(inp, out, extensions)
            writeLines(out, footer())

def processDir(inBase, outBase):
    if isdir(outBase):
        rmtree(outBase)
    for dirPath, subdirList, fileList in walk(inBase):
        dirPath += '/'
        if not dirPath.startswith(inBase + '/'):
            raise ValueError(
                'os.walk entered directory "%s" which is outside its '
                'root "%s"' % (dirPath, inBase)
                )
        outDir = outBase + dirPath[len(inBase) : ]
        for fileName in fileList:
            if fileName.endswith('.md'):
                makedirs(outDir, exist_ok = True)
                renderFile(
                    dirPath + fileName,
                    outDir + fileName[ : -3] + '.html'
                    )

if __name__ == '__main__':
    processDir('docs', 'output/docs')

from os import makedirs, walk
from os.path import isdir
from shutil import rmtree

from markdown import markdownFromFile
from markdown.extensions import Extension
from markdown.treeprocessors import Treeprocessor
from markdown.util import etree

class DocumentProcessor(Treeprocessor):
    def run(self, root):
        titleText = root.find('./h1').text
        content = list(root)
        root.clear()
        root.tag = 'html'
        root.text = root.tail = '\n'
        header = etree.SubElement(root, 'head')
        header.text = header.tail = '\n'
        title = etree.SubElement(header, 'title')
        title.text = titleText
        title.tail = '\n'
        body = etree.SubElement(root, 'body')
        body.text = body.tail = '\n'
        body.extend(content)

class DocumentExtension(Extension):
    """Wraps the generated content in a full document.
    """
    def extendMarkdown(self, md):
        md.stripTopLevelTags = False
        md.treeprocessors.register(
            DocumentProcessor(md), 'retroasm.document', 0
            )

extensions = (
    'markdown.extensions.def_list',
    'markdown.extensions.smarty',
    'markdown.extensions.tables',
    DocumentExtension()
    )

def renderFile(inFileName, outFileName):
    print(inFileName, '->', outFileName)
    markdownFromFile(
        input=str(inFileName), output=str(outFileName), extensions=extensions
        )

def renderDir(inBase, outBase):
    inPrefix = str(inBase) + '/'
    outPrefix = str(outBase) + '/'
    for dirPath, subdirList, fileList in walk(inBase):
        dirPath += '/'
        if not dirPath.startswith(inPrefix):
            raise ValueError(
                'os.walk entered directory "%s" which is outside its '
                'root "%s"' % (dirPath, inBase)
                )
        outDir = outPrefix + dirPath[len(inPrefix) : ]
        for fileName in fileList:
            if fileName.endswith('.md'):
                makedirs(outDir, exist_ok = True)
                renderFile(
                    dirPath + fileName,
                    outDir + fileName[ : -3] + '.html'
                    )

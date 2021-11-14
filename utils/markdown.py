from os import makedirs, walk

from markdown import markdownFromFile
from markdown.extensions import Extension
from markdown.treeprocessors import Treeprocessor
from markdown.util import etree


class DocumentProcessor(Treeprocessor):
    def run(self, root):
        titleText = root.find("./h1").text
        content = list(root)
        root.clear()
        root.tag = "html"
        root.text = root.tail = "\n"
        header = etree.SubElement(root, "head")
        header.text = header.tail = "\n"
        title = etree.SubElement(header, "title")
        title.text = titleText
        title.tail = "\n"
        body = etree.SubElement(root, "body")
        body.text = body.tail = "\n"
        body.extend(content)


class DocumentExtension(Extension):
    """Wraps the generated content in a full document."""

    def extendMarkdown(self, md):
        md.stripTopLevelTags = False
        md.treeprocessors.register(DocumentProcessor(md), "retroasm.document", 0)


extensions = (
    "markdown.extensions.def_list",
    "markdown.extensions.smarty",
    "markdown.extensions.tables",
    DocumentExtension(),
)


def render_file(in_file_name, out_file_name):
    print(in_file_name, "->", out_file_name)
    markdownFromFile(
        input=str(in_file_name), output=str(out_file_name), extensions=extensions
    )


def render_dir(in_base, out_base):
    in_prefix = str(in_base) + "/"
    out_prefix = str(out_base) + "/"
    for dir_path, subdir_list, file_list in walk(in_base):
        dir_path += "/"
        if not dir_path.startswith(in_prefix):
            raise ValueError(
                'os.walk entered directory "%s" which is outside its '
                'root "%s"' % (dir_path, in_base)
            )
        out_dir = out_prefix + dir_path[len(in_prefix) :]
        for file_name in file_list:
            if file_name.endswith(".md"):
                makedirs(out_dir, exist_ok=True)
                render_file(dir_path + file_name, out_dir + file_name[:-3] + ".html")

import sphinx_bootstrap_theme
from pygments.lexer import RegexLexer
from pygments import token
from sphinx.highlighting import lexers
from pygments.style import Style
from re import escape

from sphinx_markdown_parser.parser import MarkdownParser

def setup(app):
    app.add_source_suffix('.md', 'markdown')
    app.add_source_parser(MarkdownParser)
    app.add_config_value('markdown_parser_config', {
        'auto_toc_tree_section': 'Content',
        'enable_auto_doc_ref': True,
        'enable_auto_toc_tree': True,
        'enable_eval_rst': True,
        'extensions': [
            'extra',
            'nl2br',
            'sane_lists',
            'smarty',
            'toc',
            'wikilinks',
            'pymdownx.arithmatex',
        ],
    }, True)


project = "Site-32"
copyright = "2020, thautwarm"
author = "thautwarm"

language = None
exclude_patterns = ["_build", "_sources", ".shadow", "Thumbs.db", ".DS_Store"]

sijuiacion_keywords = [
    "runtime",
    "load",
    "store",
    "deref",
    "deref!",
    "const",
    "print",
    "pop",
    "prj",
    "prj!",
    "indir",
    "rot",
    "dup",
    "goto",
    "goto-if",
    "goto-if-not",
    "label",
    "blockaddr",
    "call",
    "list",
    "tuple",
    "return",
    "line",
    "defun",
    "switch",
    "document",
    "filename",
    "free",
    "name",
    "args",
    "firstlineno",
]
common_operators = ["{", "}", "|", "=>", "_", "[", "]"]


class WurusaiStyle(Style):
    background_color = "#FFFFAA"
    styles = {
        token.Text: "#AA3939",
        token.String: "#479030",
        token.Keyword: "#A600A6",
        token.Operator: "#246C60",
        token.Number: "#779D34",
        token.Comment: "#AA6F39",
        token.Punctuation: "#DE369D",
        token.Literal: "#4671D5",
    }


def pygments_monkeypatch_style(mod_name, cls):
    import sys
    import pygments.styles

    cls_name = cls.__name__
    mod = type(__import__("os"))(mod_name)
    setattr(mod, cls_name, cls)
    setattr(pygments.styles, mod_name, mod)
    sys.modules["pygments.styles." + mod_name] = mod
    from pygments.styles import STYLE_MAP

    STYLE_MAP[mod_name] = mod_name + "::" + cls_name


# unused so far
pygments_monkeypatch_style("wurusai", WurusaiStyle)
pygments_style = "colorful"


class SijLexer(RegexLexer):
    name = "sijuiacion"

    tokens = {
        "root": [
            *[(escape(k), token.Keyword) for k in sijuiacion_keywords],
            *[(escape(o), token.Operator) for o in sijuiacion_keywords],
            (r"#([^\\#]+|\\.)*?#", token.Literal),
            (r"\d+", token.Number),
            (
                r"[-$\.a-zA-Z_\u4e00-\u9fa5][\-\!-$\.a-zA-Z0-9_\u4e00-\u9fa5]*",
                token.Name,
            ),
            (r'''"([^\\"]+|\\.)*?"''', token.String),
            (r"\s+", token.Whitespace),
        ]
    }


class RBNFLexer(RegexLexer):
    name = "rbnf"

    tokens = {
        "root": [
            *[(escape(o), token.Punctuation) for o in ["->", "|", ";", ":", "=", "?"]],
            (r"#([^\\#]+|\\.)*?#", token.Comment),
            (r"[-$\.a-zA-Z_\u4e00-\u9fa5][a-zA-Z0-9_\u4e00-\u9fa5]*", token.Keyword),
            (r'''"([^\\"]+|\\.)*?"''', token.Operator),
            (r"""'([^\\']+|\\.)*?'""", token.Operator),
            (r"\<.*\>", token.Operator),
            (r"\s+", token.Whitespace),
        ]
    }


lexers[SijLexer.name] = SijLexer(startinline=True)
lexers[RBNFLexer.name] = RBNFLexer(startinline=True)

extensions = ["sphinx.ext.mathjax"]
templates_path = ["_templates"]
master_doc = "guide"

todo_include_todos = True

Topics = [
    "PL",
    # 'Compiler',
    "Design",
    "DSL",
    "Fiction",
    "Others",
    "Backup",
]

html_theme = "bootstrap"
html_theme_path = sphinx_bootstrap_theme.get_html_theme_path()
html_title = "thautwarm's blog pages"
html_theme_options = {
    # Navigation bar title. (Default: ``project`` value)
    "navbar_site_name": f"{project}",
    "navbar_title": f"{project}",
    # Tab name for entire site. (Default: "Site")
    # A list of tuples containing pages or urls to link to.
    # Valid tuples should be in the following forms:
    #    (name, page)                 # a link to a page
    #    (name, "/aa/bb", 1)          # a link to an arbitrary relative url
    #    (name, "http://example.com", True) # arbitrary absolute url
    # Note the "1" or "True" value above as the third argument to indicate
    # an arbitrary url.
    "navbar_links":  [("GitHub", "https://github.com/thautwarm", True)]
    + [(topic, f"{topic}/index") for topic in Topics],
    # Render the next and previous page links in navbar. (Default: true)
    "navbar_sidebarrel": False,
    # Render the current pages TOC in the navbar. (Default: true)
    "navbar_pagenav": True,
    # Tab name for the current pages TOC. (Default: "Page")
    "navbar_pagenav_name": "Subsections",
    # Global TOC depth for "site" navbar tab. (Default: 1)
    # Switching to -1 shows all levels.
    "globaltoc_depth": -1,
    # Include hidden TOCs in Site navbar?
    #
    # Note: If this is "false", you cannot have mixed ``:hidden:`` and
    # non-hidden ``toctree`` directives in the same page, or else the build
    # will break.
    #
    # Values: "true" (default) or "false"
    "globaltoc_includehidden": "true",
    # HTML navbar class (Default: "navbar") to attach to <div> element.
    # For black navbar, do "navbar navbar-inverse"
    "navbar_class": "navbar navbar-inverse",
    # Fix navigation bar to top of page?
    # Values: "true" (default) or "false"
    "navbar_fixed_top": "false",
    # Location of link to source.
    # Options are "nav" (default), "footer" or anything else to exclude.
    "source_link_position": "footer",
    # Bootswatch (http://bootswatch.com/) theme.
    #
    # Options are nothing (default) or the name of a valid theme
    # such as "cosmo" or "sandstone".
    #
    # The set of valid themes depend on the version of Bootstrap
    # that's used (the next config option).
    #
    # Currently, the supported themes are:
    # - Bootstrap 2: https://bootswatch.com/2
    # - Bootstrap 3: https://bootswatch.com/3
    "bootswatch_theme": "readable",
    # "bootswatch_theme": "united",
    # Choose Bootstrap version.
    # Values: "3" (default) or "2" (in quotes)
    "bootstrap_version": "3",
}
# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
# html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".

html_static_path = ["static"]

html_favicon = "./favicon.ico"


# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# This is required for the alabaster theme
# refs: http://alabaster.readthedocs.io/en/latest/installation.html#sidebars
html_sidebars = {"**": []}


# -- Options for HTMLHelp output ------------------------------------------

# Output file base name for HTML help builder.
htmlhelp_basename = "site32_"


# -- Options for LaTeX output ---------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',
    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',
    # Additional stuff for the LaTeX preamble.
    #
    # 'preamble': '',
    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, f"{project}.tex", f"{project}", "thautwarm", "manual"),
]


# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [(master_doc, f"{project}", f"{project}", [author], 1)]


# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (
        master_doc,
        f"{project}",
        f"{project}",
        author,
        f"{project}",
        "Redy redy!",
        "Miscellaneous",
    ),
]


# -- Options for Epub output ----------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project
epub_author = author
epub_publisher = author
epub_copyright = copyright

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#
# epub_identifier = ''

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ["search.html"]


"""Inspect generated Manual HTML and hover data without a browser or Blueprint."""

import json
from html.parser import HTMLParser
from pathlib import Path
import sys
from urllib.parse import unquote, urlsplit


class Page(HTMLParser):
    def __init__(self, path):
        super().__init__(convert_charrefs=True)
        self.ids = []
        self.links = []
        self.hovers = []
        self.panels = []
        self.code = []
        self.in_code = False
        self.references = []
        self.in_reference = False
        self.spans = []
        self.current_span = None
        self.link_stack = []
        self.feed(path.read_text(encoding="utf-8"))

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "section" and "lsp-panel" in attrs.get("class", "").split():
            self.panels.append(attrs.get("id"))
        if tag == "a":
            self.links.append(attrs.get("href", ""))
            self.link_stack.append(attrs.get("href", ""))
            if self.in_reference:
                self.references[-1].append(attrs.get("href", ""))
        if "data-verso-hover" in attrs:
            self.hovers.append(attrs["data-verso-hover"])
        if tag == "code" and "lsp" in attrs.get("class", "").split():
            self.in_code = True
            self.code.append("")
            self.in_reference = "lsp-reference" in attrs.get("class", "").split()
            if self.in_reference:
                self.references.append(list(self.link_stack))
        if tag == "span" and self.in_code:
            self.current_span = [attrs.get("class", "").split(), ""]
            self.spans.append(self.current_span)

    def handle_endtag(self, tag):
        if tag == "code":
            self.in_code = False
            self.in_reference = False
        if tag == "span":
            self.current_span = None
        if tag == "a":
            self.link_stack.pop()

    def handle_data(self, data):
        if self.in_code:
            self.code[-1] += data
        if self.current_span is not None:
            self.current_span[1] += data


def local_target(parsed):
    assert not parsed.scheme and not parsed.netloc, parsed.geturl()
    # Manual's base element resolves site-relative links from the root.
    path = Path(unquote(parsed.path))
    if parsed.path.endswith("/") or not parsed.path:
        path /= "index.html"
    return path, unquote(parsed.fragment)


def rendered_files(root):
    return {p.relative_to(root): p.read_bytes() for p in root.rglob("*") if p.is_file()}


def check(output):
    for mode in ("html-single", "html-multi"):
        analyzed = rendered_files(output / "analyzed" / mode)
        replayed = rendered_files(output / "replayed" / mode)
        assert analyzed and analyzed == replayed, f"live and replayed {mode} output differ"
        root = output / "escaped" / mode
        pages = {p.relative_to(root): Page(p) for p in root.rglob("*.html")}
        if mode == "html-single":
            first = Path("index.html")
            page = pages[first]
            assert len(page.panels) == 2 and len(set(page.panels)) == 2, page.panels
            assert page.code.count("def sample := 1\n") == 2, page.code
            assert page.code.count("sample") == 2, page.code
        else:
            first = Path("first/index.html")
            first_page = pages[first]
            second = pages[Path("second/index.html")]
            assert len(first_page.panels) == len(second.panels) == 1
            assert len(first_page.references) == len(second.references) == 1
            assert second.hovers, "missing second-page reference"
        expected_target = first, pages[first].panels[0]
        docs = json.loads((root / "-verso-docs.json").read_text(encoding="utf-8"))
        hover_ids = [h for page in pages.values() for h in page.hovers]
        assert hover_ids, "missing hover attributes"
        for hover in hover_ids:
            content = docs[int(hover)] if isinstance(docs, list) else docs[hover]
            assert "&lt;script&gt;" in content and "<script>" not in content, content
        for path, page in pages.items():
            assert len(page.ids) == len(set(page.ids)), f"duplicate IDs in {path}"
            for href in page.links:
                parsed = urlsplit(href)
                if parsed.fragment.startswith("rosetta-"):
                    assert local_target(parsed) == expected_target, (path, href, expected_target)
        references = [links for page in pages.values() for links in page.references]
        assert len(references) == 2, references
        for links in references:
            assert len(links) == 1, f"reference must have exactly one definition link: {links}"
            assert local_target(urlsplit(links[0])) == expected_target, (links, expected_target)
    unicode = Page(output / "unicode/html-single/index.html")
    assert unicode.code == ["😀sample <>&\n  βeta\n"], unicode.code
    assert [text for classes, text in unicode.spans if "lsp-function" in classes] == ["sample"], unicode.spans
    print("Byte-identical replay, repeated IDs, first-occurrence links, hover escaping and Unicode HTML passed")


if __name__ == "__main__":
    check(Path(sys.argv[1]))

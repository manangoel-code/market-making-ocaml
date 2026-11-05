# Puts the compiled engine into the page, giving one file that can be opened or published.
# First run: opam exec -- dune build web/engine.bc.js --profile release
from pathlib import Path

web = Path(__file__).parent
engine = (web.parent / "_build/default/web/engine.bc.js").read_text(encoding="utf-8")
if "</script" in engine:
    engine = engine.replace("</script", r"<\/script")
page = (web / "page.html").read_text(encoding="utf-8").replace("/*ENGINE*/", engine)
(web / "hidden_value.html").write_text(page, encoding="utf-8")
print(f"web/hidden_value.html: {len(page) / 1024:.0f} KB")

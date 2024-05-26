import re
import collections
import os.path

SPLICING = re.compile(r"^([^ ]+):(\d+):(\d+)-(\d+): Splicing declarations$")
MODULE_NAME = re.compile(r"\b(?:[A-Z]\w*\.)+")

FORBIDDEN_IMPORTS: set[str] = set()

def remap_import(name: str) -> str:
    if name == "Data.Aeson.Types.Internal":
        return "Data.Aeson.Types"

    if name in ("Data.Aeson.Types.ToJSON", "Data.Aeson.Internal.ByteString", "Data.Aeson.TH"):
        return "Language.LSP.Protocol.AesonUnsafe"

    return name

def patch(input: str, src_dir: str):
    file_splices: dict[str, tuple[str, int, int, int, str]] = collections.defaultdict(list)

    with open(input, "r") as handle:
        header = SPLICING.match(handle.readline().rstrip())

        while True:
            file, start_line, start_col, end_col = header.groups()
            start_line = int(start_line)
            start_col = int(start_col)
            end_col = int(end_col)

            line = handle.readline()
            while line != "  ======>\n":
                line = handle.readline()

            body = []
            while True:
                line = handle.readline()
                if line == "" or (header := SPLICING.match(line.rstrip())):
                    break

                if not line.startswith("    "):
                    raise ValueError("Unexpected indent on line")

                body.append(line[4:])

            file_splices[file].append((start_line, start_col, end_col, "".join(body)))

            if line == "":
                break

    for file, splices in file_splices.items():
        splices.sort(reverse=True)

        with open(os.path.join(src_dir, file), "r") as h:
            lines = h.readlines()

        imports: set[str] = set()

        module_name = file.removeprefix("src/").removesuffix(".hs").replace("/", ".")

        for (start_line, start_col, end_col, body) in splices:
            line = lines[start_line - 1]

            # Include the previous bit of the line. The ranges given by GHC for $(...) are wrong, so
            # strip out the previous "$"
            before = line[:start_col - 1]
            if before.endswith("$"):
                before = before[:-2]

            chunk_imports = {match.group(0)[:-1] for match in re.finditer(MODULE_NAME, body)}
            for imported_module in chunk_imports:
                if imported_module == module_name or imported_module in FORBIDDEN_IMPORTS:
                    continue

                remapped_module = remap_import(imported_module)
                if remapped_module != imported_module:
                    body = body.replace(imported_module, remapped_module)

                imports.add(remapped_module)

            body = body.replace("aeson-2.1.2.1-1YB20WAhI7FDBnxJnrTmE5:", "")

            lines[start_line - 1] = before + body + line[end_col:]

        for idx, line in enumerate(lines):
            if line.startswith("import "):
                break
        else:
            raise ValueError("Cannot find imports")

        lines[0] = "{-# LANGUAGE MagicHash #-}\n" + lines[0]

        lines[idx] += "".join(f"import qualified {x}\n" for x in imports) + "import Data.Kind\n"

        with open(os.path.join(src_dir, file), "w") as h:
            h.writelines(lines)

if __name__ == '__main__':
    patch(
        input = "./dist-newstyle/build/x86_64-linux/ghc-9.4.8/lsp-types-2.2.0.0/build/ghc.dump.dump-splices",
        src_dir = "lsp-types"
    )
    patch(
        input = "./dist-newstyle/build/x86_64-linux/ghc-9.4.8/lsp-2.6.0.0/build/ghc.dump.dump-splices",
        src_dir = "lsp"
    )

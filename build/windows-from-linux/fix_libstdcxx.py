#!/usr/bin/env python3
"""Make every DLL in the Windows runtime load with the bundled C++ runtime (ANCHOR Studio 1.4.2).

conda-forge R 4.3 packages built with GCC 13 (sass, isoband, hunspell, sourcetools, vroom, tokenizers,
wordcloud) import std::ios_base_library_init() from libstdc++-6.dll. The GCC 14+ libstdc++ that newer packages
(httpuv, Rcpp, fixest, ...) need no longer exports that name on Windows, so Windows refuses to load those DLLs:
"LoadLibrary failure: The specified procedure could not be found." (Wine hides this by stubbing the import.)

In GCC 14+ the same work is done by std::ios_base::Init::Init(): its machine code is identical to GCC 13's
ios_base_library_init (a reference-counted, one-time set-up of the standard streams that never touches its
argument). This script renames the import in the affected DLLs to that function, then checks that every import
of every DLL in the runtime resolves against the DLLs shipped with it, and exits 1 if anything is still missing.

usage: fix_libstdcxx.py <runtime dir>        (needs the Python 'pefile' package)
"""
import glob, os, sys
import pefile

RENAME = {b"_ZSt21ios_base_library_initv": b"_ZNSt8ios_base4InitC1Ev"}   # old import -> equivalent export

def exports(path):
    pe = pefile.PE(path, fast_load=True); pe.parse_data_directories(directories=[pefile.DIRECTORY_ENTRY["IMAGE_DIRECTORY_ENTRY_EXPORT"]])
    if not hasattr(pe, "DIRECTORY_ENTRY_EXPORT"): return set()
    return {s.name.decode() for s in pe.DIRECTORY_ENTRY_EXPORT.symbols if s.name}

def imports(path):
    pe = pefile.PE(path, fast_load=True); pe.parse_data_directories(directories=[pefile.DIRECTORY_ENTRY["IMAGE_DIRECTORY_ENTRY_IMPORT"]])
    out = {}
    for e in getattr(pe, "DIRECTORY_ENTRY_IMPORT", []):
        out.setdefault(e.dll.decode().lower(), []).extend(i for i in e.imports if i.name)
    return out

def all_dlls(rt):
    pats = ["Library/bin/*.dll", "Library/mingw-w64/bin/*.dll", "lib/R/bin/x64/*.dll", "lib/R/library/*/libs/x64/*.dll", "*.dll"]
    return sorted({os.path.normpath(p) for pat in pats for p in glob.glob(os.path.join(rt, pat))})

def check(rt):
    dlls = all_dlls(rt); by_name = {}
    for d in dlls: by_name.setdefault(os.path.basename(d).lower(), d)
    cache = {}; missing = []
    for d in dlls:
        for dll, imps in imports(d).items():
            if dll not in by_name: continue                       # Windows system DLLs
            if dll not in cache: cache[dll] = exports(by_name[dll])
            miss = sorted({i.name.decode() for i in imps if i.name.decode() not in cache[dll]})
            if miss: missing.append((d, dll, miss))
    return missing

def patch(path, old, new):
    data = bytearray(open(path, "rb").read()); key = old + b"\x00"
    if data.count(key) != 1: raise SystemExit(f"{path}: expected one import name {old!r}, found {data.count(key)}")
    off = data.find(key)
    data[off - 2:off] = b"\x00\x00"                                   # hint (index guess): unknown
    data[off:off + len(key)] = new + b"\x00" * (len(key) - len(new))
    open(path, "wb").write(bytes(data)); return 1

def main(rt):
    for d, dll, miss in check(rt):
        for m in miss:
            new = RENAME.get(m.encode())
            if dll == "libstdc++-6.dll" and new and patch(d, m.encode(), new):
                print(f"patched {os.path.relpath(d, rt)}: {m} -> {new.decode()}")
    left = check(rt)
    for d, dll, m in left: print(f"UNRESOLVED {os.path.relpath(d, rt)} -> {dll}: {', '.join(m[:5])}")
    print("all imports resolve" if not left else f"{len(left)} DLLs still have unresolved imports")
    sys.exit(1 if left else 0)

if __name__ == "__main__": main(sys.argv[1])

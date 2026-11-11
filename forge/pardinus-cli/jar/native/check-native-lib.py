#!/usr/bin/env python3

import argparse
import re
import struct
import sys

MACHINES = {0x03: "i386", 0x3E: "x86_64", 0xB7: "aarch64", 0x28: "arm"}

SHT_DYNSYM = 11
SHT_DYNAMIC = 6
SHT_GNU_VERNEED = 0x6FFFFFFE
DT_NEEDED = 1
DT_RPATH = 15
DT_RUNPATH = 29
ET_DYN = 3
PT_GNU_STACK = 0x6474E551
PF_X = 1

EXPECTED_MIN_JNI = {
    "libminisat.so": 6,
    "libglucose.so": 6,
    "liblingeling.so": 6,
    "libminisatprover.so": 7,
}

ALLOWED_NEEDED = re.compile(
    r"^(libc\.so\.6|libm\.so\.6|libdl\.so\.2|libpthread\.so\.0|librt\.so\.1|"
    r"ld-linux[-\w.]*\.so\.\d+)$"
)

FORBIDDEN_VERSION_PREFIXES = ("GLIBCXX_", "CXXABI_", "GCC_")

class Elf:
    def __init__(self, path):
        with open(path, "rb") as f:
            self.data = f.read()
        d = self.data
        if d[:4] != b"\x7fELF":
            raise ValueError("not an ELF file")
        self.is64 = d[4] == 2
        if d[5] != 1:
            raise ValueError("big-endian ELF is not supported")
        if self.is64:
            hdr = struct.unpack_from("<HHIQQQIHHHHHH", d, 16)
            self.sh_fmt = "<IIQQQQIIQQ"
            self.dyn_fmt = "<qQ"
            self.sym_fmt = "<IBBHQQ"
        else:
            hdr = struct.unpack_from("<HHIIIIIHHHHHH", d, 16)
            self.sh_fmt = "<IIIIIIIIII"
            self.dyn_fmt = "<iI"
            self.sym_fmt = "<IIIBBH"
        self.e_type, self.e_machine = hdr[0], hdr[1]
        e_phoff, e_phentsize, e_phnum = hdr[4], hdr[8], hdr[9]
        e_shoff, e_shentsize, e_shnum = hdr[5], hdr[10], hdr[11]
        if e_phnum > 4096 or e_shnum > 65536:
            raise ValueError("implausible ELF header (e_phnum=%d, e_shnum=%d)" % (e_phnum, e_shnum))
        self.ph_fmt = "<IIQQQQQQ" if self.is64 else "<IIIIIIII"
        self.ph_flags_index = 1 if self.is64 else 6
        self.program_headers = []
        for i in range(e_phnum):
            ph = struct.unpack_from(self.ph_fmt, d, e_phoff + i * e_phentsize)
            self.program_headers.append((ph[0], ph[self.ph_flags_index]))
        self.machine = MACHINES.get(self.e_machine, "unknown(0x%x)" % self.e_machine)
        self.sections = []
        for i in range(e_shnum):
            f = struct.unpack_from(self.sh_fmt, d, e_shoff + i * e_shentsize)
            self.sections.append({"type": f[1], "offset": f[4], "size": f[5], "link": f[6], "entsize": f[9]})

    def string(self, strtab_index, offset):
        sec = self.sections[strtab_index]
        start = sec["offset"] + offset
        end = self.data.index(b"\0", start)
        return self.data[start:end].decode("ascii", "replace")

    def needed(self):
        out = []
        for sec in self.sections:
            if sec["type"] != SHT_DYNAMIC:
                continue
            size = struct.calcsize(self.dyn_fmt)
            for off in range(sec["offset"], sec["offset"] + sec["size"], size):
                tag, val = struct.unpack_from(self.dyn_fmt, self.data, off)
                if tag == DT_NEEDED:
                    out.append(self.string(sec["link"], val))
        return out

    def dynamic_tags(self):
        out = set()
        for sec in self.sections:
            if sec["type"] != SHT_DYNAMIC:
                continue
            size = struct.calcsize(self.dyn_fmt)
            for off in range(sec["offset"], sec["offset"] + sec["size"], size):
                tag, _val = struct.unpack_from(self.dyn_fmt, self.data, off)
                out.add(tag)
        return out

    def executable_stack(self):
        return any(t == PT_GNU_STACK and (flags & PF_X) for t, flags in self.program_headers)

    def version_needs(self):
        out = []
        for sec in self.sections:
            if sec["type"] != SHT_GNU_VERNEED:
                continue
            off = sec["offset"]
            while True:
                _version, cnt, _file, aux, nxt = struct.unpack_from("<HHIII", self.data, off)
                aoff = off + aux
                for _ in range(cnt):
                    _hash, _flags, _other, name, anext = struct.unpack_from("<IHHII", self.data, aoff)
                    out.append(self.string(sec["link"], name))
                    if anext == 0:
                        break
                    aoff += anext
                if nxt == 0:
                    break
                off += nxt
        return out

    def jni_exports(self):
        out = []
        for sec in self.sections:
            if sec["type"] != SHT_DYNSYM:
                continue
            size = struct.calcsize(self.sym_fmt)
            for off in range(sec["offset"], sec["offset"] + sec["size"], size):
                f = struct.unpack_from(self.sym_fmt, self.data, off)
                if self.is64:
                    name_off, shndx = f[0], f[3]
                else:
                    name_off, shndx = f[0], f[5]
                if shndx == 0:
                    continue
                name = self.string(sec["link"], name_off)
                if name.startswith("Java_"):
                    out.append(name)
        return out

def version_tuple(text):
    return tuple(int(p) for p in text.split("."))

def check(path, arch, max_glibc):
    try:
        return _check(path, arch, max_glibc)
    except (ValueError, OSError, struct.error, IndexError, OverflowError, MemoryError) as e:
        return None, ["cannot parse as a well-formed ELF file: %s" % e], {}

def _check(path, arch, max_glibc):
    problems = []
    elf = Elf(path)

    if elf.machine != arch:
        problems.append("built for %s, expected %s" % (elf.machine, arch))

    if elf.e_type != ET_DYN:
        problems.append("not a shared object (ELF type %d, expected ET_DYN)" % elf.e_type)

    tags = elf.dynamic_tags()
    if DT_RPATH in tags or DT_RUNPATH in tags:
        problems.append("has an RPATH/RUNPATH (it hardcodes search paths from the build machine)")

    if elf.executable_stack():
        problems.append("requests an executable stack (PT_GNU_STACK has the X flag)")

    needed = elf.needed()
    for n in needed:
        if not ALLOWED_NEEDED.match(n):
            problems.append("depends on %s (only glibc-family libraries are allowed)" % n)

    versions = elf.version_needs()
    for v in versions:
        if v.startswith(FORBIDDEN_VERSION_PREFIXES):
            problems.append("requires symbol version %s (C++ runtime or libgcc is not statically linked)" % v)

    glibc = sorted({version_tuple(v[len("GLIBC_"):]) for v in versions if re.match(r"^GLIBC_\d+(\.\d+)*$", v)})
    top = glibc[-1] if glibc else None
    if top is not None and top > version_tuple(max_glibc):
        problems.append("requires GLIBC_%s, newer than the allowed %s" % (".".join(map(str, top)), max_glibc))

    jni = elf.jni_exports()
    if not jni:
        problems.append("exports no JNI symbols (Java_*)")
    else:
        base = path.replace("\\", "/").split("/")[-1]
        expected = EXPECTED_MIN_JNI.get(base)
        if expected is not None and len(jni) < expected:
            problems.append("exports %d JNI symbols but %s is expected to export at least %d" % (len(jni), base, expected))

    info = {
        "machine": elf.machine,
        "needed": needed,
        "max_glibc": ".".join(map(str, top)) if top else "none",
        "jni": len(jni),
    }
    return elf, problems, info

def main(argv):
    ap = argparse.ArgumentParser(description="Check a Linux JNI solver library.")
    ap.add_argument("--arch", required=True, choices=["x86_64", "aarch64", "i386"])
    ap.add_argument("--max-glibc", default="2.31")
    ap.add_argument("libs", nargs="+")
    args = ap.parse_args(argv)

    failed = 0
    for lib in args.libs:
        _elf, problems, info = check(lib, args.arch, args.max_glibc)
        status = "FAIL" if problems else "ok"
        print("%-4s %s" % (status, lib))
        if info:
            print("       cpu=%s  glibc<=%s  jni-exports=%d  needs=%s"
                  % (info["machine"], info["max_glibc"], info["jni"], " ".join(info["needed"])))
        for p in problems:
            print("       PROBLEM: %s" % p)
        if problems:
            failed += 1
    if failed:
        print("%d of %d libraries FAILED" % (failed, len(args.libs)))
        return 1
    print("all %d libraries passed" % len(args.libs))
    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

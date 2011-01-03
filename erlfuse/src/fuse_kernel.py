#!/usr/bin/env python

# NOTE: you can probably ignore this script, its used to generate
# parts of the code in fuse_kernel.erl and fuse_kernel.hrl

# generates opcode_to_atom, encode, decode and records from fuse_kernel.h
# note that fuse_kernel.h must not include multiline comments! this
# makes it easier to parse, so fix that first.
# run ex: ./fuse_kernel.py fuse_kernel.h encode

# TODO: generate encode/decode for all fuse_*, usefull when debugging?
# TODO: Op output arg, not used yet.. dont know if it possible
# to generate smart encode code, op result output depends on many things

import sys

def out(s):
    sys.stdout.write(s)

# like range but includes argument if last element in sequence
def range_last(seq):
    for (e, n) in zip(seq, range(1, len(seq) + 1)):
        yield (e, n == len(seq))

def escape_atom(name):
    if name in ('end'): # more?
        return "'%s'" % name
    else:
        return name


class Op:
    def __init__(self, code, name, input, output):
        self.code = code
        self.name = name
        self.input = input
        self.output = output

    def decode(self, struct):
        s = ""
        s += "decode(%s," % self.name
        if len(self.input) > 0:
            s += "\n"
            s += "\t<<\n"
            for (i, last) in range_last(self.input):
                s += "\t"
                if type(i) is str:
                    s += "%s:?%s_SIZE/binary" % \
                            (i.capitalize(), i.upper())
                else:
                    s += "%s/binary" % i.var()
                if not last:
                    s += ","
                s += "\n"

            s += "\t>>) ->\n"
        else:
            s += " <<>>) ->\n"

        if len(self.input) == 0:
            s += "\tok"
        else:
            if len(self.input) > 1:
                s += "\t{\n"

            for (i, last) in range_last(self.input):
                s += "\t"
                if type(i) is str:
                    s += "decode(%s, %s)" % (i, i.capitalize())
                else:
                    s += i.decode(i.var())
                if not last:
                    s += ","
                if len(self.input) > 1:
                    s += "\n"

            if len(self.input) > 1:
                s += "\t}"

        return s

class Input_string_null:
    def var(self):
        return "Name"

    def decode(self, var):
        return "decode_name_null(%s)" % var

class Input_strings_null:
    def var(self):
        return "Names"

    def decode(self, var):
        return "decode_name_null2(%s)" % var

class Input_data:
    def var(self):
        return "Data"

    def decode(self, var):
        return var

input_string_null = Input_string_null()
input_strings_null = Input_strings_null()
input_data = Input_data()

ops = [
    Op(1, "lookup", [input_string_null], []),
    Op(2, "forget", ["fuse_forget_in"], []),
    Op(3, "getattr", [], []),
    Op(4, "setattr", ["fuse_setattr_in"], []),
    Op(5, "readlink", [], []),
    Op(6, "symlink", [input_strings_null], []),
    # 7
    Op(8, "mknod", ["fuse_mknod_in", input_string_null], []),
    Op(9, "mkdir", ["fuse_mkdir_in", input_string_null], []),
    Op(10, "unlink", [input_string_null], []),
    Op(11, "rmdir", [input_string_null], []),
    Op(12, "rename", ["fuse_rename_in", input_strings_null], []),
    Op(13, "link", ["fuse_link_in", input_string_null], []),
    Op(14, "open", ["fuse_open_in"], []),
    Op(15, "read", ["fuse_read_in"], []),
    Op(16, "write", ["fuse_write_in", input_data],[]),
    Op(17, "statfs", [], []),
    Op(18, "release", ["fuse_release_in"], []),
    # 19
    Op(20, "fsync", ["fuse_fsync_in"], []),
    Op(21, "setxattr", ["fuse_setxattr_in", input_strings_null], []),
    Op(22, "getxattr", ["fuse_getxattr_in", input_string_null], []),
    Op(23, "listxattr", ["fuse_getxattr_in"], []),
    Op(24, "removexattr", [input_string_null], []),
    Op(25, "flush", ["fuse_flush_in"], []),
    Op(26, "init", ["fuse_init_in"], []),
    Op(27, "opendir", ["fuse_open_in"], []),
    Op(28, "readdir", ["fuse_read_in"], []),
    Op(29, "releasedir", ["fuse_release_in"], []),
    Op(30, "fsyncdir", ["fuse_fsync_in"], []),
    Op(31, "getlk", ["fuse_lk_in"], []),
    Op(32, "setlk", ["fuse_lk_in"], []),
    Op(33, "setlkw", ["fuse_lk_in"], []),
    Op(34, "access", ["fuse_access_in"], []),
    Op(35, "create", ["fuse_open_in", input_string_null], []),
    Op(36, "interrupt", ["fuse_interrupt_in"], []),
    Op(37, "bmap", ["fuse_bmap_in"], []),
    Op(38, "destroy", [], []),
]

class Struct:
    def __init__(self, structs, name):
        self.structs = structs
        self.name = name
        self.members = []

    def find(self, name):
        for s in self.structs:
            if s.name == name:
                return s

        return None

    def size(self):
        s = 0

        for m in self.members:
            #print m.name, m.type, m.size()
            s += m.size()

        return s

    def decode(self):
        s = ""
        s += "decode(%s,\n" % self.name
        s += "\t<<\n"
        for (m, last) in range_last(self.members):
            s += "\t"
            if m.is_padding():
                s += "_"
            s += "%s%s" % (m.name.capitalize(), m.bintype())
            if not last:
                s += ","
            s += "\n"
        s += "\t>>) ->\n"
        s += "\t#%s{\n" % self.name

        # skip paddings
        members = filter(lambda x: not x.is_padding(), self.members)

        for (m, last) in range_last(members):
            s += "\t\t%s=" % escape_atom(m.name)
            if m.type.startswith("struct"):
                s += "decode(%s, %s)" % \
                        (m.type[len("struct "):], m.name.capitalize())
            else:
                s += m.name.capitalize()

            if not last:
                s += ","
            s += "\n"
        s += "\t}"

        return s

    def encode(self):

	# group members into continuous bin or list groups
	groups = []

	l = []
	lastgroup = ""
	for m in self.members:
            if m.type.startswith("struct") or m.type == "char":
		group = "list"
	    else:
		group = "bin"

	    if lastgroup != "" and lastgroup != group:
		groups.append((lastgroup, l))
		l = []
	    
	    l.append(m)

	    lastgroup = group

	groups.append((lastgroup, l))

	# count number of different continuous groups
	group_count = {}
	for (group, _) in groups:
	    group_count[group] = group_count.get(group, 0) + 1

	s = ""
        
	s += "encode(R) when is_record(R, %s) ->\n" % self.name

	# NOTE: special 64 bit alignment
	if self.name == "fuse_dirent":
	    s += "\tiolist_align(\n"

	# if there are both list and bin items or more then one list, we need a list
	if (group_count.get("bin", 0) > 0 and group_count.get("list", 0) > 0 or \
		group_count.get("list", 0) > 1):
	    s += "\t[\n"

	for ((group, members), last) in range_last(groups):
	    if group == "bin":
		s += "\t<<\n"
            
	    for (m, last2) in range_last(members):
		
		s += "\t"

		if m.type.startswith("struct"):
		    s += "encode(R#%s.%s)" % \
			    (self.name, escape_atom(m.name))
		else:
		    if m.is_padding():
			s += "0%s" % m.bintype()
		    else:
			if self.name == "fuse_out_header" and m.name == "error":
			    s += "(encode_errno(R#%s.%s))%s" % \
				    (self.name, escape_atom(m.name), m.bintype())
			elif m.type == "char":
			    s += "R#%s.%s" % \
				    (self.name, escape_atom(m.name))
			else:
			    s += "(R#%s.%s)%s" % \
				    (self.name, escape_atom(m.name), m.bintype())
		if len(self.members) > 1 or group == "bin":
		    if not last2:
			s += ","
		
		if m.is_padding():
		    s += " %% %s" % m.name

		if len(self.members) > 1 or group == "bin":
		    s += "\n"

	    if group == "bin":
		s += "\t>>"

	    if not last:
		s += ","
	    
	    if group == "bin" and not last:
		s += "\n"
	
	if group_count.get("bin", 0) > 0 and group_count.get("list", 0) > 0:
	    s += "\t]"

	# NOTE: special 64 bit alignment
	if self.name == "fuse_dirent":
	    s += ",\n\t64)"

        return s

    def record(self):
        s = ""
        s += "-record(%s, {\n" % self.name
        
        # skip paddings
        members = filter(lambda x: not x.is_padding(), self.members)
        
        for (m, last) in range_last(members):
            s += "\t%s" % escape_atom(m.name)
            if not last:
                s += ","
            s += " %% %s" % m.type
            if m.comment is not None:
                s += ", %s" % m.comment.strip()
            s += "\n"
        s += "\t})."

        return s


class Member:
    def __init__(self, struct, name, type, array_size, comment):
        self.struct = struct
        self.name = name
        self.type = type
        self.array_size = array_size
        self.comment = comment

    def is_complex(self):
        return (self.type == "char" and self.size is not None) \
                or self.type.startswith("struct")

    def is_padding(self):
        return self.name in ("padding", "spare", "dummy") or \
                self.name.startswith("unused")

    def size(self):
        if self.type.startswith("struct"):
            return self.struct.find(self.type[len("struct "):]).size()
        else:
            if self.type.startswith("__"):
                s = int(self.type[3:]) / 8
                if self.array_size is not None:
                    s *= self.array_size

                return s
            else:
                # probably char
                return self.array_size
            
    def bintype(self):
        type = []
        size = self.size()
        if self.is_complex():
            type.append("binary")
            s = ":?%s_SIZE" % self.type[len("struct "):].upper()
        else:
            if self.type.startswith("__s"):
                type.append("signed")
            type.append("native")
            size *= 8

            s = ""
            if size is not None:
                s = ":%d" % size

        return s + "/" + "-".join(type)

def parse(lines):
    structs = []
    struct = None

    for line in lines:
        line = line.strip()

        comment = None

        if line.find("/*") != -1:
            comment = line[line.find("/*") + 2:line.find("*/")]
            comment.strip()
            line = line[0:line.find("/*")] + line[line.find("*/") + 2:]

        line = line.strip()

        if struct:
            if line == "};":
                if struct is not None:
                    structs.append(struct)
                struct = None
            elif line.find("/*") == -1 and line.find("*/") != -1:
                pass
            else:
                s = line.split()
                name = s[-1][:-1] # strip ;
                type = " ".join(s[:-1])

                size = None
                if name.find("[") != -1:
                    size = int(name[name.find("[")+1:-1])
                    name = name[0:name.find("[")]

                struct.members.append(
                        Member(struct, name, " ".join(s[:-1]), size, comment))
        else:
            if line.startswith("struct"):
                s = line.split()
                struct = Struct(structs, s[1])
            else:
                pass
            
    return structs

def decode_all(ops, structs):
    return decode(ops, structs, True)

def decode(ops, structs, all=False):

    s = ""

    s += "decode_aux(<<Header:?FUSE_IN_HEADER_SIZE/binary, Rest/binary>> = Bin, Acc) ->\n"
    s += "\tH = decode(fuse_in_header, Header),\n"
    s += "\tLen = H#fuse_in_header.len - ?FUSE_IN_HEADER_SIZE,\n"
    s += "\tcase Rest of\n"
    s += "\t<<Body:Len/binary, NewBin/binary>> ->\n"
    s += "\t\tOpatom = opcode_to_atom(H#fuse_in_header.opcode),\n"
    s += "\t\tdecode_aux(NewBin, [{H, Opatom, decode(Opatom, Body)}|Acc]);\n"
    s += "\tRest ->\n"
    s += "\t\t{Acc, Bin}\n"
    s += "\tend;\n"
    s += "decode_aux(Bin, Acc) ->\n"
    s += "\t{lists:reverse(Acc), Bin}.\n"
    s += "decode(Bin) ->\n"
    s += "\tdecode_aux(Bin, []).\n"

    for op in ops:
        s += op.decode(structs[0])
        s += ";\n"

    if all:
        structs_filered = structs
    else:
        # only structs used in input
        structs_filered = filter(
                lambda x: x.name.endswith("_in") or \
                        x.name in ("fuse_in_header", "fuse_file_lock"), 
                structs)
        
    for (struct, last) in range_last(structs_filered):
        s += struct.decode()
        if last:
            s += "."
        else:
            s += ";"
        s += "\n"

    return s

def encode_all(ops, structs):
    return encode(ops, structs, True)

def encode(ops, structs, all=False):
    
    s = ""

    s += "encode({Header, Body}) ->\n"
    s += "\tBodyOut = encode(Body),\n"
    s += "\tLen = iolist_size(BodyOut) + ?FUSE_OUT_HEADER_SIZE,\n"
    s += "\tHeaderOut = encode(Header#fuse_out_header{len=Len}),\n"
    s += "\t[HeaderOut, BodyOut];\n"

    s += "% pass thru binaries\n"
    s += "encode(Bin) when is_binary(Bin) -> Bin;\n"
    s += "% pass thru strings\n"
    s += "encode([H|_T] = L) when is_integer(H) -> L;\n"
    s += "% encode list of records (also takes care of empty list)\n"
    s += "encode(L) when is_list(L) -> [encode(X) || X <- L];\n"

    if all:
        structs_filered = structs
    else:
        # only structs used in output
        structs_filered = filter(
                lambda x: x.name.endswith("_out") or \
                        x.name in ("fuse_kstatfs", "fuse_out_header", \
                                    "fuse_entry_out", "fuse_attr", "fuse_dirent"), 
                structs)

    for (struct, last) in range_last(structs_filered):
        s += struct.encode()
        if last:
            s += "."
        else:
            s += ";"
        s += "\n"

    return s

def opcode_to_atom(ops, structs):
    s = ""
    for (op, last) in range_last(ops):
        s += "opcode_to_atom(%d) -> %s" % (op.code, op.name)
        if last:
            s += "."
        else:
            s += ";"
        s += "\n"

    return s

def record(ops, structs):
    s = ""
    for struct in structs:
        s += struct.record()
        s += "\n\n"

    return s

def size(ops, structs):
    s = ""
    for struct in structs:
        s += "-define(%s_SIZE, %d).\n" % \
            (struct.name.upper(), struct.size())

    return s



structs = parse(file(sys.argv[1]).readlines())

s = {
"decode": decode,
"decode_all": decode_all,
"encode": encode,
"encode_all": encode_all,
"opcode_to_atom": opcode_to_atom,
"record": record,
"size": size
}[sys.argv[2]](ops, structs)
out(s)


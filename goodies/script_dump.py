import sys

TABLE_ADDR_FOMT = 0x080F89D4
TABLE_ADDR_MFOMT = 0x081014BC


def read_int(input, byte_count, signed=False):
    return int.from_bytes(input.read(byte_count), byteorder="little", signed=signed)


OPCODE_NAME_TABLE = {
    0x00: "nop",
    0x01: "equ",
    0x02: "addequ",
    0x03: "subequ",
    0x04: "mulequ",
    0x05: "divequ",
    0x06: "modequ",
    0x07: "add",
    0x08: "sub",
    0x09: "mul",
    0x0A: "div",
    0x0B: "mod",
    0x0C: "and",
    0x0D: "or",
    0x0E: "inc",
    0x0F: "dec",
    0x10: "neg",
    0x11: "not",
    0x12: "cmp",
    0x13: "pushm",
    0x14: "popm",
    0x15: "dup",
    0x16: "disc",
    0x17: "push",
    0x18: "jmp",
    0x19: "blt",
    0x1A: "ble",
    0x1B: "beq",
    0x1C: "bne",
    0x1D: "bge",
    0x1E: "bgt",
    0x1F: "jpi",
    0x20: "end",
    0x21: "call",
    0x22: "push16",
    0x23: "push8",
    0x24: "switch",
}

CONDITIONAL_BRANCHES = (0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E)
BRANCHES = (0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E)

OPCODE_OPERAND_SIZE_TABLE = {
    0x13: 4,  # push [A]
    0x14: 4,  # pop [A]
    0x17: 4,  # push imm32
    0x22: 2,  # push imm16
    0x23: 1,  # push imm8
    0x18: 4,  # b label
    0x19: 4,  # blt label
    0x1A: 4,  # ble label
    0x1B: 4,  # beq label
    0x1C: 4,  # bne label
    0x1D: 4,  # bge label
    0x1E: 4,  # bgt label
    0x21: 4,  # call id
    0x24: 4,  # switch id
}


def offof(addr: int) -> int:
    return addr & 0x01FFFFFF


def u32(b: bytes) -> int:
    return int.from_bytes(b[0:4], byteorder="little", signed=False)


def i32(b: bytes) -> int:
    return int.from_bytes(b[0:4], byteorder="little", signed=True)


def decode_jump_chunk(jump_data: bytes) -> list[tuple[int, dict[int, int]]]:
    i32s = [i32(jump_data[i:]) for i in range(0, len(jump_data), 4)]

    # head word is entry count
    count = i32s[0]

    # next are the offsets for each entry
    offs = [(i // 4) - count for i in i32s[1 : count + 1]]

    # the data starts with the offsets
    data = i32s[count + 1 :]

    jump_tables = []

    for off in offs:
        entries = data[off]
        default = data[off + 1]

        entry_map = {}

        for i in range(entries):
            compare = data[off + 2 + i * 2]
            target = data[off + 2 + i * 2 + 1]

            # print(f"{off} {compare} {target}")
            entry_map[compare] = target

        jump_tables.append((default, entry_map))

    return jump_tables


def read_str(data: bytes) -> bytes:
    end_off = 0

    while end_off < len(data) and data[end_off] != 0:
        end_off = end_off + 1

    return data[0:end_off]


def decode_str_chunk(str_data: bytes) -> list[bytes]:
    count = i32(str_data[0:])
    pool = str_data[4 + count * 4 :]

    string_table = []

    for i in range(count):
        off = i32(str_data[4 + i * 4 : 8 + i * 4])
        string_table.append(read_str(pool[off:]))

    return string_table


def decode_code_chunk(
    code_data: bytes,
) -> dict[int, tuple[int, tuple[int, bool] | None]]:
    pc = 0

    ins = {}

    head = i32(code_data[0:])
    code = code_data[4:]

    print(f"# Code len: {head} bytes")

    while pc < len(code):
        opcode = code[pc] & 0x7F
        offset_addressed = (code[pc] & 0x80) != 0

        operand_size = 0
        operand = None

        if opcode in OPCODE_OPERAND_SIZE_TABLE:
            operand_size = OPCODE_OPERAND_SIZE_TABLE[opcode]
            operand = (
                int.from_bytes(
                    code[pc + 1 : pc + 1 + operand_size],
                    byteorder="little",
                    signed=True,
                ),
                offset_addressed,
            )

        ins[pc] = (opcode, operand)
        pc = pc + 1 + operand_size

    return ins


def dump_script(f, table_addr, id):
    f.seek(offof(table_addr + id * 4))
    script_addr = read_int(f, 4)

    if script_addr == 0 and id == 0:
        return True

    if script_addr < 0x08000000 or script_addr > 0x09000000:
        return False

    f.seek(offof(script_addr))

    riff = f.read(4)
    size = read_int(f, 4)
    name = f.read(4)

    if riff != b"RIFF" or name != b"SCR ":
        return False

    print(f"# Script id: {id}")
    print(f"# Script addr: {script_addr:08X}")
    print(f"# Script size: {size} bytes")

    offset = 12

    chunks = {}

    while offset < size:
        chunk_name = f.read(4).decode("utf-8")
        chunk_size = read_int(f, 4)
        chunk_data = f.read(chunk_size)

        print(f"# Chunk '{chunk_name}' @ {offset}: {chunk_size} bytes")

        chunks[chunk_name] = chunk_data

        offset = offset + chunk_size + 8

    jump_table = decode_jump_chunk(chunks["JUMP"]) if "JUMP" in chunks else []
    str_table = decode_str_chunk(chunks["STR "]) if "STR " in chunks else []
    code_table = decode_code_chunk(chunks["CODE"]) if "CODE" in chunks else {}

    print(f"# String table: {str_table}")
    print(f"# Jump table: {jump_table}")
    # print(f"# Instructions: {code_table}")

    labels = {}

    for pc in code_table:
        opcode, operand = code_table[pc]

        if opcode in BRANCHES and operand is not None:
            if operand[0] < pc:
                labels[operand[0]] = f"loop_{operand[0]}"

            else:
                labels[operand[0]] = f"code_{operand[0]}"

    for i, jump_ent in enumerate(jump_table):
        if jump_ent[0] >= 0:
            labels[jump_ent[0]] = f"default_{jump_ent[0]}"

        for case in jump_ent[1]:
            target = jump_ent[1][case]
            labels[target] = f"case_{i}_{case}_{target}"

    had_end = False

    for pc in code_table:
        opcode, operand = code_table[pc]

        if pc in labels:
            print(f"{labels[pc]}:")

        name = (
            OPCODE_NAME_TABLE[opcode]
            if opcode in OPCODE_NAME_TABLE
            else f"err_{opcode:02X}"
        )

        warn = " (!)" if had_end and name != "nop" else ""
        had_end = had_end or name == "end"

        if operand is not None:
            if opcode in BRANCHES and operand[0] in labels:
                print(f"    /* {pc:04X} */ {name} {labels[operand[0]]}{warn}")
            else:
                print(f"    /* {pc:04X} */ {name} 0x{operand[0]:02X}{warn}")
        else:
            print(f"    /* {pc:04X} */ {name}{warn}")

    return True


def find_script_table_addr(f):
    f.seek(0xA0)  # ROM name in header
    game_title = f.read(12)

    if game_title == b"HARVESTMOGBA":
        return TABLE_ADDR_FOMT

    elif game_title == b"HM MFOM USA\0":
        return TABLE_ADDR_MFOMT

    return 0


def main(args: list[str]):
    try:
        rom_path = args[1]
        dump_id = int(args[2], base=0) if len(args) > 2 else None

    except IndexError:
        return f"Usage: {args[0]} ROM"

    with open(rom_path, "rb") as f:
        table_addr = find_script_table_addr(f)

        if table_addr == 0:
            return "Couldn't find script table (bad ROM?)"

        if dump_id is not None:
            if not dump_script(f, table_addr, dump_id):
                return f"Failed to dump script {dump_id}"

        else:
            for i in range(2000):  # big numb
                if not dump_script(f, table_addr, i):
                    break


if __name__ == "__main__":
    sys.exit(main(sys.argv))

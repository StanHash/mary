use std::io;

pub enum FomtVariant {
    FomtUs,
    MfomtUs,
}

fn read_u32(from: &[u8]) -> usize {
    (from[0] as usize)
        | ((from[1] as usize) << 8)
        | ((from[2] as usize) << 16)
        | ((from[3] as usize) << 24)
}

pub fn identify_rom(rom: &[u8]) -> Option<FomtVariant> {
    match &rom[0xA0..0xAC] {
        b"HARVESTMOGBA" => Some(FomtVariant::FomtUs),
        b"HM MFOM USA\0" => Some(FomtVariant::MfomtUs),
        _ => None,
    }
}

pub fn get_all_scripts(rom: &[u8]) -> io::Result<Vec<&[u8]>> {
    let variant = identify_rom(rom).unwrap();

    let (addr, size) = match variant {
        FomtVariant::FomtUs => (0x080F89D4, 1329),
        FomtVariant::MfomtUs => (0x081014BC, 1416),
    };

    let mut result = vec![];
    let table_bytes = &rom[addr & 0x01FFFFFF..];

    /* script 0 is NULL, so start with 1 */
    for i in 1..size {
        let script_addr = read_u32(&table_bytes[i * 4..]);
        let script_offs = script_addr & 0x01FFFFFF;

        if script_offs + 8 >= rom.len() {
            continue;
        }

        let riff_unbound = &rom[script_offs..];
        let riff_len = read_u32(&riff_unbound[4..8]);

        if script_offs + riff_len >= rom.len() {
            continue;
        }

        result.push(&riff_unbound[0..riff_len]);
    }

    Ok(result)
}

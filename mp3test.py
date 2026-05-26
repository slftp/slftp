import re

def get_number_of_disks_from_tag(word):
    """Simulates GetNumberOfDisksFromTag"""
    f_number = 0
    i = 0
    for i, ch in enumerate(word):
        if ch.isdigit():
            f_number = f_number * 10 + int(ch)
        else:
            break
    
    if f_number == 0:
        return "", 0
    
    # This is the f603fa19 check
    if f_number > 100:
        return "", 0
    
    # Skip 'x' if present (like 2xCD)
    if i < len(word) - 1 and word[i] == 'x':
        i += 1
    
    source_type = word[i:]
    return source_type, f_number


def parse_release(rlsname, section):
    # Simulate TRelease word splitting
    s = rlsname.replace('(', '').replace(')', '').replace('.', ' ').replace('-', ' ').replace('_', ' ')
    words = s.split()
    
    print(f"Release: {rlsname}")
    print(f"Words: {words}")
    
    # Simulate MP3 source detection
    # GlMP3Sources is empty in tests, so we only use GetNumberOfDisksFromTag
    mp3_source = ""
    mp3_numdisks = 1
    
    for i in range(len(words) - 1, 0, -1):
        word = words[i]
        source, numdisks = get_number_of_disks_from_tag(word)
        
        if numdisks != 0:
            mp3_source = source
            mp3_numdisks = numdisks
            print(f"  Found at word '{word}': source={source}, numdisks={numdisks}")
            break
    
    if not mp3_source:
        mp3_source = "CD"
    
    print(f"Result: source={mp3_source}, numdisks={mp3_numdisks}")
    print()
    return mp3_source, mp3_numdisks


# Test 1
source, disks = parse_release("VA-Serious_Beats_92-(541833CD)-4CD-FLAC-2019-WRE", "FLAC")
assert source == "CD", f"Expected CD, got {source}"
assert disks == 4, f"Expected 4, got {disks}"

# Test 2
source, disks = parse_release("The_Black_Mandala_-_Paradox-(CS132)-WEB-2020-ZzZz", "MP3")
assert source == "WEB", f"Expected WEB, got {source}"
assert disks == 1, f"Expected 1, got {disks}"

# Test from f603fa19
source, disks = parse_release("This_Is_The_Remix_Again.._(Remixes)-(5054197560477)-WEB-2023-GRP", "MP3")
assert source == "CD", f"Expected CD, got {source}"
assert disks == 1, f"Expected 1, got {disks}"

print("All tests passed!")
